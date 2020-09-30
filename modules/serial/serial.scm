#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; Cross platform rs232 (serial) bindings
;; This implementation assumes that the communication flow control is
;; completely controlled by the external device

(c-declare  #<<end-of-c-declare

// the timeout for reading a character is hardcoded here (in milliseconds)
#define SERIALTIMEOUT 10

#include <stdio.h>
#include <string.h>

#if defined(WINDOWS) || defined(WIN32)
#include <windows.h>
#endif

#if defined(LINUX)
#include <sys/file.h>
#endif

#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <termios.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/ioctl.h>
#endif

#ifdef OPENBSD
#include <sys/stat.h>
#endif

// baud rates
#define RS232_110BAUD     110
#define RS232_300BAUD     300
#define RS232_600BAUD     600
#define RS232_1200BAUD    1200
#define RS232_2400BAUD    2400
#define RS232_4800BAUD    4800
#define RS232_9600BAUD    9600
#define RS232_19200BAUD   19200
#define RS232_38400BAUD   38400
#define RS232_14400BAUD   14400
#define RS232_28800BAUD   28800
#define RS232_57600BAUD   57600
#define RS232_76800BAUD   76800
#define RS232_115200BAUD  115200

#ifndef BB10
#define RS232_230400BAUD  230400
#endif

// parity
#define RS232_NOPARITY 0
#define RS232_ODDPARITY 1
#define RS232_EVENPARITY 2

// stop bits
#define RS232_ONESTOPBIT 1
#define RS232_TWOSTOPBITS 2

// data bits
#define RS232_5BITS 5
#define RS232_6BITS 6
#define RS232_7BITS 7
#define RS232_8BITS 8

// function prototypes
int serial_open(char *dev, int baudrate, int bitsize, int parity, int stopbits);
int serial_openfile(char *filepath);
void serial_close(int dev);
void serial_writechar(int dev, int val);
int serial_readchar(int dev);
int serial_error();
int serial_timeout();
void serial_flush(int dev);
int serial_getDTR(int dev);
void serial_setDTR(int dev, int val);
int serial_getRTS(int dev);
void void_setRTS(int dev, int val);

// error codes
static int _serial_error, _serial_notready;

#ifdef ANDROID
// Android USB requires usb-serial-for-android library, which is not included here as it's LGPL!
// All serial functions are handled in JNI so nothing declared here.
// (However, two callbacks the other way around are needed if used.)
void serial_error_set(int error){ _serial_error=error; }
void serial_timeout_set(int timeout){  _serial_notready=timeout; }

#else
// open the serial device
int serial_open(char *dev, int baudrate, int bitsize, int parity, int stopbits){
  _serial_error=_serial_notready=0;

#ifdef WIN32
  HANDLE fd=0;
  DCB dcb;
  fd = CreateFile(dev, GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd==INVALID_HANDLE_VALUE) { _serial_error=1; return 0; }
  memset(&dcb,0,sizeof(dcb));
  dcb.DCBlength = sizeof(dcb);

  // configuring win32 serial communication:
  // miscellaneous control stuff
  dcb.fBinary = TRUE;
  dcb.fDsrSensitivity=FALSE;
  dcb.fNull=FALSE;
  dcb.fAbortOnError=FALSE;
  // this powers the lines, but does not actually do flow control
  // this is needed to power communication interface in some devices
  dcb.fDtrControl=DTR_CONTROL_ENABLE;
  dcb.fRtsControl=RTS_CONTROL_ENABLE;
  dcb.fOutxCtsFlow=FALSE;
  dcb.fOutxDsrFlow=FALSE;
  dcb.fOutX = FALSE;
  dcb.fInX = FALSE;

  // set the baudrate, falling back on 9600
  dcb.BaudRate = CBR_9600;
  switch (baudrate) {
    case RS232_110BAUD:  dcb.BaudRate = CBR_110; break;
    case RS232_300BAUD:  dcb.BaudRate = CBR_300; break;
    case RS232_600BAUD:  dcb.BaudRate = CBR_600; break;
    case RS232_1200BAUD:  dcb.BaudRate = CBR_1200; break;
    case RS232_2400BAUD:  dcb.BaudRate = CBR_2400; break;
    case RS232_4800BAUD:  dcb.BaudRate = CBR_4800; break;
    case RS232_19200BAUD:  dcb.BaudRate = CBR_19200; break;
    case RS232_38400BAUD:  dcb.BaudRate = CBR_38400; break;
    case RS232_14400BAUD:  dcb.BaudRate = CBR_14400; break;
    case RS232_57600BAUD:  dcb.BaudRate = CBR_57600; break;
    case RS232_115200BAUD:  dcb.BaudRate = CBR_115200; break;
  }

  // set the parity, falling back on none
  dcb.fParity = FALSE;
  dcb.Parity = NOPARITY;
  switch (parity) {
    case RS232_EVENPARITY:
       dcb.fParity = TRUE;
       dcb.Parity = EVENPARITY;
       break;
    case RS232_ODDPARITY:
       dcb.fParity = TRUE;
       dcb.Parity = ODDPARITY;
       break;
  }

  // set the stop bits, falling back on one
  dcb.StopBits = ONESTOPBIT;
  switch (stopbits) {
    case RS232_TWOSTOPBITS:
      dcb.StopBits = TWOSTOPBITS;
      break;
  }

  // set the bits size, falling back on 8
  dcb.ByteSize = 8;
  switch (bitsize) {
    case RS232_5BITS: dcb.ByteSize = 5; break;
    case RS232_6BITS: dcb.ByteSize = 6; break;
    case RS232_7BITS: dcb.ByteSize = 7; break;
  }

  // adjust driver buffers
  if (!SetupComm(fd,150000,150000)) {
    fprintf(stderr,"serial: SetupComm failed\n");
    _serial_error=1; return 0;
  }

  if (!SetCommState(fd,&dcb)) {
    fprintf(stderr,"serial: SetCommState failed\n");
    _serial_error=1; return 0;
  }

  // this is where we set the read timeout on windoze
    COMMTIMEOUTS timeouts;
    // these settings causes a read operation to return immediately
    timeouts.ReadIntervalTimeout = MAXDWORD;
    timeouts.ReadTotalTimeoutMultiplier = 0;
    timeouts.ReadTotalTimeoutConstant = 0;
    timeouts.WriteTotalTimeoutMultiplier = 0;
    timeouts.WriteTotalTimeoutConstant = 0;
    if (!SetCommTimeouts(fd,&timeouts)) {
      fprintf(stderr,"serial: SetCommTimeouts failed\n");
      _serial_error=1; return 0;
     }
#endif  // WIN32

#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  struct termios my_termios;
  int fd=0,spd;

#if defined(LINUX) || defined(BB10)
 fd = open(dev, O_RDWR | O_NOCTTY | O_NONBLOCK );
 // this blocks if already locked by another process:
 // flock(fd,LOCK_EX);
 // try to prevent blocking:
 if (flock(fd,LOCK_EX|LOCK_NB)==-1) { _serial_error=1; return 0; }
#else
 fd = open(dev, O_RDWR | O_NOCTTY | O_NONBLOCK | O_EXLOCK );
#endif
  if (fd<0) { _serial_error=1; return 0; }

  bzero(&my_termios, sizeof(my_termios));

  // configuring OPENBSD/LINUX/MACOSX serial communication:
  // misc control settings
  my_termios.c_iflag= IGNBRK;
  my_termios.c_oflag= 0;
  my_termios.c_lflag= 0;
  my_termios.c_cflag = CREAD | CLOCAL ;

  // set the baudrate, falling back to 9600
  switch (baudrate) {
    case RS232_110BAUD: spd = B110; break;
    case RS232_300BAUD: spd = B300; break;
    case RS232_600BAUD: spd = B600; break;
    case RS232_1200BAUD: spd = B1200; break;
    case RS232_2400BAUD: spd = B2400; break;
    case RS232_4800BAUD: spd = B4800; break;
    case RS232_9600BAUD: spd = B9600; break;
    case RS232_19200BAUD: spd = B19200; break;
    case RS232_38400BAUD: spd = B38400; break;
    case RS232_57600BAUD: spd = B57600; break;
    case RS232_115200BAUD: spd = B115200; break;
#ifndef BB10
    case RS232_230400BAUD: spd = B230400; break;
#endif
    default: spd = B9600; break;
  }
  cfsetospeed(&my_termios, (speed_t)spd);
  cfsetispeed(&my_termios, (speed_t)spd);

  // parity, falling back on none
  my_termios.c_cflag &= ~(PARENB | PARODD);
  switch (parity) {
    case RS232_EVENPARITY:
       my_termios.c_cflag |= PARENB;
       break;
    case RS232_ODDPARITY:
       my_termios.c_cflag |= PARODD;
       break;
  }

  // stopbits, falling back on 1
  switch (stopbits) {
     case RS232_TWOSTOPBITS:
       my_termios.c_cflag |= CSTOPB;
       break;
  }

  // enable hardware flow control
  //  my_termios.c_cflag |= CRTSCTS;

  // bits, falling back on 8
  switch (bitsize) {
     case RS232_5BITS: my_termios.c_cflag = ( my_termios.c_cflag & ~CSIZE) | CS5; break;
     case RS232_6BITS: my_termios.c_cflag = ( my_termios.c_cflag & ~CSIZE) | CS6; break;
     case RS232_7BITS: my_termios.c_cflag = ( my_termios.c_cflag & ~CSIZE) | CS7; break;
     default: my_termios.c_cflag = ( my_termios.c_cflag & ~CSIZE) | CS8; break;
  }

  tcflush(fd, TCIOFLUSH);
  if (tcsetattr(fd, TCSANOW, &my_termios)) {
    //printf("ERROR: serial: tcsetattr() failed\n");
  }
#endif   // MACOSX, LINUX, OPENBSD

  return (int)fd;
}

// open a connection to a file
int serial_openfile(char *filepath){
#ifdef WIN32
  return (int)CreateFile(filepath, GENERIC_READ|GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
#else
  return open(filepath, O_RDWR);
#endif
}

void serial_close(int d){
  _serial_error=_serial_notready=0;
#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  if (!fd) { _serial_error=1;  return; }
  CloseHandle(fd);
#endif
#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (!fd) { _serial_error=1;  return; }
  if (close(fd)) {
    //printf("ERROR: serial: close failed err=%i [%s]\n",errno,strerror(errno));
    _serial_error=1;
  }
#endif
}

// write data to the serial port
void serial_writechar(int d, int val){
  unsigned char buf = (unsigned char)val;
  _serial_error=_serial_notready=0;

#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  if (!fd) { _serial_error=1; return; }
  DWORD n_written;
  if(!WriteFile(fd,&buf,1,&n_written,NULL)) {
    if(GetLastError() != ERROR_IO_PENDING) { _serial_error=1; return; }
    if (n_written!=1) { _serial_error=1; return; }
  //  FlushFileBuffers(fd);
  }
#endif

#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (!fd) { _serial_error=1; return; }
  ssize_t n_written=0;
  n_written = write(fd,&buf,1);
#endif
  if (n_written!=1) {
    // printf("writechar: errno=%i [%s]\n",errno,strerror(errno));
    _serial_error=1;
  }
}

// read data from the serial port
int serial_readchar(int d){
  unsigned char buf[1]={0};
  _serial_error=_serial_notready=0;

#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  DWORD n_read;
  if (!ReadFile(fd,&buf,1,&n_read,NULL)) {
    if(GetLastError() == ERROR_IO_PENDING)
      _serial_notready=1;
    else
     _serial_error=1;
  }
  if (n_read==0)
   _serial_notready=1;
#endif

#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (read(fd,&buf,1)!=1) {
    if (errno==35) {
      _serial_notready=1;
    }else {
      _serial_error=1;
      // printf("readchar: errno=%i [%s]\n",errno,strerror(errno));
    }
  }
#endif
  return (int)buf[0];
}

void serial_flush(int d){
  #ifdef WIN32
    HANDLE fd=(HANDLE)d;
    PurgeComm(fd, PURGE_RXABORT | PURGE_RXCLEAR | PURGE_TXABORT | PURGE_TXCLEAR);
  #else
    int fd=d;
    tcflush(fd, TCIOFLUSH);
  #endif
}

// Change and query control lines
int serial_getDTR(int d){
  _serial_error=_serial_notready=0;
#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  if (!fd) { _serial_error=1;  return 0; }
  DCB dcb;
  if (!GetCommState(fd,&dcb)){
    fprintf(stderr,"serial: GetCommState failed\n");
    _serial_error=1; return 0;
  }
  return dcb.fDtrControl;
#endif
#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (!fd) { _serial_error=1;  return 0; }
  int serial;
  ioctl(fd, TIOCMGET, &serial);
  return (serial & TIOCM_DTR);
#endif
}

int serial_getRTS(int d){
  _serial_error=_serial_notready=0;
#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  if (!fd) { _serial_error=1;  return 0; }
  DCB dcb;
  if (!GetCommState(fd,&dcb)){
    fprintf(stderr,"serial: GetCommState failed\n");
    _serial_error=1; return 0;
  }
  return dcb.fRtsControl;
#endif
#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (!fd) { _serial_error=1;  return 0; }
  int serial;
  ioctl(fd, TIOCMGET, &serial);
  return (serial & TIOCM_RTS);
#endif
}

void serial_setDTR(int d, int s){
  _serial_error=_serial_notready=0;
#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  if (!fd) { _serial_error=1;  return; }
  DCB dcb;
  if (!GetCommState(fd,&dcb)){
    fprintf(stderr,"serial: GetCommState failed\n");
    _serial_error=1; return;
  }
  dcb.fDtrControl=DTR_CONTROL_ENABLE;
  if (!SetCommState(fd,&dcb)) {
    fprintf(stderr,"serial: SetCommState failed\n");
    _serial_error=1; return;
  }
#endif
#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (!fd) { _serial_error=1;  return; }
  if (s==0) {
    ioctl(fd, TIOCMBIC, TIOCM_DTR);
  } else {
   ioctl(fd, TIOCMBIS, TIOCM_DTR);
  }
#endif
}

void serial_setRTS(int d, int s){
  _serial_error=_serial_notready=0;
#ifdef WIN32
  HANDLE fd=(HANDLE)d;
  if (!fd) { _serial_error=1;  return; }
  DCB dcb;
  if (!GetCommState(fd,&dcb)){
    fprintf(stderr,"serial: GetCommState failed\n");
    _serial_error=1; return;
  }
  dcb.fRtsControl=RTS_CONTROL_ENABLE;
  if (!SetCommState(fd,&dcb)) {
    fprintf(stderr,"serial: SetCommState failed\n");
    _serial_error=1; return;
  }
#endif
#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD) || defined(LINUX) || defined(MACOSX) || defined(IOS) || defined(BB10)
  int fd=d;
  if (!fd) { _serial_error=1;  return; }
  if (s==0) {
    ioctl(fd, TIOCMBIC, TIOCM_RTS);
  } else {
   ioctl(fd, TIOCMBIS, TIOCM_RTS);
  }
#endif
}
#endif // ANDROID

int serial_error(void) { return _serial_error; }
int serial_timeout(void) { return _serial_notready; }

// eof

end-of-c-declare
)

(define serial:open (c-lambda (char-string int int int int) int "serial_open"))
(define serial:openfile (c-lambda (char-string) int "serial_openfile"))
(define serial-close (c-lambda (int) void "serial_close"))

(define serial-readchar (c-lambda (int) int "serial_readchar"))
(define serial-writechar (c-lambda (int int) void "serial_writechar"))
(define serial-flush (c-lambda (int) void "serial_flush"))

(define serial:error (c-lambda () int "serial_error"))
(define serial:timeout (c-lambda () int "serial_timeout"))
(define (serial-error) (not (fx= (serial:error) 0)))
(define (serial-timeout) (not (fx= (serial:timeout) 0)))

(define serial-dtr-set! (c-lambda (int bool) void "serial_setDTR"))
(define serial-dtr-get (c-lambda (int) bool "serial_getDTR"))
(define serial-rts-set! (c-lambda (int bool) void "serial_setRTS"))
(define serial-rts-get (c-lambda (int) bool "serial_getRTS"))

;; ----------------------
;; device autodetection
;; list of devices to autodect on - this is platform dependent...
(define serial:devicelist '("/dev/ttyUSB0" "/dev/ttyU0" "/dev/ttyS0"))
(define serial:ratelist   '(9600 19200 38400 115200))

;; autodect list
;;  (baudrate databits parity stopbits)
(define serial:commlist '(
  (9600   8 0 1)
  (19200  8 0 1)
  (19200  8 2 1)
  (38400  8 0 1)
  (115200 8 0 1)
))

(define (serial-try devname baudrate databits parity stopbits testproc)
  (let ((dev (serial:open devname baudrate databits parity stopbits)))
    (if (or (serial-error) (serial-timeout))
      #f
      (if testproc
        (if (testproc dev)
          dev
          (begin (serial-close dev) #f)
        )
        dev
      )
    )
  ))

(define (serial-tryfile filepath)
  (let ((dev (serial:openfile filepath)))
     (if (or (serial-error) (serial-timeout)) #f
        dev)))


(define (serial-autodetect devname testproc)
;;  (log-status "serial: starting device detection..")
  (let loop ((d (if devname (list devname) serial:devicelist)))
    (if (fx= (length d) 0)
      (begin
        (log-warning "serial: device autodetection failed")
        #f
      )
      (let ((res (let loop2 ((r serial:commlist))
        (if (fx= (length r) 0)
          #f
          (let ((dev (serial-try (car d) (car (car r))
                (cadr (car r)) (caddr (car r)) (cadddr (car r)) testproc)))
            (if dev
              (begin
                (log-status (string-append "serial: found device at " (car d)
                                           " using " (number->string (car (car r)))
                                           " " (number->string (cadr (car r)))
                                           (if (fx= (caddr (car r)) 0) "N" (if (fx= (caddr (car r)) 1) "O" "E"))
                                           (number->string  (cadddr (car r)))
                ))
                dev
              )
              (loop2 (cdr r))
            )
          )
        ))))
        (if res res (loop (cdr d)))
      )
    )
  ))

;; Find a usb-serial converter on macos and linux
(define (detect-usb-serial)
  (let loop ((files (directory-files "/dev")))
    (if (fx= (length files) 0)
      ""
      (let ((file (car files)))
        (if (and (string-contains file "tty")
                 (or (string-contains-ci file "serial") (string-contains-ci file "usb")))
          (string-append "/dev/" file)
          (loop (cdr files))
        )
      )
    )
  ))

;; rs232 communication protocol often define a start and end character
;; this generic cache system can capture such delimited messages
(define serial:cache (make-table))

(define (serial-cache-setup dev char1 char2)
  (table-set! serial:cache dev (list char1 char2 #f)))

(define (serial-cache-clear dev)
  (let* ((entry (table-ref serial:cache dev #f))
         (c1  (if entry (car entry) #f))
         (c2  (if entry (cadr entry) #f)))
    (if entry
      (table-set! serial:cache dev (list c1 c2 #f))
      (log-error "serial-cache-clear: cache not initialized"))
  ))

(define (serial-cache-read dev . hook)
  (let* ((entry (table-ref serial:cache dev #f))
         (c1  (if entry (car entry) #f))
         (c2  (if entry (cadr entry) #f))
         (buf (if entry (caddr entry) #f))
         (hookproc (if (fx= (length hook) 1) (car hook) #f)))
    (if entry
      (call/cc (lambda (return)
        ;; step 1: seek start char
        (if (not buf)
          (let loop ()
            (let ((c (serial-readchar dev)))
              (if (or (serial-timeout) (serial-error)) (return #f))
              (if hookproc (hookproc c))
              (if (if (procedure? c1) (c1 c) (fx= c c1)) (set! buf (string (integer->char c))) (loop))
            )
          )
        )
        ;; step 2: seek end char
        (let loop2 ((r (if buf buf "")))
          (let ((c (serial-readchar dev)))
            (if (or (serial-timeout) (serial-error)) (begin
               (table-set! serial:cache dev (list c1 c2 r))
               (return #f)
            ))
            (if hookproc (hookproc c))
            (if (if (procedure? c2) (c2 c (fx+ (string-length r) 1)) (fx= c c2)) (begin
              (serial-cache-clear dev)
              (return (if (fx<= (string-length r) 1)
                #f
                (string-append r (string (integer->char c)))
              ))
            ))
            (loop2 (string-append r (string (integer->char c))))
          )
        )
      ))
      (begin
        (log-error "serial-cache-read: cache not initialized")
        #f
      )
    )
  ))

;; As the timing accuracy of serial may depend on frequent runs make
;; sure this behavior does not change with new glgui-event sleeps.
(eval-if-exists "glgui-timings-at-10msec!")

;; eof
