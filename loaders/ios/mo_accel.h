/*----------------------------------------------------------------------------
  MoMu: A Mobile Music Toolkit
  Copyright (c) 2010 Nicholas J. Bryan, Jorge Herrera, Jieun Oh, and Ge Wang
    All rights reserved.
    http://momu.stanford.edu/toolkit/
 
  Mobile Music Research @ CCRMA
  Music, Computing, Design Group
  Stanford University
    http://momu.stanford.edu/
    http://ccrma.stanford.edu/groups/mcd/
 
  MoMu is distributed under the following BSD style open source license:
 
  Permission is hereby granted, free of charge, to any person obtaining a 
  copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:
 
  The authors encourage users of MoMu to include this copyright notice,
  and to let us know that you are using MoMu. Any person wishing to 
  distribute modifications to the Software is encouraged to send the 
  modifications to the original authors so that they can be incorporated 
  into the canonical version.

  The Software is provided "as is", WITHOUT ANY WARRANTY, express or implied,
  including but not limited to the warranties of MERCHANTABILITY, FITNESS
  FOR A PARTICULAR PURPOSE and NONINFRINGEMENT.  In no event shall the authors
  or copyright holders by liable for any claim, damages, or other liability,
  whether in an actino of a contract, tort or otherwise, arising from, out of
  or in connection with the Software or the use or other dealings in the 
  software.
 -----------------------------------------------------------------------------*/
//-----------------------------------------------------------------------------
// name: mo_accel.h
// desc: MoPhO API for accelerometer
//
// authors: Nick Bryan
//          Jieun Oh
//          Jorge Herrera
//          Ge Wang
//
//    date: Fall 2009
//    version: 1.0.0
//
// Mobile Music research @ CCRMA, Stanford University:
//     http://momu.stanford.edu/
//-----------------------------------------------------------------------------
#ifndef __MO_ACCEL_H__
#define __MO_ACCEL_H__

#include "mo_def.h"
#import <UIKit/UIKit.h>

//#include <vector>


@interface AccelDelegate : NSObject <UIAccelerometerDelegate>
{ }
@end


// type definition for accelerometer callback function
typedef void (* MoAccelCallback)( double x, double y, double z, void * data );


//-----------------------------------------------------------------------------
// name: class MoAccel
// desc: accelerometer stuff (singleton)
//-----------------------------------------------------------------------------
class MoAccel
{
public: // setting values
    
    // set the global update interval
    static void setUpdateInterval( double seconds );
    // get the global update interval
    static double getUpdateInterval();

public: // getting values

    // returns current (x,y,z) values (for polling)
    static void getXYZ( double & px, double & py, double & pz );
    // returns current (x) value (for polling)
    static double getX();
    // returns current (y) value (for polling)
    static double getY();
    // returns current (z) value (for polling)
    static double getZ();

public: // callbacks
    
    // register a callback to be invoked on subsequent updates
    static void addCallback( const MoAccelCallback & callback, void * data );
    // unregister a callback
    static void removeCallback( const MoAccelCallback & callback );
    static const double MAX_ACCEL_RATE;

public:
    // this should be called on new values (UIAccelerometerDelegate)
    static void update( double x, double y, double z );

private:
    // check if one time set up is needed
    static void checkSetup();

    // current values;
    static double m_x;
    static double m_y;
    static double m_z;

    // update interval
    static double m_updateInterval;

    // accelerometer Delegate
    static AccelDelegate * accelDelegate;

    // queue of callbacks
//    static std::vector<MoAccelCallback> m_clients;
//    static std::vector<void *> m_clientData;
};


#endif
