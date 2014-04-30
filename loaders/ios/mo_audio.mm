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
// name: mo_audio.cpp
// desc: MoPhO audio layer
//       - adapted from the smule audio layer & library (SMALL)
//         (see original header below)
//
// authors: Ge Wang (ge@ccrma.stanford.edu | ge@smule.com)
//          Spencer Salazar (spencer@smule.com)
//    date: October 2009
//    version: 1.0.0
//
// Mobile Music research @ CCRMA, Stanford University:
//     http://momu.stanford.edu/
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// name: small.cpp (original)
// desc: the smule audio layer & library (SMALL)
//
// created by Ge Wang on 6/27/2008
// re-implemented using Audio Unit Remote I/O on 8/12/2008
// updated for iPod Touch by Spencer Salazar and Ge wang on 8/10/2009
//-----------------------------------------------------------------------------
#include "mo_audio.h"
#include <AudioToolbox/AudioToolbox.h>
#include <AVFoundation/AVAudioSession.h>

extern int iphone_audioroute;

// static member initialization
bool MoAudio::m_hasInit = false;
bool MoAudio::m_isRunning = false;
bool MoAudio::m_isMute = false;
bool MoAudio::m_handleInput = false;
Float64 MoAudio::m_srate = 44100.0;
Float64 MoAudio::m_hwSampleRate = 44100.0;
UInt32 MoAudio::m_frameSize = 0;
UInt32 MoAudio::m_numChannels = 2;
AudioUnit MoAudio::m_au;
MoAudioUnitInfo * MoAudio::m_info = NULL;
MoCallback MoAudio::m_callback = NULL;
// Float32 * MoAudio::m_buffer = NULL;
// UInt32 MoAudio::m_bufferFrames = 2048;
AURenderCallbackStruct MoAudio::m_renderProc;
void * MoAudio::m_bindle = NULL;

// number of buffers
#define MO_DEFAULT_NUM_BUFFERS   3

// prototypes
bool setupRemoteIO( AudioUnit & inRemoteIOUnit, AURenderCallbackStruct inRenderProc,
                    AudioStreamBasicDescription & outFormat);

//-----------------------------------------------------------------------------
// name: setMicrophoneGain()
// desc: set input gain (0 ~ 1.0)
//-----------------------------------------------------------------------------
extern "C" void setMicrophoneGain(Float32 inputGain)
{
    UInt32 ui32propSize = sizeof(UInt32);
    UInt32 inputGainAvailable = 0;

    AudioSessionGetProperty(kAudioSessionProperty_InputGainAvailable
                            , &ui32propSize
                            , &inputGainAvailable);

    if (inputGainAvailable) {
        AudioSessionSetProperty(kAudioSessionProperty_InputGainScalar
                             ,sizeof(inputGain)  
                            , &inputGain);
    } 
}

extern "C" float getMicrophoneGain()
{
	AVAudioSession* avSession = [AVAudioSession sharedInstance];
	return avSession.inputGain;
}

//-----------------------------------------------------------------------------
// name: silenceData()
// desc: zero out a buffer list of audio data
//-----------------------------------------------------------------------------
void silenceData( AudioBufferList * inData )
{
    for( UInt32 i = 0; i < inData->mNumberBuffers; i++ )
        memset( inData->mBuffers[i].mData, 0, inData->mBuffers[i].mDataByteSize );
}




//-----------------------------------------------------------------------------
// name: convertToUser()
// desc: convert to user data (stereo)
//-----------------------------------------------------------------------------
void convertToUser( AudioBufferList * inData, Float32 * buffy, 
                    UInt32 numFrames, UInt32 & actualFrames )
{
    // make sure there are exactly two channels
    assert( inData->mNumberBuffers == MoAudio::m_numChannels );
    // get number of frames
    UInt32 inFrames = inData->mBuffers[0].mDataByteSize / sizeof(SInt32);
    // make sure enough space
    assert( inFrames <= numFrames );
    // channels
    SInt32 * left = (SInt32 *)inData->mBuffers[0].mData;
    SInt32 * right = (SInt32 *)inData->mBuffers[1].mData;
    // fixed to float scaling factor
    Float32 factor = (Float32)(1 << 24);
    // interleave (AU is by default non interleaved)
    for( UInt32 i = 0; i < inFrames; i++ )
    {
        // convert (AU is by default 8.24 fixed)
        buffy[2*i] = ((Float32)left[i]) / factor;
        buffy[2*i+1] = ((Float32)right[i]) / factor;
    }
    // return
    actualFrames = inFrames;
}




//-----------------------------------------------------------------------------
// name: convertFromUser()
// desc: convert from user data (stereo)
//-----------------------------------------------------------------------------
void convertFromUser( AudioBufferList * inData, Float32 * buffy, UInt32 numFrames )
{
    // make sure there are exactly two channels
    assert( inData->mNumberBuffers == MoAudio::m_numChannels );
    // get number of frames
    UInt32 inFrames = inData->mBuffers[0].mDataByteSize / 4;
    // make sure enough space
    assert( inFrames <= numFrames );
    // channels
    SInt32 * left = (SInt32 *)inData->mBuffers[0].mData;
    SInt32 * right = (SInt32 *)inData->mBuffers[1].mData;
    // fixed to float scaling factor
    Float32 factor = (Float32)(1 << 24);
    // interleave (AU is by default non interleaved)
    for( UInt32 i = 0; i < inFrames; i++ )
    {
        // convert (AU is by default 8.24 fixed)
        left[i] = (SInt32)(buffy[2*i] * factor);
        right[i] = (SInt32)(buffy[2*i+1] * factor);
    }
}




//-----------------------------------------------------------------------------
// name: SMALLRenderProc()
// desc: callback procedure awaiting audio unit audio buffers
//-----------------------------------------------------------------------------
static OSStatus SMALLRenderProc(
    void * inRefCon, 
    AudioUnitRenderActionFlags * ioActionFlags, 
    const AudioTimeStamp * inTimeStamp, 
    UInt32 inBusNumber, 
    UInt32 inNumberFrames, 
    AudioBufferList * ioData ) 
{
    OSStatus err = noErr;

    // render if full-duplex available and enabled
    if( MoAudio::m_handleInput )
    {
        err = AudioUnitRender( MoAudio::m_au, ioActionFlags, inTimeStamp, 1, inNumberFrames, ioData );
        if( err )
        {
            // print error
            printf( "MoAudio: render procedure encountered error %d\n", (int)err );
            return err;
        }
    }

    // actual frames
    UInt32 actualFrames = 0;
    
    // convert
    convertToUser( ioData, MoAudio::m_info->m_ioBuffer, MoAudio::m_info->m_bufferSize, actualFrames );
    
    // callback
    MoAudio::m_callback( MoAudio::m_info->m_ioBuffer, actualFrames, MoAudio::m_bindle );
    
    // convert back
    convertFromUser( ioData, MoAudio::m_info->m_ioBuffer, MoAudio::m_info->m_bufferSize );
    
    // mute?
    if( MoAudio::m_isMute )
    { silenceData( ioData ); }
    
    return err;
}




//-----------------------------------------------------------------------------
// name: rioInterruptionListener()
// desc: handler for interruptions to start and end
//-----------------------------------------------------------------------------
static void rioInterruptionListener( void * inUserData, UInt32 inInterruption )
{
    // printf( "Session interrupted! --- %s ---", 
    //     inInterruption == kAudioSessionBeginInterruption 
    //     ? "Begin Interruption" : "End Interruption" );
    
    AudioUnit * rio = (AudioUnit *)inUserData;
    
    // end
    if( inInterruption == kAudioSessionEndInterruption )
    {
        // make sure we are again the active session
        AudioSessionSetActive( true );
        AudioOutputUnitStart( *rio );
    }
    
    // begin
    if( inInterruption == kAudioSessionBeginInterruption )
        AudioOutputUnitStop( *rio );        
}




//-----------------------------------------------------------------------------
// name: propListener()
// desc: audio session property listener
//-----------------------------------------------------------------------------
static void propListener( void * inClientData, AudioSessionPropertyID inID,
                          UInt32 inDataSize, const void * inData )
{
    // detect audio retain change
    if( inID == kAudioSessionProperty_AudioRouteChange )
    {
        // status code
        OSStatus err;

        // if there was a route change, we need to dispose the current rio unit and create a new one
        err = AudioComponentInstanceDispose( MoAudio::m_au );
        if( err )
        {
            // TODO: "couldn't dispose remote i/o unit"
            return;
        }
        
        // set up
        setupRemoteIO( MoAudio::m_au, MoAudio::m_renderProc, MoAudio::m_info->m_dataFormat );
            
        UInt32 size = sizeof(MoAudio::m_hwSampleRate);
        // get sample rate
        err = AudioSessionGetProperty( kAudioSessionProperty_CurrentHardwareSampleRate, 
                                       &size, &MoAudio::m_hwSampleRate );
        if( err )
        {
            // TODO: "couldn't get new sample rate"
            return;
        }
        
        // check input
        MoAudio::checkInput();
        
        // start audio unit
        err = AudioOutputUnitStart( MoAudio::m_au );
        if( err )
        {
            // TODO: "couldn't start unit"
            return;
        }
            
        // get route
        CFStringRef newRoute;
        size = sizeof(CFStringRef);
        err = AudioSessionGetProperty( kAudioSessionProperty_AudioRoute, &size, &newRoute );
        if( err )
        {
            // TODO: "couldn't get new audio route"
            return;
        }
        
        // check route
        if( newRoute )
        {
            // CFShow( newRoute );
            if( CFStringCompare( newRoute, CFSTR("Headset"), NULL ) == kCFCompareEqualTo )
            { iphone_audioroute=1; }
            else if( CFStringCompare( newRoute, CFSTR("Receiver" ), NULL ) == kCFCompareEqualTo )
            { iphone_audioroute=2; }         
            else // unknown
            { iphone_audioroute=3; } 
        }
    }
}




//-----------------------------------------------------------------------------
// name: setupRemoteIO()
// desc: setup Audio Unit Remote I/O
//-----------------------------------------------------------------------------
bool setupRemoteIO( AudioUnit & inRemoteIOUnit, AURenderCallbackStruct inRenderProc,
                    AudioStreamBasicDescription & outFormat )
{
    // open the output unit
    AudioComponentDescription desc;
    desc.componentType = kAudioUnitType_Output;
    desc.componentSubType = kAudioUnitSubType_RemoteIO;
    desc.componentManufacturer = kAudioUnitManufacturer_Apple;
    desc.componentFlags = 0;
    desc.componentFlagsMask = 0;
    
    // find next component
    AudioComponent comp = AudioComponentFindNext( NULL, &desc );
    
    // status code
    OSStatus err;
    
    // the stream description
    AudioStreamBasicDescription localFormat;
    
    // open remote I/O unit
    err = AudioComponentInstanceNew( comp, &inRemoteIOUnit );
    if( err )
    {
        // TODO: "couldn't open the remote I/O unit"
        return false;
    }

    UInt32 one = 1;
    // enable input
    err = AudioUnitSetProperty( inRemoteIOUnit, kAudioOutputUnitProperty_EnableIO, 
                                kAudioUnitScope_Input, 1, &one, sizeof(one) );
    if( err )
    {
        // TODO: "couldn't enable input on the remote I/O unit"
        return false;
    }

    // set render proc
    err = AudioUnitSetProperty( inRemoteIOUnit, kAudioUnitProperty_SetRenderCallback,
                                kAudioUnitScope_Input, 0, &inRenderProc, sizeof(inRenderProc) );
    if( err )
    {
        // TODO: "couldn't set remote i/o render callback"
        return false;
    }
        
    UInt32 size = sizeof(localFormat);
    // get and set client format
    err = AudioUnitGetProperty( inRemoteIOUnit, kAudioUnitProperty_StreamFormat,
                                kAudioUnitScope_Input, 0, &localFormat, &size );
    if( err )
    {
        // TODO: "couldn't get the remote I/O unit's output client format"
        return false;
    }

    localFormat.mSampleRate = outFormat.mSampleRate;
    localFormat.mChannelsPerFrame = outFormat.mChannelsPerFrame;
    
    localFormat.mFormatID = outFormat.mFormatID;
    localFormat.mSampleRate = outFormat.mSampleRate;
    localFormat.mFormatFlags = kAudioFormatFlagIsSignedInteger |
    kAudioFormatFlagIsPacked |
    kAudioFormatFlagIsNonInterleaved |
    (24 << kLinearPCMFormatFlagsSampleFractionShift);
    localFormat.mChannelsPerFrame = outFormat.mChannelsPerFrame;
    localFormat.mBitsPerChannel = 32;
    localFormat.mFramesPerPacket = 1;
    localFormat.mBytesPerFrame = 4;
    localFormat.mBytesPerPacket = 4;
    
    // set stream property
    err = AudioUnitSetProperty( inRemoteIOUnit, kAudioUnitProperty_StreamFormat,
                                kAudioUnitScope_Input, 0, &localFormat, sizeof(localFormat) );
    if( err )
    {
        // TODO: "couldn't set the remote I/O unit's input client format"
        return false;
    }
    
    size = sizeof(outFormat);
    // get it again
    err = AudioUnitGetProperty( inRemoteIOUnit, kAudioUnitProperty_StreamFormat,
                                kAudioUnitScope_Input, 0, &outFormat, &size );
    if( err )
    {
        // TODO: "couldn't get the remote I/O unit's output client format"
        return false;
    }
    err = AudioUnitSetProperty( inRemoteIOUnit, kAudioUnitProperty_StreamFormat,
                                kAudioUnitScope_Output, 1, &outFormat, sizeof(outFormat) );
    if( err )
    {
        // TODO: "couldn't set the remote I/O unit's input client format"
        return false;
    }

    // print the format
    // printf( "format for remote i/o:\n" );
    // outFormat.Print();

    // initialize remote I/O unit
    err = AudioUnitInitialize( inRemoteIOUnit );
    if( err )
    {
        // TODO: "couldn't initialize the remote I/O unit"
        return false;
    }

    return true;
}




//-----------------------------------------------------------------------------
// name: init()
// desc: initialize the MoAudio
//-----------------------------------------------------------------------------
bool MoAudio::init( Float64 srate, UInt32 frameSize, UInt32 numChannels )
{
    // sanity check
    if( m_hasInit )
    {
        // TODO: error message
        return false;
    }
    
    // TODO: fix this
    assert( numChannels == 2 );
    
    // set audio unit callback
    m_renderProc.inputProc = SMALLRenderProc;
    // uh, this probably shouldn't be NULL
    m_renderProc.inputProcRefCon = NULL;
    
    // allocate info
    m_info = new MoAudioUnitInfo();

    // set the desired data format
    m_info->m_dataFormat.mSampleRate = srate;
    m_info->m_dataFormat.mFormatID = kAudioFormatLinearPCM;
    m_info->m_dataFormat.mChannelsPerFrame = numChannels;
    m_info->m_dataFormat.mBitsPerChannel = 32;
    m_info->m_dataFormat.mBytesPerPacket = m_info->m_dataFormat.mBytesPerFrame =
    m_info->m_dataFormat.mChannelsPerFrame * sizeof(SInt32);
    m_info->m_dataFormat.mFramesPerPacket = 1;
    m_info->m_dataFormat.mReserved = 0;
    m_info->m_dataFormat.mFormatFlags = kLinearPCMFormatFlagIsSignedInteger;
    m_info->m_done = 0;

    // bound parameters
    if( frameSize > m_info->m_bufferSize )
        frameSize = m_info->m_bufferSize;
    if( numChannels != 2 )
        numChannels = 2;
    
    // copy parameters
    m_srate = srate;
    m_frameSize = frameSize;
    m_numChannels = numChannels;

    // return status code
    OSStatus err;
    
    // initialize and configure the audio session
    err = AudioSessionInitialize( NULL, NULL, rioInterruptionListener, m_au );
    if( err )
    {
        // TODO: "couldn't initialize audio session"
        return false;
    }
    
    // set audio session active
    err = AudioSessionSetActive( true );
    if( err )
    {
        // TODO: "couldn't set audio session active\n"
        return false;
    }

    UInt32 category = kAudioSessionCategory_PlayAndRecord;
    // set audio category
    err = AudioSessionSetProperty( kAudioSessionProperty_AudioCategory, sizeof(category), &category );
    if( err )
    {
        // TODO: "couldn't set audio category"
        return false;
    }

    // set property listener
    err = AudioSessionAddPropertyListener( kAudioSessionProperty_AudioRouteChange, propListener, NULL );
    if( err )
    {
        // TODO: "couldn't set property listener"
        return false;
    }
    
    // check for >= OS 2.1?
    {
        // check for headset
        // dont override if headset is plugged in
        
        // get route
        CFStringRef route;
        UInt32 size = sizeof(CFStringRef);
        err = AudioSessionGetProperty( kAudioSessionProperty_AudioRoute, &size, &route );
        if( err )
        {
            // TODO: "couldn't get new audio route\n"
        }
        
        UInt32 override;
        
        CFRange range = CFStringFind(route, CFSTR("Headset"), 0);
        if(range.location != kCFNotFound)
        {
            override = kAudioSessionOverrideAudioRoute_None;
        }
        else
        {
            range = CFStringFind(route, CFSTR("Headphone"), 0);
            if(range.location != kCFNotFound)
            {
                override = kAudioSessionOverrideAudioRoute_None;
            }
            else
            {
                override = kAudioSessionOverrideAudioRoute_Speaker;
            }
        }
        
        // set speaker override
        err = AudioSessionSetProperty( kAudioSessionProperty_OverrideAudioRoute, sizeof(override), &override );
        if( err )
        {
            // TODO: "couldn't get new audio route\n"
            return false;
        }        
    }
    
    // compute durations
    Float32 preferredBufferSize = (Float32)(frameSize / srate); // .020;
    // set I/O buffer duration
    AudioSessionSetProperty( kAudioSessionProperty_PreferredHardwareIOBufferDuration,
                             sizeof(preferredBufferSize), &preferredBufferSize );
    if( err )
    {
        // TODO: "couldn't set i/o buffer duration"
        return false;
    }

    Float64 preferredSampleRate = srate;
    // set sample rate
    AudioSessionSetProperty( kAudioSessionProperty_PreferredHardwareSampleRate,
                             sizeof(preferredSampleRate), &preferredSampleRate );
    if( err )
    {
        // TODO: "couldn't set preferred sample rate"
        return false;
    }
    
    UInt32 size = sizeof(MoAudio::m_hwSampleRate);
    // get sample rate
    err = AudioSessionGetProperty( kAudioSessionProperty_CurrentHardwareSampleRate, &size, &MoAudio::m_hwSampleRate );
    if ( err )
    {
        // TODO: "couldn't get hw sample rate"
        return false;
    }
    
    // set up remote I/O
    if( !setupRemoteIO( m_au, m_renderProc, m_info->m_dataFormat ) )
    {
        // TODO: "couldn't setup remote i/o unit"
        return false;
    }
    
    // initialize buffer
    m_info->m_ioBuffer = new Float32[m_info->m_bufferSize * m_numChannels];
    // make sure
    if( !m_info->m_ioBuffer )
    {
        // TODO: "couldn't allocate memory for I/O buffer"
        return false;
    }
    
    // check audio input
    checkInput();

    // done with initialization
    m_hasInit = true;

    return true;
}




//-----------------------------------------------------------------------------
// name: start()
// desc: start the MoAudio
//-----------------------------------------------------------------------------
bool MoAudio::start( MoCallback callback, void * bindle )
{
    // assert
    assert( callback != NULL );

    // sanity check
    if( !m_hasInit )
    {
        // TODO: error message
        return false;
    }
    
    // sanity check 2
    if( m_isRunning )
    {
        // TODO: warning message
        return false;
    }
    
    // remember the callback
    m_callback = callback;
    // remember the bindle
    m_bindle = bindle;
    
    // status code
    OSStatus err;

    // start audio unit
    err = AudioOutputUnitStart( m_au );
    if( err )
    {
        // TODO: "couldn't start audio unit...\n" );
        return false;
    }

    // started
    m_isRunning = true;

    return true;
}




//-----------------------------------------------------------------------------
// name: stop()
// desc: stop the MoAudio
//-----------------------------------------------------------------------------
void MoAudio::stop()
{
    // sanity check
    if( !m_isRunning )
        return;
    
    // status code
    OSStatus err;
    
    // stop audio unit
    err = AudioOutputUnitStop( m_au );

    // flag
    m_isRunning = false;
}




//-----------------------------------------------------------------------------
// name: shutdown()
// desc: shutdown the MoAudio
//-----------------------------------------------------------------------------
void MoAudio::shutdown()
{
    // sanity check
    if( !m_hasInit )
        return;

    // stop
    stop();
    
    // flag
    m_hasInit = false;
    
    // clear the callback
    m_callback = NULL;
    
    // clean up
    SAFE_DELETE( m_info );
}




//-----------------------------------------------------------------------------
// name: checkInput()
// desc: check audio input, and sets appropriate flag
//-----------------------------------------------------------------------------
void MoAudio::checkInput()
{
    // handle input in callback
    m_handleInput = true;

    UInt32 has_input;
    UInt32 size = sizeof(has_input);
    // get property
    OSStatus err = AudioSessionGetProperty( kAudioSessionProperty_AudioInputAvailable, &size, &has_input );        
    if( err )
    {
        // TODO: "warning: unable to determine availability of audio input"
    }
    else if( !has_input  )
    {
        // TODO: "warning: full duplex enabled without available input"
        m_handleInput = false;
    }
}




//------------------------------------------------------------------------------
// name: vibrate()
// desc: trigger vibration
//------------------------------------------------------------------------------
void MoAudio::vibrate()
{
    AudioServicesPlaySystemSound( kSystemSoundID_Vibrate );
}
