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
// name: mo_compass.h
// desc: MoPhO API for compass 
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
#ifndef __MO_COMPASS_H__
#define __MO_COMPASS_H__

#import "mo_def.h"
#import <vector>
#import <CoreLocation/CoreLocation.h>


@interface CompassDelegate : NSObject <CLLocationManagerDelegate>
{
    CLLocationManager *locationManager;
}
@property (nonatomic, retain) CLLocationManager *locationManager;

@end


// type definition for accelerometer callback function
typedef void (* MoCompassCallback)( CLHeading * heading, void * data );


//-----------------------------------------------------------------------------
// name: class MoLocation
// desc: location stuff, GPS/Edge/Wifi + Compass
//-----------------------------------------------------------------------------
class MoCompass
{
public: // setting values

    static void setOffset();
    // clear the current compass offset
    static void clearOffset();

    static double getMagneticOffset();
    static double getTrueOffset();

public: // getting values

    // get the magnetic heading of the compass
    static double getMagneticHeading();
    // get the magnetic heading of the compass
    static double getTrueHeading();
    // get the estimated accuracy of the compass
    static double getAccuracy();
	// get the timestamp
	static NSDate * getTimestamp();
	
	

    // register a callback to be invoked on subsequent updates
    static void addCallback( const MoCompassCallback & callback, void * data );
    // unregister a callback
    static void removeCallback( const MoCompassCallback & callback );
    
public:
    static void update( CLHeading * heading ); // do not call outside of mo_compass.mm...
    

private:
    // current values;
    static CLHeading * m_heading;
    static double m_magneticHeading;
    static double m_trueHeading;
    static CLLocationDirection m_accuracy;
    // store an offset 
    static double m_trueOffset;
    static double m_magneticOffset;
    static CompassDelegate * compassDelegate;

    // queue of callbacks
    static std::vector< MoCompassCallback > m_clients;
    static std::vector<void *> m_clientData;

    // internal setup of the compass
    static void checkSetup();
};


#endif
