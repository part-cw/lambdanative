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
// name: mo_location.h
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
#ifndef __MO_LOCATION_H__
#define __MO_LOCATION_H__

#include "mo_def.h"
#import <CoreLocation/CoreLocation.h>
//#include <vector>


@interface LocationDelegate : NSObject <CLLocationManagerDelegate>
{
    CLLocationManager *locationManager;
}
@property (nonatomic, retain) CLLocationManager *locationManager;

@end


// type definition for accelerometer callback function
typedef void (* MoLocationCallback)( CLLocation * newLocation, 
                                     CLLocation * oldLocation,
                                     void * data );

//-----------------------------------------------------------------------------
// name: class MoLocation
// desc: location stuff, GPS/Edge/Wifi + Compass (singleton)
//-----------------------------------------------------------------------------
class MoLocation
{
public: // getting values

    // register a callback to be invoked on subsequent updates
    static void addCallback( const MoLocationCallback & callback, void * data );
    // unregister a callback
    static void removeCallback( const MoLocationCallback & callback );
    

    // get the newest location
    static CLLocation * getLocation();
    // get the previous location
    static CLLocation * getOldLocation();

    /*
    Location Attributes
    coordinate  property
    altitude  property
    horizontalAccuracy  property
    verticalAccuracy  property
    timestamp  property
    – description
    Measuring the Distance Between Coordinates
    – getDistanceFrom:
    Getting Speed and Course Information
    speed  property
    course  property
    */

public: // setting values
    static void update( CLLocation * newLoc, CLLocation * oldLoc );
    
private:
    // set up the Location delegate
    static void checkSetup();

    // current values;
    static CLLocation * m_newLocation;
    static CLLocation * m_oldLocation;

    static LocationDelegate * locationDelegate;
    
    // queue of callbacks
//    static std::vector< MoLocationCallback > m_clients;
//    static std::vector<void *> m_clientData; 

};


#endif
