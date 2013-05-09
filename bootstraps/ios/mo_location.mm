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
// name: mo_location.mm
// desc: MoPhO API for compass
//
// authors: Nick Bryan
//          Jorge Herrera
//          Jieun Oh
//          Ge Wang
//
//    date: Fall 2009
//    version: 1.0.0
//
// Mobile Music research @ CCRMA, Stanford University:
//     http://momu.stanford.edu/
//-----------------------------------------------------------------------------
#include "mo_location.h"


@implementation LocationDelegate
@synthesize locationManager;

- (void)locationManager:(CLLocationManager *)manager
    didUpdateToLocation:(CLLocation *)newLocation 
           fromLocation:(CLLocation *)oldLocation
{
    // update the Location data
    MoLocation::update(newLocation, oldLocation);
}

- (void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error
{
    // the location "unknown" error simply means the manager is currently unable to get the location.
    // if ([error code] != kCLErrorLocationUnknown) {
    //    [self stopUpdatingLocation:NSLocalizedString(@"Error", @"Error")];
    // }
}

@end


// static initialization
CLLocation * MoLocation::m_newLocation = NULL;
CLLocation *  MoLocation::m_oldLocation = NULL;
LocationDelegate * MoLocation::locationDelegate = NULL;
//std::vector< MoLocationCallback > MoLocation::m_clients;
//std::vector<void *> MoLocation::m_clientData; 


//-----------------------------------------------------------------------------
// name: checkSetup()
// desc: returns the singelton instance
//-----------------------------------------------------------------------------
void MoLocation::checkSetup()
{
    if(locationDelegate==NULL)
    {
        // allocate a location Delegate object
        locationDelegate = [LocationDelegate alloc];
        locationDelegate.locationManager = [[[CLLocationManager alloc] init] autorelease];

        // heading service configuration
        locationDelegate.locationManager.distanceFilter = kCLDistanceFilterNone;
        //This is default anyway, but just to make sure
        locationDelegate.locationManager.desiredAccuracy = kCLLocationAccuracyBest;
        // setup delegate callbacks
        locationDelegate.locationManager.delegate = locationDelegate;
        // start the location stuff
        [locationDelegate.locationManager startUpdatingLocation];
    }
}


//-----------------------------------------------------------------------------
// name: update()
// desc: update the internal accelerometer data and process any callbacks
//-----------------------------------------------------------------------------
void MoLocation::update( CLLocation * newLoc, CLLocation * oldLoc )
{
    [m_newLocation release];
    [m_oldLocation release];
    
    m_newLocation = (CLLocation *)newLoc;
    m_oldLocation = (CLLocation *)oldLoc;
    
    [m_newLocation retain];
    [m_oldLocation retain];
        
    // process all callbacks
//    for( int i=0; i < m_clients.size(); i++ )  
//        (m_clients[i])( m_newLocation, m_oldLocation, m_clientData[i]);  
}


//-----------------------------------------------------------------------------
// name: getLocation()
// desc: ...    
//-----------------------------------------------------------------------------
CLLocation * MoLocation::getLocation()
{
    // one-time setup, if needed
    checkSetup();

    return m_newLocation;
}


//-----------------------------------------------------------------------------
// name: getOldLocation()
// desc: ...    
//-----------------------------------------------------------------------------
CLLocation * MoLocation::getOldLocation()
{
    // one-time setup, if needed
    checkSetup();

    return m_oldLocation;
}


//-----------------------------------------------------------------------------
// name: addCallback()
// desc: registers a callback to be invoked on subsequent updates       
//-----------------------------------------------------------------------------
void MoLocation::addCallback(const MoLocationCallback & callback, void * data )
{
    // one-time setup, if needed
    checkSetup();

    // NSLog(@"Adding MoLocationCallback\n");
//    m_clients.push_back( callback );
//    m_clientData.push_back( data );
}


//-----------------------------------------------------------------------------
// name: removeCallback()
// desc: unregisters a callback to be invoked on subsequent updates       
//-----------------------------------------------------------------------------
void MoLocation::removeCallback(const MoLocationCallback & callback )
{
    // one-time setup, if needed
    checkSetup();

    // NSLog(@"Removing MoLocationCallback\n");
    // find the callback and remove
//    for(int i=0; i < m_clients.size(); i++)     
//    {
//        if(m_clients[i]==callback)
//        {
//            m_clients.erase( m_clients.begin()+i );
//            m_clientData.erase( m_clientData.begin()+i );
//        }
//    }

}
