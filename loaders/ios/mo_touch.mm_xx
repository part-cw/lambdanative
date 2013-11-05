/*----------------------------------------------------------------------------
  MoMu: A Mobile Music Toolkit
  Copyright ( c ) 2010 Nicholas J. Bryan, Jorge Herrera, Jieun Oh, and Ge Wang
  All rights reserved.
    http://momu.stanford.edu/toolkit/
 
  Mobile Music Research @ CCRMA
  Music, Computing, Design Group
  Stanford University
    http://momu.stanford.edu/
    http://ccrma.stanford.edu/groups/mcd/
 
 MoMu is distributed under the following BSD style open source license:
 
 Permission is hereby granted, free of charge, to any person obtaining a 
 copy of this software and associated documentation files ( the
 "Software" ), to deal in the Software without restriction, including
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
// name: mo_touch.mm
// desc: MoPhO API for multi-touch
//
// authors: Jieun Oh
//          Nick Bryan
//          Jorge Herrera
//          Ge Wang
//
//    date: Fall 2009
//    version: 1.0.0
//
// Mobile Music research @ CCRMA, Stanford University:
//     http://momu.stanford.edu/
//-----------------------------------------------------------------------------
#include "mo_touch.h"
#import <objc/objc.h>
#import <objc/runtime.h>
#import <objc/message.h>
#import "mo_thread.h"


//-----------------------------------------------------------------------------
// name: touchesBegan()
// desc: handles the start of a touch
//-----------------------------------------------------------------------------
void myTouchesBegan( id self, SEL _cmd, NSSet * touches, UIEvent * event )
{
   // NSSet * allTouches = [event allTouches];
    MoTouch::update( touches, self );    
}


//-----------------------------------------------------------------------------
// name: touchesMoved()
// desc: handles the continuation of a touch
//-----------------------------------------------------------------------------
void myTouchesMoved( id self, SEL _cmd, NSSet * touches, UIEvent *event )
{   
   // NSSet * allTouches = [event allTouches];
    MoTouch::update( touches, self );
}


//-----------------------------------------------------------------------------
// name: touchesEnded()
// desc: handles the end of a touch event when the touch is a tap
//-----------------------------------------------------------------------------
void myTouchesEnded( id self, SEL _cmd, NSSet *touches, UIEvent * event )
{
   // NSSet * allTouches = [event allTouches];
    MoTouch::update( touches, self );
}


//-----------------------------------------------------------------------------
// name: touchesCancelled()
// desc: handles the end of a touch event
//-----------------------------------------------------------------------------
void myTouchesCancelled( id self, SEL _cmd, NSSet *touches, UIEvent * event )
{
   // NSSet * allTouches = [event allTouches];
    MoTouch::update( touches, self );
}


// static initialization
std::vector< MoTouchCallback > MoTouch::m_clients;
std::vector<void *> MoTouch::m_clientData;
std::vector<MoTouchTrack> MoTouch::m_touchVec;


//-----------------------------------------------------------------------------
// name: checkSetup()
// desc: idempotent set up
//-----------------------------------------------------------------------------
void MoTouch::checkSetup()
{
    // override the touches methods, so the user doesn't have to do it
    Method method = class_getInstanceMethod( [UIView class], @selector( touchesBegan:withEvent: ) );
    method_setImplementation( method, ( IMP )myTouchesBegan );
    
    method = class_getInstanceMethod( [UIView class], @selector( touchesMoved:withEvent: ) );
    method_setImplementation( method, ( IMP )myTouchesMoved );
    
    method = class_getInstanceMethod( [UIView class], @selector( touchesEnded:withEvent: ) );
    method_setImplementation( method, ( IMP )myTouchesEnded );
    
    method = class_getInstanceMethod( [UIView class], @selector( touchesCancelled:withEvent: ) );
    method_setImplementation( method, ( IMP )myTouchesCancelled );
}


//-----------------------------------------------------------------------------
// name: addCallback()
// desc: registers a callback to be invoked on subsequent updates       
//-----------------------------------------------------------------------------
void MoTouch::addCallback( const MoTouchCallback & callback, void * data )
{
    // set up, if necessary
    checkSetup();

    NSLog( @"adding MoTouch callback..." );
    m_clients.push_back( callback );
    m_clientData.push_back( data );
}


//-----------------------------------------------------------------------------
// name:  removeCallback()
// desc:  unregisters a callback to be invoked on subsequent updates       
//-----------------------------------------------------------------------------
void MoTouch::removeCallback( const MoTouchCallback & callback )
{

    NSLog( @"removing MoTouch callback..." );
    // find the callback and remove
    for( int i = 0; i < m_clients.size(); i++ )
    {
        if( m_clients[i] == callback )
        {
            m_clients.erase( m_clients.begin()+i );
            m_clientData.erase( m_clientData.begin()+i );
        }
    }
}


//-----------------------------------------------------------------------------
// name:  clear()
// desc:  clear the state of the internal touch data structure.
//        Maybe necessary for when a view is switched out underneath ( such as 
//        going to a flip view ) while touch( s ) remains on the screen
//        If clear is not employed when needed, stale pointers are possible.
//        Caution should be taken when implmenting multi-touch with a screen
//        using multiple view.
//-----------------------------------------------------------------------------
void MoTouch::clear()
{
    m_touchVec.clear();
}


//-----------------------------------------------------------------------------
// name:  update()
// desc:  updated by the system
//        update the internal touch data and process any callbacks       
//-----------------------------------------------------------------------------
void MoTouch::update( NSSet * touches, UIView * view )
{

    // Set the view to be multi touch enabled
    [view setMultipleTouchEnabled:YES];
            
    // Populate the vector
    for( UITouch * touch in touches )
    {
        //populate the vector
        if( ( touch.phase == UITouchPhaseBegan ) || ( touch.phase == UITouchPhaseMoved ) 
            || ( touch.phase == UITouchPhaseStationary ) )
        {    
            //try to find the touch
            bool found = false;
            for( int i=0; i < m_touchVec.size(); i++ )
            {
                if( m_touchVec[i].touch == touch )
                {
                    found = true;
                    break;
                }
            }
            if( !found )
            {
                MoTouchTrack track;
                track.touch = touch;
                m_touchVec.push_back( track );
            }
        }
    }
    
    // NSLog( @"Internal vec Pointer:%d", (int)&m_touchVec );
    
    // process client callbacks
    for ( int i=0; i<m_clients.size(); i++ )
    {
        m_clients[i]( touches, NULL, m_touchVec, m_clientData[i] );
    }   

    // post erase from the vector
    for( UITouch * touch in touches )
    {
        if( touch.phase == UITouchPhaseEnded || touch.phase == UITouchPhaseCancelled )
        {
            for( int i=0; i < m_touchVec.size(); i++ )
            {
                if( m_touchVec[i].touch == touch )
                {
                    m_touchVec.erase( m_touchVec.begin()+i );
                    break;
                }
            }
        }
    }
}
