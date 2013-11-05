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
// name: mo_touch.h
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
#ifndef __MO_TOUCH_H__
#define __MO_TOUCH_H__

#import <UIKit/UIKit.h>
#include "mo_def.h"
#include <map>
#include <vector>




//-----------------------------------------------------------------------------
// name: class MoTouchTrack
// desc: one track associated with a touch gesture (from begin to end/cancel)
//-----------------------------------------------------------------------------
struct MoTouchTrack
{
    UITouch * touch;  
    void * data;
};


// type definition for general touch callback function
typedef void (* MoTouchCallback)( NSSet * touches, UIView * view, const std::vector<MoTouchTrack> & tracks, void * data);

//-----------------------------------------------------------------------------
// name: class MoTouch
// desc: multi-touch stuff  (singleton)
//-----------------------------------------------------------------------------
class MoTouch
{

public: // client-side stuff
    static void addCallback( const MoTouchCallback & callback, void * data );
    static void removeCallback( const MoTouchCallback & callback );
    static void clear();
    
public:
    // updated by the system, not you!
    static void update( NSSet * touches, UIView * view );   

private:
    static void checkSetup();
    
    // queue of callbacks
    static std::vector< MoTouchCallback > m_clients;
    static std::vector<void *> m_clientData;
    static std::vector<MoTouchTrack> m_touchVec;
};




#endif
