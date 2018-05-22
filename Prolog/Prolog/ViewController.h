/*  Part of SWI-Prolog

    Author:        Yoshiki Ohshima
    E-mail:        Yoshiki.Ohshima@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018 All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#import <UIKit/UIKit.h>

#import "PrologTextView.h"
#import "PrologInputView.h"

#include "SWI-Prolog.h"
#include "SWI-Stream.h"


@interface ViewController : UIViewController <UITextViewDelegate>

@property PrologTextView *prologView;
@property PrologInputView *inputView;
@property NSDictionary *viewsDictionary;
@property NSArray *keyConstraints;
@property NSLayoutConstraint *theKeyConstraint;

@property NSThread *interpreterThread;
@property NSCondition *inputCondition;

@property char *inputString;
@property int inputLength;

@property int threadId;

// --- variables for firstTime/eachTime/lastTime
@property fid_t fid;
@property term_t userTerm;
@property functor_t functor;
@property int arity;
@property module_t module;
@property predicate_t pred;
@property term_t userArgs;
@property char *vars;
@property char **names;
@property qid_t qid;

@property int queryState; // 0: not started, 1: call eachtime




//@property NSArray* outputArray;


- (void)appendText: (NSString*)str;
- (void)readFromPrologInput;


@end

