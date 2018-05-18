//
//  ViewController.h
//  Prolog
//
//  Created by Yoshiki Ohshima on 2018/05/07.
//  Copyright © 2018年 Yoshiki Ohshima. All rights reserved.
//

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

