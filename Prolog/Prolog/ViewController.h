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

@interface ViewController : UIViewController <UITextViewDelegate>

@property PrologTextView *prologView;
@property PrologInputView *inputView;
@property NSDictionary *viewsDictionary;
@property NSThread *interpreterThread;
@property NSCondition *inputCondition;

@property char *inputString;
@property int *inputLength;

//@property NSArray* outputArray;


- (void)appendText: (NSString*)str;
- (void)readFromPrologInput;


@end

