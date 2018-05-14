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

- (void)appendText: (NSString*)str;


@end

