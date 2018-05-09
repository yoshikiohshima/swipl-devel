//
//  ViewController.m
//  Prolog
//
//  Created by Yoshiki Ohshima on 2018/05/07.
//  Copyright © 2018年 Yoshiki Ohshima. All rights reserved.
//

#import "ViewController.h"
#import "PrologTextView.h"
#import "PrologInputView.h"

static PrologTextView *prologView = NULL;
static PrologInputView *inputView = NULL;

void appendText(NSString *str) {
    [prologView setText:[prologView.text stringByAppendingString:str]];
}

int Swrite_fileToPrologTextView(char *buf, size_t size) {
    NSString *str = [[NSString alloc] initWithBytes: buf length: size encoding:NSASCIIStringEncoding];
    appendText(str);
    return 0;
}

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    
    PrologTextView *ctv = [[PrologTextView alloc] init];
    prologView = ctv;
    ctv.layer.borderWidth = 2.0f;
    ctv.layer.borderColor = [[UIColor grayColor] CGColor];
    ctv.text = @"replace this with a lengthy text.....";
    prologView = ctv;

    inputView = [[PrologInputView alloc] init];
    inputView.layer.borderWidth = 2.0f;
    inputView.layer.borderColor = [[UIColor grayColor] CGColor];
    inputView.text = @"";

    UIButton *button = [UIButton buttonWithType:UIButtonTypeSystem];
    [button setTitle:@"Go!" forState:UIControlStateNormal];

    [button addTarget:self 
	       action:@selector(doButton)
	    forControlEvents:UIControlEventTouchUpInside];

    [self.view addSubview:button];


 
    [ctv setTranslatesAutoresizingMaskIntoConstraints:NO];
    [inputView setTranslatesAutoresizingMaskIntoConstraints:NO];
    [button setTranslatesAutoresizingMaskIntoConstraints:NO];

    [self.view addSubview:ctv];
    [self.view addSubview:inputView];

    NSDictionary *viewsDictionary = NSDictionaryOfVariableBindings(ctv, inputView, button);

    [self.view addConstraints:
     [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[ctv]-5-|"
                                             options:0
                                             metrics:nil views:viewsDictionary]];
    [self.view addConstraints:
     [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[inputView]-[button]-5-|"
                                             options:0
                                             metrics:nil views:viewsDictionary]];
    [self.view addConstraints:
     [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-20-[ctv]-[inputView]-5-|"
                                             options:0
                                             metrics:nil views:viewsDictionary]];

    NSLayoutConstraint *eq = [NSLayoutConstraint constraintWithItem:inputView
                                                         attribute:NSLayoutAttributeCenterY
                                                         relatedBy:NSLayoutRelationEqual
                                                            toItem:button
                                                         attribute:NSLayoutAttributeCenterY
                                                        multiplier:1
                                                           constant:0];
    NSArray *cnts = @[eq];
    [self.view addConstraints: cnts];

    // Do any additional setup after loading the view, typically from a nib.
    extern int ios_main(void);
    ios_main();
}

- (void)doButton {
    NSString *textValue = [NSString stringWithFormat:@"%@\n", inputView.text];
    appendText(textValue);
    [inputView setText: @""];
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end





  
