//
//  ViewController.m
//  Prolog
//
//  Created by Yoshiki Ohshima on 2018/05/07.
//  Copyright © 2018年 Yoshiki Ohshima. All rights reserved.
//

#import "ViewController.h"
#import "PrologTextView.h"

static PrologTextView *prologView = NULL;

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    
    PrologTextView *ctv = [[PrologTextView alloc] init];
    prologView = ctv;
    ctv.layer.borderWidth = 5.0f;
    ctv.layer.borderColor = [[UIColor grayColor] CGColor];
    ctv.text = @"replace this with a lengthy text.....";

    prologView = ctv;
 
    [ctv setTranslatesAutoresizingMaskIntoConstraints:NO];
 
    [self.view addSubview:ctv];

    NSDictionary *viewsDictionary = NSDictionaryOfVariableBindings(ctv);

    [self.view addConstraints:
     [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[ctv]-5-|"
                                             options:0
                                             metrics:nil views:viewsDictionary]];
    [self.view addConstraints:
     [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-20-[ctv]-5-|"
                                             options:0
                                             metrics:nil views:viewsDictionary]];

    // Do any additional setup after loading the view, typically from a nib.
    extern int ios_main(void);
    ios_main();
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end

int Swrite_fileToPrologTextView(char *buf, size_t size) {
  NSString *str = [[NSString alloc] initWithBytes: buf length: size encoding:NSASCIIStringEncoding];
  [prologView setText:str];
    return 0;
}
  




  
