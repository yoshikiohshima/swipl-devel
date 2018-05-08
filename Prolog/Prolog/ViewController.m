//
//  ViewController.m
//  Prolog
//
//  Created by Yoshiki Ohshima on 2018/05/07.
//  Copyright © 2018年 Yoshiki Ohshima. All rights reserved.
//

#import "ViewController.h"

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.
    extern int ios_main(void);

    ios_main();
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


@end
