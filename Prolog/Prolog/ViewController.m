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

#include "SWI-Prolog.h"

static PrologTextView *prologView = NULL;
static PrologInputView *inputView = NULL;
static NSDictionary *viewsDictionary = NULL;

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
    ctv.text = @"SWI-Prolog!\n";
    prologView = ctv;

    inputView = [[PrologInputView alloc] init];
    inputView.layer.borderWidth = 2.0f;
    inputView.layer.borderColor = [[UIColor grayColor] CGColor];
    inputView.text = @"X is 3 + 5.";
    inputView.delegate = self;

    [self.view addSubview:ctv];
    [self.view addSubview:inputView];
    
    [ctv resignFirstResponder];
    [inputView resignFirstResponder];

    UIButton *goButton = [UIButton buttonWithType:UIButtonTypeSystem];
    [goButton setTitle:@"Go!" forState:UIControlStateNormal];

    [goButton addTarget:self 
               action:@selector(doGoButton)
            forControlEvents:UIControlEventTouchUpInside];

    [self.view addSubview:goButton];

    UIButton *qButton = [UIButton buttonWithType:UIButtonTypeSystem];
    [qButton setTitle:@"Query!" forState:UIControlStateNormal];

    [qButton addTarget:self 
               action:@selector(doQueryButton)
            forControlEvents:UIControlEventTouchUpInside];

    [self.view addSubview:qButton];

    [ctv setTranslatesAutoresizingMaskIntoConstraints:NO];
    [inputView setTranslatesAutoresizingMaskIntoConstraints:NO];
    [goButton setTranslatesAutoresizingMaskIntoConstraints:NO];
    [qButton setTranslatesAutoresizingMaskIntoConstraints:NO];


    viewsDictionary = NSDictionaryOfVariableBindings(ctv, inputView, goButton, qButton);

    extern int ios_initialize(void);
    ios_initialize();
    
    [inputView performSelector:@selector(becomeFirstResponder) withObject:nil afterDelay:0];

    [[NSNotificationCenter defaultCenter] addObserver:self 
					     selector:@selector(keyboardWillShow:)
						 name:UIKeyboardWillShowNotification
					       object:nil];
}

- (void)doGoButton {
    NSString *textValue = [NSString stringWithFormat:@"%@\n", inputView.text];
    appendText(textValue);
    [inputView setText: @""];

    extern void set_ios_input_string(char *str, int len);
    char *in = [textValue cStringUsingEncoding:NSUTF8StringEncoding];
    int len = strlen(in);
    set_ios_input_string(in, len);

    extern int PL_query_loop(void);

    int status = PL_query_loop();
}

- (void)doQueryButton {
  int status;
  predicate_t teaches = PL_predicate("teaches", 2, NULL);

  char *newTerm = "learner(haruko, scratch)";

  term_t av = PL_new_term_refs(2);
  PL_put_atom_chars(av, "suzuko");
  pid_t q = PL_open_query(NULL, PL_Q_NODEBUG|PL_Q_ALLOW_YIELD|PL_Q_EXT_STATUS, teaches, av);
    if (q == 0) {
        printf("not enough memory\n");
        return;
    }
  while (true) {
    status = PL_next_solution(q);
    if (status) {
      char *str;
      status = PL_get_atom_chars(av+1, &str);
      if (status) {
	Swrite_fileToPrologTextView(str, strlen(str));
	Swrite_fileToPrologTextView("\n", 1);
      }
    } else {
      PL_close_query(q);
      break;
    }
  }
}

- (BOOL)textViewShouldBeginEditing:(UITextView *)textView {
  return TRUE;
}

- (void)keyboardWillShow:(NSNotification *)notification
{
  [self.view addConstraints:
	 [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[ctv]-5-|"
						 options:0
						 metrics:nil views:viewsDictionary]];
  [self.view addConstraints:
	 [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[inputView]-[goButton]-[qButton]-5-|"
						 options:0
						 metrics:nil views:viewsDictionary]];

  char layout[1000];
  int keyboardH = (int)([notification.userInfo[UIKeyboardFrameEndUserInfoKey] CGRectValue].size.height) + 5;

  sprintf(layout, "V:|-20-[ctv]-[inputView]-%d-|", keyboardH);


  [self.view addConstraints:
    [NSLayoutConstraint constraintsWithVisualFormat:[NSString stringWithUTF8String: layout]
					    options:0
					    metrics:nil views:viewsDictionary]];

  NSLayoutConstraint *eq = [NSLayoutConstraint constraintWithItem:[viewsDictionary objectForKey: @"inputView"]
							attribute:NSLayoutAttributeCenterY
							relatedBy:NSLayoutRelationEqual
							   toItem:[viewsDictionary objectForKey: @"goButton"]
							attribute:NSLayoutAttributeCenterY
						       multiplier:1
							 constant:0];
  NSLayoutConstraint *eq2 = [NSLayoutConstraint constraintWithItem:[viewsDictionary objectForKey: @"inputView"]
                                                         attribute:NSLayoutAttributeCenterY
                                                         relatedBy:NSLayoutRelationEqual
                                                            toItem:[viewsDictionary objectForKey: @"qButton"]
                                                         attribute:NSLayoutAttributeCenterY
                                                        multiplier:1
							  constant:0];

  NSArray *cnts = @[eq, eq2];
  [self.view addConstraints: cnts];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
