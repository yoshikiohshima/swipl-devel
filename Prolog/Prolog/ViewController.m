//
//  ViewController.m
//  SWI-Prolog on iPhone
//
//  Created by Yoshiki Ohshima on 2018/05/07.
//  Copyright © 2018 Yoshiki Ohshima. All rights reserved.
//

#import "ViewController.h"

#include "SWI-Prolog.h"

static ViewController *theView = NULL;

int Swrite_fileToPrologTextView(char *buf, size_t size) {
  NSString *str = [[NSString alloc] initWithBytes: buf length: size encoding:NSASCIIStringEncoding];

  [theView performSelectorOnMainThread:@selector(appendText:)
			    withObject:(id)str 
			 waitUntilDone:(BOOL)NO];
    return 0;
}

int read_from_input() {
  [theView readFromPrologInput];
  return 0;
}



@implementation ViewController

- (void)viewDidLoad {
  [super viewDidLoad];

  self.inputString = "";
  self.inputLength = 0;

  theView = self;
    
  PrologTextView *prologView = [[PrologTextView alloc] init];
  prologView.layer.borderWidth = 2.0f;
  prologView.layer.borderColor = [[UIColor grayColor] CGColor];
  prologView.text = @"SWI-Prolog!\n";
  self.prologView = prologView;

  PrologInputView *inputView = [[PrologInputView alloc] init];
  inputView.layer.borderWidth = 2.0f;
  inputView.layer.borderColor = [[UIColor grayColor] CGColor];
  inputView.autocapitalizationType = UITextAutocapitalizationTypeNone;
  inputView.text = @"X is 3 + 5.";
  inputView.delegate = self;
  self.inputView = inputView;

  [self.view addSubview:prologView];
  [self.view addSubview:inputView];
    
  [prologView resignFirstResponder];
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

  [prologView setTranslatesAutoresizingMaskIntoConstraints:NO];
  [inputView setTranslatesAutoresizingMaskIntoConstraints:NO];
  [goButton setTranslatesAutoresizingMaskIntoConstraints:NO];
  [qButton setTranslatesAutoresizingMaskIntoConstraints:NO];

  self.viewsDictionary = NSDictionaryOfVariableBindings(prologView, inputView, goButton, qButton);

  [inputView performSelector:@selector(becomeFirstResponder) withObject:nil afterDelay:0];
  [[NSNotificationCenter defaultCenter] addObserver:self
					   selector:@selector(keyboardWillShow:)
					       name:UIKeyboardWillShowNotification
					     object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(keyboardWillHide:)
                                                 name:UIKeyboardWillHideNotification
                                               object:nil];

  [self layout];

  self.interpreterThread = [[NSThread alloc] initWithTarget:self
						   selector:@selector(runInterpreterThread:)
						   object:nil];

  /*[self runInterpreterThread: nil];*/
  [self.interpreterThread start];
}

- (void) runInterpreterThread: (NSObject*)obj {
  extern int ios_initialize(void);
  extern int PL_toplevel(void);
  ios_initialize();

  self.inputCondition = [[NSCondition alloc] init];
  for(;;) {
    int status = PL_toplevel() ? 0 : 1;
    PL_halt(status);
  }
}

-(void)readFromPrologInput {
  [self.inputCondition lock];
  while (self.inputLength == 0) {
    [self.inputCondition wait];
  }
  extern void set_ios_input_string(char *str, int len);
  set_ios_input_string(self.inputString, self.inputLength);
  self.inputString = "";
  self.inputLength = 0;
  [self.inputCondition unlock];
}


- (void)doGoButton {
  NSString *textValue = [NSString stringWithFormat:@"%@\n", self.inputView.text];
  [self appendText: textValue];
  [self.inputView setText: @""];

  [self.inputCondition lock];
  self.inputString = [textValue cStringUsingEncoding:NSUTF8StringEncoding];
  self.inputLength = strlen(self.inputString);

  if (self.inputString[self.inputLength-1] == '\n') {
    self.inputString[self.inputLength-1] = '\0';
    self.inputLength--;
  }
    
  [self.inputCondition signal];
  [self.inputCondition unlock];
}

- (void)addOutput: (NSString*)str {
  [self appendText: str];
}

- (void)doQueryButton {
  int status;
  predicate_t teaches = PL_predicate("teaches", 2, NULL);

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

- (void)layout {
  [self.view addConstraints:
         [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[prologView]-5-|"
                                                 options:0
                                                 metrics:nil views:self.viewsDictionary]];
  [self.view addConstraints:
         [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[inputView]-[goButton]-[qButton]-5-|"
                                                 options:0
                                                 metrics:nil views:self.viewsDictionary]];
    
  self.keyConstraints = [NSLayoutConstraint constraintsWithVisualFormat:@"V:|-20-[prologView]-[inputView]-5-|"
								options:0
								metrics:nil views:self.viewsDictionary];

  self.theKeyConstraint = [self.keyConstraints objectAtIndex: 2];
  [self.view addConstraints: self.keyConstraints];

  NSLayoutConstraint *eq = [NSLayoutConstraint constraintWithItem:[self.viewsDictionary objectForKey: @"inputView"]
                                                        attribute:NSLayoutAttributeCenterY
                                                        relatedBy:NSLayoutRelationEqual
                                                           toItem:[self.viewsDictionary objectForKey: @"goButton"]
                                                        attribute:NSLayoutAttributeCenterY
                                                       multiplier:1
                                                         constant:0];
  NSLayoutConstraint *eq2 = [NSLayoutConstraint constraintWithItem:[self.viewsDictionary objectForKey: @"inputView"]
                                                         attribute:NSLayoutAttributeCenterY
                                                         relatedBy:NSLayoutRelationEqual
                                                            toItem:[self.viewsDictionary objectForKey: @"qButton"]
                                                         attribute:NSLayoutAttributeCenterY
                                                        multiplier:1
                                                          constant:0];

  NSArray *cnts = @[eq, eq2];
  [self.view addConstraints: cnts];
}


- (BOOL)textViewShouldBeginEditing:(UITextView *)textView {
  return TRUE;
}

- (void)keyboardWillShow:(NSNotification *)notification
{
  float keyH = ([notification.userInfo[UIKeyboardFrameEndUserInfoKey] CGRectValue].size.height) + 5;
  self.theKeyConstraint.constant = keyH;
  [self.view setNeedsLayout];
  [self.view layoutIfNeeded];
}

- (void)keyboardWillHide:(NSNotification *)notification
{
  self.theKeyConstraint.constant = 5.0;
  [self.view setNeedsLayout];
  [self.view layoutIfNeeded];
}

- (void)appendText: (NSString*)str {
  [self.prologView setText:[self.prologView.text stringByAppendingString:str]];
  [self.prologView scrollRangeToVisible:NSMakeRange([self.prologView.text length], 0)];
}


- (void)didReceiveMemoryWarning {
  [super didReceiveMemoryWarning];
  // Dispose of any resources that can be recreated.
}

@end
