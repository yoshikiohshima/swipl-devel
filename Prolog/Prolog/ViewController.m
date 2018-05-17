//
//  ViewController.m
//  SWI-Prolog on iPhone
//
//  Created by Yoshiki Ohshima on 2018/05/07.
//  Copyright © 2018 Yoshiki Ohshima. All rights reserved.
//

#import "ViewController.h"

#include "SWI-Prolog.h"
#include "SWI-Stream.h"
extern int PL_unify_stream(term_t t, IOSTREAM *s);
extern foreign_t pl_write_term3(term_t stream, term_t term, term_t opts);
extern varName(term_t t, char *name);


static ViewController *theView = NULL;

int Swrite_fileToPrologTextView(char *buf, size_t size) {
  NSString *str = [[NSString alloc] initWithBytes: buf length: size encoding:NSASCIIStringEncoding];

  [theView performSelectorOnMainThread:@selector(appendText:)
			    withObject:(id)str 
			 waitUntilDone:(BOOL)NO];
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
  inputView.text = @"X is 3 + 4";
  inputView.delegate = self;
  self.inputView = inputView;

  [self.view addSubview:prologView];
  [self.view addSubview:inputView];
    
  [prologView resignFirstResponder];
  [inputView resignFirstResponder];

  UIButton *goButton = [UIButton buttonWithType:UIButtonTypeSystem];
  [goButton setTitle:@"Go!" forState:UIControlStateNormal];

  [goButton addTarget:self 
	      action:@selector(doQueryButton)
     forControlEvents:UIControlEventTouchUpInside];

  [self.view addSubview:goButton];

  [prologView setTranslatesAutoresizingMaskIntoConstraints:NO];
  [inputView setTranslatesAutoresizingMaskIntoConstraints:NO];
  [goButton setTranslatesAutoresizingMaskIntoConstraints:NO];

  self.viewsDictionary = NSDictionaryOfVariableBindings(prologView, inputView, goButton);

  [self.inputView performSelector:@selector(becomeFirstResponder) withObject:nil afterDelay:0];

  [[NSNotificationCenter defaultCenter] addObserver:self
					   selector:@selector(keyboardWillShow:)
					       name:UIKeyboardWillShowNotification
					     object:nil];
  [[NSNotificationCenter defaultCenter] addObserver:self
					   selector:@selector(keyboardWillHide:)
					       name:UIKeyboardWillHideNotification
					     object:nil];

  [self layout];
  [self runInterpreterThread: nil];
}

- (void) runInterpreterThread: (NSObject*)loop {
  extern int ios_initialize(void);
  extern int PL_toplevel(void);
  ios_initialize();

  if (loop != NULL) {
    for(;;) {
      int status = PL_toplevel() ? 0 : 1;
      PL_halt(status);
    }
  }
}

-(void)readFromPrologInput {
  extern void set_ios_input_string(char *str, int len);
  set_ios_input_string(self.inputString, self.inputLength);
  self.inputString = "";
  self.inputLength = 0;
}

- (void)addOutput: (NSString*)str {
  [self appendText: str];
}

- (char*)printAllOf: (int)varsSize variables: (char*)vars names: (char*[])names terms:(term_t)av {
  int status;
  char buf[1024];
  char *bufp = buf;
  size_t size = sizeof(buf);

  IOSTREAM *s = Sopenmem(&bufp, &size, "w");
  term_t write0 = PL_new_term_ref();
  PL_unify_stream(write0, s);
  term_t opts = PL_new_term_ref();
  status = PL_put_nil(opts);
  for (int ind = 0; ind < varsSize; ind++) {
    if (vars[ind]) {
      Sfputs(names[ind], s);
      Sfputs(" = ", s);
      pl_write_term3(write0, av+ind, opts);
    }
  }
  Sclose(s);
  return bufp;
}

- (void)doQueryButton {
  int status;

  if (self.threadId == 0) {
    self.threadId = PL_thread_attach_engine(NULL);
  }
  printf("threadId: %d\n", PL_thread_self());

  NSString *textValue = [NSString stringWithFormat:@"%@\n", self.inputView.text];
  [self.inputView setText: @""];
  self.inputString = [textValue cStringUsingEncoding:NSUTF8StringEncoding];
  self.inputLength = strlen(self.inputString);

  if (self.inputLength == 0) {
    return;
  }

  [self appendText: textValue];

  fid_t fid = PL_open_foreign_frame();
  if (!fid) {
    printf("opening foreign frame failed\n");
    return;
  }

  term_t term = PL_new_term_ref();
  status = PL_chars_to_term(self.inputString, term);

  status = PL_is_callable(term);
  printf("callable: %s\n", status ? "yes" : "no");

  functor_t functor;
  status = PL_get_functor(term, &functor);
  printf("functor: %s\n",  status ? "yes" : "no");

  atom_t atom;
  int arity = 0;
  status = PL_get_name_arity(term, &atom, &arity);
  printf("arity: %d\n", arity);

  module_t module;
  status = PL_get_module(term, &module);
  printf("module: %s\n",  status ? "yes" : "no");

  predicate_t pred;
  pred = PL_pred(functor, module);

  term_t av = PL_new_term_refs(arity);
  char *vars = (char*)PL_malloc(arity);
  char **names = (char**)PL_malloc(arity * sizeof(char*));

  for (int ind = 1; ind <= arity; ind++) {
    status = PL_get_arg(ind, term, av+(ind-1));
    vars[ind-1] = PL_is_variable(av+(ind-1));
    if (vars[ind-1]) {
      names[ind-1] = PL_malloc(32);
        varName(av+(ind-1), names[ind-1]);
    } else {
      names[ind-1] = NULL;
    }
  }
 /* for (int ind = 1; ind <= arity; ind++) {
    status =  PL_unify_arg(ind, term, av+(ind-1));
  }*/

  qid_t qid = PL_open_query(NULL, PL_Q_NODEBUG|PL_Q_ALLOW_YIELD|PL_Q_EXT_STATUS, pred, av);

  if (qid == 0) {
    printf("not enough memory\n");
    return;
  }
  while (true) {
    status = PL_next_solution(qid);
    if (status) {
      char *result = [self printAllOf: arity variables: vars names: names terms: av];
      Swrite_fileToPrologTextView(result, strlen(result));
      Swrite_fileToPrologTextView("\n", 1);
    } else {
      PL_close_query(qid);
      break;
    }
  }
  for (int ind = 1; ind <= arity; ind++) {
    if (vars[ind-1]) {
      PL_free(names[ind-1]);
    }
  }
  PL_free(names);
  PL_free(vars);
  PL_discard_foreign_frame(fid);
}

- (void)layout {
  [self.view addConstraints:
         [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[prologView]-5-|"
                                                 options:0
                                                 metrics:nil views:self.viewsDictionary]];
  [self.view addConstraints:
         [NSLayoutConstraint constraintsWithVisualFormat:@"H:|-5-[inputView]-[goButton]-5-|"
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

  NSArray *cnts = @[eq];
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
