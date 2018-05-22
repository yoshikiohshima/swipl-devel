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

#import "ViewController.h"

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
  [prologView setFont:[UIFont fontWithName:@"Helvetica" size:14]];
  prologView.layer.borderWidth = 2.0f;
  prologView.layer.borderColor = [[UIColor grayColor] CGColor];
  prologView.text = @"SWI-Prolog!\n";
  self.prologView = prologView;

  PrologInputView *inputView = [[PrologInputView alloc] init];
  [inputView setFont:[UIFont fontWithName:@"Helvetica" size:14]];
  inputView.layer.borderWidth = 2.0f;
  inputView.layer.borderColor = [[UIColor grayColor] CGColor];
  inputView.autocapitalizationType = UITextAutocapitalizationTypeNone;
  inputView.text = @"likes(sam,X)";
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

- (void)firstTime {
  int status;

  self.fid = PL_open_foreign_frame();
  if (!self.fid) {
    printf("opening foreign frame failed\n");
    return;
  }

  self.userTerm = PL_new_term_ref();
  status = PL_chars_to_term(self.inputString, self.userTerm);

  status = PL_is_callable(self.userTerm);
    if (!status) { return;}

  status = PL_get_functor(self.userTerm, &_functor);
    if (!status) {return;}
 
  atom_t atom;
  self.arity = 0;
  status = PL_get_name_arity(self.userTerm, &atom, &_arity);
  printf("arity: %d\n", self.arity);
    
  status = PL_get_module(self.userTerm, &_module);
  printf("module: %s\n",  status ? "yes" : "no");

  self.pred = PL_pred(self.functor, self.module);
  //self.pred = PL_predicate("teaches", 2, NULL);
  
  self.userArgs = PL_new_term_refs(self.arity);
  self.vars = (char*)PL_malloc(self.arity);
  self.names = (char**)PL_malloc(self.arity * sizeof(char*));

  for (int ind = 1; ind <= self.arity; ind++) {
    status = PL_get_arg(ind, self.userTerm, self.userArgs+(ind-1));
    self.vars[ind-1] = PL_is_variable(self.userArgs+(ind-1));
    if (self.vars[ind-1]) {
      self.names[ind-1] = PL_malloc(32);
      varName(self.userArgs+(ind-1), self.names[ind-1]);
    } else {
      self.names[ind-1] = NULL;
    }
  }
 /* for (int ind = 1; ind <= arity; ind++) {
    status =  PL_unify_arg(ind, term, av+(ind-1));
  }*/

  self.qid = PL_open_query(NULL, PL_Q_NODEBUG|PL_Q_ALLOW_YIELD|PL_Q_EXT_STATUS, self.pred, self.userArgs);

  if (self.qid == 0) {
    printf("not enough memory\n");
    [self lastTime];
  }
  self.queryState = 1;
}

- (void)eachTime {
  int status;
  if (self.qid) {
    status = PL_next_solution(self.qid);
    if (status) {
      char *result = [self printAllOf: self.arity variables: self.vars names: self.names terms: self.userArgs];
      Swrite_fileToPrologTextView(result, strlen(result));
      Swrite_fileToPrologTextView("\n", 1);
        
       UIButton *goButton = [self.viewsDictionary objectForKey: @"goButton"];
        [goButton setTitle:@";" forState:UIControlStateNormal];

    } else {
      [self lastTime];
    }
  }
}

- (void)lastTime {
  UIButton *goButton = [self.viewsDictionary objectForKey: @"goButton"];
  [goButton setTitle:@"Go!" forState:UIControlStateNormal];
  if (self.qid) {
    PL_close_query(self.qid);
    self.qid = 0;
  }

  for (int ind = 1; ind <= self.arity; ind++) {
    if (self.vars[ind-1]) {
      PL_free(self.names[ind-1]);
    }
  }
  PL_free(self.names);
  self.names = NULL;
  PL_free(self.vars);
  self.vars = NULL;

  if (self.fid) {
    PL_discard_foreign_frame(self.fid);
    self.fid = 0;
  }

  self.queryState = 0;
}

- (void)doQueryButton {
  if (self.threadId == 0) {
    self.threadId = PL_thread_attach_engine(NULL);
  }

  NSString *textValue = [NSString stringWithFormat:@"%@\n", self.inputView.text];
  [self.inputView setText: @""];
  self.inputString = [textValue cStringUsingEncoding:NSUTF8StringEncoding];
  self.inputLength = strlen(self.inputString);

  if (self.queryState == 0) {
    if (self.inputLength == 1 && self.inputString[0] == '\n') {
      return;
    }

    [self appendText: textValue];
    [self firstTime];
    // fall thrugh to queryState == 1 to execute the query once
  }

  if (self.queryState == 1) {
    if (self.inputLength >= 1 && self.inputString[0] == '.') {
        [self lastTime];
        return;
    }
    if (self.inputLength >= 1 && self.inputString[0] != '\n') {
      [self lastTime];
        [self appendText: textValue];
      [self firstTime];
    }
    [self eachTime];
  }
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
