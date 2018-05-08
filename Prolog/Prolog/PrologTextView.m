//
//  PrologTextView.m
//  Prolog
//
//  Created by Yoshiki Ohshima on 2018/05/08.
//  Copyright © 2018年 Yoshiki Ohshima. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "PrologTextView.h"

@implementation PrologTextView
- (id)initWithCoder:(NSCoder *)aDecoder{
    self = [super initWithCoder:aDecoder];
    if (self)
        [self initialSetup];
    return self;
}

- (id)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    if (self)
        [self initialSetup];
    return self;
}

- (void)initialSetup{
    self.textContainer.maximumNumberOfLines = 1;
    self.textContainer.lineBreakMode = NSLineBreakByTruncatingTail;
    self.scrollEnabled = false;
    self.editable = FALSE;
    self.selectable = FALSE;
}

- (void)awakeFromNib{
    [super awakeFromNib];
}

@end

