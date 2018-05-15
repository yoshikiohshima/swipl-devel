//
//  PrologInputView.m
//  Prolog
//
//  Created by Yoshiki Ohshima on 2018/05/08.
//  Copyright © 2018年 Yoshiki Ohshima. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "PrologInputView.h"


@implementation PrologInputView
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
    self.textContainer.maximumNumberOfLines = 3;
    self.textContainer.lineBreakMode = NSLineBreakByTruncatingTail;
    self.scrollEnabled = false;
    self.editable = TRUE;
    self.selectable = TRUE;
}

- (void)awakeFromNib{
    [super awakeFromNib];
}

@end
