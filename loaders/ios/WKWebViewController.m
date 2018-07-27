// LambdaNative WKWebView loader

/* portions:

 Copyright (c) 2014 Lee Barney
 Permission is hereby granted, free of charge, to any person obtaining a
 copy of this software and associated documentation files (the "Software"),
 to deal in the Software without restriction, including without limitation the
 rights to use, copy, modify, merge, publish, distribute, sublicense,
 and/or sell copies of the Software, and to permit persons to whom the Software
 is furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 
*/

#ifdef USE_HYBRID

#import <LNCONFIG.h>

#import "WKWebViewController.h"

@interface WKWebViewController ()
@property (strong, nonatomic)WKWebView *appWebView;
@property (nonatomic, assign) NSTimer *heartbeatTimer;
@end

@implementation WKWebViewController

@synthesize heartbeatTimer;

- (void)viewDidLoad {
  [super viewDidLoad];
  WKWebViewConfiguration *theConfiguration = [[WKWebViewConfiguration alloc] init];
  WKPreferences *thisPref = [[WKPreferences alloc] init];
  thisPref.javaScriptCanOpenWindowsAutomatically = YES;
  thisPref.javaScriptEnabled = YES;
  theConfiguration.preferences = thisPref;
  _appWebView = [[WKWebView alloc] initWithFrame:self.view.frame configuration:theConfiguration];
  _appWebView.navigationDelegate = self;
  _appWebView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
  NSURL *nsurl=[NSURL URLWithString:@"http://localhost:8080"];
  NSURLRequest *nsrequest=[NSURLRequest requestWithURL:nsurl];
  [_appWebView loadRequest:nsrequest];
  [self.view addSubview:_appWebView];
  self.heartbeatTimer = [NSTimer scheduledTimerWithTimeInterval:0.02
    target:self selector:@selector(heartbeatCallback) userInfo:nil repeats:YES];
}

- (void)webView:(WKWebView *)webView didFinishNavigation:(WKNavigation *)navigation {
}

- (void)didReceiveMemoryWarning {
  [super didReceiveMemoryWarning];
}

- (void) heartbeatCallback {
  ffi_event(EVENT_IDLE,0,0);
}

@end

#endif // USE_HYBRID

