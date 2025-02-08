# erlsvgtps

[![Build Status](https://app.travis-ci.com/silviucpp/erlsvgtps.svg?branch=main)](https://travis-ci.com/github/silviucpp/erlsvgtps)
[![GitHub](https://img.shields.io/github/license/silviucpp/erlsvgtps)](https://github.com/silviucpp/erlsvgtps/blob/main/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/erlsvgtps)](https://hex.pm/packages/erlsvgtps)

Erlang SVG (Portable and Secure) converter for BIMI compliance. Project inspired by [php-svg-ps-converter][1].

## Summary

The choice of a specific SVG format for BIMI is intentional. Standard SVG files can include JavaScript, posing a security threat if displayed in an email client. To prevent this risk, BIMI requires the use of SVG Tiny P/S (Portable/Secure), a streamlined version of SVG that eliminates scripting and interactive elements. This guarantees that the logo remains purely a visual identifier, free from any potentially harmful code that could jeopardize the recipient’s security.

This project allows you to identify all issues that your vector image has and also to convert the vector image into a more constrained and secure SVG format. 
The new format is based [SVG Tiny 1.2][2] as defined by the W3C.

## Usage

### Identifying issues

```erlang
{ok, State} = erlsvgtps_check:from_file(<<"test/assets/waffle-icon.svg">>).

% identify issues

erlsvgtps_check:issues(S).

% or getting file metadata

erlsvgtps_check:metadata(S).
```

### Convert file to a BIMI compliant format

```erlang
{ok, _NewSvgContent} = erlsvgtps_converter:from_file(<<"test/assets/waffle-icon.svg">>).
```

Some issues cannot be resolved automatically. You can retrieve them by rerunning `erlsvgtps_check` functions.  Specifically, the issues that 
require manual intervention include:

- The SVG contains an image
- The SVG is not square
- The SVG size > 32kb

### Other

For more functionality, check the exported functions from:
- `erlsvgtps_check` - the module responsible for checking issues.
- `erlsvgtps_converter` - the module responsible for converting the file.

## Running Tests

```sh
rebar3 eunit
```

[1]:https://github.com/SRWieZ/php-svg-ps-converter
[2]:https://www.w3.org/TR/SVGTiny12/
