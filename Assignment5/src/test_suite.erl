-module(test_suite).
-export([test/0]).
% -on_load(test/0).

test() -> 
  simple_test:runall()
  .