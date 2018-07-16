open OUnit

let test1 _ = 
    assert_equal (Mymodule.hello_world ()) "hello world"

let suite =
  "suite">:::
  [
    "test1">:: test1;
  ]

let () = 
  ignore (run_test_tt_main suite)
