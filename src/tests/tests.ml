
(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/cardinalPrint.bird" "3\n2\n1";
    test_runtime_failure "test_code/cardinalIntTest.bird" 1;
    test_runtime_failure "test_code/cardinalIntTest2.bird" 1;
    (*test for int check*)
    test_runtime_failure "test_code/cardinalTestOpPlus1.bird" 1;
    test_runtime_failure "test_code/cardinalTestOpPlus2.bird" 1;
    test_runtime_failure "test_code/cardinalTestOpPlus3.bird" 0;
    test_runtime_failure "test_code/cadinalTestMult2.bird" 1;
    test_runtime_failure "test_code/cadinalTestMult1.bird" 1;
    test_runtime_failure "test_code/cadinalTestMult3.bird" 0;


    (*test for bool check*)
    test_runtime_failure "test_code/cardinalAndTest1.bird" 2;
 
    test_success "test_code/cardinaltest1.bird" "7";
    test_success "test_code/isint2.bird" "true";
    test_success "test_code/trueandfalse.bird" "false";
    test_runtime_failure "test_code/4and1.bird" 2;
    test_success "test_code/4equal3.bird" "false";
    test_success "test_code/4equal4.bird" "true";
    test_success "test_code/4greaterThan3.bird" "true";
    test_success "test_code/4multiply3.bird" "12";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/minus2.bird" "-2";

    (* Dove tests 
    test_success "test_code/dove_single_param.bird" "5";
    test_success "test_code/dove_two_params.bird" "7";
    test_success "test_code/dove_arg_order.bird" "-2";
    test_success "test_code/dove_recursive.bird" "15";
    test_success "test_code/dove_mutual.bird" "true";
    test_success "test_code/dove_print_order.bird" "1\n3\n4";
    test_compile_failure "test_code/dove_dup_params.bird" "Function f has duplicate parameter x.";
    test_compile_failure "test_code/dove_dup_functions.bird" "Duplicate definition of function f.";
    test_compile_failure "test_code/dove_wrong_args.bird" "Function f is called with an incorrect number of arguments";
    test_compile_failure "test_code/dove_undefined_function.bird" "Function g is not defined.";
    *)
    
    (*Falcon Tests*)
    test_success "test_code/FalconStep1.bird" "<closure@0000000000401188>[0/3](?, ?, ?)";
    test_success "test_code/FalconStep2.bird" "<closure@0000000000401188>[0/6](?, ?, ?, ?, ?, ?)"; 
    test_success "test_code/falconStep3.bird" "<closure@0000000000401188>[2/3](10, 20, ?)"; 
    test_success "test_code/FalconTest4.bird" "6"; 
    test_success "test_code/FalconTuple5.bird" "(4, 8)"; 
    test_success "test_code/FalconFalse.bird" "false"; 
    test_success "test_code/ProfTestFalcon.bird" "(1, (2, (4, (8, (16, false)))))\n(2, (4, (8, (16, false))))\n(4, (8, (16, false)))\n(8, (16, false))\n(16, false)\nfalse\n31";
    (* Eagle tests *)
    test_success "test_code/eagle_simple_tuple.bird" "(6, 11)";
    test_success "test_code/eagle_nested_tuple.bird" "(1, (false, 3))";
    test_success "test_code/eagle_print_order.bird" "1\nfalse\n1";
    test_success "test_code/eagle_istuple_true.bird" "true";
    test_success "test_code/eagle_istuple_false_int.bird" "false";
    test_success "test_code/eagle_istuple_false_bool.bird" "false";
    test_success "test_code/eagle_istuple_mixed.bird" "(false, true)";
    test_success "test_code/eagle_index_basic.bird" "22";
    test_success "test_code/eagle_index_mixed.bird" "(3, 4)";
    test_success "test_code/eagle_index_calc.bird" "30";
    test_success "test_code/eagleFinalTest.bird" "9\n45";
    test_runtime_failure "test_code/eagle_index_err_not_tuple.bird" 3;
    test_runtime_failure "test_code/eagle_index_err_not_tuple_bool.bird" 3;
    test_runtime_failure "test_code/eagle_index_err_not_int.bird" 1;
    test_runtime_failure "test_code/eagle_index_err_not_int_tuple.bird" 1;
    test_runtime_failure "test_code/eagle_index_err_neg.bird" 4;
    test_runtime_failure "test_code/eagle_index_err_oob.bird" 4;
    test_runtime_failure "test_code/eagle_op_plus_tuple.bird" 1;
    test_runtime_failure "test_code/eagle_op_after_tuple.bird" 1;
    test_runtime_failure "test_code/eagle_op_and_tuple.bird" 2;
    test_runtime_failure "test_code/eagle_op_if_tuple.bird" 2;
    
    (*Lorikeet 
    test_success "test_code/parallelTest1.bird" "10\n8\n2"; *)
    test_success "test_code/parallelTest2.bird" "42\n42";
  ];;



let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
