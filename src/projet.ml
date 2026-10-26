

let () =
 
 
  Printf.printf " TESTS DES OPÉRATIONS DE BASE\n";
  Tests_operations.run_ops_tests ();

  Printf.printf "\n TESTS DES DEPENDANCES)\n";
  
  Tests_deps.run_deps_tests ()
;;

  