int main() {
  Object const main_term = create_main_term();
  sll_print_value(main_term, constructor_names);
  sll_print_value(sll_read_value("\nx", constructor_names, SllNumofCtrs), constructor_names);
  sll_finalize();
  return 0;
}
