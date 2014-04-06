int main() {
  sll_initialize();
  Object const main_term = create_main_term();
  sll_print_value(main_term, constructor_names);
  sll_finalize();
  return 0;
}
