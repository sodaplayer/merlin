open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper
  let default_loc = ref Location.none
  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))
  let default_pattern () = Pat.any ~loc:!default_loc ()
  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_HASH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> []
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;2;3;1;1;2;3;3;4;1;2;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;1;1;1;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;4;2;3;4;2;3;4;1;1;1;1;1;1;2;3;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;1;2;1;2;3;4;5;1;1;1;1;2;1;2;3;4;1;2;3;4;1;2;1;2;3;1;1;1;2;3;1;2;3;1;2;1;2;3;1;4;1;1;2;2;1;2;2;1;1;1;1;1;2;1;2;2;2;3;4;5;6;6;1;1;2;1;2;3;1;4;1;1;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;2;1;2;1;1;1;2;1;2;1;2;1;2;3;4;5;6;1;1;1;1;1;2;1;1;2;1;2;2;1;1;2;2;1;2;1;1;2;1;2;1;2;3;3;4;2;3;2;3;1;3;2;3;2;1;2;3;4;1;2;3;3;1;1;3;4;2;3;1;2;1;3;4;2;1;3;2;3;4;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;3;2;3;4;5;4;2;3;2;3;2;3;1;2;2;1;1;2;3;1;2;3;2;3;4;5;6;7;1;3;4;1;2;1;1;2;1;1;1;1;2;1;1;2;3;1;2;3;2;1;1;2;3;4;2;3;4;1;1;2;1;1;2;2;1;2;3;1;2;3;1;2;1;2;3;4;5;6;4;4;3;4;5;3;3;1;7;8;1;2;1;1;2;3;4;1;2;3;1;1;2;3;1;2;3;1;1;1;1;2;2;1;2;2;2;3;3;4;5;6;6;1;2;3;4;1;2;1;2;3;4;5;6;7;8;1;2;1;1;2;1;1;2;1;2;3;4;5;1;1;1;1;2;1;1;1;2;2;3;1;2;1;2;3;4;1;5;2;1;2;1;1;2;3;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;2;3;4;1;3;2;3;2;3;2;1;2;3;3;1;1;1;4;5;3;4;1;5;2;1;2;3;4;1;2;3;4;1;2;1;2;3;1;2;3;1;2;3;4;4;5;6;1;2;2;3;4;1;2;3;4;2;3;2;3;4;5;1;1;1;2;3;2;1;1;2;1;2;1;1;2;3;1;3;2;3;2;1;2;3;4;1;2;3;3;5;1;5;2;3;4;4;5;6;2;3;4;5;2;1;1;2;3;1;4;2;3;3;4;2;3;3;4;1;3;2;3;2;3;2;1;2;3;3;1;1;2;3;3;2;3;3;2;6;1;2;7;2;3;4;1;1;2;3;4;1;5;2;3;1;2;3;1;1;2;3;4;1;1;1;1;2;3;1;1;1;1;2;3;2;3;3;1;1;2;2;1;1;1;2;1;2;3;3;1;2;3;1;1;2;1;1;1;1;2;1;1;4;1;1;2;3;1;1;1;2;3;3;4;4;1;2;3;1;1;1;2;3;2;3;3;2;1;2;1;1;2;4;4;5;4;5;5;2;3;3;2;3;3;2;3;1;2;2;3;3;3;3;4;2;3;3;1;2;3;3;1;2;2;3;3;2;3;4;5;1;6;5;1;2;3;1;1;2;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;4;4;5;4;5;5;3;4;2;3;3;3;1;1;1;2;3;1;1;1;2;3;2;3;3;2;1;2;1;2;4;2;3;2;3;3;3;4;5;2;3;3;2;3;3;4;1;1;2;3;4;5;1;2;3;1;2;3;4;5;6;7;1;1;2;3;1;1;2;1;1;1;2;1;2;3;3;4;2;1;2;3;1;1;1;1;1;2;3;4;5;6;7;1;1;2;2;3;3;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;1;1;2;3;1;2;3;1;1;2;1;2;3;1;1;2;1;2;3;3;4;5;1;2;4;5;1;2;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;9;10;2;2;1;1;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;2;3;1;1;1;2;1;2;1;2;2;3;2;3;1;2;3;1;1;2;3;4;1;2;3;4;5;6;7;1;1;2;3;4;5;6;7;8;9;1;2;1;2;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;1;2;3;4;1;2;1;2;1;2;2;1;2;2;3;4;1;2;3;1;1;1;2;5;1;2;3;2;3;3;3;4;4;5;5;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;2;3;4;5;1;2;3;2;3;3;2;3;3;2;3;3;2;3;3;2;1;1;2;3;3;4;2;2;3;5;6;1;1;7;8;9;10;11;1;2;3;4;5;6;7;8;9;10;11;1;2;3;4;1;1;1;2;1;1;2;3;4;4;5;6;7;8;9;9;10;1;1;1;1;1;2;3;4;2;3;2;3;1;1;1;2;1;2;1;2;2;3;2;3;4;5;5;1;2;1;2;1;2;3;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;3;4;5;5;3;4;5;5;3;4;5;6;7;7;5;6;7;7;5;6;7;7;3;1;2;2;3;4;5;5;6;7;3;4;5;5;6;7;3;4;5;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;1;2;2;1;2;4;5;3;4;5;3;4;5;4;5;5;3;4;4;5;5;1;2;3;3;4;4;5;5;3;4;5;5;3;1;2;3;1;1;2;2;1;2;2;3;4;1;2;3;4;5;1;4;5;5;1;2;3;3;4;4;5;5;3;4;4;5;5;3;4;5;5;3;4;5;5;3;3;4;5;5;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;5;2;3;3;2;1;2;3;4;1;4;5;5;3;4;5;5;3;4;5;5;3;4;5;6;7;7;5;6;7;7;5;6;7;7;3;1;2;2;3;4;5;5;6;7;3;4;5;5;6;7;3;4;5;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;1;2;2;1;2;4;5;6;7;8;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;4;5;5;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;1;2;3;4;1;4;5;6;7;8;2;3;2;4;4;4;5;5;4;2;3;2;2;3;3;2;3;3;2;3;3;8;3;4;5;6;7;2;3;4;5;1;2;3;4;6;7;8;1;2;2;3;4;5;6;7;8;9;2;3;4;5;6;2;2;3;4;2;2;3;3;2;4;5;2;2;3;2;3;3;4;5;6;2;2;3;3;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;4;5;6;6;5;6;7;2;1;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;1;3;4;1;2;3;4;1;2;1;2;3;1;1;2;5;5;6;7;8;9;10;11;1;2;3;6;7;8;1;5;2;3;1;1;2;1;2;2;3;4;1;2;1;2;3;5;1;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;1;2;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;4;5;1;1;1;2;3;6;7;8;5;6;7;1;1;2;3;4;5;6;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;5;6;6;7;1;2;5;6;1;2;4;5;6;7;8;1;2;3;4;5;6;7;9;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;1;2;3;1;2;3;4;1;2;3;1;2;3;1;2;1;2;3;3;4;4;5;5;1;2;1;1;2;1;2;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;2;3;1;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;3;1;2;3;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;1;1;7;2;3;4;5;6;3;4;1;2;3;4;4;5;5;1;2;1;1;2;1;2;3;4;1;1;1;2;1;2;3;1;2;3;1;4;1;3;5;4;5;5;4;1;2;5;6;2;3;4;5;4;5;5;1;2;3;3;3;4;5;5;1;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;7;3;4;5;6;2;1;1;2;3;4;1;2;3;4;5;6;1;1;2;3;1;2;3;4;1;1;7;2;3;4;5;6;1;1;2;1;2;3;1;2;3;1;4;1;3;5;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;1;2;1;1;2;1;1;2;3;4;5;6;7;8;2;1;1;1;1;2;3;3;4;1;1;1;4;5;5;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;2;1;2;1;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;3;4;3;4;3;4;5;6;6;2;2;3;3;2;2;3;4;5;6;6;7;7;8;2;3;2;3;3;3;4;4;5;5;5;6;6;7;5;6;6;7;7;8;9;1;2;3;4;1;5;2;3;2;3;3;3;4;5;5;2;2;1;2;1;2;3;3;3;4;5;5;2;5;6;4;5;6;7;1;2;3;4;5;6;8;3;4;2;3;4;3;4;9;6;7;8;1;1;2;3;1;2;1;1;2;1;1;2;3;1;2;3;2;1;3;1;1;1;2;3;4;5;1;2;3;4;2;3;4;9;10;2;2;1;1;1;1;1;2;3;4;2;3;4;5;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;1;2;6;7;2;3;4;1;2;1;2;3;4;5;6;1;2;4;5;6;7;1;2;3;4;5;6;8;1;2;3;4;1;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;2;3;4;5;1;2;3;5;6;1;2;3;4;5;6;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;4;5;6;7;8;5;6;7;1;2;3;4;5;6;7;8;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;1;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;1;1;2;3;4;2;1;2;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;1;2;1;3;4;4;1;2;3;1;5;7;8;8;2;3;3;3;4;4;5;5;2;2;3;3;2;3;4;4;5;6;6;4;5;6;7;8;5;6;4;5;5;5;6;6;7;5;6;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;9;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;3;4;5;6;7;2;2;3;3;4;5;2;3;4;5;4;2;3;1;2;3;4;2;1;2;1;1;2;1;1;2;2;1;1;2;3;1;2;1;2;3;4;5;6;4;4;3;4;5;3;3;1;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;5;6;6;7;1;2;5;6;1;2;4;5;6;7;8;1;2;3;4;5;6;7;9;1;2;3;4;5;1;2;3;4;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;2;1;2;1;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;3;4;4;1;7;8;8;5;6;3;4;5;6;4;5;6;4;5;6;7;3;4;4;5;5;6;6;7;3;4;4;5;6;6;7;3;4;3;4;5;6;6;4;5;6;7;2;3;4;5;6;6;7;7;8;2;3;3;3;4;2;4;5;6;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;1;2;0;1;2;1;0;1;2;1;0;1;2;3;3;3;3;3;3;2;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SIG -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTIONQUESTION -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET_LWT -> true
  | T_LET -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_HASH -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 571] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 125] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 277 :: r6 in
  let r8 = R 189 :: r7 in
  let r9 = [R 698] in
  let r10 = S (T T_AND) :: r9 in
  let r11 = [R 32] in
  let r12 = Sub (r10) :: r11 in
  let r13 = [R 190] in
  let r14 = Sub (r12) :: r13 in
  let r15 = [R 33] in
  let r16 = Sub (r12) :: r15 in
  let r17 = [R 492] in
  let r18 = S (N N_structure) :: r17 in
  let r19 = [R 34] in
  let r20 = S (T T_RBRACKET) :: r19 in
  let r21 = Sub (r18) :: r20 in
  let r22 = Sub (r12) :: r21 in
  let r23 = [R 141] in
  let r24 = S (T T_DONE) :: r23 in
  let r25 = Sub (r1) :: r24 in
  let r26 = S (T T_DO) :: r25 in
  let r27 = Sub (r1) :: r26 in
  let r28 = R 277 :: r27 in
  let r29 = R 189 :: r28 in
  let r30 = [R 332] in
  let r31 = [R 121] in
  let r32 = Sub (r1) :: r31 in
  let r33 = R 277 :: r32 in
  let r34 = R 189 :: r33 in
  let r35 = [R 137] in
  let r36 = S (N N_match_cases) :: r35 in
  let r37 = S (T T_WITH) :: r36 in
  let r38 = Sub (r1) :: r37 in
  let r39 = R 277 :: r38 in
  let r40 = R 189 :: r39 in
  let r41 = [R 107] in
  let r42 = S (T T_FALSE) :: r41 in
  let r43 = [R 639] in
  let r44 = Sub (r42) :: r43 in
  let r45 = [R 641] in
  let r46 = Sub (r44) :: r45 in
  let r47 = [R 56] in
  let r48 = S (T T_LIDENT) :: r47 in
  let r49 = [R 633] in
  let r50 = Sub (r48) :: r49 in
  let r51 = R 277 :: r50 in
  let r52 = R 189 :: r51 in
  let r53 = [R 57] in
  let r54 = S (T T_LIDENT) :: r53 in
  let r55 = S (T T_DOT) :: r54 in
  let r56 = [R 333] in
  let r57 = [R 278] in
  let r58 = R 277 :: r57 in
  let r59 = [R 811] in
  let r60 = S (T T_error) :: r59 in
  let r61 = [R 145] in
  let r62 = S (T T_END) :: r61 in
  let r63 = R 294 :: r62 in
  let r64 = R 60 :: r63 in
  let r65 = R 277 :: r64 in
  let r66 = R 189 :: r65 in
  let r67 = S (T T_LPAREN) :: r60 in
  let r68 = [R 15] in
  let r69 = S (T T_UNDERSCORE) :: r68 in
  let r70 = [R 786] in
  let r71 = Sub (r69) :: r70 in
  let r72 = [R 203] in
  let r73 = Sub (r71) :: r72 in
  let r74 = [R 9] in
  let r75 = Sub (r73) :: r74 in
  let r76 = [R 115] in
  let r77 = Sub (r75) :: r76 in
  let r78 = [R 820] in
  let r79 = R 283 :: r78 in
  let r80 = Sub (r77) :: r79 in
  let r81 = S (T T_COLON) :: r80 in
  let r82 = Sub (r67) :: r81 in
  let r83 = R 277 :: r82 in
  let r84 = R 189 :: r83 in
  let r85 = [R 812] in
  let r86 = S (T T_error) :: r85 in
  let r87 = [R 397] in
  let r88 = S (T T_RPAREN) :: r87 in
  let r89 = [R 399] in
  let r90 = [R 401] in
  let r91 = [R 809] in
  let r92 = S (T T_RPAREN) :: r91 in
  let r93 = [R 328] in
  let r94 = [R 223] in
  let r95 = S (T T_LIDENT) :: r94 in
  let r96 = [R 14] in
  let r97 = Sub (r95) :: r96 in
  let r98 = [R 446] in
  let r99 = S (T T_COLON) :: r98 in
  let r100 = [R 13] in
  let r101 = S (T T_RPAREN) :: r100 in
  let r102 = S (N N_module_type) :: r101 in
  let r103 = R 277 :: r102 in
  let r104 = R 189 :: r103 in
  let r105 = S (T T_MODULE) :: r104 in
  let r106 = [R 576] in
  let r107 = R 285 :: r106 in
  let r108 = [R 350] in
  let r109 = S (T T_END) :: r108 in
  let r110 = Sub (r107) :: r109 in
  let r111 = R 277 :: r110 in
  let r112 = [R 353] in
  let r113 = S (N N_module_expr) :: r112 in
  let r114 = R 277 :: r113 in
  let r115 = S (T T_OF) :: r114 in
  let r116 = S (T T_TYPE) :: r115 in
  let r117 = [R 339] in
  let r118 = S (T T_END) :: r117 in
  let r119 = S (N N_structure) :: r118 in
  let r120 = R 277 :: r119 in
  let r121 = [R 120] in
  let r122 = S (N N_match_cases) :: r121 in
  let r123 = S (T T_WITH) :: r122 in
  let r124 = Sub (r1) :: r123 in
  let r125 = R 277 :: r124 in
  let r126 = R 189 :: r125 in
  let r127 = [R 136] in
  let r128 = S (N N_match_cases) :: r127 in
  let r129 = S (T T_WITH) :: r128 in
  let r130 = Sub (r1) :: r129 in
  let r131 = R 277 :: r130 in
  let r132 = R 189 :: r131 in
  let r133 = [R 176] in
  let r134 = S (N N_expr) :: r133 in
  let r135 = S (T T_LESSMINUS) :: r134 in
  let r136 = [R 749] in
  let r137 = Sub (r1) :: r136 in
  let r138 = S (T T_EQUAL) :: r137 in
  let r139 = [R 249] in
  let r140 = Sub (r138) :: r139 in
  let r141 = Sub (r67) :: r140 in
  let r142 = [R 307] in
  let r143 = R 283 :: r142 in
  let r144 = Sub (r141) :: r143 in
  let r145 = R 510 :: r144 in
  let r146 = R 277 :: r145 in
  let r147 = R 189 :: r146 in
  let r148 = [R 678] in
  let r149 = [R 598] in
  let r150 = S (T T_INT) :: r149 in
  let r151 = [R 596] in
  let r152 = S (T T_INT) :: r151 in
  let r153 = [R 105] in
  let r154 = [R 676] in
  let r155 = S (T T_RPAREN) :: r154 in
  let r156 = S (T T_UIDENT) :: r155 in
  let r157 = R 277 :: r156 in
  let r158 = [R 677] in
  let r159 = S (T T_RPAREN) :: r158 in
  let r160 = S (N N_module_type) :: r159 in
  let r161 = [R 795] in
  let r162 = S (T T_LIDENT) :: r161 in
  let r163 = [R 100] in
  let r164 = S (T T_FALSE) :: r163 in
  let r165 = [R 217] in
  let r166 = R 277 :: r165 in
  let r167 = R 212 :: r166 in
  let r168 = Sub (r164) :: r167 in
  let r169 = [R 523] in
  let r170 = Sub (r168) :: r169 in
  let r171 = [R 583] in
  let r172 = R 283 :: r171 in
  let r173 = Sub (r170) :: r172 in
  let r174 = R 503 :: r173 in
  let r175 = S (T T_PLUSEQ) :: r174 in
  let r176 = Sub (r162) :: r175 in
  let r177 = R 798 :: r176 in
  let r178 = R 277 :: r177 in
  let r179 = R 189 :: r178 in
  let r180 = [R 584] in
  let r181 = R 283 :: r180 in
  let r182 = Sub (r170) :: r181 in
  let r183 = R 503 :: r182 in
  let r184 = S (T T_PLUSEQ) :: r183 in
  let r185 = Sub (r162) :: r184 in
  let r186 = R 798 :: r185 in
  let r187 = [R 802] in
  let r188 = S (T T_UNDERSCORE) :: r187 in
  let r189 = [R 797] in
  let r190 = Sub (r188) :: r189 in
  let r191 = R 803 :: r190 in
  let r192 = [R 547] in
  let r193 = Sub (r191) :: r192 in
  let r194 = [R 800] in
  let r195 = S (T T_RPAREN) :: r194 in
  let r196 = [R 801] in
  let r197 = [R 548] in
  let r198 = [R 382] in
  let r199 = S (T T_DOTDOT) :: r198 in
  let r200 = [R 794] in
  let r201 = Sub (r199) :: r200 in
  let r202 = [R 383] in
  let r203 = S (T T_DOTDOT) :: r202 in
  let r204 = [R 98] in
  let r205 = S (T T_RPAREN) :: r204 in
  let r206 = [R 205] in
  let r207 = Sub (r73) :: r206 in
  let r208 = S (T T_MINUSGREATER) :: r207 in
  let r209 = Sub (r71) :: r208 in
  let r210 = S (T T_COLON) :: r209 in
  let r211 = [R 20] in
  let r212 = S (T T_GREATER) :: r211 in
  let r213 = [R 499] in
  let r214 = Sub (r75) :: r213 in
  let r215 = [R 318] in
  let r216 = R 277 :: r215 in
  let r217 = Sub (r214) :: r216 in
  let r218 = [R 534] in
  let r219 = Sub (r95) :: r218 in
  let r220 = [R 191] in
  let r221 = S (T T_RBRACKET) :: r220 in
  let r222 = Sub (r18) :: r221 in
  let r223 = Sub (r12) :: r222 in
  let r224 = [R 517] in
  let r225 = Sub (r168) :: r224 in
  let r226 = [R 761] in
  let r227 = R 283 :: r226 in
  let r228 = Sub (r225) :: r227 in
  let r229 = R 503 :: r228 in
  let r230 = S (T T_PLUSEQ) :: r229 in
  let r231 = Sub (r162) :: r230 in
  let r232 = R 798 :: r231 in
  let r233 = R 277 :: r232 in
  let r234 = R 189 :: r233 in
  let r235 = [R 220] in
  let r236 = R 283 :: r235 in
  let r237 = R 526 :: r236 in
  let r238 = R 793 :: r237 in
  let r239 = S (T T_LIDENT) :: r238 in
  let r240 = R 798 :: r239 in
  let r241 = R 277 :: r240 in
  let r242 = R 189 :: r241 in
  let r243 = [R 762] in
  let r244 = R 283 :: r243 in
  let r245 = Sub (r225) :: r244 in
  let r246 = R 503 :: r245 in
  let r247 = S (T T_PLUSEQ) :: r246 in
  let r248 = Sub (r162) :: r247 in
  let r249 = R 798 :: r248 in
  let r250 = [R 221] in
  let r251 = R 283 :: r250 in
  let r252 = R 526 :: r251 in
  let r253 = R 793 :: r252 in
  let r254 = S (T T_LIDENT) :: r253 in
  let r255 = R 798 :: r254 in
  let r256 = [R 558] in
  let r257 = Sub (r77) :: r256 in
  let r258 = [R 543] in
  let r259 = Sub (r257) :: r258 in
  let r260 = [R 29] in
  let r261 = S (T T_RBRACKET) :: r260 in
  let r262 = Sub (r259) :: r261 in
  let r263 = R 427 :: r262 in
  let r264 = [R 28] in
  let r265 = S (T T_RBRACKET) :: r264 in
  let r266 = [R 27] in
  let r267 = S (T T_RBRACKET) :: r266 in
  let r268 = Sub (r259) :: r267 in
  let r269 = [R 371] in
  let r270 = Sub (r95) :: r269 in
  let r271 = S (T T_BACKQUOTE) :: r270 in
  let r272 = [R 774] in
  let r273 = R 277 :: r272 in
  let r274 = Sub (r271) :: r273 in
  let r275 = [R 24] in
  let r276 = S (T T_RBRACKET) :: r275 in
  let r277 = Sub (r274) :: r276 in
  let r278 = [R 21] in
  let r279 = Sub (r48) :: r278 in
  let r280 = [R 25] in
  let r281 = S (T T_RBRACKET) :: r280 in
  let r282 = Sub (r259) :: r281 in
  let r283 = [R 206] in
  let r284 = Sub (r73) :: r283 in
  let r285 = [R 555] in
  let r286 = Sub (r69) :: r285 in
  let r287 = [R 331] in
  let r288 = S (T T_error) :: r287 in
  let r289 = [R 796] in
  let r290 = S (T T_LIDENT) :: r289 in
  let r291 = S (T T_DOT) :: r290 in
  let r292 = [R 330] in
  let r293 = S (T T_RPAREN) :: r292 in
  let r294 = [R 329] in
  let r295 = S (T T_UIDENT) :: r294 in
  let r296 = [R 22] in
  let r297 = Sub (r48) :: r296 in
  let r298 = [R 204] in
  let r299 = Sub (r73) :: r298 in
  let r300 = S (T T_MINUSGREATER) :: r299 in
  let r301 = Sub (r71) :: r300 in
  let r302 = [R 556] in
  let r303 = Sub (r69) :: r302 in
  let r304 = [R 544] in
  let r305 = [R 539] in
  let r306 = Sub (r75) :: r305 in
  let r307 = [R 773] in
  let r308 = R 277 :: r307 in
  let r309 = Sub (r306) :: r308 in
  let r310 = [R 540] in
  let r311 = [R 10] in
  let r312 = Sub (r95) :: r311 in
  let r313 = S (T T_QUOTE) :: r312 in
  let r314 = [R 26] in
  let r315 = S (T T_RBRACKET) :: r314 in
  let r316 = Sub (r259) :: r315 in
  let r317 = S (T T_BAR) :: r316 in
  let r318 = [R 532] in
  let r319 = Sub (r271) :: r318 in
  let r320 = [R 30] in
  let r321 = S (T T_RBRACKET) :: r320 in
  let r322 = Sub (r319) :: r321 in
  let r323 = [R 99] in
  let r324 = S (T T_RPAREN) :: r323 in
  let r325 = [R 18] in
  let r326 = Sub (r162) :: r325 in
  let r327 = S (T T_RPAREN) :: r326 in
  let r328 = [R 23] in
  let r329 = Sub (r48) :: r328 in
  let r330 = [R 551] in
  let r331 = [R 12] in
  let r332 = S (T T_RPAREN) :: r331 in
  let r333 = [R 552] in
  let r334 = [R 97] in
  let r335 = S (T T_RBRACKET) :: r334 in
  let r336 = [R 227] in
  let r337 = R 277 :: r336 in
  let r338 = Sub (r214) :: r337 in
  let r339 = S (T T_COLON) :: r338 in
  let r340 = S (T T_LIDENT) :: r339 in
  let r341 = R 364 :: r340 in
  let r342 = [R 229] in
  let r343 = Sub (r341) :: r342 in
  let r344 = [R 387] in
  let r345 = S (T T_RBRACE) :: r344 in
  let r346 = Sub (r343) :: r345 in
  let r347 = [R 228] in
  let r348 = R 277 :: r347 in
  let r349 = S (T T_SEMI) :: r348 in
  let r350 = R 277 :: r349 in
  let r351 = Sub (r214) :: r350 in
  let r352 = S (T T_COLON) :: r351 in
  let r353 = [R 500] in
  let r354 = Sub (r75) :: r353 in
  let r355 = [R 535] in
  let r356 = [R 216] in
  let r357 = R 277 :: r356 in
  let r358 = R 212 :: r357 in
  let r359 = [R 110] in
  let r360 = Sub (r69) :: r359 in
  let r361 = [R 213] in
  let r362 = Sub (r360) :: r361 in
  let r363 = [R 112] in
  let r364 = S (T T_RBRACE) :: r363 in
  let r365 = Sub (r343) :: r364 in
  let r366 = [R 111] in
  let r367 = Sub (r69) :: r366 in
  let r368 = S (T T_STAR) :: r367 in
  let r369 = [R 215] in
  let r370 = Sub (r69) :: r369 in
  let r371 = [R 214] in
  let r372 = Sub (r69) :: r371 in
  let r373 = S (T T_MINUSGREATER) :: r372 in
  let r374 = Sub (r164) :: r358 in
  let r375 = [R 386] in
  let r376 = S (T T_RBRACE) :: r375 in
  let r377 = Sub (r343) :: r376 in
  let r378 = [R 384] in
  let r379 = S (T T_DOTDOT) :: r378 in
  let r380 = [R 385] in
  let r381 = S (T T_DOTDOT) :: r380 in
  let r382 = [R 389] in
  let r383 = S (T T_RBRACE) :: r382 in
  let r384 = Sub (r343) :: r383 in
  let r385 = [R 388] in
  let r386 = S (T T_RBRACE) :: r385 in
  let r387 = Sub (r343) :: r386 in
  let r388 = [R 501] in
  let r389 = S (T T_RBRACKET) :: r388 in
  let r390 = Sub (r18) :: r389 in
  let r391 = Sub (r12) :: r390 in
  let r392 = [R 289] in
  let r393 = R 288 :: r392 in
  let r394 = [R 390] in
  let r395 = R 283 :: r394 in
  let r396 = S (N N_module_expr) :: r395 in
  let r397 = R 277 :: r396 in
  let r398 = R 189 :: r397 in
  let r399 = [R 391] in
  let r400 = R 283 :: r399 in
  let r401 = S (N N_module_expr) :: r400 in
  let r402 = R 277 :: r401 in
  let r403 = R 189 :: r402 in
  let r404 = [R 449] in
  let r405 = S (T T_RPAREN) :: r404 in
  let r406 = S (N N_module_expr) :: r405 in
  let r407 = [R 451] in
  let r408 = S (T T_RPAREN) :: r407 in
  let r409 = S (N N_expr) :: r408 in
  let r410 = R 277 :: r409 in
  let r411 = [R 262] in
  let r412 = Sub (r138) :: r411 in
  let r413 = Sub (r67) :: r412 in
  let r414 = [R 265] in
  let r415 = Sub (r413) :: r414 in
  let r416 = [R 174] in
  let r417 = Sub (r1) :: r416 in
  let r418 = S (T T_IN) :: r417 in
  let r419 = Sub (r415) :: r418 in
  let r420 = [R 605] in
  let r421 = S (T T_BARRBRACKET) :: r420 in
  let r422 = [R 495] in
  let r423 = [R 104] in
  let r424 = S (T T_RBRACKET) :: r423 in
  let r425 = [R 565] in
  let r426 = S (N N_pattern) :: r425 in
  let r427 = [R 602] in
  let r428 = S (T T_RBRACKET) :: r427 in
  let r429 = Sub (r426) :: r428 in
  let r430 = [R 334] in
  let r431 = S (N N_module_expr) :: r430 in
  let r432 = S (T T_EQUAL) :: r431 in
  let r433 = [R 764] in
  let r434 = R 283 :: r433 in
  let r435 = Sub (r432) :: r434 in
  let r436 = S (T T_UIDENT) :: r435 in
  let r437 = R 277 :: r436 in
  let r438 = R 189 :: r437 in
  let r439 = [R 361] in
  let r440 = R 283 :: r439 in
  let r441 = R 437 :: r440 in
  let r442 = Sub (r95) :: r441 in
  let r443 = R 277 :: r442 in
  let r444 = R 189 :: r443 in
  let r445 = [R 438] in
  let r446 = S (N N_module_type) :: r445 in
  let r447 = [R 354] in
  let r448 = S (T T_RPAREN) :: r447 in
  let r449 = S (N N_module_type) :: r448 in
  let r450 = [R 352] in
  let r451 = S (N N_module_type) :: r450 in
  let r452 = S (T T_MINUSGREATER) :: r451 in
  let r453 = S (N N_functor_args) :: r452 in
  let r454 = R 277 :: r453 in
  let r455 = [R 207] in
  let r456 = [R 208] in
  let r457 = S (T T_RPAREN) :: r456 in
  let r458 = S (N N_module_type) :: r457 in
  let r459 = S (T T_UIDENT) :: r30 in
  let r460 = S (T T_UIDENT) :: r93 in
  let r461 = [R 831] in
  let r462 = Sub (r460) :: r461 in
  let r463 = S (T T_EQUAL) :: r462 in
  let r464 = Sub (r459) :: r463 in
  let r465 = S (T T_MODULE) :: r464 in
  let r466 = [R 541] in
  let r467 = Sub (r465) :: r466 in
  let r468 = [R 359] in
  let r469 = Sub (r467) :: r468 in
  let r470 = [R 234] in
  let r471 = S (T T_LIDENT) :: r470 in
  let r472 = [R 830] in
  let r473 = Sub (r75) :: r472 in
  let r474 = S (T T_COLONEQUAL) :: r473 in
  let r475 = Sub (r471) :: r474 in
  let r476 = R 798 :: r475 in
  let r477 = [R 235] in
  let r478 = S (T T_LIDENT) :: r477 in
  let r479 = S (T T_DOT) :: r478 in
  let r480 = [R 829] in
  let r481 = R 526 :: r480 in
  let r482 = Sub (r75) :: r481 in
  let r483 = [R 527] in
  let r484 = Sub (r77) :: r483 in
  let r485 = S (T T_EQUAL) :: r484 in
  let r486 = Sub (r77) :: r485 in
  let r487 = S (T T_UIDENT) :: r56 in
  let r488 = [R 832] in
  let r489 = Sub (r460) :: r488 in
  let r490 = [R 542] in
  let r491 = Sub (r465) :: r490 in
  let r492 = [R 358] in
  let r493 = S (N N_module_type) :: r492 in
  let r494 = [R 363] in
  let r495 = Sub (r95) :: r494 in
  let r496 = S (T T_DOT) :: r495 in
  let r497 = [R 19] in
  let r498 = S (T T_GREATER) :: r497 in
  let r499 = [R 284] in
  let r500 = R 283 :: r499 in
  let r501 = [R 341] in
  let r502 = S (N N_module_expr) :: r501 in
  let r503 = S (T T_MINUSGREATER) :: r502 in
  let r504 = S (N N_functor_args) :: r503 in
  let r505 = R 277 :: r504 in
  let r506 = [R 346] in
  let r507 = S (T T_RPAREN) :: r506 in
  let r508 = [R 661] in
  let r509 = S (T T_BARRBRACKET) :: r508 in
  let r510 = [R 58] in
  let r511 = S (T T_RPAREN) :: r510 in
  let r512 = [R 303] in
  let r513 = R 439 :: r512 in
  let r514 = R 433 :: r513 in
  let r515 = Sub (r471) :: r514 in
  let r516 = [R 600] in
  let r517 = S (T T_RBRACE) :: r516 in
  let r518 = Sub (r515) :: r517 in
  let r519 = [R 434] in
  let r520 = [R 440] in
  let r521 = S (T T_UNDERSCORE) :: r148 in
  let r522 = [R 673] in
  let r523 = Sub (r521) :: r522 in
  let r524 = [R 480] in
  let r525 = Sub (r523) :: r524 in
  let r526 = R 277 :: r525 in
  let r527 = R 189 :: r526 in
  let r528 = [R 92] in
  let r529 = [R 683] in
  let r530 = Sub (r162) :: r529 in
  let r531 = S (T T_INT) :: r528 in
  let r532 = [R 595] in
  let r533 = Sub (r531) :: r532 in
  let r534 = [R 680] in
  let r535 = Sub (r533) :: r534 in
  let r536 = [R 685] in
  let r537 = S (T T_RBRACKET) :: r536 in
  let r538 = S (T T_LBRACKET) :: r537 in
  let r539 = S (T T_DOT) :: r538 in
  let r540 = [R 686] in
  let r541 = S (T T_RPAREN) :: r540 in
  let r542 = [R 470] in
  let r543 = S (N N_pattern) :: r542 in
  let r544 = R 277 :: r543 in
  let r545 = R 189 :: r544 in
  let r546 = [R 471] in
  let r547 = S (N N_pattern) :: r546 in
  let r548 = [R 461] in
  let r549 = S (N N_pattern) :: r548 in
  let r550 = [R 479] in
  let r551 = S (N N_pattern) :: r550 in
  let r552 = [R 478] in
  let r553 = S (N N_pattern) :: r552 in
  let r554 = [R 103] in
  let r555 = S (T T_RPAREN) :: r554 in
  let r556 = [R 687] in
  let r557 = S (T T_RPAREN) :: r556 in
  let r558 = [R 472] in
  let r559 = S (N N_pattern) :: r558 in
  let r560 = [R 468] in
  let r561 = S (N N_pattern) :: r560 in
  let r562 = [R 465] in
  let r563 = S (T T_error) :: r562 in
  let r564 = [R 604] in
  let r565 = S (T T_BARRBRACKET) :: r564 in
  let r566 = [R 305] in
  let r567 = [R 59] in
  let r568 = S (T T_RPAREN) :: r567 in
  let r569 = [R 816] in
  let r570 = Sub (r1) :: r569 in
  let r571 = S (T T_EQUAL) :: r570 in
  let r572 = S (T T_LIDENT) :: r571 in
  let r573 = R 364 :: r572 in
  let r574 = R 277 :: r573 in
  let r575 = [R 45] in
  let r576 = R 283 :: r575 in
  let r577 = [R 817] in
  let r578 = Sub (r1) :: r577 in
  let r579 = S (T T_EQUAL) :: r578 in
  let r580 = S (T T_LIDENT) :: r579 in
  let r581 = R 364 :: r580 in
  let r582 = [R 131] in
  let r583 = Sub (r1) :: r582 in
  let r584 = S (T T_IN) :: r583 in
  let r585 = S (N N_module_expr) :: r584 in
  let r586 = R 277 :: r585 in
  let r587 = R 189 :: r586 in
  let r588 = S (T T_OPEN) :: r587 in
  let r589 = [R 255] in
  let r590 = R 283 :: r589 in
  let r591 = Sub (r141) :: r590 in
  let r592 = R 510 :: r591 in
  let r593 = R 277 :: r592 in
  let r594 = R 189 :: r593 in
  let r595 = [R 132] in
  let r596 = Sub (r1) :: r595 in
  let r597 = S (T T_IN) :: r596 in
  let r598 = S (N N_module_expr) :: r597 in
  let r599 = R 277 :: r598 in
  let r600 = R 189 :: r599 in
  let r601 = [R 559] in
  let r602 = S (N N_expr) :: r601 in
  let r603 = [R 665] in
  let r604 = S (T T_RBRACKET) :: r603 in
  let r605 = Sub (r602) :: r604 in
  let r606 = [R 246] in
  let r607 = [R 232] in
  let r608 = S (T T_LIDENT) :: r607 in
  let r609 = [R 245] in
  let r610 = S (T T_RPAREN) :: r609 in
  let r611 = [R 233] in
  let r612 = [R 242] in
  let r613 = [R 241] in
  let r614 = S (T T_RPAREN) :: r613 in
  let r615 = R 441 :: r614 in
  let r616 = [R 442] in
  let r617 = [R 645] in
  let r618 = S (T T_GREATERRBRACE) :: r617 in
  let r619 = [R 562] in
  let r620 = R 435 :: r619 in
  let r621 = [R 436] in
  let r622 = [R 643] in
  let r623 = S (T T_GREATERRBRACE) :: r622 in
  let r624 = [R 568] in
  let r625 = R 435 :: r624 in
  let r626 = R 443 :: r625 in
  let r627 = Sub (r471) :: r626 in
  let r628 = [R 512] in
  let r629 = Sub (r627) :: r628 in
  let r630 = [R 655] in
  let r631 = S (T T_RBRACE) :: r630 in
  let r632 = Sub (r629) :: r631 in
  let r633 = [R 608] in
  let r634 = Sub (r44) :: r633 in
  let r635 = [R 607] in
  let r636 = S (T T_GREATERDOT) :: r635 in
  let r637 = S (N N_expr) :: r636 in
  let r638 = [R 144] in
  let r639 = Sub (r44) :: r638 in
  let r640 = R 277 :: r639 in
  let r641 = R 189 :: r640 in
  let r642 = [R 631] in
  let r643 = S (T T_END) :: r642 in
  let r644 = R 277 :: r643 in
  let r645 = R 189 :: r644 in
  let r646 = [R 140] in
  let r647 = S (N N_expr) :: r646 in
  let r648 = S (T T_THEN) :: r647 in
  let r649 = Sub (r1) :: r648 in
  let r650 = R 277 :: r649 in
  let r651 = R 189 :: r650 in
  let r652 = [R 133] in
  let r653 = S (N N_match_cases) :: r652 in
  let r654 = R 277 :: r653 in
  let r655 = R 189 :: r654 in
  let r656 = [R 310] in
  let r657 = Sub (r1) :: r656 in
  let r658 = S (T T_MINUSGREATER) :: r657 in
  let r659 = S (N N_pattern) :: r658 in
  let r660 = [R 537] in
  let r661 = Sub (r659) :: r660 in
  let r662 = [R 311] in
  let r663 = Sub (r1) :: r662 in
  let r664 = S (T T_MINUSGREATER) :: r663 in
  let r665 = Sub (r1) :: r664 in
  let r666 = [R 248] in
  let r667 = Sub (r523) :: r666 in
  let r668 = [R 199] in
  let r669 = Sub (r1) :: r668 in
  let r670 = S (T T_MINUSGREATER) :: r669 in
  let r671 = [R 134] in
  let r672 = Sub (r670) :: r671 in
  let r673 = Sub (r667) :: r672 in
  let r674 = R 277 :: r673 in
  let r675 = R 189 :: r674 in
  let r676 = [R 491] in
  let r677 = S (T T_UNDERSCORE) :: r676 in
  let r678 = [R 244] in
  let r679 = [R 243] in
  let r680 = S (T T_RPAREN) :: r679 in
  let r681 = R 441 :: r680 in
  let r682 = [R 261] in
  let r683 = [R 372] in
  let r684 = S (T T_LIDENT) :: r683 in
  let r685 = [R 135] in
  let r686 = Sub (r670) :: r685 in
  let r687 = S (T T_RPAREN) :: r686 in
  let r688 = Sub (r684) :: r687 in
  let r689 = [R 127] in
  let r690 = S (T T_DONE) :: r689 in
  let r691 = Sub (r1) :: r690 in
  let r692 = S (T T_DO) :: r691 in
  let r693 = Sub (r1) :: r692 in
  let r694 = S (T T_IN) :: r693 in
  let r695 = S (N N_pattern) :: r694 in
  let r696 = R 277 :: r695 in
  let r697 = R 189 :: r696 in
  let r698 = [R 118] in
  let r699 = S (T T_DOWNTO) :: r698 in
  let r700 = [R 142] in
  let r701 = S (T T_DONE) :: r700 in
  let r702 = Sub (r1) :: r701 in
  let r703 = S (T T_DO) :: r702 in
  let r704 = Sub (r1) :: r703 in
  let r705 = Sub (r699) :: r704 in
  let r706 = Sub (r1) :: r705 in
  let r707 = S (T T_EQUAL) :: r706 in
  let r708 = S (N N_pattern) :: r707 in
  let r709 = R 277 :: r708 in
  let r710 = R 189 :: r709 in
  let r711 = [R 642] in
  let r712 = Sub (r44) :: r711 in
  let r713 = [R 653] in
  let r714 = S (T T_RPAREN) :: r713 in
  let r715 = S (T T_LPAREN) :: r714 in
  let r716 = S (T T_DOT) :: r715 in
  let r717 = [R 671] in
  let r718 = S (T T_error) :: r717 in
  let r719 = S (T T_COLON) :: r718 in
  let r720 = S (N N_module_expr) :: r719 in
  let r721 = R 277 :: r720 in
  let r722 = [R 670] in
  let r723 = S (T T_RPAREN) :: r722 in
  let r724 = [R 263] in
  let r725 = Sub (r1) :: r724 in
  let r726 = S (T T_EQUAL) :: r725 in
  let r727 = [R 143] in
  let r728 = Sub (r44) :: r727 in
  let r729 = R 277 :: r728 in
  let r730 = R 189 :: r729 in
  let r731 = [R 651] in
  let r732 = Sub (r44) :: r731 in
  let r733 = [R 616] in
  let r734 = S (T T_RBRACKET) :: r733 in
  let r735 = S (N N_expr) :: r734 in
  let r736 = S (T T_LBRACKET) :: r735 in
  let r737 = [R 618] in
  let r738 = S (T T_RPAREN) :: r737 in
  let r739 = S (N N_expr) :: r738 in
  let r740 = [R 171] in
  let r741 = S (N N_expr) :: r740 in
  let r742 = [R 238] in
  let r743 = S (T T_LIDENT) :: r742 in
  let r744 = [R 239] in
  let r745 = S (T T_LIDENT) :: r744 in
  let r746 = [R 240] in
  let r747 = Sub (r44) :: r746 in
  let r748 = [R 650] in
  let r749 = S (T T_LIDENT) :: r748 in
  let r750 = [R 628] in
  let r751 = S (T T_RBRACE) :: r750 in
  let r752 = S (N N_expr) :: r751 in
  let r753 = S (T T_LBRACE) :: r752 in
  let r754 = [R 612] in
  let r755 = S (T T_RPAREN) :: r754 in
  let r756 = Sub (r1) :: r755 in
  let r757 = [R 553] in
  let r758 = S (N N_expr) :: r757 in
  let r759 = [R 119] in
  let r760 = Sub (r1) :: r759 in
  let r761 = S (T T_IN) :: r760 in
  let r762 = [R 173] in
  let r763 = Sub (r1) :: r762 in
  let r764 = S (T T_IN) :: r763 in
  let r765 = [R 161] in
  let r766 = S (N N_expr) :: r765 in
  let r767 = [R 155] in
  let r768 = S (N N_expr) :: r767 in
  let r769 = [R 172] in
  let r770 = S (N N_expr) :: r769 in
  let r771 = [R 574] in
  let r772 = Sub (r1) :: r771 in
  let r773 = Sub (r12) :: r772 in
  let r774 = [R 158] in
  let r775 = S (N N_expr) :: r774 in
  let r776 = [R 162] in
  let r777 = S (N N_expr) :: r776 in
  let r778 = [R 154] in
  let r779 = S (N N_expr) :: r778 in
  let r780 = [R 157] in
  let r781 = S (N N_expr) :: r780 in
  let r782 = [R 156] in
  let r783 = S (N N_expr) :: r782 in
  let r784 = [R 166] in
  let r785 = S (N N_expr) :: r784 in
  let r786 = [R 160] in
  let r787 = S (N N_expr) :: r786 in
  let r788 = [R 159] in
  let r789 = S (N N_expr) :: r788 in
  let r790 = [R 164] in
  let r791 = S (N N_expr) :: r790 in
  let r792 = [R 153] in
  let r793 = S (N N_expr) :: r792 in
  let r794 = [R 152] in
  let r795 = S (N N_expr) :: r794 in
  let r796 = [R 175] in
  let r797 = S (N N_expr) :: r796 in
  let r798 = [R 151] in
  let r799 = S (N N_expr) :: r798 in
  let r800 = [R 165] in
  let r801 = S (N N_expr) :: r800 in
  let r802 = [R 163] in
  let r803 = S (N N_expr) :: r802 in
  let r804 = [R 167] in
  let r805 = S (N N_expr) :: r804 in
  let r806 = [R 168] in
  let r807 = S (N N_expr) :: r806 in
  let r808 = [R 169] in
  let r809 = S (N N_expr) :: r808 in
  let r810 = [R 554] in
  let r811 = S (N N_expr) :: r810 in
  let r812 = [R 170] in
  let r813 = S (N N_expr) :: r812 in
  let r814 = [R 11] in
  let r815 = R 283 :: r814 in
  let r816 = Sub (r141) :: r815 in
  let r817 = R 277 :: r816 in
  let r818 = [R 197] in
  let r819 = Sub (r138) :: r818 in
  let r820 = [R 751] in
  let r821 = Sub (r819) :: r820 in
  let r822 = S (T T_RPAREN) :: r821 in
  let r823 = Sub (r684) :: r822 in
  let r824 = S (T T_TYPE) :: r823 in
  let r825 = [R 247] in
  let r826 = [R 792] in
  let r827 = S (T T_error) :: r826 in
  let r828 = [R 791] in
  let r829 = S (T T_error) :: r828 in
  let r830 = [R 789] in
  let r831 = Sub (r77) :: r830 in
  let r832 = [R 198] in
  let r833 = Sub (r1) :: r832 in
  let r834 = S (T T_EQUAL) :: r833 in
  let r835 = [R 750] in
  let r836 = Sub (r819) :: r835 in
  let r837 = [R 106] in
  let r838 = S (T T_RPAREN) :: r837 in
  let r839 = [R 674] in
  let r840 = S (T T_RPAREN) :: r839 in
  let r841 = [R 693] in
  let r842 = S (T T_error) :: r841 in
  let r843 = [R 691] in
  let r844 = S (T T_RPAREN) :: r843 in
  let r845 = [R 252] in
  let r846 = Sub (r1) :: r845 in
  let r847 = S (T T_EQUAL) :: r846 in
  let r848 = Sub (r77) :: r847 in
  let r849 = S (T T_DOT) :: r848 in
  let r850 = Sub (r684) :: r849 in
  let r851 = [R 251] in
  let r852 = Sub (r1) :: r851 in
  let r853 = S (T T_EQUAL) :: r852 in
  let r854 = Sub (r77) :: r853 in
  let r855 = S (T T_DOT) :: r854 in
  let r856 = [R 250] in
  let r857 = Sub (r1) :: r856 in
  let r858 = S (T T_EQUAL) :: r857 in
  let r859 = [R 254] in
  let r860 = Sub (r1) :: r859 in
  let r861 = S (T T_EQUAL) :: r860 in
  let r862 = Sub (r77) :: r861 in
  let r863 = [R 253] in
  let r864 = Sub (r1) :: r863 in
  let r865 = S (T T_EQUAL) :: r864 in
  let r866 = [R 475] in
  let r867 = [R 481] in
  let r868 = [R 488] in
  let r869 = [R 485] in
  let r870 = [R 474] in
  let r871 = [R 126] in
  let r872 = S (T T_DONE) :: r871 in
  let r873 = Sub (r1) :: r872 in
  let r874 = S (T T_DO) :: r873 in
  let r875 = Sub (r1) :: r874 in
  let r876 = Sub (r699) :: r875 in
  let r877 = Sub (r1) :: r876 in
  let r878 = [R 614] in
  let r879 = S (T T_RBRACKET) :: r878 in
  let r880 = Sub (r1) :: r879 in
  let r881 = [R 622] in
  let r882 = S (T T_RBRACKET) :: r881 in
  let r883 = S (N N_expr) :: r882 in
  let r884 = S (T T_LBRACKET) :: r883 in
  let r885 = [R 624] in
  let r886 = S (T T_RPAREN) :: r885 in
  let r887 = S (N N_expr) :: r886 in
  let r888 = [R 626] in
  let r889 = S (T T_RBRACE) :: r888 in
  let r890 = S (N N_expr) :: r889 in
  let r891 = [R 237] in
  let r892 = Sub (r44) :: r891 in
  let r893 = [R 182] in
  let r894 = S (N N_expr) :: r893 in
  let r895 = [R 181] in
  let r896 = S (N N_expr) :: r895 in
  let r897 = [R 620] in
  let r898 = S (T T_RBRACE) :: r897 in
  let r899 = S (N N_expr) :: r898 in
  let r900 = [R 183] in
  let r901 = S (N N_expr) :: r900 in
  let r902 = [R 178] in
  let r903 = S (N N_expr) :: r902 in
  let r904 = [R 179] in
  let r905 = S (N N_expr) :: r904 in
  let r906 = [R 180] in
  let r907 = S (N N_expr) :: r906 in
  let r908 = [R 185] in
  let r909 = S (N N_expr) :: r908 in
  let r910 = [R 184] in
  let r911 = S (N N_expr) :: r910 in
  let r912 = [R 186] in
  let r913 = S (N N_expr) :: r912 in
  let r914 = [R 177] in
  let r915 = S (N N_expr) :: r914 in
  let r916 = [R 647] in
  let r917 = S (T T_RPAREN) :: r916 in
  let r918 = [R 663] in
  let r919 = S (T T_BARRBRACKET) :: r918 in
  let r920 = [R 662] in
  let r921 = S (T T_BARRBRACKET) :: r920 in
  let r922 = [R 668] in
  let r923 = S (T T_RBRACKET) :: r922 in
  let r924 = [R 667] in
  let r925 = S (T T_RBRACKET) :: r924 in
  let r926 = S (T T_LIDENT) :: r620 in
  let r927 = [R 648] in
  let r928 = S (T T_GREATERRBRACE) :: r927 in
  let r929 = Sub (r926) :: r928 in
  let r930 = [R 657] in
  let r931 = S (T T_RBRACE) :: r930 in
  let r932 = Sub (r629) :: r931 in
  let r933 = [R 513] in
  let r934 = Sub (r627) :: r933 in
  let r935 = [R 630] in
  let r936 = S (T T_END) :: r935 in
  let r937 = [R 202] in
  let r938 = Sub (r670) :: r937 in
  let r939 = S (T T_RPAREN) :: r938 in
  let r940 = Sub (r684) :: r939 in
  let r941 = [R 200] in
  let r942 = Sub (r1) :: r941 in
  let r943 = S (T T_MINUSGREATER) :: r942 in
  let r944 = Sub (r69) :: r943 in
  let r945 = [R 201] in
  let r946 = Sub (r670) :: r945 in
  let r947 = [R 538] in
  let r948 = Sub (r659) :: r947 in
  let r949 = [R 139] in
  let r950 = S (N N_expr) :: r949 in
  let r951 = [R 264] in
  let r952 = Sub (r1) :: r951 in
  let r953 = [R 266] in
  let r954 = [R 129] in
  let r955 = Sub (r1) :: r954 in
  let r956 = S (T T_IN) :: r955 in
  let r957 = Sub (r432) :: r956 in
  let r958 = S (T T_UIDENT) :: r957 in
  let r959 = R 277 :: r958 in
  let r960 = R 189 :: r959 in
  let r961 = [R 335] in
  let r962 = S (N N_module_expr) :: r961 in
  let r963 = S (T T_EQUAL) :: r962 in
  let r964 = S (N N_module_type) :: r963 in
  let r965 = [R 336] in
  let r966 = Sub (r432) :: r965 in
  let r967 = [R 130] in
  let r968 = Sub (r1) :: r967 in
  let r969 = S (T T_IN) :: r968 in
  let r970 = R 277 :: r969 in
  let r971 = R 212 :: r970 in
  let r972 = Sub (r164) :: r971 in
  let r973 = R 277 :: r972 in
  let r974 = R 189 :: r973 in
  let r975 = [R 659] in
  let r976 = S (T T_BARRBRACKET) :: r975 in
  let r977 = [R 819] in
  let r978 = Sub (r1) :: r977 in
  let r979 = [R 815] in
  let r980 = Sub (r77) :: r979 in
  let r981 = S (T T_COLON) :: r980 in
  let r982 = [R 818] in
  let r983 = Sub (r1) :: r982 in
  let r984 = [R 322] in
  let r985 = Sub (r138) :: r984 in
  let r986 = S (T T_LIDENT) :: r985 in
  let r987 = R 503 :: r986 in
  let r988 = R 277 :: r987 in
  let r989 = [R 46] in
  let r990 = R 283 :: r989 in
  let r991 = [R 323] in
  let r992 = Sub (r138) :: r991 in
  let r993 = S (T T_LIDENT) :: r992 in
  let r994 = R 503 :: r993 in
  let r995 = [R 497] in
  let r996 = Sub (r77) :: r995 in
  let r997 = [R 325] in
  let r998 = Sub (r1) :: r997 in
  let r999 = S (T T_EQUAL) :: r998 in
  let r1000 = [R 327] in
  let r1001 = Sub (r1) :: r1000 in
  let r1002 = S (T T_EQUAL) :: r1001 in
  let r1003 = Sub (r77) :: r1002 in
  let r1004 = S (T T_DOT) :: r1003 in
  let r1005 = [R 498] in
  let r1006 = Sub (r77) :: r1005 in
  let r1007 = S (T T_DOT) :: r1006 in
  let r1008 = [R 321] in
  let r1009 = Sub (r996) :: r1008 in
  let r1010 = S (T T_COLON) :: r1009 in
  let r1011 = [R 324] in
  let r1012 = Sub (r1) :: r1011 in
  let r1013 = S (T T_EQUAL) :: r1012 in
  let r1014 = [R 326] in
  let r1015 = Sub (r1) :: r1014 in
  let r1016 = S (T T_EQUAL) :: r1015 in
  let r1017 = Sub (r77) :: r1016 in
  let r1018 = S (T T_DOT) :: r1017 in
  let r1019 = [R 226] in
  let r1020 = S (T T_RBRACKET) :: r1019 in
  let r1021 = Sub (r18) :: r1020 in
  let r1022 = Sub (r12) :: r1021 in
  let r1023 = [R 194] in
  let r1024 = S (T T_RBRACKET) :: r1023 in
  let r1025 = Sub (r18) :: r1024 in
  let r1026 = Sub (r12) :: r1025 in
  let r1027 = [R 770] in
  let r1028 = R 283 :: r1027 in
  let r1029 = S (N N_module_expr) :: r1028 in
  let r1030 = R 277 :: r1029 in
  let r1031 = R 189 :: r1030 in
  let r1032 = [R 374] in
  let r1033 = S (T T_STRING) :: r1032 in
  let r1034 = [R 502] in
  let r1035 = R 283 :: r1034 in
  let r1036 = Sub (r1033) :: r1035 in
  let r1037 = S (T T_EQUAL) :: r1036 in
  let r1038 = Sub (r77) :: r1037 in
  let r1039 = S (T T_COLON) :: r1038 in
  let r1040 = Sub (r67) :: r1039 in
  let r1041 = R 277 :: r1040 in
  let r1042 = R 189 :: r1041 in
  let r1043 = [R 748] in
  let r1044 = R 283 :: r1043 in
  let r1045 = R 277 :: r1044 in
  let r1046 = Sub (r42) :: r1045 in
  let r1047 = S (T T_EQUAL) :: r1046 in
  let r1048 = Sub (r164) :: r1047 in
  let r1049 = R 277 :: r1048 in
  let r1050 = R 189 :: r1049 in
  let r1051 = [R 575] in
  let r1052 = R 283 :: r1051 in
  let r1053 = R 277 :: r1052 in
  let r1054 = R 212 :: r1053 in
  let r1055 = Sub (r164) :: r1054 in
  let r1056 = R 277 :: r1055 in
  let r1057 = R 189 :: r1056 in
  let r1058 = S (T T_RPAREN) :: r153 in
  let r1059 = S (T T_COLONCOLON) :: r555 in
  let r1060 = S (T T_LPAREN) :: r1059 in
  let r1061 = [R 493] in
  let r1062 = [R 219] in
  let r1063 = R 283 :: r1062 in
  let r1064 = R 526 :: r1063 in
  let r1065 = Sub (r199) :: r1064 in
  let r1066 = [R 218] in
  let r1067 = R 283 :: r1066 in
  let r1068 = R 526 :: r1067 in
  let r1069 = Sub (r199) :: r1068 in
  let r1070 = [R 286] in
  let r1071 = R 285 :: r1070 in
  let r1072 = [R 392] in
  let r1073 = R 283 :: r1072 in
  let r1074 = Sub (r460) :: r1073 in
  let r1075 = R 277 :: r1074 in
  let r1076 = R 189 :: r1075 in
  let r1077 = [R 393] in
  let r1078 = R 283 :: r1077 in
  let r1079 = Sub (r460) :: r1078 in
  let r1080 = R 277 :: r1079 in
  let r1081 = R 189 :: r1080 in
  let r1082 = [R 349] in
  let r1083 = S (T T_error) :: r1082 in
  let r1084 = S (T T_COLONEQUAL) :: r1083 in
  let r1085 = S (T T_UIDENT) :: r1084 in
  let r1086 = R 277 :: r1085 in
  let r1087 = [R 337] in
  let r1088 = S (N N_module_type) :: r1087 in
  let r1089 = S (T T_COLON) :: r1088 in
  let r1090 = [R 586] in
  let r1091 = R 283 :: r1090 in
  let r1092 = Sub (r1089) :: r1091 in
  let r1093 = S (T T_UIDENT) :: r1092 in
  let r1094 = R 277 :: r1093 in
  let r1095 = R 189 :: r1094 in
  let r1096 = [R 587] in
  let r1097 = R 283 :: r1096 in
  let r1098 = Sub (r459) :: r1097 in
  let r1099 = [R 348] in
  let r1100 = R 283 :: r1099 in
  let r1101 = [R 338] in
  let r1102 = Sub (r1089) :: r1101 in
  let r1103 = [R 589] in
  let r1104 = R 275 :: r1103 in
  let r1105 = R 283 :: r1104 in
  let r1106 = S (N N_module_type) :: r1105 in
  let r1107 = S (T T_COLON) :: r1106 in
  let r1108 = S (T T_UIDENT) :: r1107 in
  let r1109 = [R 276] in
  let r1110 = R 275 :: r1109 in
  let r1111 = R 283 :: r1110 in
  let r1112 = S (N N_module_type) :: r1111 in
  let r1113 = S (T T_COLON) :: r1112 in
  let r1114 = S (T T_UIDENT) :: r1113 in
  let r1115 = R 277 :: r1114 in
  let r1116 = [R 592] in
  let r1117 = R 283 :: r1116 in
  let r1118 = S (N N_module_type) :: r1117 in
  let r1119 = R 277 :: r1118 in
  let r1120 = R 189 :: r1119 in
  let r1121 = [R 90] in
  let r1122 = S (T T_LIDENT) :: r1121 in
  let r1123 = [R 69] in
  let r1124 = Sub (r1122) :: r1123 in
  let r1125 = [R 85] in
  let r1126 = Sub (r1124) :: r1125 in
  let r1127 = [R 593] in
  let r1128 = R 269 :: r1127 in
  let r1129 = R 283 :: r1128 in
  let r1130 = Sub (r1126) :: r1129 in
  let r1131 = S (T T_COLON) :: r1130 in
  let r1132 = S (T T_LIDENT) :: r1131 in
  let r1133 = R 195 :: r1132 in
  let r1134 = R 821 :: r1133 in
  let r1135 = R 277 :: r1134 in
  let r1136 = R 189 :: r1135 in
  let r1137 = [R 89] in
  let r1138 = R 271 :: r1137 in
  let r1139 = R 283 :: r1138 in
  let r1140 = Sub (r1124) :: r1139 in
  let r1141 = S (T T_EQUAL) :: r1140 in
  let r1142 = S (T T_LIDENT) :: r1141 in
  let r1143 = R 195 :: r1142 in
  let r1144 = R 821 :: r1143 in
  let r1145 = R 277 :: r1144 in
  let r1146 = R 189 :: r1145 in
  let r1147 = [R 196] in
  let r1148 = S (T T_RBRACKET) :: r1147 in
  let r1149 = [R 72] in
  let r1150 = S (T T_END) :: r1149 in
  let r1151 = R 292 :: r1150 in
  let r1152 = R 62 :: r1151 in
  let r1153 = R 277 :: r1152 in
  let r1154 = [R 61] in
  let r1155 = S (T T_RPAREN) :: r1154 in
  let r1156 = [R 64] in
  let r1157 = R 283 :: r1156 in
  let r1158 = Sub (r77) :: r1157 in
  let r1159 = S (T T_COLON) :: r1158 in
  let r1160 = S (T T_LIDENT) :: r1159 in
  let r1161 = R 366 :: r1160 in
  let r1162 = [R 65] in
  let r1163 = R 283 :: r1162 in
  let r1164 = Sub (r996) :: r1163 in
  let r1165 = S (T T_COLON) :: r1164 in
  let r1166 = S (T T_LIDENT) :: r1165 in
  let r1167 = R 505 :: r1166 in
  let r1168 = [R 79] in
  let r1169 = Sub (r48) :: r1168 in
  let r1170 = [R 35] in
  let r1171 = Sub (r1169) :: r1170 in
  let r1172 = [R 51] in
  let r1173 = Sub (r1171) :: r1172 in
  let r1174 = S (T T_EQUAL) :: r1173 in
  let r1175 = [R 768] in
  let r1176 = R 267 :: r1175 in
  let r1177 = R 283 :: r1176 in
  let r1178 = Sub (r1174) :: r1177 in
  let r1179 = S (T T_LIDENT) :: r1178 in
  let r1180 = R 195 :: r1179 in
  let r1181 = R 821 :: r1180 in
  let r1182 = R 277 :: r1181 in
  let r1183 = R 189 :: r1182 in
  let r1184 = [R 81] in
  let r1185 = S (T T_error) :: r1184 in
  let r1186 = R 294 :: r1185 in
  let r1187 = R 60 :: r1186 in
  let r1188 = R 277 :: r1187 in
  let r1189 = [R 48] in
  let r1190 = R 283 :: r1189 in
  let r1191 = Sub (r1) :: r1190 in
  let r1192 = [R 43] in
  let r1193 = R 283 :: r1192 in
  let r1194 = R 431 :: r1193 in
  let r1195 = Sub (r1171) :: r1194 in
  let r1196 = [R 44] in
  let r1197 = R 283 :: r1196 in
  let r1198 = R 431 :: r1197 in
  let r1199 = Sub (r1171) :: r1198 in
  let r1200 = [R 109] in
  let r1201 = Sub (r77) :: r1200 in
  let r1202 = S (T T_EQUAL) :: r1201 in
  let r1203 = Sub (r77) :: r1202 in
  let r1204 = [R 47] in
  let r1205 = R 283 :: r1204 in
  let r1206 = Sub (r1203) :: r1205 in
  let r1207 = [R 49] in
  let r1208 = [R 295] in
  let r1209 = [R 77] in
  let r1210 = S (T T_RPAREN) :: r1209 in
  let r1211 = Sub (r1171) :: r1210 in
  let r1212 = [R 38] in
  let r1213 = Sub (r1171) :: r1212 in
  let r1214 = S (T T_IN) :: r1213 in
  let r1215 = Sub (r459) :: r1214 in
  let r1216 = R 277 :: r1215 in
  let r1217 = S (T T_OPEN) :: r1216 in
  let r1218 = [R 258] in
  let r1219 = R 283 :: r1218 in
  let r1220 = Sub (r141) :: r1219 in
  let r1221 = R 510 :: r1220 in
  let r1222 = R 277 :: r1221 in
  let r1223 = [R 39] in
  let r1224 = Sub (r1171) :: r1223 in
  let r1225 = S (T T_IN) :: r1224 in
  let r1226 = Sub (r459) :: r1225 in
  let r1227 = R 277 :: r1226 in
  let r1228 = [R 545] in
  let r1229 = Sub (r77) :: r1228 in
  let r1230 = [R 80] in
  let r1231 = Sub (r48) :: r1230 in
  let r1232 = S (T T_RBRACKET) :: r1231 in
  let r1233 = Sub (r1229) :: r1232 in
  let r1234 = [R 546] in
  let r1235 = [R 54] in
  let r1236 = Sub (r1171) :: r1235 in
  let r1237 = S (T T_MINUSGREATER) :: r1236 in
  let r1238 = Sub (r667) :: r1237 in
  let r1239 = [R 36] in
  let r1240 = Sub (r1238) :: r1239 in
  let r1241 = R 277 :: r1240 in
  let r1242 = [R 37] in
  let r1243 = Sub (r1171) :: r1242 in
  let r1244 = S (T T_IN) :: r1243 in
  let r1245 = [R 257] in
  let r1246 = R 283 :: r1245 in
  let r1247 = Sub (r141) :: r1246 in
  let r1248 = [R 82] in
  let r1249 = S (T T_RPAREN) :: r1248 in
  let r1250 = Sub (r1126) :: r1249 in
  let r1251 = [R 63] in
  let r1252 = R 283 :: r1251 in
  let r1253 = Sub (r1124) :: r1252 in
  let r1254 = [R 75] in
  let r1255 = Sub (r1124) :: r1254 in
  let r1256 = S (T T_IN) :: r1255 in
  let r1257 = Sub (r459) :: r1256 in
  let r1258 = R 277 :: r1257 in
  let r1259 = S (T T_OPEN) :: r1258 in
  let r1260 = [R 76] in
  let r1261 = Sub (r1124) :: r1260 in
  let r1262 = S (T T_IN) :: r1261 in
  let r1263 = Sub (r459) :: r1262 in
  let r1264 = R 277 :: r1263 in
  let r1265 = [R 70] in
  let r1266 = Sub (r1122) :: r1265 in
  let r1267 = S (T T_RBRACKET) :: r1266 in
  let r1268 = Sub (r1229) :: r1267 in
  let r1269 = [R 91] in
  let r1270 = S (T T_LIDENT) :: r1269 in
  let r1271 = S (T T_DOT) :: r1270 in
  let r1272 = [R 66] in
  let r1273 = R 283 :: r1272 in
  let r1274 = Sub (r1203) :: r1273 in
  let r1275 = [R 67] in
  let r1276 = [R 293] in
  let r1277 = [R 87] in
  let r1278 = Sub (r1126) :: r1277 in
  let r1279 = S (T T_MINUSGREATER) :: r1278 in
  let r1280 = Sub (r71) :: r1279 in
  let r1281 = S (T T_COLON) :: r1280 in
  let r1282 = [R 88] in
  let r1283 = Sub (r1126) :: r1282 in
  let r1284 = S (T T_MINUSGREATER) :: r1283 in
  let r1285 = [R 86] in
  let r1286 = Sub (r1126) :: r1285 in
  let r1287 = S (T T_MINUSGREATER) :: r1286 in
  let r1288 = Sub (r71) :: r1287 in
  let r1289 = [R 432] in
  let r1290 = [R 52] in
  let r1291 = Sub (r1171) :: r1290 in
  let r1292 = S (T T_EQUAL) :: r1291 in
  let r1293 = Sub (r1126) :: r1292 in
  let r1294 = [R 53] in
  let r1295 = Sub (r1174) :: r1294 in
  let r1296 = [R 268] in
  let r1297 = R 267 :: r1296 in
  let r1298 = R 283 :: r1297 in
  let r1299 = Sub (r1174) :: r1298 in
  let r1300 = S (T T_LIDENT) :: r1299 in
  let r1301 = R 195 :: r1300 in
  let r1302 = R 821 :: r1301 in
  let r1303 = R 277 :: r1302 in
  let r1304 = [R 291] in
  let r1305 = R 288 :: r1304 in
  let r1306 = [R 756] in
  let r1307 = R 283 :: r1306 in
  let r1308 = [R 760] in
  let r1309 = R 279 :: r1308 in
  let r1310 = [R 280] in
  let r1311 = R 279 :: r1310 in
  let r1312 = R 283 :: r1311 in
  let r1313 = R 526 :: r1312 in
  let r1314 = R 793 :: r1313 in
  let r1315 = S (T T_LIDENT) :: r1314 in
  let r1316 = R 798 :: r1315 in
  let r1317 = R 277 :: r1316 in
  let r1318 = [R 753] in
  let r1319 = R 288 :: r1318 in
  let r1320 = R 283 :: r1319 in
  let r1321 = [R 272] in
  let r1322 = R 271 :: r1321 in
  let r1323 = R 283 :: r1322 in
  let r1324 = Sub (r1124) :: r1323 in
  let r1325 = S (T T_EQUAL) :: r1324 in
  let r1326 = S (T T_LIDENT) :: r1325 in
  let r1327 = R 195 :: r1326 in
  let r1328 = R 821 :: r1327 in
  let r1329 = R 277 :: r1328 in
  let r1330 = [R 270] in
  let r1331 = R 269 :: r1330 in
  let r1332 = R 283 :: r1331 in
  let r1333 = Sub (r1126) :: r1332 in
  let r1334 = S (T T_COLON) :: r1333 in
  let r1335 = S (T T_LIDENT) :: r1334 in
  let r1336 = R 195 :: r1335 in
  let r1337 = R 821 :: r1336 in
  let r1338 = R 277 :: r1337 in
  let r1339 = [R 287] in
  let r1340 = R 285 :: r1339 in
  let r1341 = [R 577] in
  let r1342 = R 283 :: r1341 in
  let r1343 = [R 581] in
  let r1344 = R 279 :: r1343 in
  let r1345 = [R 582] in
  let r1346 = R 281 :: r1345 in
  let r1347 = [R 282] in
  let r1348 = R 281 :: r1347 in
  let r1349 = R 283 :: r1348 in
  let r1350 = R 526 :: r1349 in
  let r1351 = Sub (r199) :: r1350 in
  let r1352 = S (T T_COLONEQUAL) :: r1351 in
  let r1353 = S (T T_LIDENT) :: r1352 in
  let r1354 = R 798 :: r1353 in
  let r1355 = R 277 :: r1354 in
  let r1356 = [R 634] in
  let r1357 = S (T T_RPAREN) :: r1356 in
  let r1358 = S (N N_module_expr) :: r1357 in
  let r1359 = R 277 :: r1358 in
  let r1360 = [R 636] in
  let r1361 = S (T T_error) :: r1360 in
  let r1362 = [R 635] in
  let r1363 = S (T T_RPAREN) :: r1362 in
  let r1364 = [R 609] in
  let r1365 = S (T T_RPAREN) :: r1364 in
  let r1366 = [R 611] in
  let r1367 = S (T T_RPAREN) :: r1366 in
  let r1368 = [R 456] in
  let r1369 = S (T T_error) :: r1368 in
  let r1370 = [R 454] in
  let r1371 = S (T T_RPAREN) :: r1370 in
  let r1372 = [R 455] in
  let r1373 = S (T T_error) :: r1372 in
  let r1374 = [R 452] in
  let r1375 = S (T T_RPAREN) :: r1374 in
  let r1376 = [R 453] in
  let r1377 = S (T T_RPAREN) :: r1376 in
  let r1378 = S (N N_module_type) :: r1377 in
  let r1379 = [R 447] in
  let r1380 = S (T T_RPAREN) :: r1379 in
  let r1381 = S (N N_module_type) :: r1380 in
  let r1382 = [R 765] in
  let r1383 = R 273 :: r1382 in
  let r1384 = R 283 :: r1383 in
  let r1385 = Sub (r432) :: r1384 in
  let r1386 = S (T T_UIDENT) :: r1385 in
  let r1387 = [R 274] in
  let r1388 = R 273 :: r1387 in
  let r1389 = R 283 :: r1388 in
  let r1390 = Sub (r432) :: r1389 in
  let r1391 = S (T T_UIDENT) :: r1390 in
  let r1392 = R 277 :: r1391 in
  let r1393 = [R 496] in
  let r1394 = [R 192] in
  let r1395 = R 277 :: r1394 in
  let r1396 = Sub (r42) :: r1395 in
  let r1397 = [R 193] in
  let r1398 = R 277 :: r1397 in
  let r1399 = Sub (r42) :: r1398 in
  let r1400 = [R 290] in
  let r1401 = R 288 :: r1400 in
  let r1402 = R 283 :: r1401 in
  let r1403 = [R 122] in
  let r1404 = S (N N_match_cases) :: r1403 in
  let r1405 = [R 124] in
  let r1406 = Sub (r1) :: r1405 in
  let r1407 = [R 123] in
  let r1408 = Sub (r1) :: r1407 in
  let r1409 = [R 316] in
  let r1410 = [R 224] in
  let r1411 = [R 225] in
  let r1412 = [R 458] in
  let r1413 = [R 459] in
  let r1414 = [R 460] in
  let r1415 = [R 775] in
  let r1416 = [R 784] in
  let r1417 = [R 297] in
  let r1418 = [R 782] in
  let r1419 = S (T T_SEMISEMI) :: r1418 in
  let r1420 = [R 783] in
  let r1421 = [R 299] in
  let r1422 = [R 302] in
  let r1423 = [R 301] in
  let r1424 = [R 300] in
  let r1425 = R 298 :: r1424 in
  let r1426 = [R 807] in
  let r1427 = S (T T_EOF) :: r1426 in
  let r1428 = R 298 :: r1427 in
  let r1429 = [R 806] in
  function
  | 0 | 3157 | 3161 | 3165 | 3169 | 3173 | 3194 -> Nothing
  | 3156 -> One ([R 0])
  | 3160 -> One ([R 1])
  | 3162 -> One ([R 2])
  | 3168 -> One ([R 3])
  | 3172 -> One ([R 4])
  | 3184 -> One ([R 5])
  | 3204 -> One ([R 6])
  | 92 -> One ([R 7])
  | 91 -> One ([R 8])
  | 339 | 664 -> One ([R 16])
  | 357 | 677 -> One ([R 17])
  | 353 | 673 -> One ([R 31])
  | 2180 | 2302 -> One ([R 40])
  | 2177 | 2299 -> One ([R 41])
  | 2175 | 2297 -> One ([R 42])
  | 2144 -> One ([R 50])
  | 2183 | 2304 -> One ([R 55])
  | 2236 -> One ([R 68])
  | 2217 | 2253 | 2331 | 2348 -> One ([R 71])
  | 2232 | 2404 -> One ([R 73])
  | 2220 | 2334 -> One ([R 74])
  | 2195 | 2275 -> One ([R 78])
  | 2259 | 2279 -> One ([R 83])
  | 2141 | 2272 -> One ([R 84])
  | 793 | 877 -> One ([R 93])
  | 73 | 266 -> One ([R 94])
  | 791 | 875 -> One ([R 95])
  | 300 | 322 | 442 | 2561 -> One ([R 96])
  | 301 | 323 -> One ([R 101])
  | 1978 | 2582 -> One ([R 102])
  | 72 | 265 -> One ([R 108])
  | 441 | 2924 -> One ([R 113])
  | 462 | 2926 -> One ([R 114])
  | 387 | 699 -> One ([R 116])
  | 1279 -> One ([R 117])
  | 1088 | 1331 -> One ([R 128])
  | 2836 | 3134 -> One ([R 138])
  | 2472 | 3111 -> One ([R 146])
  | 1492 | 1681 -> One ([R 147])
  | 1124 | 1345 -> One ([R 148])
  | 1145 | 1362 -> One ([R 149])
  | 1127 | 1348 -> One ([R 150])
  | 1143 | 1360 -> One ([R 187])
  | 64 | 503 -> One ([R 188])
  | 560 -> One ([R 209])
  | 559 -> One ([R 210])
  | 566 -> One ([R 211])
  | 203 | 224 | 608 | 723 -> One ([R 222])
  | 436 -> One ([R 230])
  | 437 -> One ([R 231])
  | 1491 | 1680 -> One ([R 236])
  | 1274 | 2787 -> One ([R 256])
  | 2181 -> One ([R 259])
  | 1021 -> One ([R 260])
  | 938 -> One (R 277 :: r581)
  | 1886 -> One (R 277 :: r994)
  | 2088 -> One (R 277 :: r1161)
  | 2099 -> One (R 277 :: r1167)
  | 2122 -> One (R 277 :: r1191)
  | 2126 -> One (R 277 :: r1195)
  | 2127 -> One (R 277 :: r1199)
  | 2132 -> One (R 277 :: r1206)
  | 2201 -> One (R 277 :: r1253)
  | 2227 -> One (R 277 :: r1274)
  | 2903 -> One (R 277 :: r1409)
  | 2142 -> One (R 283 :: r1207)
  | 2234 -> One (R 283 :: r1275)
  | 3189 -> One (R 283 :: r1419)
  | 3200 -> One (R 283 :: r1425)
  | 3205 -> One (R 283 :: r1428)
  | 2237 -> One (R 292 :: r1276)
  | 2145 -> One (R 294 :: r1208)
  | 3187 -> One (R 296 :: r1417)
  | 3195 -> One (R 298 :: r1421)
  | 3196 -> One (R 298 :: r1422)
  | 3197 -> One (R 298 :: r1423)
  | 864 -> One ([R 304])
  | 868 -> One ([R 306])
  | 1134 | 2784 -> One ([R 308])
  | 1275 | 2783 -> One ([R 309])
  | 1589 | 1747 -> One ([R 312])
  | 1592 | 1750 -> One ([R 313])
  | 2905 -> One ([R 314])
  | 635 -> One ([R 315])
  | 634 -> One ([R 317])
  | 633 -> One ([R 319])
  | 630 -> One ([R 320])
  | 2809 | 3092 -> One ([R 340])
  | 746 | 2513 -> One ([R 342])
  | 1066 | 2525 -> One ([R 343])
  | 1067 | 2526 -> One ([R 344])
  | 1065 | 2524 -> One ([R 345])
  | 1068 | 2528 -> One ([R 347])
  | 3072 | 3100 -> One ([R 351])
  | 728 | 731 -> One ([R 355])
  | 605 | 720 -> One ([R 356])
  | 570 | 617 -> One ([R 357])
  | 611 | 726 -> One ([R 360])
  | 610 | 725 -> One ([R 362])
  | 420 | 1872 -> One ([R 365])
  | 2092 -> One ([R 367])
  | 2090 -> One ([R 368])
  | 2093 -> One ([R 369])
  | 2091 -> One ([R 370])
  | 1032 -> One ([R 373])
  | 1968 | 2712 -> One ([R 375])
  | 477 | 2939 -> One ([R 376])
  | 467 | 2931 -> One ([R 377])
  | 490 | 2952 -> One ([R 378])
  | 468 | 2932 -> One ([R 379])
  | 489 | 2951 -> One ([R 380])
  | 484 | 2946 -> One ([R 381])
  | 158 | 759 -> One ([R 394])
  | 168 | 1071 -> One ([R 395])
  | 191 -> One ([R 396])
  | 181 -> One ([R 398])
  | 184 -> One ([R 400])
  | 187 -> One ([R 402])
  | 175 -> One ([R 403])
  | 190 | 1318 -> One ([R 404])
  | 174 -> One ([R 405])
  | 173 -> One ([R 406])
  | 172 -> One ([R 407])
  | 171 -> One ([R 408])
  | 170 -> One ([R 409])
  | 161 | 268 | 1056 -> One ([R 410])
  | 160 | 1055 -> One ([R 411])
  | 159 -> One ([R 412])
  | 167 | 1070 | 1220 -> One ([R 413])
  | 166 | 1069 -> One ([R 414])
  | 157 -> One ([R 415])
  | 162 -> One ([R 416])
  | 177 -> One ([R 417])
  | 169 -> One ([R 418])
  | 176 -> One ([R 419])
  | 163 -> One ([R 420])
  | 189 -> One ([R 421])
  | 192 -> One ([R 422])
  | 193 -> One ([R 423])
  | 188 -> One ([R 424])
  | 374 -> One ([R 425])
  | 373 -> One (R 426 :: r309)
  | 328 -> One ([R 428])
  | 865 -> One (R 429 :: r566)
  | 866 -> One ([R 430])
  | 1537 -> One ([R 444])
  | 209 -> One ([R 445])
  | 2522 | 2534 -> One ([R 448])
  | 2518 | 2530 -> One ([R 450])
  | 2496 | 2846 -> One ([R 457])
  | 822 | 916 -> One ([R 462])
  | 815 | 909 -> One ([R 463])
  | 847 | 935 -> One ([R 464])
  | 816 | 910 -> One ([R 466])
  | 820 | 914 -> One ([R 467])
  | 838 | 931 -> One ([R 469])
  | 835 | 925 -> One ([R 473])
  | 1256 -> One ([R 476])
  | 814 | 908 | 1072 -> One ([R 477])
  | 1267 -> One ([R 482])
  | 1268 -> One ([R 483])
  | 1266 -> One ([R 484])
  | 1269 -> One ([R 486])
  | 1259 -> One ([R 487])
  | 1262 -> One ([R 489])
  | 1027 -> One ([R 490])
  | 2464 -> One ([R 494])
  | 1888 | 1924 -> One ([R 504])
  | 2103 -> One ([R 506])
  | 2101 -> One ([R 507])
  | 2104 -> One ([R 508])
  | 2102 -> One ([R 509])
  | 2190 -> One (R 510 :: r1247)
  | 253 -> One ([R 511])
  | 465 | 2929 -> One ([R 514])
  | 466 | 2930 -> One ([R 515])
  | 464 | 2928 -> One ([R 516])
  | 2600 | 2886 -> One ([R 518])
  | 2599 | 2885 -> One ([R 519])
  | 2601 | 2887 -> One ([R 520])
  | 2596 | 2882 -> One ([R 521])
  | 2597 | 2883 -> One ([R 522])
  | 2005 | 2964 -> One ([R 524])
  | 2003 | 2962 -> One ([R 525])
  | 612 -> One ([R 528])
  | 567 -> One ([R 529])
  | 1494 | 1683 -> One ([R 530])
  | 1493 | 1682 -> One ([R 531])
  | 402 -> One ([R 533])
  | 1596 | 1754 -> One ([R 536])
  | 366 -> One ([R 557])
  | 1513 -> One ([R 560])
  | 1514 -> One ([R 561])
  | 1815 -> One ([R 563])
  | 1816 -> One ([R 564])
  | 854 -> One ([R 566])
  | 855 -> One ([R 567])
  | 1540 -> One ([R 569])
  | 1541 -> One ([R 570])
  | 1148 | 1365 -> One ([R 572])
  | 1152 | 1369 -> One ([R 573])
  | 2459 | 3068 -> One ([R 578])
  | 2436 | 3045 -> One ([R 579])
  | 2439 | 3048 -> One ([R 580])
  | 2438 | 3047 -> One ([R 585])
  | 2442 | 3051 -> One ([R 588])
  | 2441 | 3050 -> One ([R 590])
  | 2440 | 3049 -> One ([R 591])
  | 2460 | 3069 -> One ([R 594])
  | 261 | 515 -> One ([R 597])
  | 258 | 270 -> One ([R 599])
  | 771 | 783 -> One ([R 601])
  | 851 | 900 -> One ([R 603])
  | 860 | 2829 -> One ([R 606])
  | 2491 | 3127 -> One ([R 610])
  | 1343 | 1598 -> One ([R 613])
  | 1413 | 1602 -> One ([R 615])
  | 1445 | 1634 -> One ([R 617])
  | 1439 | 1628 -> One ([R 619])
  | 1451 | 1640 -> One ([R 621])
  | 1427 | 1616 -> One ([R 623])
  | 1423 | 1612 -> One ([R 625])
  | 1431 | 1620 -> One ([R 627])
  | 1417 | 1606 -> One ([R 629])
  | 1570 | 1766 -> One ([R 632])
  | 1047 | 1302 -> One ([R 637])
  | 1130 | 1321 -> One ([R 638])
  | 1050 | 1126 | 1305 | 1347 -> One ([R 640])
  | 988 | 1819 -> One ([R 644])
  | 1433 | 1488 | 1622 | 1677 -> One ([R 646])
  | 1522 | 1561 -> One ([R 649])
  | 1128 | 1319 -> One ([R 652])
  | 1505 | 1709 -> One ([R 654])
  | 1773 | 1776 -> One ([R 656])
  | 1544 | 1565 -> One ([R 658])
  | 1835 | 2476 -> One ([R 660])
  | 1510 | 1552 -> One ([R 664])
  | 1779 | 1828 -> One ([R 666])
  | 1518 | 1557 -> One ([R 669])
  | 796 | 878 -> One ([R 672])
  | 799 | 881 -> One ([R 675])
  | 800 | 882 -> One ([R 679])
  | 857 | 904 -> One ([R 681])
  | 804 | 886 -> One ([R 682])
  | 856 | 902 -> One ([R 684])
  | 832 | 895 -> One ([R 688])
  | 809 | 890 -> One ([R 689])
  | 1224 | 2820 -> One ([R 690])
  | 1229 | 2825 -> One ([R 692])
  | 2817 | 3076 -> One ([R 694])
  | 825 | 903 -> One ([R 695])
  | 28 | 120 -> One ([R 696])
  | 8 | 100 -> One ([R 697])
  | 52 | 144 -> One ([R 699])
  | 51 | 143 -> One ([R 700])
  | 50 | 142 -> One ([R 701])
  | 49 | 141 -> One ([R 702])
  | 48 | 140 -> One ([R 703])
  | 47 | 139 -> One ([R 704])
  | 46 | 138 -> One ([R 705])
  | 45 | 137 -> One ([R 706])
  | 44 | 136 -> One ([R 707])
  | 43 | 135 -> One ([R 708])
  | 42 | 134 -> One ([R 709])
  | 41 | 133 -> One ([R 710])
  | 40 | 132 -> One ([R 711])
  | 39 | 131 -> One ([R 712])
  | 38 | 130 -> One ([R 713])
  | 37 | 129 -> One ([R 714])
  | 36 | 128 -> One ([R 715])
  | 35 | 127 -> One ([R 716])
  | 34 | 126 -> One ([R 717])
  | 33 | 125 -> One ([R 718])
  | 32 | 124 -> One ([R 719])
  | 31 | 123 -> One ([R 720])
  | 30 | 122 -> One ([R 721])
  | 29 | 121 -> One ([R 722])
  | 27 | 119 -> One ([R 723])
  | 26 | 118 -> One ([R 724])
  | 25 | 117 -> One ([R 725])
  | 24 | 116 -> One ([R 726])
  | 23 | 115 -> One ([R 727])
  | 22 | 114 -> One ([R 728])
  | 21 | 113 -> One ([R 729])
  | 20 | 112 -> One ([R 730])
  | 19 | 111 -> One ([R 731])
  | 18 | 110 -> One ([R 732])
  | 17 | 109 -> One ([R 733])
  | 16 | 108 -> One ([R 734])
  | 15 | 107 -> One ([R 735])
  | 14 | 106 -> One ([R 736])
  | 13 | 105 -> One ([R 737])
  | 12 | 104 -> One ([R 738])
  | 11 | 103 -> One ([R 739])
  | 10 | 102 -> One ([R 740])
  | 9 | 101 -> One ([R 741])
  | 7 | 99 -> One ([R 742])
  | 6 | 98 -> One ([R 743])
  | 5 | 97 -> One ([R 744])
  | 4 | 96 -> One ([R 745])
  | 3 | 95 -> One ([R 746])
  | 2373 | 2774 -> One ([R 747])
  | 2401 | 2814 -> One ([R 752])
  | 2377 | 2400 | 2778 | 2805 -> One ([R 754])
  | 2379 | 2402 | 2786 | 2807 -> One ([R 755])
  | 2392 | 2800 -> One ([R 757])
  | 2374 | 2775 -> One ([R 758])
  | 2369 | 2770 -> One ([R 759])
  | 2372 | 2773 -> One ([R 763])
  | 2376 | 2777 -> One ([R 766])
  | 2375 | 2776 -> One ([R 767])
  | 2393 | 2801 -> One ([R 769])
  | 241 -> One ([R 771])
  | 240 -> One ([R 772])
  | 3177 -> One ([R 776])
  | 3178 -> One ([R 777])
  | 3180 -> One ([R 778])
  | 3181 -> One ([R 779])
  | 3179 -> One ([R 780])
  | 3176 -> One ([R 781])
  | 3183 -> One ([R 785])
  | 342 | 667 -> One ([R 787])
  | 1209 | 1534 -> One ([R 788])
  | 1206 | 1531 -> One ([R 790])
  | 591 -> One ([R 799])
  | 283 -> One ([R 804])
  | 285 -> One ([R 805])
  | 197 | 545 | 991 | 1548 -> One ([R 808])
  | 195 | 845 -> One ([R 810])
  | 1048 | 1303 -> One ([R 813])
  | 1567 | 1568 -> One ([R 814])
  | 2074 -> One ([R 822])
  | 1870 -> One ([R 823])
  | 1873 -> One ([R 824])
  | 1871 -> One ([R 825])
  | 1922 -> One ([R 826])
  | 1925 -> One ([R 827])
  | 1923 -> One ([R 828])
  | 580 -> One ([R 833])
  | 581 -> One ([R 834])
  | 1264 -> One (S (T T_error) :: r869)
  | 1525 -> One (S (T T_WITH) :: r934)
  | 3185 -> One (S (T T_SEMISEMI) :: r1416)
  | 3192 -> One (S (T T_SEMISEMI) :: r1420)
  | 558 -> One (S (T T_RPAREN) :: r455)
  | 182 -> One (S (T T_RBRACKET) :: r89)
  | 185 -> One (S (T T_RBRACE) :: r90)
  | 178 -> One (S (T T_LPAREN) :: r88)
  | 206 -> One (S (T T_LIDENT) :: r99)
  | 421 -> One (S (T T_LIDENT) :: r352)
  | 962 -> One (S (T T_LIDENT) :: r606)
  | 970 -> One (S (T T_LIDENT) :: r612)
  | 1874 -> One (S (T T_LIDENT) :: r981)
  | 1926 -> One (S (T T_LIDENT) :: r1010)
  | 2262 -> One (S (T T_LIDENT) :: r1289)
  | 1699 -> One (S (T T_EQUAL) :: r952)
  | 1862 -> One (S (T T_EQUAL) :: r978)
  | 1882 -> One (S (T T_EQUAL) :: r983)
  | 3154 -> One (S (T T_EOF) :: r1410)
  | 3158 -> One (S (T T_EOF) :: r1411)
  | 3163 -> One (S (T T_EOF) :: r1412)
  | 3166 -> One (S (T T_EOF) :: r1413)
  | 3170 -> One (S (T T_EOF) :: r1414)
  | 3209 -> One (S (T T_EOF) :: r1429)
  | 424 -> One (S (T T_DOT) :: r354)
  | 309 -> One (S (T T_COLON) :: r217)
  | 562 -> One (S (T T_COLON) :: r458)
  | 542 -> One (S (N N_pattern) :: r422)
  | 764 -> One (S (N N_pattern) :: r511)
  | 777 -> One (S (N N_pattern) :: r520)
  | 1255 -> One (S (N N_pattern) :: r866)
  | 1258 -> One (S (N N_pattern) :: r867)
  | 1261 -> One (S (N N_pattern) :: r868)
  | 1270 -> One (S (N N_pattern) :: r870)
  | 1020 -> One (S (N N_let_pattern) :: r681)
  | 976 -> One (S (N N_expr) :: r621)
  | 973 -> One (Sub (r1) :: r616)
  | 2553 -> One (Sub (r1) :: r1393)
  | 2151 -> One (Sub (r12) :: r1222)
  | 377 -> One (Sub (r75) :: r310)
  | 411 -> One (Sub (r77) :: r330)
  | 415 -> One (Sub (r77) :: r333)
  | 774 -> One (Sub (r77) :: r519)
  | 927 -> One (Sub (r77) :: r568)
  | 965 -> One (Sub (r77) :: r611)
  | 1022 -> One (Sub (r77) :: r682)
  | 1073 -> One (Sub (r77) :: r726)
  | 2084 -> One (Sub (r77) :: r1155)
  | 2166 -> One (Sub (r77) :: r1234)
  | 289 -> One (Sub (r95) :: r196)
  | 425 -> One (Sub (r95) :: r355)
  | 3174 -> One (Sub (r95) :: r1415)
  | 1987 -> One (Sub (r107) :: r1061)
  | 1705 -> One (Sub (r141) :: r953)
  | 295 -> One (Sub (r191) :: r197)
  | 286 -> One (Sub (r193) :: r195)
  | 2076 -> One (Sub (r193) :: r1148)
  | 370 -> One (Sub (r257) :: r304)
  | 1200 -> One (Sub (r523) :: r825)
  | 937 -> One (Sub (r574) :: r576)
  | 963 -> One (Sub (r608) :: r610)
  | 971 -> One (Sub (r608) :: r615)
  | 1018 -> One (Sub (r677) :: r678)
  | 1905 -> One (Sub (r684) :: r1004)
  | 1939 -> One (Sub (r684) :: r1018)
  | 1885 -> One (Sub (r988) :: r990)
  | 1904 -> One (Sub (r996) :: r999)
  | 1938 -> One (Sub (r996) :: r1013)
  | 1138 | 1355 -> One (r0)
  | 3116 | 3153 -> One (r2)
  | 3115 | 3152 -> One (r3)
  | 3114 | 3151 -> One (r4)
  | 3113 | 3150 -> One (r5)
  | 90 | 3149 -> One (r6)
  | 58 | 89 -> One (r7)
  | 1 | 88 -> One (r8)
  | 53 | 145 -> One (r9)
  | 54 | 146 -> One (r11)
  | 57 | 149 -> One (r13)
  | 2 | 94 -> One (r14)
  | 56 | 148 -> One (r15)
  | 55 | 147 -> One (r16)
  | 2394 -> One (r17)
  | 3107 | 3148 -> One (r19)
  | 3106 | 3147 -> One (r20)
  | 60 | 152 -> One (r21)
  | 59 | 151 -> One (r22)
  | 3090 | 3146 -> One (r23)
  | 3089 | 3145 -> One (r24)
  | 3088 | 3144 -> One (r25)
  | 3087 | 3143 -> One (r26)
  | 63 | 239 -> One (r27)
  | 62 | 238 -> One (r28)
  | 61 | 237 -> One (r29)
  | 65 | 264 -> One (r30)
  | 2838 | 3136 -> One (r31)
  | 68 | 506 -> One (r32)
  | 67 | 505 -> One (r33)
  | 66 | 504 -> One (r34)
  | 2837 | 3135 -> One (r35)
  | 2835 | 3133 -> One (r36)
  | 2834 | 3132 -> One (r37)
  | 71 | 509 -> One (r38)
  | 70 | 508 -> One (r39)
  | 69 | 507 -> One (r40)
  | 792 | 876 -> One (r41)
  | 1129 | 1144 | 1320 | 1361 -> One (r43)
  | 2478 | 3131 -> One (r45)
  | 74 | 753 -> One (r46)
  | 78 | 659 -> One (r47)
  | 83 | 757 -> One (r49)
  | 77 | 756 -> One (r50)
  | 76 | 755 -> One (r51)
  | 75 | 754 -> One (r52)
  | 82 | 662 -> One (r53)
  | 80 | 661 -> One (r54)
  | 79 | 660 -> One (r55)
  | 81 | 807 -> One (r56)
  | 85 | 2148 -> One (r57)
  | 84 | 2147 -> One (r58)
  | 87 | 517 -> One (r59)
  | 86 | 156 | 262 | 516 | 758 | 842 | 1029 | 1110 | 1573 | 1729 -> One (r60)
  | 2473 | 3112 -> One (r61)
  | 2471 | 3110 -> One (r62)
  | 936 | 3109 -> One (r63)
  | 763 | 3108 -> One (r64)
  | 150 | 762 -> One (r65)
  | 93 | 761 -> One (r66)
  | 200 | 222 -> One (r68)
  | 362 | 682 -> One (r70)
  | 340 | 665 -> One (r72)
  | 378 | 687 -> One (r74)
  | 388 | 700 -> One (r76)
  | 3098 | 3105 -> One (r78)
  | 3097 | 3104 -> One (r79)
  | 199 | 221 -> One (r80)
  | 198 | 220 -> One (r81)
  | 155 | 219 -> One (r82)
  | 154 | 218 -> One (r83)
  | 153 | 217 -> One (r84)
  | 165 | 520 -> One (r85)
  | 164 | 271 | 519 | 843 | 1057 | 1310 | 2479 | 3117 -> One (r86)
  | 180 -> One (r87)
  | 179 -> One (r88)
  | 183 -> One (r89)
  | 186 -> One (r90)
  | 196 | 846 -> One (r91)
  | 194 | 844 -> One (r92)
  | 201 | 214 | 277 | 704 -> One (r93)
  | 204 | 225 -> One (r94)
  | 205 | 226 -> One (r96)
  | 202 | 223 -> One (r97)
  | 208 -> One (r98)
  | 207 -> One (r99)
  | 3096 | 3103 -> One (r100)
  | 3095 | 3102 -> One (r101)
  | 213 | 230 -> One (r102)
  | 212 | 229 -> One (r103)
  | 211 | 228 -> One (r104)
  | 210 | 227 -> One (r105)
  | 2463 | 3074 -> One (r106)
  | 3073 | 3101 -> One (r108)
  | 3071 | 3099 -> One (r109)
  | 216 | 279 -> One (r110)
  | 215 | 278 -> One (r111)
  | 2815 | 3094 -> One (r112)
  | 234 | 529 -> One (r113)
  | 233 | 528 -> One (r114)
  | 232 | 527 -> One (r115)
  | 231 | 526 -> One (r116)
  | 2810 | 3093 -> One (r117)
  | 2808 | 3091 -> One (r118)
  | 236 | 531 -> One (r119)
  | 235 | 530 -> One (r120)
  | 1860 | 3086 -> One (r121)
  | 1859 | 3085 -> One (r122)
  | 1858 | 3084 -> One (r123)
  | 244 | 945 -> One (r124)
  | 243 | 944 -> One (r125)
  | 242 | 943 -> One (r126)
  | 1832 | 3083 -> One (r127)
  | 1831 | 3082 -> One (r128)
  | 1830 | 3081 -> One (r129)
  | 247 | 956 -> One (r130)
  | 246 | 955 -> One (r131)
  | 245 | 954 -> One (r132)
  | 1826 | 3080 -> One (r133)
  | 249 | 959 -> One (r134)
  | 958 -> One (r135)
  | 1203 | 1896 -> One (r136)
  | 1202 | 1895 -> One (r137)
  | 1246 | 2682 -> One (r139)
  | 1195 | 2666 -> One (r140)
  | 2692 | 3079 -> One (r142)
  | 2691 | 3078 -> One (r143)
  | 254 | 2665 -> One (r144)
  | 252 | 2664 -> One (r145)
  | 251 | 2663 -> One (r146)
  | 250 | 2662 -> One (r147)
  | 255 | 263 -> One (r148)
  | 257 | 269 -> One (r149)
  | 256 | 512 -> One (r150)
  | 260 | 514 -> One (r151)
  | 259 | 513 -> One (r152)
  | 267 | 518 -> One (r153)
  | 275 | 524 -> One (r154)
  | 274 | 523 -> One (r155)
  | 273 | 522 -> One (r156)
  | 272 | 521 -> One (r157)
  | 2818 | 3077 -> One (r158)
  | 2816 | 3075 -> One (r159)
  | 276 | 525 -> One (r160)
  | 298 | 307 | 537 | 669 | 2609 | 2966 -> One (r161)
  | 439 | 2567 -> One (r163)
  | 471 | 2607 -> One (r165)
  | 470 | 2606 -> One (r166)
  | 469 | 2602 | 2888 | 2933 -> One (r167)
  | 2004 | 2963 -> One (r169)
  | 2019 | 2975 -> One (r171)
  | 2018 | 2974 -> One (r172)
  | 2017 | 2973 -> One (r173)
  | 2016 | 2972 -> One (r174)
  | 2015 | 2971 -> One (r175)
  | 2965 -> One (r176)
  | 282 -> One (r177)
  | 281 -> One (r178)
  | 280 -> One (r179)
  | 2002 | 2961 -> One (r180)
  | 2001 | 2960 -> One (r181)
  | 2000 | 2959 -> One (r182)
  | 1999 | 2958 -> One (r183)
  | 1998 | 2957 -> One (r184)
  | 297 -> One (r185)
  | 284 -> One (r186)
  | 288 -> One (r187)
  | 291 -> One (r189)
  | 287 -> One (r190)
  | 292 -> One (r192)
  | 294 -> One (r194)
  | 293 -> One (r195)
  | 290 -> One (r196)
  | 296 -> One (r197)
  | 475 | 2937 -> One (r198)
  | 476 | 2938 -> One (r200)
  | 299 | 321 -> One (r201)
  | 440 | 2923 -> One (r202)
  | 302 | 324 -> One (r203)
  | 304 | 326 -> One (r204)
  | 303 | 325 | 443 | 2562 -> One (r205)
  | 686 | 2911 -> One (r206)
  | 627 | 2910 -> One (r207)
  | 626 | 2909 -> One (r208)
  | 306 | 625 -> One (r209)
  | 624 -> One (r210)
  | 629 | 2906 -> One (r211)
  | 308 | 628 -> One (r212)
  | 433 -> One (r213)
  | 2902 -> One (r215)
  | 2901 -> One (r216)
  | 310 -> One (r217)
  | 1930 -> One (r218)
  | 1929 -> One (r219)
  | 2551 | 2900 -> One (r220)
  | 2550 | 2899 -> One (r221)
  | 314 | 547 -> One (r222)
  | 313 | 546 -> One (r223)
  | 2598 | 2884 -> One (r224)
  | 2617 | 2898 -> One (r226)
  | 2616 | 2897 -> One (r227)
  | 2615 | 2896 -> One (r228)
  | 2614 | 2895 -> One (r229)
  | 2613 | 2894 -> One (r230)
  | 2608 -> One (r231)
  | 534 -> One (r232)
  | 533 -> One (r233)
  | 532 -> One (r234)
  | 2014 | 2612 -> One (r235)
  | 2013 | 2611 -> One (r236)
  | 2012 | 2610 -> One (r237)
  | 2595 | 2881 -> One (r243)
  | 2594 | 2880 -> One (r244)
  | 2560 | 2874 -> One (r245)
  | 2559 | 2873 -> One (r246)
  | 2558 | 2872 -> One (r247)
  | 536 -> One (r248)
  | 535 -> One (r249)
  | 2557 | 2871 -> One (r250)
  | 492 | 539 -> One (r251)
  | 491 | 538 -> One (r252)
  | 386 -> One (r256)
  | 367 -> One (r258)
  | 398 | 639 -> One (r260)
  | 397 | 638 -> One (r261)
  | 329 | 637 -> One (r262)
  | 327 | 636 -> One (r263)
  | 331 | 644 -> One (r264)
  | 330 | 643 -> One (r265)
  | 396 | 647 -> One (r266)
  | 395 | 646 -> One (r267)
  | 332 | 645 -> One (r268)
  | 338 | 795 -> One (r269)
  | 337 | 794 -> One (r270)
  | 385 -> One (r272)
  | 372 -> One (r273)
  | 390 | 653 -> One (r275)
  | 389 | 652 -> One (r276)
  | 333 | 648 -> One (r277)
  | 335 | 663 -> One (r278)
  | 334 | 658 -> One (r279)
  | 369 | 651 -> One (r280)
  | 368 | 650 -> One (r281)
  | 336 | 649 -> One (r282)
  | 365 | 685 -> One (r283)
  | 341 | 666 -> One (r284)
  | 354 | 674 -> One (r285)
  | 343 | 668 -> One (r286)
  | 346 | 707 -> One (r287)
  | 345 | 706 -> One (r288)
  | 352 | 672 -> One (r289)
  | 351 | 671 -> One (r290)
  | 670 -> One (r291)
  | 348 | 709 -> One (r292)
  | 347 | 708 -> One (r293)
  | 350 | 711 -> One (r294)
  | 349 | 710 -> One (r295)
  | 356 | 676 -> One (r296)
  | 355 | 675 -> One (r297)
  | 361 | 681 -> One (r298)
  | 360 | 680 -> One (r299)
  | 359 | 679 -> One (r300)
  | 358 | 678 -> One (r301)
  | 364 | 684 -> One (r302)
  | 363 | 683 -> One (r303)
  | 371 -> One (r304)
  | 384 -> One (r305)
  | 383 -> One (r307)
  | 376 -> One (r308)
  | 375 -> One (r309)
  | 379 -> One (r310)
  | 382 | 691 -> One (r311)
  | 381 | 690 -> One (r312)
  | 380 | 689 -> One (r313)
  | 394 | 657 -> One (r314)
  | 393 | 656 -> One (r315)
  | 392 | 655 -> One (r316)
  | 391 | 654 -> One (r317)
  | 403 -> One (r318)
  | 401 | 642 -> One (r320)
  | 400 | 641 -> One (r321)
  | 399 | 640 -> One (r322)
  | 405 | 2564 -> One (r323)
  | 404 | 2563 -> One (r324)
  | 410 | 2916 -> One (r325)
  | 407 | 2913 -> One (r326)
  | 406 | 2912 -> One (r327)
  | 409 | 2915 -> One (r328)
  | 408 | 2914 -> One (r329)
  | 412 -> One (r330)
  | 414 | 2918 -> One (r331)
  | 413 | 2917 -> One (r332)
  | 416 -> One (r333)
  | 418 | 2566 -> One (r334)
  | 417 | 444 | 2565 | 2919 -> One (r335)
  | 438 -> One (r342)
  | 435 | 2922 -> One (r344)
  | 434 | 2921 -> One (r345)
  | 419 | 2920 -> One (r346)
  | 432 -> One (r347)
  | 431 -> One (r348)
  | 430 -> One (r349)
  | 429 -> One (r350)
  | 423 -> One (r351)
  | 422 -> One (r352)
  | 428 -> One (r353)
  | 427 -> One (r354)
  | 426 -> One (r355)
  | 461 | 2593 -> One (r356)
  | 460 | 2592 -> One (r357)
  | 445 | 2569 | 2876 | 2925 -> One (r358)
  | 454 | 2578 -> One (r359)
  | 453 | 2577 -> One (r361)
  | 446 | 2570 -> One (r362)
  | 449 | 2573 -> One (r363)
  | 448 | 2572 -> One (r364)
  | 447 | 2571 -> One (r365)
  | 452 | 2576 -> One (r366)
  | 451 | 2575 -> One (r367)
  | 450 | 2574 -> One (r368)
  | 459 | 2591 -> One (r369)
  | 455 | 2587 -> One (r370)
  | 458 | 2590 -> One (r371)
  | 457 | 2589 -> One (r372)
  | 456 | 2588 -> One (r373)
  | 463 | 2568 | 2875 | 2927 -> One (r374)
  | 474 | 2936 -> One (r375)
  | 473 | 2935 -> One (r376)
  | 472 | 2934 -> One (r377)
  | 488 | 2950 -> One (r378)
  | 478 | 2940 -> One (r379)
  | 483 | 2945 -> One (r380)
  | 479 | 2941 -> One (r381)
  | 482 | 2944 -> One (r382)
  | 481 | 2943 -> One (r383)
  | 480 | 2942 -> One (r384)
  | 487 | 2949 -> One (r385)
  | 486 | 2948 -> One (r386)
  | 485 | 2947 -> One (r387)
  | 2556 | 2870 -> One (r388)
  | 2555 | 2869 -> One (r389)
  | 494 | 541 -> One (r390)
  | 493 | 540 -> One (r391)
  | 2806 | 2868 -> One (r392)
  | 495 | 2618 -> One (r393)
  | 2628 | 2864 -> One (r394)
  | 2627 | 2863 -> One (r395)
  | 2626 | 2862 -> One (r396)
  | 2625 | 2861 -> One (r397)
  | 496 | 2619 -> One (r398)
  | 2624 | 2860 -> One (r399)
  | 2623 | 2859 -> One (r400)
  | 499 | 2622 -> One (r401)
  | 498 | 2621 -> One (r402)
  | 497 | 2620 -> One (r403)
  | 2519 | 2531 -> One (r404)
  | 2517 | 2529 -> One (r405)
  | 500 | 750 -> One (r406)
  | 2497 | 2847 -> One (r407)
  | 2495 | 2845 -> One (r408)
  | 502 | 752 -> One (r409)
  | 501 | 751 -> One (r410)
  | 1825 -> One (r411)
  | 961 -> One (r412)
  | 1707 -> One (r414)
  | 1704 | 2833 -> One (r416)
  | 1703 | 2832 -> One (r417)
  | 1702 | 2831 -> One (r418)
  | 510 | 960 -> One (r419)
  | 858 | 2827 -> One (r420)
  | 511 | 778 -> One (r421)
  | 2552 -> One (r422)
  | 544 | 780 -> One (r423)
  | 1977 | 2581 -> One (r424)
  | 853 -> One (r425)
  | 852 | 901 -> One (r427)
  | 850 | 899 -> One (r428)
  | 779 -> One (r429)
  | 2536 | 2642 -> One (r430)
  | 741 | 2641 -> One (r431)
  | 2538 | 2648 -> One (r433)
  | 2537 | 2647 -> One (r434)
  | 740 | 2640 -> One (r435)
  | 739 | 2639 -> One (r436)
  | 738 | 2638 -> One (r437)
  | 548 | 2629 -> One (r438)
  | 737 | 2637 -> One (r439)
  | 734 | 2636 -> One (r440)
  | 552 | 2633 -> One (r441)
  | 551 | 2632 -> One (r442)
  | 550 | 2631 -> One (r443)
  | 549 | 2630 -> One (r444)
  | 733 | 2635 -> One (r445)
  | 553 | 2634 -> One (r446)
  | 729 | 732 -> One (r447)
  | 727 | 730 -> One (r448)
  | 554 | 555 -> One (r449)
  | 571 | 618 -> One (r450)
  | 569 | 616 -> One (r451)
  | 568 | 615 -> One (r452)
  | 557 | 565 -> One (r453)
  | 556 | 564 -> One (r454)
  | 561 -> One (r455)
  | 614 -> One (r456)
  | 613 -> One (r457)
  | 563 -> One (r458)
  | 595 | 705 -> One (r461)
  | 594 | 703 -> One (r462)
  | 593 | 702 -> One (r463)
  | 592 | 701 -> One (r464)
  | 599 | 714 -> One (r466)
  | 600 | 715 -> One (r468)
  | 572 | 619 -> One (r469)
  | 575 | 766 -> One (r470)
  | 583 | 688 -> One (r472)
  | 582 | 623 -> One (r473)
  | 579 | 622 -> One (r474)
  | 574 | 621 -> One (r475)
  | 573 | 620 -> One (r476)
  | 578 | 769 -> One (r477)
  | 577 | 768 -> One (r478)
  | 576 | 767 | 1419 | 1471 | 1608 | 1660 -> One (r479)
  | 586 | 694 -> One (r480)
  | 585 | 693 -> One (r481)
  | 584 | 692 -> One (r482)
  | 590 | 698 -> One (r483)
  | 589 | 697 -> One (r484)
  | 588 | 696 -> One (r485)
  | 587 | 695 -> One (r486)
  | 596 | 2527 | 3182 -> One (r487)
  | 598 | 713 -> One (r488)
  | 597 | 712 -> One (r489)
  | 602 | 717 -> One (r490)
  | 601 | 716 -> One (r491)
  | 604 | 719 -> One (r492)
  | 603 | 718 -> One (r493)
  | 609 | 724 -> One (r494)
  | 607 | 722 -> One (r495)
  | 606 | 721 -> One (r496)
  | 632 | 2908 -> One (r497)
  | 631 | 2907 -> One (r498)
  | 736 | 1867 -> One (r499)
  | 735 | 1866 -> One (r500)
  | 747 | 2514 -> One (r501)
  | 745 | 2512 -> One (r502)
  | 744 | 2511 -> One (r503)
  | 743 | 2510 -> One (r504)
  | 742 | 2509 -> One (r505)
  | 749 | 2516 -> One (r506)
  | 748 | 2515 -> One (r507)
  | 1833 | 2474 -> One (r508)
  | 760 | 953 -> One (r509)
  | 923 -> One (r510)
  | 922 -> One (r511)
  | 863 -> One (r512)
  | 776 -> One (r513)
  | 773 -> One (r514)
  | 772 | 784 -> One (r516)
  | 770 | 782 -> One (r517)
  | 765 | 781 -> One (r518)
  | 775 -> One (r519)
  | 862 -> One (r520)
  | 797 | 879 | 1247 | 2683 -> One (r522)
  | 798 | 880 -> One (r524)
  | 787 | 871 -> One (r525)
  | 786 | 870 -> One (r526)
  | 785 | 869 -> One (r527)
  | 788 | 872 -> One (r528)
  | 790 | 874 -> One (r529)
  | 789 | 873 -> One (r530)
  | 803 | 885 -> One (r532)
  | 802 | 884 -> One (r534)
  | 801 | 883 -> One (r535)
  | 849 | 898 -> One (r536)
  | 848 | 897 -> One (r537)
  | 806 | 888 -> One (r538)
  | 805 -> One (r539)
  | 810 | 891 -> One (r540)
  | 808 | 889 -> One (r541)
  | 828 | 921 -> One (r542)
  | 813 | 907 -> One (r543)
  | 812 | 906 -> One (r544)
  | 811 | 905 -> One (r545)
  | 818 | 912 -> One (r546)
  | 817 | 911 -> One (r547)
  | 821 | 915 -> One (r548)
  | 819 | 913 -> One (r549)
  | 824 | 918 -> One (r550)
  | 823 -> One (r551)
  | 827 | 920 -> One (r552)
  | 826 -> One (r553)
  | 830 | 893 -> One (r554)
  | 829 | 892 -> One (r555)
  | 833 | 896 -> One (r556)
  | 831 | 894 -> One (r557)
  | 836 | 926 -> One (r558)
  | 834 | 924 -> One (r559)
  | 839 | 932 -> One (r560)
  | 837 | 930 -> One (r561)
  | 841 | 934 -> One (r562)
  | 840 | 933 -> One (r563)
  | 861 | 2830 -> One (r564)
  | 859 | 2828 -> One (r565)
  | 867 -> One (r566)
  | 929 -> One (r567)
  | 928 -> One (r568)
  | 1881 -> One (r569)
  | 1880 -> One (r570)
  | 1879 -> One (r571)
  | 1878 -> One (r572)
  | 1869 -> One (r573)
  | 1868 -> One (r575)
  | 1865 -> One (r576)
  | 1861 -> One (r577)
  | 942 -> One (r578)
  | 941 -> One (r579)
  | 940 -> One (r580)
  | 939 -> One (r581)
  | 1786 | 1842 -> One (r582)
  | 1785 | 1841 -> One (r583)
  | 1784 | 1840 -> One (r584)
  | 1783 | 1839 -> One (r585)
  | 1782 | 1838 -> One (r586)
  | 947 | 978 -> One (r587)
  | 977 -> One (r588)
  | 1812 | 2698 -> One (r589)
  | 1811 | 2697 -> One (r590)
  | 1810 | 2696 -> One (r591)
  | 1809 | 2695 -> One (r592)
  | 1808 | 2694 -> One (r593)
  | 2371 | 2772 -> One (r594)
  | 1781 | 1837 -> One (r595)
  | 952 | 983 -> One (r596)
  | 951 | 982 -> One (r597)
  | 950 | 981 -> One (r598)
  | 949 | 980 -> One (r599)
  | 948 | 979 -> One (r600)
  | 1512 -> One (r601)
  | 1780 | 1829 -> One (r603)
  | 1778 | 1827 -> One (r604)
  | 984 -> One (r605)
  | 969 -> One (r606)
  | 964 -> One (r607)
  | 968 -> One (r609)
  | 967 -> One (r610)
  | 966 -> One (r611)
  | 1824 -> One (r612)
  | 1823 -> One (r613)
  | 1822 -> One (r614)
  | 972 -> One (r615)
  | 1821 -> One (r616)
  | 986 | 1817 -> One (r617)
  | 974 | 985 -> One (r618)
  | 1814 -> One (r619)
  | 975 -> One (r620)
  | 1813 -> One (r621)
  | 989 | 1820 -> One (r622)
  | 987 | 1818 -> One (r623)
  | 1539 -> One (r624)
  | 1538 -> One (r625)
  | 1528 -> One (r626)
  | 1542 -> One (r628)
  | 1774 | 1777 -> One (r630)
  | 1772 | 1775 -> One (r631)
  | 990 | 992 -> One (r632)
  | 1762 | 1771 -> One (r633)
  | 993 | 1004 -> One (r634)
  | 1761 | 1770 -> One (r635)
  | 1760 | 1769 -> One (r636)
  | 994 | 1005 -> One (r637)
  | 1763 | 1768 -> One (r638)
  | 997 | 1003 -> One (r639)
  | 996 | 1002 -> One (r640)
  | 995 | 1001 -> One (r641)
  | 1300 | 1764 -> One (r642)
  | 1000 | 1299 -> One (r643)
  | 999 | 1298 -> One (r644)
  | 998 | 1297 -> One (r645)
  | 1687 | 1757 -> One (r646)
  | 1686 | 1756 -> One (r647)
  | 1685 | 1755 -> One (r648)
  | 1008 | 1086 -> One (r649)
  | 1007 | 1085 -> One (r650)
  | 1006 | 1084 -> One (r651)
  | 1595 | 1753 -> One (r652)
  | 1011 | 1101 -> One (r653)
  | 1010 | 1100 -> One (r654)
  | 1009 | 1099 -> One (r655)
  | 1590 | 1748 -> One (r656)
  | 1588 | 1746 -> One (r657)
  | 1013 | 1103 -> One (r658)
  | 1591 | 1749 -> One (r660)
  | 1012 | 1102 -> One (r661)
  | 1587 | 1745 -> One (r662)
  | 1106 | 1744 -> One (r663)
  | 1105 | 1743 -> One (r664)
  | 1014 | 1104 -> One (r665)
  | 1216 -> One (r666)
  | 1572 | 1728 -> One (r668)
  | 1035 | 1114 -> One (r669)
  | 1586 | 1742 -> One (r671)
  | 1585 | 1741 -> One (r672)
  | 1017 | 1109 -> One (r673)
  | 1016 | 1108 -> One (r674)
  | 1015 | 1107 -> One (r675)
  | 1019 -> One (r676)
  | 1028 -> One (r678)
  | 1026 -> One (r679)
  | 1025 -> One (r680)
  | 1024 -> One (r681)
  | 1023 -> One (r682)
  | 1031 -> One (r683)
  | 1584 | 1740 -> One (r685)
  | 1034 | 1113 -> One (r686)
  | 1033 | 1112 -> One (r687)
  | 1030 | 1111 -> One (r688)
  | 1123 | 1720 -> One (r689)
  | 1122 | 1719 -> One (r690)
  | 1121 | 1718 -> One (r691)
  | 1120 | 1717 -> One (r692)
  | 1040 | 1119 -> One (r693)
  | 1039 | 1118 -> One (r694)
  | 1038 | 1117 -> One (r695)
  | 1037 | 1116 -> One (r696)
  | 1036 | 1115 -> One (r697)
  | 1280 -> One (r698)
  | 1296 | 1716 -> One (r700)
  | 1295 | 1715 -> One (r701)
  | 1294 | 1714 -> One (r702)
  | 1293 | 1713 -> One (r703)
  | 1292 | 1712 -> One (r704)
  | 1291 | 1711 -> One (r705)
  | 1045 | 1290 -> One (r706)
  | 1044 | 1289 -> One (r707)
  | 1043 | 1288 -> One (r708)
  | 1042 | 1287 -> One (r709)
  | 1041 | 1286 -> One (r710)
  | 1049 | 1304 -> One (r711)
  | 1046 | 1301 -> One (r712)
  | 1054 | 1309 -> One (r713)
  | 1053 | 1308 -> One (r714)
  | 1052 | 1307 | 1547 -> One (r715)
  | 1306 | 1546 -> One (r716)
  | 1062 | 1315 -> One (r717)
  | 1061 | 1314 -> One (r718)
  | 1060 | 1313 -> One (r719)
  | 1059 | 1312 -> One (r720)
  | 1058 | 1311 -> One (r721)
  | 1064 | 1317 -> One (r722)
  | 1063 | 1316 -> One (r723)
  | 1698 -> One (r724)
  | 1075 -> One (r725)
  | 1074 -> One (r726)
  | 1079 | 1325 -> One (r727)
  | 1078 | 1324 -> One (r728)
  | 1077 | 1323 -> One (r729)
  | 1076 | 1322 -> One (r730)
  | 1081 | 1327 -> One (r731)
  | 1080 | 1326 -> One (r732)
  | 1446 | 1500 | 1635 | 1694 -> One (r733)
  | 1444 | 1499 | 1633 | 1693 -> One (r734)
  | 1443 | 1498 | 1632 | 1692 -> One (r735)
  | 1082 | 1328 | 1436 | 1625 -> One (r736)
  | 1440 | 1497 | 1629 | 1691 -> One (r737)
  | 1438 | 1496 | 1627 | 1690 -> One (r738)
  | 1083 | 1329 | 1437 | 1626 -> One (r739)
  | 1495 | 1684 -> One (r740)
  | 1087 | 1330 -> One (r741)
  | 1090 | 1333 -> One (r742)
  | 1089 | 1332 -> One (r743)
  | 1092 | 1335 -> One (r744)
  | 1091 | 1334 -> One (r745)
  | 1094 | 1337 -> One (r746)
  | 1093 | 1336 -> One (r747)
  | 1096 | 1339 -> One (r748)
  | 1095 | 1338 -> One (r749)
  | 1418 | 1468 | 1607 | 1657 -> One (r750)
  | 1416 | 1467 | 1605 | 1656 -> One (r751)
  | 1415 | 1466 | 1604 | 1655 -> One (r752)
  | 1097 | 1340 | 1455 | 1644 -> One (r753)
  | 1344 | 1458 | 1599 | 1647 -> One (r754)
  | 1342 | 1457 | 1597 | 1646 -> One (r755)
  | 1098 | 1341 | 1456 | 1645 -> One (r756)
  | 1276 | 1410 -> One (r757)
  | 1125 | 1346 -> One (r758)
  | 1133 | 1351 -> One (r759)
  | 1132 | 1350 -> One (r760)
  | 1131 | 1349 -> One (r761)
  | 1137 | 1354 -> One (r762)
  | 1136 | 1353 -> One (r763)
  | 1135 | 1352 -> One (r764)
  | 1140 | 1357 -> One (r765)
  | 1139 | 1356 -> One (r766)
  | 1142 | 1359 -> One (r767)
  | 1141 | 1358 -> One (r768)
  | 1147 | 1364 -> One (r769)
  | 1146 | 1363 -> One (r770)
  | 1151 | 1368 -> One (r771)
  | 1150 | 1367 -> One (r772)
  | 1149 | 1366 -> One (r773)
  | 1154 | 1371 -> One (r774)
  | 1153 | 1370 -> One (r775)
  | 1156 | 1373 -> One (r776)
  | 1155 | 1372 -> One (r777)
  | 1158 | 1375 -> One (r778)
  | 1157 | 1374 -> One (r779)
  | 1160 | 1377 -> One (r780)
  | 1159 | 1376 -> One (r781)
  | 1162 | 1379 -> One (r782)
  | 1161 | 1378 -> One (r783)
  | 1164 | 1381 -> One (r784)
  | 1163 | 1380 -> One (r785)
  | 1166 | 1383 -> One (r786)
  | 1165 | 1382 -> One (r787)
  | 1168 | 1385 -> One (r788)
  | 1167 | 1384 -> One (r789)
  | 1170 | 1387 -> One (r790)
  | 1169 | 1386 -> One (r791)
  | 1172 | 1389 -> One (r792)
  | 1171 | 1388 -> One (r793)
  | 1174 | 1391 -> One (r794)
  | 1173 | 1390 -> One (r795)
  | 1176 | 1393 -> One (r796)
  | 1175 | 1392 -> One (r797)
  | 1178 | 1395 -> One (r798)
  | 1177 | 1394 -> One (r799)
  | 1180 | 1397 -> One (r800)
  | 1179 | 1396 -> One (r801)
  | 1182 | 1399 -> One (r802)
  | 1181 | 1398 -> One (r803)
  | 1184 | 1401 -> One (r804)
  | 1183 | 1400 -> One (r805)
  | 1186 | 1403 -> One (r806)
  | 1185 | 1402 -> One (r807)
  | 1188 | 1405 -> One (r808)
  | 1187 | 1404 -> One (r809)
  | 1190 | 1407 -> One (r810)
  | 1189 | 1406 -> One (r811)
  | 1192 | 1409 -> One (r812)
  | 1191 | 1408 -> One (r813)
  | 1273 | 2782 -> One (r814)
  | 1272 | 2781 -> One (r815)
  | 1194 | 2780 -> One (r816)
  | 1193 | 2779 -> One (r817)
  | 1215 | 1900 -> One (r818)
  | 1219 | 1903 -> One (r820)
  | 1199 | 1894 -> One (r821)
  | 1198 | 1893 -> One (r822)
  | 1197 | 1892 -> One (r823)
  | 1196 | 1891 -> One (r824)
  | 1201 -> One (r825)
  | 1205 | 1530 -> One (r826)
  | 1204 | 1529 -> One (r827)
  | 1208 | 1533 -> One (r828)
  | 1207 | 1231 | 1532 | 2667 -> One (r829)
  | 1211 | 1536 -> One (r830)
  | 1210 | 1535 -> One (r831)
  | 1214 | 1899 -> One (r832)
  | 1213 | 1898 -> One (r833)
  | 1212 | 1897 -> One (r834)
  | 1218 | 1902 -> One (r835)
  | 1217 | 1901 -> One (r836)
  | 1222 | 2489 -> One (r837)
  | 1221 | 2488 -> One (r838)
  | 1225 | 2821 -> One (r839)
  | 1223 | 2819 -> One (r840)
  | 1227 | 2823 -> One (r841)
  | 1226 | 2822 -> One (r842)
  | 1230 | 2826 -> One (r843)
  | 1228 | 2824 -> One (r844)
  | 1237 | 2673 -> One (r845)
  | 1236 | 2672 -> One (r846)
  | 1235 | 2671 -> One (r847)
  | 1234 | 2670 -> One (r848)
  | 1233 | 2669 -> One (r849)
  | 1232 | 2668 -> One (r850)
  | 1242 | 2678 -> One (r851)
  | 1241 | 2677 -> One (r852)
  | 1240 | 2676 -> One (r853)
  | 1239 | 2675 -> One (r854)
  | 1238 | 2674 -> One (r855)
  | 1245 | 2681 -> One (r856)
  | 1244 | 2680 -> One (r857)
  | 1243 | 2679 -> One (r858)
  | 1251 | 2687 -> One (r859)
  | 1250 | 2686 -> One (r860)
  | 1249 | 2685 -> One (r861)
  | 1248 | 2684 -> One (r862)
  | 1254 | 2690 -> One (r863)
  | 1253 | 2689 -> One (r864)
  | 1252 | 2688 -> One (r865)
  | 1257 -> One (r866)
  | 1260 -> One (r867)
  | 1263 -> One (r868)
  | 1265 -> One (r869)
  | 1271 -> One (r870)
  | 1285 | 1727 -> One (r871)
  | 1284 | 1726 -> One (r872)
  | 1283 | 1725 -> One (r873)
  | 1282 | 1724 -> One (r874)
  | 1281 | 1723 -> One (r875)
  | 1278 | 1722 -> One (r876)
  | 1277 | 1721 -> One (r877)
  | 1414 | 1463 | 1603 | 1652 -> One (r878)
  | 1412 | 1462 | 1601 | 1651 -> One (r879)
  | 1411 | 1461 | 1600 | 1650 -> One (r880)
  | 1428 | 1480 | 1617 | 1669 -> One (r881)
  | 1426 | 1479 | 1615 | 1668 -> One (r882)
  | 1425 | 1478 | 1614 | 1667 -> One (r883)
  | 1420 | 1472 | 1609 | 1661 -> One (r884)
  | 1424 | 1475 | 1613 | 1664 -> One (r885)
  | 1422 | 1474 | 1611 | 1663 -> One (r886)
  | 1421 | 1473 | 1610 | 1662 -> One (r887)
  | 1432 | 1485 | 1621 | 1674 -> One (r888)
  | 1430 | 1484 | 1619 | 1673 -> One (r889)
  | 1429 | 1483 | 1618 | 1672 -> One (r890)
  | 1435 | 1624 -> One (r891)
  | 1434 | 1623 -> One (r892)
  | 1442 | 1631 -> One (r893)
  | 1441 | 1630 -> One (r894)
  | 1448 | 1637 -> One (r895)
  | 1447 | 1636 -> One (r896)
  | 1452 | 1503 | 1641 | 1697 -> One (r897)
  | 1450 | 1502 | 1639 | 1696 -> One (r898)
  | 1449 | 1501 | 1638 | 1695 -> One (r899)
  | 1454 | 1643 -> One (r900)
  | 1453 | 1642 -> One (r901)
  | 1460 | 1649 -> One (r902)
  | 1459 | 1648 -> One (r903)
  | 1465 | 1654 -> One (r904)
  | 1464 | 1653 -> One (r905)
  | 1470 | 1659 -> One (r906)
  | 1469 | 1658 -> One (r907)
  | 1477 | 1666 -> One (r908)
  | 1476 | 1665 -> One (r909)
  | 1482 | 1671 -> One (r910)
  | 1481 | 1670 -> One (r911)
  | 1487 | 1676 -> One (r912)
  | 1486 | 1675 -> One (r913)
  | 1490 | 1679 -> One (r914)
  | 1489 | 1678 -> One (r915)
  | 1506 | 1710 -> One (r916)
  | 1504 | 1708 -> One (r917)
  | 1508 | 1550 -> One (r918)
  | 1507 | 1549 -> One (r919)
  | 1511 | 1553 -> One (r920)
  | 1509 | 1551 -> One (r921)
  | 1516 | 1555 -> One (r922)
  | 1515 | 1554 -> One (r923)
  | 1519 | 1558 -> One (r924)
  | 1517 | 1556 -> One (r925)
  | 1523 | 1562 -> One (r927)
  | 1521 | 1560 -> One (r928)
  | 1520 | 1559 -> One (r929)
  | 1545 | 1566 -> One (r930)
  | 1543 | 1564 -> One (r931)
  | 1524 | 1563 -> One (r932)
  | 1527 -> One (r933)
  | 1526 -> One (r934)
  | 1571 | 1767 -> One (r935)
  | 1569 | 1765 -> One (r936)
  | 1583 | 1739 -> One (r937)
  | 1576 | 1732 -> One (r938)
  | 1575 | 1731 -> One (r939)
  | 1574 | 1730 -> One (r940)
  | 1580 | 1736 -> One (r941)
  | 1579 | 1735 -> One (r942)
  | 1578 | 1734 -> One (r943)
  | 1577 | 1733 -> One (r944)
  | 1582 | 1738 -> One (r945)
  | 1581 | 1737 -> One (r946)
  | 1594 | 1752 -> One (r947)
  | 1593 | 1751 -> One (r948)
  | 1689 | 1759 -> One (r949)
  | 1688 | 1758 -> One (r950)
  | 1701 -> One (r951)
  | 1700 -> One (r952)
  | 1706 -> One (r953)
  | 1797 | 1849 -> One (r954)
  | 1796 | 1848 -> One (r955)
  | 1795 | 1847 -> One (r956)
  | 1790 | 1846 -> One (r957)
  | 1789 | 1845 -> One (r958)
  | 1788 | 1844 -> One (r959)
  | 1787 | 1843 -> One (r960)
  | 1794 | 2646 -> One (r961)
  | 1793 | 2645 -> One (r962)
  | 1792 | 2644 -> One (r963)
  | 1791 | 2643 -> One (r964)
  | 1799 | 2650 -> One (r965)
  | 1798 | 2649 -> One (r966)
  | 1807 | 1857 -> One (r967)
  | 1806 | 1856 -> One (r968)
  | 1805 | 1855 -> One (r969)
  | 1804 | 1854 -> One (r970)
  | 1803 | 1853 -> One (r971)
  | 1802 | 1852 -> One (r972)
  | 1801 | 1851 -> One (r973)
  | 1800 | 1850 -> One (r974)
  | 1836 | 2477 -> One (r975)
  | 1834 | 2475 -> One (r976)
  | 1864 -> One (r977)
  | 1863 -> One (r978)
  | 1877 -> One (r979)
  | 1876 -> One (r980)
  | 1875 -> One (r981)
  | 1884 -> One (r982)
  | 1883 -> One (r983)
  | 1948 -> One (r984)
  | 1937 -> One (r985)
  | 1936 -> One (r986)
  | 1921 -> One (r987)
  | 1920 -> One (r989)
  | 1919 -> One (r990)
  | 1918 -> One (r991)
  | 1890 -> One (r992)
  | 1889 -> One (r993)
  | 1887 -> One (r994)
  | 1917 | 1935 -> One (r995)
  | 1916 -> One (r997)
  | 1915 -> One (r998)
  | 1914 -> One (r999)
  | 1910 -> One (r1000)
  | 1909 -> One (r1001)
  | 1908 -> One (r1002)
  | 1907 -> One (r1003)
  | 1906 -> One (r1004)
  | 1913 | 1933 -> One (r1005)
  | 1912 | 1932 -> One (r1006)
  | 1911 | 1931 -> One (r1007)
  | 1934 -> One (r1008)
  | 1928 -> One (r1009)
  | 1927 -> One (r1010)
  | 1947 -> One (r1011)
  | 1946 -> One (r1012)
  | 1945 -> One (r1013)
  | 1944 -> One (r1014)
  | 1943 -> One (r1015)
  | 1942 -> One (r1016)
  | 1941 -> One (r1017)
  | 1940 -> One (r1018)
  | 2468 | 2470 -> One (r1019)
  | 2467 | 2469 -> One (r1020)
  | 1950 | 1952 -> One (r1021)
  | 1949 | 1951 -> One (r1022)
  | 2399 | 2466 -> One (r1023)
  | 2398 | 2465 -> One (r1024)
  | 1954 | 2111 -> One (r1025)
  | 1953 | 2110 -> One (r1026)
  | 1959 | 2703 -> One (r1027)
  | 1958 | 2702 -> One (r1028)
  | 1957 | 2701 -> One (r1029)
  | 1956 | 2700 -> One (r1030)
  | 1955 | 2699 -> One (r1031)
  | 1967 | 2711 -> One (r1032)
  | 1970 | 2714 -> One (r1034)
  | 1969 | 2713 -> One (r1035)
  | 1966 | 2710 -> One (r1036)
  | 1965 | 2709 -> One (r1037)
  | 1964 | 2708 -> One (r1038)
  | 1963 | 2707 -> One (r1039)
  | 1962 | 2706 -> One (r1040)
  | 1961 | 2705 -> One (r1041)
  | 1960 | 2704 -> One (r1042)
  | 1983 | 2722 -> One (r1043)
  | 1982 | 2721 -> One (r1044)
  | 1981 | 2720 -> One (r1045)
  | 1975 | 2719 -> One (r1046)
  | 2718 -> One (r1047)
  | 2717 -> One (r1048)
  | 2716 -> One (r1049)
  | 2715 -> One (r1050)
  | 1986 | 2725 -> One (r1051)
  | 1985 | 2724 -> One (r1052)
  | 1984 | 2723 -> One (r1053)
  | 2069 | 3025 -> One (r1054)
  | 2068 | 3024 -> One (r1055)
  | 2067 | 3023 -> One (r1056)
  | 2066 | 3022 -> One (r1057)
  | 1976 | 2580 -> One (r1058)
  | 1980 | 2584 -> One (r1059)
  | 1979 | 2583 -> One (r1060)
  | 2462 -> One (r1061)
  | 1997 | 2956 -> One (r1062)
  | 1996 | 2955 -> One (r1063)
  | 1995 | 2954 -> One (r1064)
  | 1994 | 2953 -> One (r1065)
  | 2011 | 2970 -> One (r1066)
  | 2010 | 2969 -> One (r1067)
  | 2009 | 2968 -> One (r1068)
  | 2008 | 2967 -> One (r1069)
  | 2461 | 3070 -> One (r1070)
  | 2020 | 2976 -> One (r1071)
  | 2030 | 2986 -> One (r1072)
  | 2029 | 2985 -> One (r1073)
  | 2028 | 2984 -> One (r1074)
  | 2027 | 2983 -> One (r1075)
  | 2021 | 2977 -> One (r1076)
  | 2026 | 2982 -> One (r1077)
  | 2025 | 2981 -> One (r1078)
  | 2024 | 2980 -> One (r1079)
  | 2023 | 2979 -> One (r1080)
  | 2022 | 2978 -> One (r1081)
  | 2039 | 2995 -> One (r1082)
  | 2038 | 2994 -> One (r1083)
  | 2043 | 2999 -> One (r1087)
  | 2042 | 2998 -> One (r1088)
  | 2045 | 3001 -> One (r1090)
  | 2044 | 3000 -> One (r1091)
  | 2990 -> One (r1092)
  | 2989 -> One (r1093)
  | 2988 -> One (r1094)
  | 2987 -> One (r1095)
  | 2037 | 2993 -> One (r1096)
  | 2036 | 2992 -> One (r1097)
  | 2035 | 2991 -> One (r1098)
  | 2041 | 2997 -> One (r1099)
  | 2040 | 2996 -> One (r1100)
  | 2047 | 3003 -> One (r1101)
  | 2046 | 3002 -> One (r1102)
  | 2060 | 3016 -> One (r1103)
  | 2052 | 3008 -> One (r1104)
  | 2051 | 3007 -> One (r1105)
  | 2050 | 3006 -> One (r1106)
  | 2049 | 3005 -> One (r1107)
  | 2048 | 3004 -> One (r1108)
  | 2059 | 3015 -> One (r1109)
  | 2058 | 3014 -> One (r1110)
  | 2057 | 3013 -> One (r1111)
  | 2056 | 3012 -> One (r1112)
  | 2055 | 3011 -> One (r1113)
  | 2054 | 3010 -> One (r1114)
  | 2053 | 3009 -> One (r1115)
  | 2065 | 3021 -> One (r1116)
  | 2064 | 3020 -> One (r1117)
  | 2063 | 3019 -> One (r1118)
  | 2062 | 3018 -> One (r1119)
  | 2061 | 3017 -> One (r1120)
  | 2203 | 2323 -> One (r1121)
  | 2218 | 2332 -> One (r1123)
  | 2255 | 2350 -> One (r1125)
  | 2435 | 3044 -> One (r1127)
  | 2425 | 3034 -> One (r1128)
  | 2424 | 3033 -> One (r1129)
  | 2423 | 3032 -> One (r1130)
  | 2422 | 3031 -> One (r1131)
  | 2421 | 3030 -> One (r1132)
  | 2420 | 3029 -> One (r1133)
  | 2419 | 3028 -> One (r1134)
  | 2418 | 3027 -> One (r1135)
  | 2070 | 3026 -> One (r1136)
  | 2417 | 2745 -> One (r1137)
  | 2407 | 2735 -> One (r1138)
  | 2406 | 2734 -> One (r1139)
  | 2081 | 2733 -> One (r1140)
  | 2080 | 2732 -> One (r1141)
  | 2079 | 2731 -> One (r1142)
  | 2075 | 2730 -> One (r1143)
  | 2073 | 2729 -> One (r1144)
  | 2072 | 2728 -> One (r1145)
  | 2071 | 2727 -> One (r1146)
  | 2078 -> One (r1147)
  | 2077 -> One (r1148)
  | 2233 | 2405 -> One (r1149)
  | 2231 | 2403 -> One (r1150)
  | 2087 | 2200 -> One (r1151)
  | 2083 | 2199 -> One (r1152)
  | 2082 | 2198 -> One (r1153)
  | 2086 -> One (r1154)
  | 2085 -> One (r1155)
  | 2098 -> One (r1156)
  | 2097 -> One (r1157)
  | 2096 -> One (r1158)
  | 2095 -> One (r1159)
  | 2094 -> One (r1160)
  | 2089 -> One (r1161)
  | 2109 -> One (r1162)
  | 2108 -> One (r1163)
  | 2107 -> One (r1164)
  | 2106 -> One (r1165)
  | 2105 -> One (r1166)
  | 2100 -> One (r1167)
  | 2178 | 2300 -> One (r1168)
  | 2176 | 2298 -> One (r1170)
  | 2311 | 2752 -> One (r1172)
  | 2118 | 2751 -> One (r1173)
  | 2368 | 2769 -> One (r1175)
  | 2359 | 2760 -> One (r1176)
  | 2358 | 2759 -> One (r1177)
  | 2117 | 2750 -> One (r1178)
  | 2116 | 2749 -> One (r1179)
  | 2115 | 2748 -> One (r1180)
  | 2114 | 2747 -> One (r1181)
  | 2113 | 2746 -> One (r1182)
  | 2726 -> One (r1183)
  | 2140 | 2271 -> One (r1184)
  | 2139 | 2270 -> One (r1185)
  | 2121 | 2131 -> One (r1186)
  | 2120 | 2130 -> One (r1187)
  | 2119 | 2129 -> One (r1188)
  | 2125 -> One (r1189)
  | 2124 -> One (r1190)
  | 2123 -> One (r1191)
  | 2269 -> One (r1192)
  | 2268 -> One (r1193)
  | 2267 -> One (r1194)
  | 2266 -> One (r1195)
  | 2265 -> One (r1196)
  | 2264 -> One (r1197)
  | 2261 -> One (r1198)
  | 2128 -> One (r1199)
  | 2136 -> One (r1200)
  | 2135 -> One (r1201)
  | 2134 -> One (r1202)
  | 2138 -> One (r1204)
  | 2137 -> One (r1205)
  | 2133 -> One (r1206)
  | 2143 -> One (r1207)
  | 2146 -> One (r1208)
  | 2196 | 2276 -> One (r1209)
  | 2194 | 2274 -> One (r1210)
  | 2149 | 2273 -> One (r1211)
  | 2189 | 2310 -> One (r1212)
  | 2188 | 2309 -> One (r1213)
  | 2187 | 2308 -> One (r1214)
  | 2186 | 2307 -> One (r1215)
  | 2157 | 2282 -> One (r1216)
  | 2150 | 2281 -> One (r1217)
  | 2156 -> One (r1218)
  | 2155 -> One (r1219)
  | 2154 -> One (r1220)
  | 2153 -> One (r1221)
  | 2152 -> One (r1222)
  | 2185 | 2306 -> One (r1223)
  | 2161 | 2286 -> One (r1224)
  | 2160 | 2285 -> One (r1225)
  | 2159 | 2284 -> One (r1226)
  | 2158 | 2283 -> One (r1227)
  | 2168 | 2244 -> One (r1228)
  | 2165 | 2290 -> One (r1230)
  | 2164 | 2289 -> One (r1231)
  | 2163 | 2288 -> One (r1232)
  | 2162 | 2287 -> One (r1233)
  | 2167 -> One (r1234)
  | 2182 | 2303 -> One (r1235)
  | 2172 | 2294 -> One (r1236)
  | 2171 | 2293 -> One (r1237)
  | 2184 | 2305 -> One (r1239)
  | 2170 | 2292 -> One (r1240)
  | 2169 | 2291 -> One (r1241)
  | 2179 | 2301 -> One (r1242)
  | 2174 | 2296 -> One (r1243)
  | 2173 | 2295 -> One (r1244)
  | 2193 -> One (r1245)
  | 2192 -> One (r1246)
  | 2191 -> One (r1247)
  | 2260 | 2280 -> One (r1248)
  | 2258 | 2278 -> One (r1249)
  | 2197 | 2277 -> One (r1250)
  | 2226 -> One (r1251)
  | 2225 -> One (r1252)
  | 2202 -> One (r1253)
  | 2224 | 2338 -> One (r1254)
  | 2223 | 2337 -> One (r1255)
  | 2222 | 2336 -> One (r1256)
  | 2221 | 2335 -> One (r1257)
  | 2205 | 2318 -> One (r1258)
  | 2204 | 2317 -> One (r1259)
  | 2219 | 2333 -> One (r1260)
  | 2209 | 2322 -> One (r1261)
  | 2208 | 2321 -> One (r1262)
  | 2207 | 2320 -> One (r1263)
  | 2206 | 2319 -> One (r1264)
  | 2216 | 2330 -> One (r1265)
  | 2212 | 2326 -> One (r1266)
  | 2211 | 2325 -> One (r1267)
  | 2210 | 2243 | 2324 | 2339 -> One (r1268)
  | 2215 | 2252 | 2329 | 2347 -> One (r1269)
  | 2214 | 2251 | 2328 | 2346 -> One (r1270)
  | 2213 | 2250 | 2327 | 2345 -> One (r1271)
  | 2230 -> One (r1272)
  | 2229 -> One (r1273)
  | 2228 -> One (r1274)
  | 2235 -> One (r1275)
  | 2238 -> One (r1276)
  | 2257 | 2352 -> One (r1277)
  | 2242 | 2316 -> One (r1278)
  | 2241 | 2315 -> One (r1279)
  | 2240 | 2314 -> One (r1280)
  | 2239 | 2313 -> One (r1281)
  | 2256 | 2351 -> One (r1282)
  | 2246 | 2341 -> One (r1283)
  | 2245 | 2340 -> One (r1284)
  | 2254 | 2349 -> One (r1285)
  | 2249 | 2344 -> One (r1286)
  | 2248 | 2343 -> One (r1287)
  | 2247 | 2342 -> One (r1288)
  | 2263 -> One (r1289)
  | 2355 | 2756 -> One (r1290)
  | 2354 | 2755 -> One (r1291)
  | 2353 | 2754 -> One (r1292)
  | 2312 | 2753 -> One (r1293)
  | 2357 | 2758 -> One (r1294)
  | 2356 | 2757 -> One (r1295)
  | 2367 | 2768 -> One (r1296)
  | 2366 | 2767 -> One (r1297)
  | 2365 | 2766 -> One (r1298)
  | 2364 | 2765 -> One (r1299)
  | 2363 | 2764 -> One (r1300)
  | 2362 | 2763 -> One (r1301)
  | 2361 | 2762 -> One (r1302)
  | 2360 | 2761 -> One (r1303)
  | 2378 | 2785 -> One (r1304)
  | 2370 | 2771 -> One (r1305)
  | 2381 | 2789 -> One (r1306)
  | 2380 | 2788 -> One (r1307)
  | 2391 | 2799 -> One (r1308)
  | 2382 | 2790 -> One (r1309)
  | 2390 | 2798 -> One (r1310)
  | 2389 | 2797 -> One (r1311)
  | 2388 | 2796 -> One (r1312)
  | 2387 | 2795 -> One (r1313)
  | 2386 | 2794 -> One (r1314)
  | 2385 | 2793 -> One (r1315)
  | 2384 | 2792 -> One (r1316)
  | 2383 | 2791 -> One (r1317)
  | 2397 | 2813 -> One (r1318)
  | 2396 | 2812 -> One (r1319)
  | 2395 | 2811 -> One (r1320)
  | 2416 | 2744 -> One (r1321)
  | 2415 | 2743 -> One (r1322)
  | 2414 | 2742 -> One (r1323)
  | 2413 | 2741 -> One (r1324)
  | 2412 | 2740 -> One (r1325)
  | 2411 | 2739 -> One (r1326)
  | 2410 | 2738 -> One (r1327)
  | 2409 | 2737 -> One (r1328)
  | 2408 | 2736 -> One (r1329)
  | 2434 | 3043 -> One (r1330)
  | 2433 | 3042 -> One (r1331)
  | 2432 | 3041 -> One (r1332)
  | 2431 | 3040 -> One (r1333)
  | 2430 | 3039 -> One (r1334)
  | 2429 | 3038 -> One (r1335)
  | 2428 | 3037 -> One (r1336)
  | 2427 | 3036 -> One (r1337)
  | 2426 | 3035 -> One (r1338)
  | 2443 | 3052 -> One (r1339)
  | 2437 | 3046 -> One (r1340)
  | 2445 | 3054 -> One (r1341)
  | 2444 | 3053 -> One (r1342)
  | 2447 | 3056 -> One (r1343)
  | 2446 | 3055 -> One (r1344)
  | 2458 | 3067 -> One (r1345)
  | 2448 | 3057 -> One (r1346)
  | 2457 | 3066 -> One (r1347)
  | 2456 | 3065 -> One (r1348)
  | 2455 | 3064 -> One (r1349)
  | 2454 | 3063 -> One (r1350)
  | 2453 | 3062 -> One (r1351)
  | 2452 | 3061 -> One (r1352)
  | 2451 | 3060 -> One (r1353)
  | 2450 | 3059 -> One (r1354)
  | 2449 | 3058 -> One (r1355)
  | 2483 | 3121 -> One (r1356)
  | 2482 | 3120 -> One (r1357)
  | 2481 | 3119 -> One (r1358)
  | 2480 | 3118 -> One (r1359)
  | 2485 | 3123 -> One (r1360)
  | 2484 | 3122 -> One (r1361)
  | 2487 | 3125 -> One (r1362)
  | 2486 | 3124 -> One (r1363)
  | 2492 | 3128 -> One (r1364)
  | 2490 | 3126 -> One (r1365)
  | 2494 | 3130 -> One (r1366)
  | 2493 | 3129 -> One (r1367)
  | 2499 | 2849 -> One (r1368)
  | 2498 | 2848 -> One (r1369)
  | 2501 | 2851 -> One (r1370)
  | 2500 | 2850 -> One (r1371)
  | 2503 | 2853 -> One (r1372)
  | 2502 | 2852 -> One (r1373)
  | 2505 | 2855 -> One (r1374)
  | 2504 | 2854 -> One (r1375)
  | 2508 | 2858 -> One (r1376)
  | 2507 | 2857 -> One (r1377)
  | 2506 | 2856 -> One (r1378)
  | 2523 | 2535 -> One (r1379)
  | 2521 | 2533 -> One (r1380)
  | 2520 | 2532 -> One (r1381)
  | 2549 | 2661 -> One (r1382)
  | 2542 | 2654 -> One (r1383)
  | 2541 | 2653 -> One (r1384)
  | 2540 | 2652 -> One (r1385)
  | 2539 | 2651 -> One (r1386)
  | 2548 | 2660 -> One (r1387)
  | 2547 | 2659 -> One (r1388)
  | 2546 | 2658 -> One (r1389)
  | 2545 | 2657 -> One (r1390)
  | 2544 | 2656 -> One (r1391)
  | 2543 | 2655 -> One (r1392)
  | 2554 -> One (r1393)
  | 2586 | 2879 -> One (r1394)
  | 2585 | 2878 -> One (r1395)
  | 2579 | 2877 -> One (r1396)
  | 2605 | 2891 -> One (r1397)
  | 2604 | 2890 -> One (r1398)
  | 2603 | 2889 -> One (r1399)
  | 2804 | 2867 -> One (r1400)
  | 2803 | 2866 -> One (r1401)
  | 2802 | 2865 -> One (r1402)
  | 2840 | 3138 -> One (r1403)
  | 2839 | 3137 -> One (r1404)
  | 2842 | 3140 -> One (r1405)
  | 2841 | 3139 -> One (r1406)
  | 2844 | 3142 -> One (r1407)
  | 2843 | 3141 -> One (r1408)
  | 2904 -> One (r1409)
  | 3155 -> One (r1410)
  | 3159 -> One (r1411)
  | 3164 -> One (r1412)
  | 3167 -> One (r1413)
  | 3171 -> One (r1414)
  | 3175 -> One (r1415)
  | 3186 -> One (r1416)
  | 3188 -> One (r1417)
  | 3191 -> One (r1418)
  | 3190 -> One (r1419)
  | 3193 -> One (r1420)
  | 3203 -> One (r1421)
  | 3199 -> One (r1422)
  | 3198 -> One (r1423)
  | 3202 -> One (r1424)
  | 3201 -> One (r1425)
  | 3208 -> One (r1426)
  | 3207 -> One (r1427)
  | 3206 -> One (r1428)
  | 3210 -> One (r1429)
  | 887 -> Select (function
    | -1 -> [R 102]
    | _ -> r539)
  | 1051 -> Select (function
    | -1 -> [R 102]
    | _ -> r716)
  | 2031 -> Select (function
    | -1 -> R 189 :: r1086
    | _ -> r1095)
  | 919 -> Select (function
    | -1 -> [R 681]
    | _ -> r553)
  | 917 -> Select (function
    | -1 -> [R 682]
    | _ -> r551)
  | 248 -> Select (function
    | -1 -> [R 808]
    | _ -> r135)
  | 2112 -> Select (function
    | -1 -> S (T T_TYPE) :: r1146
    | _ -> r1183)
  | 344 -> Select (function
    | -1 -> S (T T_LPAREN) :: r288
    | _ -> r291)
  | 312 -> Select (function
    | 1231 | 2667 -> r96
    | _ -> r218)
  | 311 -> Select (function
    | 1231 | 2667 -> r97
    | _ -> r219)
  | 305 -> Select (function
    | -1 -> r161
    | _ -> r210)
  | 2893 -> Select (function
    | -1 -> r238
    | _ -> r161)
  | 320 -> Select (function
    | -1 -> r253
    | _ -> r161)
  | 2007 -> Select (function
    | -1 -> r238
    | _ -> r161)
  | 1993 -> Select (function
    | -1 -> r253
    | _ -> r161)
  | 2006 -> Select (function
    | -1 -> r239
    | _ -> r176)
  | 1990 -> Select (function
    | -1 -> r240
    | _ -> r177)
  | 1989 -> Select (function
    | -1 -> r241
    | _ -> r178)
  | 1988 -> Select (function
    | -1 -> r242
    | _ -> r179)
  | 1992 -> Select (function
    | -1 -> r254
    | _ -> r185)
  | 1991 -> Select (function
    | -1 -> r255
    | _ -> r186)
  | 2892 -> Select (function
    | -1 -> r239
    | _ -> r231)
  | 317 -> Select (function
    | -1 -> r240
    | _ -> r232)
  | 316 -> Select (function
    | -1 -> r241
    | _ -> r233)
  | 315 -> Select (function
    | -1 -> r242
    | _ -> r234)
  | 319 -> Select (function
    | -1 -> r254
    | _ -> r248)
  | 318 -> Select (function
    | -1 -> r255
    | _ -> r249)
  | 543 -> Select (function
    | -1 -> r424
    | _ -> r429)
  | 957 -> Select (function
    | -1 -> r424
    | _ -> r605)
  | 946 -> Select (function
    | 60 | 152 | 314 | 494 | 541 | 547 | 1950 | 1952 | 1954 | 2111 -> r594
    | _ -> r588)
  | 2693 -> Select (function
    | 2618 -> r588
    | _ -> r594)
  | 1974 -> Select (function
    | -1 -> r1054
    | _ -> r1047)
  | 1973 -> Select (function
    | -1 -> r1055
    | _ -> r1048)
  | 1972 -> Select (function
    | -1 -> r1056
    | _ -> r1049)
  | 1971 -> Select (function
    | -1 -> r1057
    | _ -> r1050)
  | 2034 -> Select (function
    | -1 -> r1084
    | _ -> r1092)
  | 2033 -> Select (function
    | -1 -> r1085
    | _ -> r1093)
  | 2032 -> Select (function
    | -1 -> r1086
    | _ -> r1094)
  | _ -> raise Not_found
