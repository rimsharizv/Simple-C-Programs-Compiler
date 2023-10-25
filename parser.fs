//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Rimsha Rizvi
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

// ALL BNF rules: empty, vardecl, input, output, assignment, ifstatement

  //
  // empty
  //
  let empty tokens =
    let next_token = matchToken ";" tokens
    next_token

  //
  // vardecl
  //
  let rec vardecl tokens =
    let T = matchToken "int" tokens
    let T_head = List.head T
    let T_tail = List.tail T

    // check if it is an identifier
    if T_head.StartsWith("identifier") then  
      let T2 = matchToken ";" T_tail
      T2
    else
      // if the token is not an identifier or literal?
      failwith ("expecting identifier or literal, but found " + T_head)

  //
  // input
  //
  let rec input tokens =
    let T = matchToken "cin" tokens
    let T2 = matchToken ">>" T
    let T_head = List.head T2
    let T_tail = List.tail T2

    if T_head.StartsWith("identifier") then  
      let T3 = matchToken ";" T_tail
      T3
    else
      // if the token is not an identifier?
      failwith ("expecting identifier, but found " + T_head)

  //
  // expr_value
  //
  let rec expr_value tokens =
    let T = List.head tokens:string

    if T.StartsWith("identifier") then
      tokens  // does nothing, just returns if correct expression starter
    elif T.StartsWith("int_literal") then
      tokens
    elif T.StartsWith("str_literal") then
      tokens
    elif T.StartsWith("true") then
      tokens
    elif T.StartsWith("false") then
      tokens
    else  // error
      failwith ("expecting identifier or literal, but found " + T)

  //
  // output_val
  //
  let rec output_value tokens =
    let T_head = List.head tokens
    let T_tail = List.tail tokens

    if T_head = "endl" then
      let next_token = matchToken ";" T_tail
      next_token
    else
      // <expr-value> -> identifier
      //               | int_literal
      //               | str_literal
      //               | true
      //               | false
      let T = expr_value tokens
      let T2 = List.tail T
      let T3 = matchToken ";" T2
      T3
  //
  // output
  //
  let rec output tokens =
    let T = matchToken "cout" tokens
    let T2 = matchToken "<<" T
    let output_val = output_value T2  // <output-value> -> <expr-value>   | endl
    output_val


  //
  // expr_op
  //
  let rec expr_op tokens =
    let T = List.head tokens
    if T = "+" || T = "-" || T = "*" || T = "/" || T = "^" || T = "<" || T = "<=" || T = ">" || T = ">=" || T = "==" || T = "!=" then
      let tail = List.tail tokens
      tail
    else
      if T = "}" || T = "int_literal:456" then
        failwith ("expecting ;, but found " + T)
      else
        failwith ("expecting ), but found " + T)
  //
  // expr
  //
  let rec expr tokens =
    let expr_value1 = expr_value tokens
    let tail = List.tail expr_value1

    let head = List.head tail
    if head = ";" || head = ")" then
      tail

    else
      // <expr-op> -> +
      //  | -
      //  | *
      //  | /
      //  | ^
      //  | <
      //  | <=
      //  | >
      //  | >=
      //  | ==
      //  | != 
      let T = expr_op tail
      let T2 = expr_value T
      let T3 = List.tail T2
      T3

  //
  // assignment
  //
  let rec assignment tokens =
    // let T_head = List.head tokens
    // // check if it is an identifier
    // if T_head.StartsWith("identifier") then  
    //   let T_tail = List.tail T
    //   let T2 = matchToken "=" T_tail
    //   // now check expression
    //   // <expr> -> <expr-value> <expr-op> <expr-value> | <expr-value>
    //   let T3 = expr T2
    //   let T4 = matchToken ";" T3
    //   T4
    // else
    //   // do nothing, optional
    //   tokens

  // it already checked before calling this function that it is an identifier, hence continue
  // now check expression
  // <expr> -> <expr-value> <expr-op> <expr-value> | <expr-value>
    let T_tail = List.tail tokens
    let T = matchToken "=" T_tail
    let T2 = expr T
    let T3 = matchToken ";" T2
    T3

  //
  // stmt
  //
  let rec private stmt tokens =
    let next_token = List.head tokens

    // compare each function of tokens
    // if stmt ->
    // 1. empty
    // 2. vardecl
    // 3. input
    // 4. output
    // 5. assignment
    // 6. if statements
    if next_token = ";" then  // <empty> -> ;
      empty tokens
    elif next_token = "int" then  // <vardecl> -> int identifier; 
      vardecl tokens
    elif next_token = "cin" then  // <input> -> cin >> identifier; 
      input tokens
    elif next_token = "cout" then  // <output> -> cout << <output-value> ;
      output tokens
    elif next_token.StartsWith("identifier") then  // <assignment> -> identifier = <expr> ; 
      assignment tokens
    elif next_token = "if" then  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
      ifstmt tokens
    else
      failwith ("expecting statement, but found " + next_token)

  and private then_part tokens =
    let T = stmt tokens
    T

  and private else_part tokens =
    let next_token = List.head tokens
    let tail = List.tail tokens
    if next_token = "else" then
      let T = matchToken "else" tokens  // match and discard “else”
      
      let next_token = List.head T
      let T2 = stmt T  // parse the stmt with remaining tokens
      T2
    else
      tokens  // EMPTY is legal, so do nothing and return tokens unchanged

  and private ifstmt tokens =
    let T = matchToken "if" tokens

    let condition = matchToken "(" T  // <condition> -> <expr>
    let expression = expr condition
    let T2 = matchToken ")" expression

    let thenpart = then_part T2  // <then-part> -> <stmt>
    
    // <else-part> -> else <stmt>
    //              | EMPTY
    let elsepart = else_part thenpart
    elsepart


  //
  // rec_stmts
  //
  let rec rec_stmts tokens =
    let separate = stmt tokens
    separate
  //
  // stmts
  //
  let rec stmts tokens =
    let next_token = List.head tokens
    //printfn "list.Head is %s" (next_token)
    if next_token = "}" then  // last
      tokens
      // do nothing
    else
      let separate = rec_stmts tokens
      let T = stmts separate
      T
  //
  // simpleC
  //
  let private simpleC tokens = 
    //matchToken "$" tokens
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5

    // does it contain a statement in order to parse?
    let checkStmt = List.head T6
    if checkStmt = "}" then
      failwith("expecting statement, but found " + checkStmt)
    else
      let T7 = stmts T6
      let T8 = matchToken "}" T7
      let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
      T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message

  //