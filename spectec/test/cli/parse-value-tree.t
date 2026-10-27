parse -t prints each parsed value as a tree, with the source region of every
node. Atoms with no source text, such as _ID, print (no region).

  $ SPEC_IMPTY=../../specs/impty/recursion/spec.spectec
  $ CORPUS_IMPTY=../../testdata/interp/impty
  $ SPEC_DIR_P4=../../specs/p4
  $ CORPUS_P4=../../testdata/interp/p4/p4c/p4_16_samples

impty:

  $ spectec impty parse -t --spec $SPEC_IMPTY -p $CORPUS_IMPTY/base/loop.imp --color never 2>/dev/null
  command % ; %  2:1-7:4
  ├─command % ; %  2:1-3:12
  │ ├─command % % = %  2:1-2:10
  │ │ ├─type INT  2:1-2:4
  │ │ │ └─atom INT  2:1-2:4
  │ │ ├─id _ID %  2:5-2:6
  │ │ │ ├─atom _ID  (no region)
  │ │ │ └─i  2:5-2:6
  │ │ ├─atom =  2:7-2:8
  │ │ └─literal _NUM %  2:9-2:10
  │ │   ├─atom _NUM  (no region)
  │ │   └─0  2:9-2:10
  │ ├─atom ;  2:10-2:11
  │ └─command % % = %  3:1-3:12
  │   ├─type INT  3:1-3:4
  │   │ └─atom INT  3:1-3:4
  │   ├─id _ID %  3:5-3:8
  │   │ ├─atom _ID  (no region)
  │   │ └─sum  3:5-3:8
  │   ├─atom =  3:9-3:10
  │   └─literal _NUM %  3:11-3:12
  │     ├─atom _NUM  (no region)
  │     └─0  3:11-3:12
  ├─atom ;  3:12-3:13
  └─command WHILE % DO % END  4:1-7:4
    ├─atom WHILE  4:1-4:6
    ├─expr % <= %  4:7-4:14
    │ ├─id _ID %  4:7-4:8
    │ │ ├─atom _ID  (no region)
    │ │ └─i  4:7-4:8
    │ ├─atom <=  4:9-4:11
    │ └─literal _NUM %  4:12-4:14
    │   ├─atom _NUM  (no region)
    │   └─10  4:12-4:14
    ├─atom DO  4:15-4:17
    ├─command % ; %  5:3-6:12
    │ ├─command % = %  5:3-5:16
    │ │ ├─id _ID %  5:3-5:6
    │ │ │ ├─atom _ID  (no region)
    │ │ │ └─sum  5:3-5:6
    │ │ ├─atom =  5:7-5:8
    │ │ └─expr % + %  5:9-5:16
    │ │   ├─id _ID %  5:9-5:12
    │ │   │ ├─atom _ID  (no region)
    │ │   │ └─sum  5:9-5:12
    │ │   ├─atom +  5:13-5:14
    │ │   └─id _ID %  5:15-5:16
    │ │     ├─atom _ID  (no region)
    │ │     └─i  5:15-5:16
    │ ├─atom ;  5:16-5:17
    │ └─command % = %  6:3-6:12
    │   ├─id _ID %  6:3-6:4
    │   │ ├─atom _ID  (no region)
    │   │ └─i  6:3-6:4
    │   ├─atom =  6:5-6:6
    │   └─expr % + %  6:7-6:12
    │     ├─id _ID %  6:7-6:8
    │     │ ├─atom _ID  (no region)
    │     │ └─i  6:7-6:8
    │     ├─atom +  6:9-6:10
    │     └─literal _NUM %  6:11-6:12
    │       ├─atom _NUM  (no region)
    │       └─1  6:11-6:12
    └─atom END  7:1-7:4
  



P4:

  $ spectec p4 parse -t --spec-dir $SPEC_DIR_P4 -p $CORPUS_P4/stack-init.p4 --color never 2>/dev/null
  p4program % %  1:1-17:15
  ├─p4program % %  1:1-15:19
  │ ├─p4program % %  1:1-14:26
  │ │ ├─p4program % %  1:1-12:2
  │ │ │ ├─p4program % %  1:1-4:2
  │ │ │ │ ├─p4program _EMPTY  1:1
  │ │ │ │ │ └─atom _EMPTY  (no region)
  │ │ │ │ └─headerTypeDeclaration % HEADER % % {%}  1:1-4:2
  │ │ │ │   ├─annotationList _EMPTY  1:1
  │ │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │ │   ├─atom HEADER  1:1-1:7
  │ │ │ │   ├─identifier _ID %  1:8-1:9
  │ │ │ │   │ ├─atom _ID  (no region)
  │ │ │ │   │ └─H  1:8-1:9
  │ │ │ │   ├─typeParameterListOpt <%>  1:9-1:12
  │ │ │ │   │ ├─atom <  1:9-1:10
  │ │ │ │   │ ├─identifier _ID %  1:10-1:11
  │ │ │ │   │ │ ├─atom _ID  (no region)
  │ │ │ │   │ │ └─T  1:10-1:11
  │ │ │ │   │ └─atom >  1:11-1:12
  │ │ │ │   ├─atom {  1:13-1:14
  │ │ │ │   ├─typeFieldList % %  2:5-3:9
  │ │ │ │   │ ├─typeFieldList % %  2:5-2:15
  │ │ │ │   │ │ ├─typeFieldList _EMPTY  1:14
  │ │ │ │   │ │ │ └─atom _EMPTY  (no region)
  │ │ │ │   │ │ └─typeField % % % ;  2:5-2:15
  │ │ │ │   │ │   ├─annotationList _EMPTY  1:14
  │ │ │ │   │ │   │ └─atom _EMPTY  (no region)
  │ │ │ │   │ │   ├─baseType BIT <%>  2:5-2:12
  │ │ │ │   │ │   │ ├─atom BIT  2:5-2:8
  │ │ │ │   │ │   │ ├─atom <  2:8-2:9
  │ │ │ │   │ │   │ ├─32  2:9-2:11
  │ │ │ │   │ │   │ └─atom >  2:11-2:12
  │ │ │ │   │ │   ├─identifier _ID %  2:13-2:14
  │ │ │ │   │ │   │ ├─atom _ID  (no region)
  │ │ │ │   │ │   │ └─b  2:13-2:14
  │ │ │ │   │ │   └─atom ;  2:14-2:15
  │ │ │ │   │ └─typeField % % % ;  3:5-3:9
  │ │ │ │   │   ├─annotationList _EMPTY  2:15
  │ │ │ │   │   │ └─atom _EMPTY  (no region)
  │ │ │ │   │   ├─typeIdentifier _TID %  3:5-3:6
  │ │ │ │   │   │ ├─atom _TID  (no region)
  │ │ │ │   │   │ └─T  3:5-3:6
  │ │ │ │   │   ├─identifier _ID %  3:7-3:8
  │ │ │ │   │   │ ├─atom _ID  (no region)
  │ │ │ │   │   │ └─t  3:7-3:8
  │ │ │ │   │   └─atom ;  3:8-3:9
  │ │ │ │   └─atom }  4:1-4:2
  │ │ │ └─controlDeclaration % CONTROL % % (%) % {% APPLY %}  6:1-12:2
  │ │ │   ├─annotationList _EMPTY  4:2
  │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │   ├─atom CONTROL  6:1-6:8
  │ │ │   ├─identifier _ID %  6:9-6:10
  │ │ │   │ ├─atom _ID  (no region)
  │ │ │   │ └─c  6:9-6:10
  │ │ │   ├─typeParameterListOpt _EMPTY  6:10
  │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │   ├─atom (  6:10-6:11
  │ │ │   ├─parameter % % % % %  6:11-6:24
  │ │ │   │ ├─annotationList _EMPTY  6:11
  │ │ │   │ │ └─atom _EMPTY  (no region)
  │ │ │   │ ├─direction OUT  6:11-6:14
  │ │ │   │ │ └─atom OUT  6:11-6:14
  │ │ │   │ ├─baseType BIT <%>  6:15-6:22
  │ │ │   │ │ ├─atom BIT  6:15-6:18
  │ │ │   │ │ ├─atom <  6:18-6:19
  │ │ │   │ │ ├─32  6:19-6:21
  │ │ │   │ │ └─atom >  6:21-6:22
  │ │ │   │ ├─identifier _ID %  6:23-6:24
  │ │ │   │ │ ├─atom _ID  (no region)
  │ │ │   │ │ └─r  6:23-6:24
  │ │ │   │ └─initializerOpt _EMPTY  6:24
  │ │ │   │   └─atom _EMPTY  (no region)
  │ │ │   ├─atom )  6:24-6:25
  │ │ │   ├─constructorParameterListOpt _EMPTY  6:25
  │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │   ├─atom {  6:26-6:27
  │ │ │   ├─controlLocalDeclarationList _EMPTY  6:27
  │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │   ├─atom APPLY  7:5-7:10
  │ │ │   ├─blockStatement % {%}  7:11-11:6
  │ │ │   │ ├─annotationList _EMPTY  7:10
  │ │ │   │ │ └─atom _EMPTY  (no region)
  │ │ │   │ ├─atom {  7:11-7:12
  │ │ │   │ ├─blockElementStatementList % %  8:9-10:56
  │ │ │   │ │ ├─blockElementStatementList % %  8:9-9:64
  │ │ │   │ │ │ ├─blockElementStatementList % %  8:9-8:25
  │ │ │   │ │ │ │ ├─blockElementStatementList _EMPTY  7:12
  │ │ │   │ │ │ │ │ └─atom _EMPTY  (no region)
  │ │ │   │ │ │ │ └─variableDeclaration % % % % ;  8:9-8:25
  │ │ │   │ │ │ │   ├─annotationList _EMPTY  7:12
  │ │ │   │ │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │   │ │ │ │   ├─headerStackType % [%]  8:9-8:22
  │ │ │   │ │ │ │   │ ├─specializedType % <%>  8:9-8:19
  │ │ │   │ │ │ │   │ │ ├─typeIdentifier _TID %  8:9-8:10
  │ │ │   │ │ │ │   │ │ │ ├─atom _TID  (no region)
  │ │ │   │ │ │ │   │ │ │ └─H  8:9-8:10
  │ │ │   │ │ │ │   │ │ ├─atom <  8:10-8:11
  │ │ │   │ │ │ │   │ │ ├─baseType BIT <%>  8:11-8:18
  │ │ │   │ │ │ │   │ │ │ ├─atom BIT  8:11-8:14
  │ │ │   │ │ │ │   │ │ │ ├─atom <  8:14-8:15
  │ │ │   │ │ │ │   │ │ │ ├─32  8:15-8:17
  │ │ │   │ │ │ │   │ │ │ └─atom >  8:17-8:18
  │ │ │   │ │ │ │   │ │ └─atom >  8:18-8:19
  │ │ │   │ │ │ │   │ ├─atom [  8:19-8:20
  │ │ │   │ │ │ │   │ ├─number D %  8:20-8:21
  │ │ │   │ │ │ │   │ │ ├─atom D  (no region)
  │ │ │   │ │ │ │   │ │ └─3  8:20-8:21
  │ │ │   │ │ │ │   │ └─atom ]  8:21-8:22
  │ │ │   │ │ │ │   ├─identifier _ID %  8:23-8:24
  │ │ │   │ │ │ │   │ ├─atom _ID  (no region)
  │ │ │   │ │ │ │   │ └─s  8:23-8:24
  │ │ │   │ │ │ │   ├─initializerOpt _EMPTY  8:24
  │ │ │   │ │ │ │   │ └─atom _EMPTY  (no region)
  │ │ │   │ │ │ │   └─atom ;  8:24-8:25
  │ │ │   │ │ │ └─assignmentStatement % % % ;  9:9-9:64
  │ │ │   │ │ │   ├─identifier _ID %  9:9-9:10
  │ │ │   │ │ │   │ ├─atom _ID  (no region)
  │ │ │   │ │ │   │ └─s  9:9-9:10
  │ │ │   │ │ │   ├─assignop =  9:11-9:12
  │ │ │   │ │ │   │ └─atom =  9:11-9:12
  │ │ │   │ │ │   ├─castExpression (%) %  9:13-9:63
  │ │ │   │ │ │   │ ├─atom (  9:13-9:14
  │ │ │   │ │ │   │ ├─headerStackType % [%]  9:14-9:27
  │ │ │   │ │ │   │ │ ├─specializedType % <%>  9:14-9:24
  │ │ │   │ │ │   │ │ │ ├─typeIdentifier _TID %  9:14-9:15
  │ │ │   │ │ │   │ │ │ │ ├─atom _TID  (no region)
  │ │ │   │ │ │   │ │ │ │ └─H  9:14-9:15
  │ │ │   │ │ │   │ │ │ ├─atom <  9:15-9:16
  │ │ │   │ │ │   │ │ │ ├─baseType BIT <%>  9:16-9:23
  │ │ │   │ │ │   │ │ │ │ ├─atom BIT  9:16-9:19
  │ │ │   │ │ │   │ │ │ │ ├─atom <  9:19-9:20
  │ │ │   │ │ │   │ │ │ │ ├─32  9:20-9:22
  │ │ │   │ │ │   │ │ │ │ └─atom >  9:22-9:23
  │ │ │   │ │ │   │ │ │ └─atom >  9:23-9:24
  │ │ │   │ │ │   │ │ ├─atom [  9:24-9:25
  │ │ │   │ │ │   │ │ ├─number D %  9:25-9:26
  │ │ │   │ │ │   │ │ │ ├─atom D  (no region)
  │ │ │   │ │ │   │ │ │ └─3  9:25-9:26
  │ │ │   │ │ │   │ │ └─atom ]  9:26-9:27
  │ │ │   │ │ │   │ ├─atom )  9:27-9:28
  │ │ │   │ │ │   │ └─dataExpression {% %}  9:28-9:63
  │ │ │   │ │ │   │   ├─atom {  9:28-9:29
  │ │ │   │ │ │   │   ├─expressionList % , %  9:30-9:61
  │ │ │   │ │ │   │   │ ├─expressionList % , %  9:30-9:44
  │ │ │   │ │ │   │   │ │ ├─dataExpression {% %}  9:30-9:36
  │ │ │   │ │ │   │   │ │ │ ├─atom {  9:30-9:31
  │ │ │   │ │ │   │   │ │ │ ├─expressionList % , %  9:31-9:35
  │ │ │   │ │ │   │   │ │ │ │ ├─number D %  9:31-9:32
  │ │ │   │ │ │   │   │ │ │ │ │ ├─atom D  (no region)
  │ │ │   │ │ │   │   │ │ │ │ │ └─0  9:31-9:32
  │ │ │   │ │ │   │   │ │ │ │ ├─atom ,  9:32-9:33
  │ │ │   │ │ │   │   │ │ │ │ └─number D %  9:34-9:35
  │ │ │   │ │ │   │   │ │ │ │   ├─atom D  (no region)
  │ │ │   │ │ │   │   │ │ │ │   └─1  9:34-9:35
  │ │ │   │ │ │   │   │ │ │ ├─trailingCommaOpt _EMPTY  9:35
  │ │ │   │ │ │   │   │ │ │ │ └─atom _EMPTY  (no region)
  │ │ │   │ │ │   │   │ │ │ └─atom }  9:35-9:36
  │ │ │   │ │ │   │   │ │ ├─atom ,  9:36-9:37
  │ │ │   │ │ │   │   │ │ └─dataExpression {% %}  9:38-9:44
  │ │ │   │ │ │   │   │ │   ├─atom {  9:38-9:39
  │ │ │   │ │ │   │   │ │   ├─expressionList % , %  9:39-9:43
  │ │ │   │ │ │   │   │ │   │ ├─number D %  9:39-9:40
  │ │ │   │ │ │   │   │ │   │ │ ├─atom D  (no region)
  │ │ │   │ │ │   │   │ │   │ │ └─2  9:39-9:40
  │ │ │   │ │ │   │   │ │   │ ├─atom ,  9:40-9:41
  │ │ │   │ │ │   │   │ │   │ └─number D %  9:42-9:43
  │ │ │   │ │ │   │   │ │   │   ├─atom D  (no region)
  │ │ │   │ │ │   │   │ │   │   └─3  9:42-9:43
  │ │ │   │ │ │   │   │ │   ├─trailingCommaOpt _EMPTY  9:43
  │ │ │   │ │ │   │   │ │   │ └─atom _EMPTY  (no region)
  │ │ │   │ │ │   │   │ │   └─atom }  9:43-9:44
  │ │ │   │ │ │   │   │ ├─atom ,  9:44-9:45
  │ │ │   │ │ │   │   │ └─castExpression (%) %  9:46-9:61
  │ │ │   │ │ │   │   │   ├─atom (  9:46-9:47
  │ │ │   │ │ │   │   │   ├─specializedType % <%>  9:47-9:57
  │ │ │   │ │ │   │   │   │ ├─typeIdentifier _TID %  9:47-9:48
  │ │ │   │ │ │   │   │   │ │ ├─atom _TID  (no region)
  │ │ │   │ │ │   │   │   │ │ └─H  9:47-9:48
  │ │ │   │ │ │   │   │   │ ├─atom <  9:48-9:49
  │ │ │   │ │ │   │   │   │ ├─baseType BIT <%>  9:49-9:56
  │ │ │   │ │ │   │   │   │ │ ├─atom BIT  9:49-9:52
  │ │ │   │ │ │   │   │   │ │ ├─atom <  9:52-9:53
  │ │ │   │ │ │   │   │   │ │ ├─32  9:53-9:55
  │ │ │   │ │ │   │   │   │ │ └─atom >  9:55-9:56
  │ │ │   │ │ │   │   │   │ └─atom >  9:56-9:57
  │ │ │   │ │ │   │   │   ├─atom )  9:57-9:58
  │ │ │   │ │ │   │   │   └─dataExpression {#}  9:58-9:61
  │ │ │   │ │ │   │   │     └─atom {#}  9:58-9:61
  │ │ │   │ │ │   │   ├─trailingCommaOpt _EMPTY  9:61
  │ │ │   │ │ │   │   │ └─atom _EMPTY  (no region)
  │ │ │   │ │ │   │   └─atom }  9:62-9:63
  │ │ │   │ │ │   └─atom ;  9:63-9:64
  │ │ │   │ │ └─assignmentStatement % % % ;  10:9-10:56
  │ │ │   │ │   ├─identifier _ID %  10:9-10:10
  │ │ │   │ │   │ ├─atom _ID  (no region)
  │ │ │   │ │   │ └─r  10:9-10:10
  │ │ │   │ │   ├─assignop =  10:11-10:12
  │ │ │   │ │   │ └─atom =  10:11-10:12
  │ │ │   │ │   ├─binaryExpression % % %  10:13-10:55
  │ │ │   │ │   │ ├─binaryExpression % % %  10:13-10:46
  │ │ │   │ │   │ │ ├─binaryExpression % % %  10:13-10:37
  │ │ │   │ │   │ │ │ ├─binaryExpression % % %  10:13-10:28
  │ │ │   │ │   │ │ │ │ ├─memberAccessExpression % . %  10:13-10:19
  │ │ │   │ │   │ │ │ │ │ ├─indexAccessExpression % [%]  10:13-10:17
  │ │ │   │ │   │ │ │ │ │ │ ├─identifier _ID %  10:13-10:14
  │ │ │   │ │   │ │ │ │ │ │ │ ├─atom _ID  (no region)
  │ │ │   │ │   │ │ │ │ │ │ │ └─s  10:13-10:14
  │ │ │   │ │   │ │ │ │ │ │ ├─atom [  10:14-10:15
  │ │ │   │ │   │ │ │ │ │ │ ├─number D %  10:15-10:16
  │ │ │   │ │   │ │ │ │ │ │ │ ├─atom D  (no region)
  │ │ │   │ │   │ │ │ │ │ │ │ └─0  10:15-10:16
  │ │ │   │ │   │ │ │ │ │ │ └─atom ]  10:16-10:17
  │ │ │   │ │   │ │ │ │ │ ├─atom .  10:17-10:18
  │ │ │   │ │   │ │ │ │ │ └─identifier _ID %  10:18-10:19
  │ │ │   │ │   │ │ │ │ │   ├─atom _ID  (no region)
  │ │ │   │ │   │ │ │ │ │   └─b  10:18-10:19
  │ │ │   │ │   │ │ │ │ ├─binop +  10:20-10:21
  │ │ │   │ │   │ │ │ │ │ └─atom +  10:20-10:21
  │ │ │   │ │   │ │ │ │ └─memberAccessExpression % . %  10:22-10:28
  │ │ │   │ │   │ │ │ │   ├─indexAccessExpression % [%]  10:22-10:26
  │ │ │   │ │   │ │ │ │   │ ├─identifier _ID %  10:22-10:23
  │ │ │   │ │   │ │ │ │   │ │ ├─atom _ID  (no region)
  │ │ │   │ │   │ │ │ │   │ │ └─s  10:22-10:23
  │ │ │   │ │   │ │ │ │   │ ├─atom [  10:23-10:24
  │ │ │   │ │   │ │ │ │   │ ├─number D %  10:24-10:25
  │ │ │   │ │   │ │ │ │   │ │ ├─atom D  (no region)
  │ │ │   │ │   │ │ │ │   │ │ └─0  10:24-10:25
  │ │ │   │ │   │ │ │ │   │ └─atom ]  10:25-10:26
  │ │ │   │ │   │ │ │ │   ├─atom .  10:26-10:27
  │ │ │   │ │   │ │ │ │   └─identifier _ID %  10:27-10:28
  │ │ │   │ │   │ │ │ │     ├─atom _ID  (no region)
  │ │ │   │ │   │ │ │ │     └─t  10:27-10:28
  │ │ │   │ │   │ │ │ ├─binop +  10:29-10:30
  │ │ │   │ │   │ │ │ │ └─atom +  10:29-10:30
  │ │ │   │ │   │ │ │ └─memberAccessExpression % . %  10:31-10:37
  │ │ │   │ │   │ │ │   ├─indexAccessExpression % [%]  10:31-10:35
  │ │ │   │ │   │ │ │   │ ├─identifier _ID %  10:31-10:32
  │ │ │   │ │   │ │ │   │ │ ├─atom _ID  (no region)
  │ │ │   │ │   │ │ │   │ │ └─s  10:31-10:32
  │ │ │   │ │   │ │ │   │ ├─atom [  10:32-10:33
  │ │ │   │ │   │ │ │   │ ├─number D %  10:33-10:34
  │ │ │   │ │   │ │ │   │ │ ├─atom D  (no region)
  │ │ │   │ │   │ │ │   │ │ └─1  10:33-10:34
  │ │ │   │ │   │ │ │   │ └─atom ]  10:34-10:35
  │ │ │   │ │   │ │ │   ├─atom .  10:35-10:36
  │ │ │   │ │   │ │ │   └─identifier _ID %  10:36-10:37
  │ │ │   │ │   │ │ │     ├─atom _ID  (no region)
  │ │ │   │ │   │ │ │     └─b  10:36-10:37
  │ │ │   │ │   │ │ ├─binop +  10:38-10:39
  │ │ │   │ │   │ │ │ └─atom +  10:38-10:39
  │ │ │   │ │   │ │ └─memberAccessExpression % . %  10:40-10:46
  │ │ │   │ │   │ │   ├─indexAccessExpression % [%]  10:40-10:44
  │ │ │   │ │   │ │   │ ├─identifier _ID %  10:40-10:41
  │ │ │   │ │   │ │   │ │ ├─atom _ID  (no region)
  │ │ │   │ │   │ │   │ │ └─s  10:40-10:41
  │ │ │   │ │   │ │   │ ├─atom [  10:41-10:42
  │ │ │   │ │   │ │   │ ├─number D %  10:42-10:43
  │ │ │   │ │   │ │   │ │ ├─atom D  (no region)
  │ │ │   │ │   │ │   │ │ └─1  10:42-10:43
  │ │ │   │ │   │ │   │ └─atom ]  10:43-10:44
  │ │ │   │ │   │ │   ├─atom .  10:44-10:45
  │ │ │   │ │   │ │   └─identifier _ID %  10:45-10:46
  │ │ │   │ │   │ │     ├─atom _ID  (no region)
  │ │ │   │ │   │ │     └─t  10:45-10:46
  │ │ │   │ │   │ ├─binop +  10:47-10:48
  │ │ │   │ │   │ │ └─atom +  10:47-10:48
  │ │ │   │ │   │ └─memberAccessExpression % . %  10:49-10:55
  │ │ │   │ │   │   ├─indexAccessExpression % [%]  10:49-10:53
  │ │ │   │ │   │   │ ├─identifier _ID %  10:49-10:50
  │ │ │   │ │   │   │ │ ├─atom _ID  (no region)
  │ │ │   │ │   │   │ │ └─s  10:49-10:50
  │ │ │   │ │   │   │ ├─atom [  10:50-10:51
  │ │ │   │ │   │   │ ├─number D %  10:51-10:52
  │ │ │   │ │   │   │ │ ├─atom D  (no region)
  │ │ │   │ │   │   │ │ └─2  10:51-10:52
  │ │ │   │ │   │   │ └─atom ]  10:52-10:53
  │ │ │   │ │   │   ├─atom .  10:53-10:54
  │ │ │   │ │   │   └─identifier _ID %  10:54-10:55
  │ │ │   │ │   │     ├─atom _ID  (no region)
  │ │ │   │ │   │     └─b  10:54-10:55
  │ │ │   │ │   └─atom ;  10:55-10:56
  │ │ │   │ └─atom }  11:5-11:6
  │ │ │   └─atom }  12:1-12:2
  │ │ └─controlTypeDeclaration % CONTROL % % (%) ;  14:1-14:26
  │ │   ├─annotationList _EMPTY  12:2
  │ │   │ └─atom _EMPTY  (no region)
  │ │   ├─atom CONTROL  14:1-14:8
  │ │   ├─identifier _ID %  14:9-14:10
  │ │   │ ├─atom _ID  (no region)
  │ │   │ └─C  14:9-14:10
  │ │   ├─typeParameterListOpt _EMPTY  14:10
  │ │   │ └─atom _EMPTY  (no region)
  │ │   ├─atom (  14:10-14:11
  │ │   ├─parameter % % % % %  14:11-14:24
  │ │   │ ├─annotationList _EMPTY  14:11
  │ │   │ │ └─atom _EMPTY  (no region)
  │ │   │ ├─direction OUT  14:11-14:14
  │ │   │ │ └─atom OUT  14:11-14:14
  │ │   │ ├─baseType BIT <%>  14:15-14:22
  │ │   │ │ ├─atom BIT  14:15-14:18
  │ │   │ │ ├─atom <  14:18-14:19
  │ │   │ │ ├─32  14:19-14:21
  │ │   │ │ └─atom >  14:21-14:22
  │ │   │ ├─identifier _ID %  14:23-14:24
  │ │   │ │ ├─atom _ID  (no region)
  │ │   │ │ └─r  14:23-14:24
  │ │   │ └─initializerOpt _EMPTY  14:24
  │ │   │   └─atom _EMPTY  (no region)
  │ │   ├─atom )  14:24-14:25
  │ │   └─atom ;  14:25-14:26
  │ └─packageTypeDeclaration % PACKAGE % % (%) ;  15:1-15:19
  │   ├─annotationList _EMPTY  14:26
  │   │ └─atom _EMPTY  (no region)
  │   ├─atom PACKAGE  15:1-15:8
  │   ├─identifier _ID %  15:9-15:12
  │   │ ├─atom _ID  (no region)
  │   │ └─top  15:9-15:12
  │   ├─typeParameterListOpt _EMPTY  15:12
  │   │ └─atom _EMPTY  (no region)
  │   ├─atom (  15:12-15:13
  │   ├─parameter % % % % %  15:13-15:17
  │   │ ├─annotationList _EMPTY  15:13
  │   │ │ └─atom _EMPTY  (no region)
  │   │ ├─direction _EMPTY  15:13
  │   │ │ └─atom _EMPTY  (no region)
  │   │ ├─typeIdentifier _TID %  15:13-15:14
  │   │ │ ├─atom _TID  (no region)
  │   │ │ └─C  15:13-15:14
  │   │ ├─identifier _ID %  15:15-15:17
  │   │ │ ├─atom _ID  (no region)
  │   │ │ └─_c  15:15-15:17
  │   │ └─initializerOpt _EMPTY  15:17
  │   │   └─atom _EMPTY  (no region)
  │   ├─atom )  15:17-15:18
  │   └─atom ;  15:18-15:19
  └─instantiation % % (%) % ;  17:1-17:15
    ├─annotationList _EMPTY  15:19
    │ └─atom _EMPTY  (no region)
    ├─typeIdentifier _TID %  17:1-17:4
    │ ├─atom _TID  (no region)
    │ └─top  17:1-17:4
    ├─atom (  17:4-17:5
    ├─callExpression % (%)  17:5-17:8
    │ ├─typeIdentifier _TID %  17:5-17:6
    │ │ ├─atom _TID  (no region)
    │ │ └─c  17:5-17:6
    │ ├─atom (  17:6-17:7
    │ ├─argumentList _EMPTY  17:7
    │ │ └─atom _EMPTY  (no region)
    │ └─atom )  17:7-17:8
    ├─atom )  17:8-17:9
    ├─identifier _ID %  17:10-17:14
    │ ├─atom _ID  (no region)
    │ └─main  17:10-17:14
    └─atom ;  17:14-17:15
  
