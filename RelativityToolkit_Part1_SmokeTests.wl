Print["--- 1. Visualizing Tensors ---"];
(*We input unambiguous structure:x[U[mu]]*)
(*The system renders textbook notation:*)
{"Vector (Contravariant)" -> x[\[ScriptCapitalU][\[Mu]]],
  "Covector (Covariant)" -> p[\[ScriptCapitalD][\[Nu]]],
  "Mixed Tensor T^u_v" -> 
   T[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalD][\[Nu]]],
  "Mixed Tensor T_u^v" -> 
   T[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalU][\[Nu]]]} // TableForm

   Print["--- PHYSICS ENGINE DIAGNOSTICS ---"];

(*1. METRIC RAISING & LOWERING*)
Print["\n1. Metric Gymnastics:"];
termLower = 
  g[\[ScriptCapitalD][\[Mu]], \[ScriptCapitalD][\[Nu]]]*
   A[\[ScriptCapitalU][\[Nu]]];
resLower = termLower /. metricRules;
Echo[termLower, "Input (g_uv A^v): "];
Echo[resLower, "Result (A_u): "];

termInverse = 
  g[\[ScriptCapitalU][\[Mu]], \[ScriptCapitalU][\[Alpha]]]*
   g[\[ScriptCapitalD][\[Alpha]], \[ScriptCapitalD][\[Nu]]];
resInverse = termInverse /. metricRules;
Echo[termInverse, "Metric Product: g^ua*g_an"];
Echo[resInverse, "Inverse Identity (Delta^u_v): "];


(*2. VALENCE CHECKER*)
Print["\n2. Valence Safety Check:"];
(*Should be True*)
valCheck = (valence[resLower] === {{}, {\[Mu]}});
Echo[valCheck, "Did A^v become A_u? "];

(*Should be False/Error for bad physics*)
badPhysics = x[\[ScriptCapitalU][\[Mu]]] + p[\[ScriptCapitalD][\[Mu]]];
Print["Checking Bad Physics (Expect Error Message below):"];
valence[badPhysics];


(*3. ALPHA-CONVERSION (The Collision Test)*)
Print["\n3. Coordinate Transformation (Alpha-Conversion):"];
(*We transform A^a'->(dx^a'/dx^mu) A^mu*)
(*Watch for Unique indices like mu123*)
contraTrans = A[\[ScriptCapitalU][Superscript["a", "\[Prime]"]]];
contraResTrans = contraTrans /. robustTransformRules;

Echo[contraTrans, "Original Contravariant: "];
Echo[contraResTrans, "Transformed (Look for Unique Indices): "];

covTrans = A[\[ScriptCapitalD][Superscript["a", "\[Prime]"]]];
covResTrans = covTrans /. robustTransformRules;

Echo[covTrans, "Original Covariant: "];
Echo[covResTrans, "Transformed, (Look for Unique Indices): "];

