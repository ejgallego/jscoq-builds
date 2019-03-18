function(Rx){"use strict";var
cR=123,m2=";",i=108,h0="i",np="Pattern-matching lambdas not allowed in refine",fO=",",hN="Subgoal: ",m1="my_preident",nK="(",m0="Depelim",a$="src/splitting.ml",fV=148,hZ="pattern",mZ="_equation_",no="<>",nn=119,n6="wf_fix",dk="|",d_="l",n5="mfix",mY="elimination",fN="This is not an equality between constructors.",fU="refine",fM="pattern_of_glob_constr",bw=144,dj="with",eb="=>",n4="Covering",nJ="binders2",mX="decompose_app",hJ="term: ",mV="uncurry_call",mW="succeeded",nm=136,cQ=248,hI="Subterm",hH="_unfold",mU=" context map ",nl="Cannot simplify dependent pair",hG="eqns",hF="P",fH=107,hY="simplify",n3="simp",bN=156,nk=569,n2="by",fT=141,hX="Applying ",cq=112,fK=145,nI="simplification_rules",fL="Below",cr="x",nH="Schemes",mT="->",n1="deppat_equations",mS="mutfix",hE=" for ",aU="src/principles_proofs.ml",mR="interp_eqns",B=110,dm="y",nG="_rec",hW="curry",cp="equations",mQ="elim_patterns",nj="Equations_common",fG="dependent",n0="is_secvar",mP=160,nF=157,nZ="<->",mO=" Pretype error: ",nE="<-",hM=139,ni="-",nY="\xce\xbb",ng="-!",nh="def",bA="covering",fE=" := ",nX="funelim",dl=" and ",aR=109,mN="define",nD="split",hV="lident",nW="Syntax",nC=159,nf=238,fS=101,ne="Proof of mutual induction principle is not guarded ",hD="_graph",hP="equation_user_option",mM=125,nB="*",nA="split_var",bz=140,cs="}",nV="src/noconf_hom.ml",t=250,nU="Building internalization environment",hL="Elimination principle could not be proved automatically: ",nd="failed",q=246,mL="move_after_deps",a_=102,hU=" Fail error ",mK="Extra_tactics",b2=113,nc="id'",ea=122,nb="Register",cm="{",by="c",Q="",nT="Sigma_types",mJ=134,mI="get_signature_pack",nS="eqns_specialize_eqs",hC="equation_options",na=100,m$="solve_equations",m_="refine_ho",fR=103,nR=" succeeded",m9="opt",nz="Prototype ",fQ="IDENT",m8="pattern_call",m7=1046,ny="Derive",m6="Eqdec",fF="src/sigma_types.ml",b0=" : ",co=106,fP=":=!",hT="src/simplify.ml",cN=" on ",hS="$",m5="needs_generalization",hK="Type error while building context map: ",nQ=153,m4="autounfold_ref",aQ=124,mH="src/equations.ml",bx=127,nx="?",mG=" with ",X=111,nv="deppat_elim",nw=" in context ",cP="src/covering.ml",hB="deppat",d9=" ",nP="index",cO=")",nu="Noconf",fJ=":",hR=118,nt="where ",b1="Equations",hO="Failed with: ",d8=116,mF="subterm_relation",nO="_where",fI="_",nN="Splitting",hA=158,bZ="src/principles.ml",hQ="wildcard",cl=":=",nM="============================",hz="Unnexpected goal",ns="as",cn="id",d$="where",m3=146,nL="equations_plugin",nq="g_simplification_rules",nr="Elimination",aE=129,am=Rx.jsoo_runtime,aa=am.caml_check_bound,mE=am.caml_equal,cM=am.caml_fresh_oo_id,mD=am.caml_lessthan,ck=am.caml_make_vect,d=am.caml_new_string,s=am.caml_obj_tag,aB=am.caml_register_global,w=am.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):am.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):am.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):am.caml_call_gen(a,[b,c,d])}function
p(a,b,c,d,e){return a.length==4?a(b,c,d,e):am.caml_call_gen(a,[b,c,d,e])}function
z(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):am.caml_call_gen(a,[b,c,d,e,f])}function
d5(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):am.caml_call_gen(a,[b,c,d,e,f,g])}function
ax(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):am.caml_call_gen(a,[b,c,d,e,f,g,h])}function
d7(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):am.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
Rw(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):am.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
d6(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):am.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
bY(a,b,c,d,e,f,g,h,i,j,k,l,m){return a.length==12?a(b,c,d,e,f,g,h,i,j,k,l,m):am.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l,m])}var
n=am.caml_get_global_data(),gb=d(mF),cA=[0,[0,0],1],as=d(nL),r=n.CamlinternalLazy,G=n.Constr,A=n.Termops,c=n.EConstr,I=n.Assert_failure,j=n.Names,N=n.Reductionops,x=n.Context,ae=n.Inductiveops,f=n.Util,iY=n.Reduction,bg=n.Term,ac=n.Typeclasses,aF=n.Globnames,aJ=n.Hints,P=n.Evarutil,o=n.Evd,_=n.CErrors,e=n.Pp,M=n.Feedback,L=n.Option,C=n.Printer,k=n.Proofview,bP=n.Refiner,eq=n.Pretype_errors,bD=n.Himsg,E=n.Tacmach,u=n.Tactics,at=n.Libnames,ay=n.Environ,v=n.Global,T=n.Ltac_plugin,Y=n.Stdarg,y=n.Genarg,ab=n.Geninterp,aM=n.Univ,aL=n.CAst,dp=n.UnivGen,J=n.Stdlib,O=n.Not_found,m=n.Tacticals,ek=n.Evarsolve,ad=n.Retyping,ei=n.Sorts,au=n.Namegen,bb=n.Declare,ef=n.Flags,ag=n.Typing,h_=n.Univops,ap=n.Nametab,ee=n.Smartlocate,cU=n.Lib,cS=n.CString,h3=n.Stdlib__printexc,fZ=n.Stdlib__printf,h1=n.Topfmt,cu=n.Goptions,cT=n.Libobject,dB=n.Stdlib__lazy,i$=n.Evardefine,ey=n.Dumpglob,l=n.Stdlib__list,W=n.DAst,a6=n.Ppconstr,bn=n.Constrexpr_ops,K=n.Nameops,c5=n.Impargs,dK=n.Metasyntax,a7=n.Constrintern,c4=n.Loc,R=n.Int,gy=n.Failure,D=n.CList,bo=n.Invalid_argument,br=n.CClosure,cF=n.Refine,a8=n.CArray,bH=n.Inductive,dR=n.Locusops,bs=n.Tacred,gK=n.Extraction_plugin,aY=n.Stdlib__array,j4=n.Typeops,dV=n.Evarconv,aA=n.Vars,aZ=n.Obligations,bK=n.Lemmas,kr=n.Pfedit,dY=n.Proof_global,e8=n.Future,gX=n.Pretyping,fe=n.Cbv,kT=n.UnivNames,kU=n.ComInductive,k2=n.Safe_typing,k6=n.Find_subterm,hf=n.Class_tactics,df=n.Equality,d3=n.Conv_oracle,fj=n.Type_errors,cJ=n.Autorewrite,lN=n.Indschemes,hy=n.Vernac_classifier,bW=n.Ftactic,h=n.Pcoq,bk=n.Genintern,a1=n.Mltop,cK=n.CLexer,l2=n.Pvernac,fA=n.Vernacentries,p9=[0,d("src/equations_common.ml"),nk,9],ri=[0,0,0],rh=[0,0,0],rg=[0,0,0,0,0],rd=d(hN),rc=d(nR),q9=d(cN),q_=d(hE),q$=d(hU),rb=d(mO),ra=d(hO),q6=d("================="),q7=d(cN),q8=d(hX),q5=d(hN),q4=d(nR),qZ=d(cN),q0=d(hE),q1=d(hU),q3=d(mO),q2=d(hO),qX=d(cN),qY=d(hX),qU=d(" depends"),qV=d("Found no hypothesis on which "),qW=[0,d("move_before_deps")],qK=[0,0],qD=d("solve_equation"),qC=d(cr),qB=d("find_empty"),qA=d(cp),qy=d("depind"),qx=d("simpl_dep_elim"),qv=d("depelim_nosimpl"),qt=d("do_empty"),qr=d("Equations.Init.depelim"),qq=d("."),qn=d("equations.depelim.module"),qo=d("equations.depelim.module is not defined"),ql=d("Equations.Init.unfold_recursor"),qk=d("Equations.Init.specialize_mutfix"),qj=d("Equations.Init.solve_subterm"),qi=d("Equations.Init.simpl_equations"),qh=d("Equations.Init.solve_eqdec"),qg=d("Equations.Init.solve_noconf_hom"),qf=d("Equations.Init.solve_noconf"),qe=d("Equations.Tactics.set_eos"),qd=d("Equations.Tactics.pi"),qc=d("Equations.Equations.solve_rec"),p6=d("Unknown database "),p7=[0,d("autounfold")],p8=d("Nothing to unfold"),p5=[0,0,0],p2=d("sigma.pr2"),p1=d("sigma.pr1"),pW=d("impossiblecall.class"),pK=d("depelim.class"),pJ=d("funelim.class"),pI=d("funind.class"),pH=d("Cannot find tactic "),pF=[0,1],pG=[0,1],oQ=[1,10],oO=[0,0],oP=d(" is defined"),oN=[0,0],oL=d("equations."),oC=d("not found in table: "),oz=d("new_global raised an error on:"),ot=[0,[11,d("Exception while typechecking context "),[2,0,[11,d(b0),[2,0,[12,10,0]]]]],d("Exception while typechecking context %s : %s\n")],n$=d("DEPRECATED. Use flag [Equations With UIP] and introduce an axiomn [forall A, Equations.Classes.UIP A] as a type class instance using [Existing Instance] instead."),oa=[0,d(b1),[0,d("WithK"),0]],ob=d("using axiomatic K during simplification."),oe=[0,d(b1),[0,d("WithKDec"),0]],of=d("using propositional K during simplification. Use flag Equations With UIP instead."),oi=[0,d(b1),[0,d("With"),[0,d("UIP"),0]]],oj=d("allow using propositional UIP during simplification"),om=[0,d(b1),[0,d("Transparent"),0]],on=d("leave definitions transparent"),oq=[0,d(b1),[0,d("Debug"),0]],or=d("Equations debug output"),oA=d("coqlib_registered"),oD=d("COQLIBREF"),oR=d("nat.zero"),oS=d("nat.succ"),oT=d("nat.type"),oW=d("fixproto"),oX=d("equality.type"),oY=d("equality.refl"),oZ=d("equality.case"),o0=d("equality.elim"),o1=d("bottom.type"),o2=d("bottom.case"),o3=d("bottom.elim"),o4=d("top.type"),o5=d("top.intro"),o6=d("top.elim"),o7=d("conj.type"),o8=d("conj.intro"),o9=d("unit.type"),o_=d("unit.intro"),o$=d("product.type"),pa=d("product.intro"),pb=d("wellfounded.class"),pc=d("wellfounded.type"),pd=d("relation.type"),pe=d("relation.transitive_closure"),pf=d("tele.type"),ph=d("tele.tip"),pi=d("tele.ext"),pj=d("tele.interp"),pk=d("tele.measure"),pl=d("tele.fix"),pm=d("tele.fix_functional_type"),pn=d("tele.fix_unfold"),pp=d("tele.MR"),pq=d("tele.type_app"),pr=d("tele.forall_type_app"),ps=d("tele.forall_uncurry"),pt=d("tele.forall"),pu=d("tele.forall_pack"),pv=d("tele.forall_unpack"),pw=d("eqdec.class"),px=d("eqdec.dec_eq"),py=d("uip.class"),pz=d("uip.uip"),pB=d("signature.class"),pC=d("signature.signature"),pD=d("signature.pack"),pL=d("noconfusion.class"),pM=d("nocycle.class"),pN=d("internal.bang"),pO=d("internal.inaccessible_pattern"),pP=d("internal.block"),pQ=d("internal.hide_pattern"),pR=d("internal.hidebody"),pS=d("internal.add_pattern"),pT=d("eos"),pU=d("internal.the_end_of_the_section"),pV=d("internal.end_of_section"),pY=d("sigma.type"),pZ=d("sigma.intro"),p_=[0,d(fL),[0,d(b1),0]],rq=[0,0],rt=d("No derive declared for "),rr=d("Expected an inductive type"),ru=d("!"),rv=d("#"),rw=d(cO),rx=d(nK),ry=d(cO),rz=d("?("),rH=d(fO),rC=d(cl),rD=d(fP),rE=d(cs),rF=d(cm),rG=d(eb),rI=d(dj),rJ=d(d$),rK=d(cs),rL=d(cm),rN=d(b0),rM=d("by struct "),rO=d("by wf "),rV=d(fO),rQ=d(cl),rR=d(fP),rS=d(cs),rT=d(cm),rU=d(eb),rW=d(dj),r2=d(fO),rX=d(cl),rY=d(fP),rZ=d(cs),r0=d(cm),r1=d(eb),r3=d(dj),r4=d(d$),r5=d(cs),r6=d(cm),sr=d(np),ss=d(mR),st=d(np),su=d(mR),sv=d(nh),sw=d(no),sq=d("_abs_where"),so=d("Expecting a pattern for "),sp=d("interp_pats"),sm=d("Internalizing pattern"),sk=d(nU),sl=d(nU),sn=d("While translating pattern to glob constr"),sa=d("Constructor is applied to too few arguments"),sb=d(fM),sc=d("Constructor is applied to too many arguments"),sd=d(fM),sg=d(" as a constructor"),sh=d("Cannot interpret "),si=d(fM),sj=d(hQ),se=d("Cannot interpret globalized term as a pattern"),sf=d(fM),r$=d("?( _ )"),rP=d(dk),rB=d("<constr>"),r8=d("equations_list"),r_=[0,0,0],sz=d(" appears twice"),sA=d("Non-linear pattern: variable "),sB=d(hQ),sT=[0,d("src/context_map.ml"),273,12],th=d("Occurs check singleton subst"),tg=[0,1],td=d(dl),te=d("Contexts do not agree for composition: "),tf=d("check_eq_context_nolet"),tb=[0,1],tc=[0,[11,d("While comparing contexts: "),[2,0,[11,d(dl),[2,0,[11,d(b0),[2,0,[12,10,0]]]]]]],d("While comparing contexts: %s and %s : %s\n")],ta=[0,[11,d("Exception while comparing contexts "),[2,0,[11,d(dl),[2,0,[11,d(b0),[2,0,[12,10,0]]]]]]],d("Exception while comparing contexts %s and %s : %s\n")],s$=[0,0],s9=d("split_tele"),s8=d("split_context"),sZ=d("Could not generate a permutation: different variables"),s2=d("Could not generate a permutation: irreducible inaccessible"),s0=d(dl),s1=d("Could not generate a permutation, patterns differ: "),s3=d("Could not generate a permutation"),sV=d(" = "),sW=d(dl),sX=d(" in ctx2 is invertible to "),sY=d("Could not generate a permutation: two different instances:"),sU=[0,0,0],sS=[0,0,0],sM=d(hK),sN=d(cp),sO=d("Invalid_argument: "),sP=d(hK),sQ=d(cp),sJ=d("Anomaly: "),sK=d(hK),sL=d(cp),sI=[0,0],sE=d(nM),sF=d(nM),sC=d(d9),sx=[0,0],sy=[0,1],t0=d("No Signature instance found"),tW=d("Goal cannot be curried"),tU=d("No currying to do in "),tJ=[0,d(fF),531,14],tK=[0,d(fF),nk,17],tS=[0,1],tO=d("Could not revert a cut, please report."),tL=[0,1],tM=[0,1],tI=[0,0],tN=[0,1],tP=[0,0],tQ=[0,0],tR=[0,1],tG=[0,d(fF),439,22],tH=d("projs"),tB=d(".] to avoid this."),tC=d(". Use [Derive Signature for "),tD=d("Automatically inlined signature for type "),tt=d("_sig"),tu=[1,0],tv=d("_sig_pack"),tw=d("_var"),tx=[1,0],ty=d("_Signature"),tr=d("No signature to derive for non-dependent inductive types"),ts=d("Derive Signature"),tp=d(nP),tq=d(nP),to=[0,d(fF),117,10],tn=d("Cannot make telescope out of empty context"),tk=d("constrs_of_coq_sigma"),tA=d("Signature"),vB=d("Equations.Simplify"),vC=[0,1],vD=[0,1],vQ=d(nx),vR=d(nZ),vS=d(nB),vG=d("NoConfusionOut"),vH=d("NoCycle"),vI=d("ElimTrue"),vJ=d("ElimFalse"),vL=d(ng),vK=d(ni),vN=d(nE),vM=d(mT),vO=d(hS),vE=[0,d(hT),1099,19],vF=[0,1],vz=d("a product"),vo=[1,0],vp=[1,1],vq=d("Neither side of the equality is a variable."),vr=[1,0],vs=[1,1],vt=[0,0],vu=d("Could not infer a simplification step."),vn=d("[elim_true] The first hypothesis is not the empty type."),vl=d("[elim_true] The first hypothesis is not the unit type."),vm=[0,0,0],vh=d(fN),vi=d("[noConfusion] Cannot find an instance of NoCycle for type "),vj=[0,0,0],vk=d("[noCycle] Cannot infer a proof of "),ve=d("Expected a full application of [opaque_ind_pack_eq_inv]. Maybeyou did not solve completely a NoConfusion step?"),vf=d("[opaque_ind_pack_eq_inv] should be applied to [eq_refl]."),vg=[0,0,0],vb=d(fN),vc=d("[noConfusion] Cannot find an instance of NoConfusion for type "),vd=[0,0,0],u1=d(fN),u2=d(fN),u3=d(" or NoConfusion for family "),u4=d("[noConfusion] Cannot simplify without UIP on type "),u5=d(" enable [Equations With UIP] to allow this"),u6=d("] if it requires uniqueness of identity proofs and"),u7=d("], or [Derive NoConfusion for "),u8=d("Either [Derive NoConfusionHom for "),u9=d(", which does not have a [NoConfusionHom] instance."),u_=d("[noConfusion] Trying to use a non-definitional noConfusion rule on "),u$=[0,0,0],uZ=d("[solution] cannot remove dependency in the variable "),uY=d("[solution] The variable appears on both sides of the equality."),uV=[0,1],uX=[0,0,0],uW=d("[deletion] Cannot simplify without UIP on type "),uO=[0,0,0],uM=d(nl),uN=[0,0,0],uR=[0,0,0],uP=d(nl),uQ=[0,0,0],uS=d("If you see this, please report."),uI=d("The first hypothesis in the goal is not an equality."),uJ=d("The first hypothesis in the goal is not an equality between identical terms."),uK=d("The left-hand side of the first hypothesis in the goal is not a variable."),uL=d("The right-hand side of the first hypothesis in the goal is not a variable."),uH=d("The goal is not a let-in."),uG=d("The goal is not a product."),uD=[0,d(hT),302,12],uB=d("Unexpected mismatch."),uA=[0,d(hT),235,30],t7=d("depelim."),t8=d("apply_noConfusion"),t9=d("apply_noCycle_left"),t_=d("apply_noCycle_right"),t$=d("simplify_ind_pack"),ua=d("simplify_ind_pack_inv"),ub=d("opaque_ind_pack_eq_inv"),uc=d("simpl_sigma"),ud=d("simpl_sigma_dep"),ue=d("simpl_sigma_nondep_dep"),uf=d("simpl_sigma_dep_dep"),ug=d("pack_sigma_eq"),uh=d("simpl_uip"),ui=d("solution_left"),uk=d("solution_left_dep"),um=d("solution_right"),uo=d("solution_right_dep"),uz=d("Equations.Simplify.CannotSimplify"),vv=d("Equations.Simplify.Blocked"),xA=[0,d(a$),1021,9],xB=[0,d(a$),1041,7],x5=[0,d(a$),1329,13],x4=[0,d(a$),1330,24],x1=d("Defining programs, before simplify_evars "),x2=d("Defining programs "),x3=[0,d(a$),1296,4],xX=[0,d(a$),1220,28],xW=[0,d(a$),1198,18],xT=d("_obligations"),xU=[0,[0,0]],xY=[0,0],xM=d("_obligation_"),xJ=d("Cannot handle admitted proof for equations"),xK=d("end_obligations"),xL=d("Defining the initial evars accoding to the proofs"),xI=d("evar type"),xG=[0,0],xO=[0,1,0],xP=d(" refine them interactively"),xQ=d('could not be solved automatically. Use the "Equations?" command to'),xR=d("Equations definition generated subgoals that "),xS=d(mN),xD=d('Use the "Equations" command to define it.'),xE=d("Equations definition is complete and requires no further proofs. "),xF=d(mN),xC=[0,d(a$),m7,12],xy=[0,1],xv=[1,0],xw=[0,0,0],xx=[1,0],xu=[0,d(a$),909,11],xt=[0,d(a$),883,15],xr=d("make_single_program: more than one program"),xo=[1,5],xp=d("_functional"),xl=d(" mutually with other programs "),xm=d("Cannot define "),xn=d("make_programs"),xq=[1,0],xk=[1,5],xj=[1,5],xb=d("Simplifying term:"),xc=d("... in context:"),xd=d("... named context:"),xe=d("Finished simplifying"),xf=[0,1],xg=[0,1],xh=d("Should not fail here, please report."),w$=[0,d(a$),486,7],xa=[0,[0,0,2],0],xi=[0,0],w_=[0,2],w7=d("H"),w8=[0,d(a$),387,20],wM=d("*impossible case*"),wx=d(fE),wy=d(cO),wz=d(" (where context length : "),wA=d(cO),wB=d("(arity: "),wC=d(cO),wD=d("(where_term: "),wE=d(cO),wF=d("(path: "),wG=d(") "),wH=d("(program type: "),wu=d("*raised an exception"),wv=d(b0),ww=d(nt),wI=d(nw),wJ=d(b0),wK=d(fE),wL=d(" :=! "),wN=d(nw),wO=d(b0),wP=d(" split: "),wQ=d("Mapping "),wR=d("RecValid "),wS=d("New problem to problem substitution is: "),wT=d("Revctx is: "),wU=d(" eliminating "),wV=d(" for type "),wW=d("New problem: "),wX=d(" refine args "),wY=d(" refine term "),wZ=d(" in "),w0=d(d9),w1=d(d9),w2=d(b0),w3=d(fE),w4=d(mG),w5=d("Error pretty-printing splitting"),wm=d("nested but not directly recursive"),wo=d("mutually recursive on "),wp=d("mutually recursive on ? "),wq=d("nested on "),wr=d("nested on ? "),ws=d("wellfounded"),wt=d("not recursive"),wn=d(" is "),wj=d(fJ),vU=d(fI),vT=d(hH),vV=[0,d(a$),43,9],x8=[0,0,0,0,0],x9=[0,0,0],x_=[0,0,0],y0=d("splitting failed for "),yY=d("covering is not faithful to user clauses, trying the next one"),yX=d("covering failed to produce a splitting in one of the branches,trying the next one"),yZ=d("covering succeeded"),yW=d("splitting succeded for "),yV=d("trying next blocker "),yU=d("blockers are: "),yN=d(" extpats "),yO=d(" with problem "),yP=d("Launching covering on "),yQ=d(mG),yR=d("Matching "),yT=d("got stuck"),y1=d("Maybe unification is stuck as it cannot refine a context/section variable."),y2=d(" to match a user pattern."),y3=d(" of (reduced) type "),y4=d("Unable to split variable "),y5=d(nA),yS=d(nd),y6=d(mW),y7=d(Q),zi=d(fI),y8=d("clause_"),y9=d("This clause has an empty pattern, it cannot have a right hand side."),y_=d(bA),zb=d("This pattern cannot be empty, it matches value "),zc=d(bA),y$=d("This variable does not have empty type in current problem"),za=d(bA),zd=d(" matched but its interpretation failed"),ze=d("Clause "),zf=d(nA),zg=d("Empty clauses should have at least one empty pattern."),zh=d(bA),zj=d("Non-exhaustive pattern-matching, no clause found for:"),zk=d(hB),zB=d("And the user patterns are: "),zC=d("Problem is "),zD=d("Non-matching clause in with subprogram:"),zE=[0,d(bA)],zF=[0,d(cP),1213,56],zA=d("unknown"),zs=d("This pattern must be innaccessible and equal to "),zt=[0,d(bA)],zp=d("should be unifiable with "),zq=d("Incompatible innaccessible pattern "),zr=[0,d(bA)],zn=d("is not inaccessible, but should refine pattern "),zo=d("Pattern "),zl=d("Unbound variable "),zm=d(cp),zu=d("'s type is empty"),zv=d("Cannot show that "),zw=d(bA),zJ=[0,d(cP),1148,14],zx=d(fU),zy=[0,1],zz=[0,0],zG=d("And clauses: "),zH=d("Unable to build a covering for with subprogram:"),zI=d(hB),zK=[0,0],zL=d("Unable to build a covering for:"),zM=d(hB),yL=[3,1],yJ=d("The carrier type of the recursion order cannot depend on the arguments"),yK=d(n6),yH=[0,d(cP),807,11],yI=[0,d(cP),820,11],yG=[12,0,0,0],yB=d(" found"),yC=d("No argument named "),yD=d("struct_index"),yE=[0,0],yF=[0,[0,0]],yA=d("Programs: "),yw=[0,d(cP),638,32],yx=d("Mutual well-founded definitions are not supported"),yy=d(cp),yz=[0,d(cP),646,11],yu=d("Unused clause "),yv=d(bA),yr=d("In context: "),yq=d(d9),yo=d("named_of_rel_context"),yk=[0,d(cP),499,19],yh=d("Ill-typed instance in interp_constr_in_rhs"),yf=[0,[0,0],0,0],yd=[0,0,0],yb=d("This clause has not enough arguments"),yc=d(bA),x$=d("Too many patterns in clauses for this type"),ya=d(bA),x6=d("Equations.Covering.Conflict"),x7=d("Equations.Covering.Stuck"),yM=d("Equations.Covering.UnfaithfulSplit"),z7=[0,0,1],z2=d(by),z3=d(hF),z4=d("step"),z5=d("rec"),z6=d("below"),z8=d("Below_"),z9=[1,0],z_=d("below_"),z$=[1,0],zW=[0,d("src/subterm.ml"),nf,55],zT=d("_subterm"),zU=d("well_founded_"),zZ=d(dm),z0=d(cr),zV=[1,0],zX=[0,0],zY=[0,0],zR=d(fI),zS=d(fI),zQ=d("_direct_subterm"),zN=d(cr),zO=d(dm),zP=d("z"),z1=d(hI),Aa=d(fL),Ag=d("_eqdec"),Ae=[1,10],Af=d("_EqDec"),Ac=d(cr),Ad=d(dm),Ab=d("param"),Ah=d("EqDec"),Ai=d(cr),Aj=d(dm),Ak=d("NoConfusion_"),Al=d("noConfusion_"),Am=d("NoConfusionPackage_"),An=[1,0],Ao=[0,d("src/noconf.ml"),142,11],Ap=[0,0],Aq=d("NoConfusion"),AQ=d("refining with"),AM=d("Generated clauses: "),AN=[0,1,0,0,0],AO=[0,0,0],AP=d("dummy"),AI=[0,d("src/depelim.ml"),370,19],AJ=[0,1],AK=[12,0,0,0],AL=[0,0,0],AR=d("Could not eliminate variable "),AS=d("No such hypothesis: "),AH=d("Specialization not allowed on dependent hypotheses"),AG=d("Nothing to do in hypothesis "),AE=d("destPolyRef"),AD=[0,0],AA=d("DependentElimination_"),Aw=d(hF),Ax=d(hF),Ay=d("_dep_elim"),Az=[1,7],Av=[0,0],Au=[0,1],At=[0,1],As=d("Equations.Depelim.Seen"),AC=d("DependentElimination"),AW=d(" is not declared."),AV=[1,7],AU=d("fold_left'"),AT=d("List.split_when: Invalid argument"),A$=d("Not enough products in "),Ba=d("Cannot do a fixpoint on a non inductive type."),C5=[0,d(aU),1129,14],C4=d(nX),C6=d("exception"),C0=d("Proving unfolding lemma of: "),C1=[0,1],C2=d("and of: "),C3=[0,1],CW=d("refine after replace"),CV=d("Unexpected unfolding lemma goal"),CS=d("Unexpected unfolding goal"),CI=[1,[0,1,0]],CJ=[1,[0,1,0]],CH=[0,d(aU),994,16],CN=d(mS),CK=d(d$),CL=d("where fixpoint"),CM=d("where before unfold"),CG=[0,d(aU),968,29],CQ=[0,d(aU),1033,9],CO=d("compute rhs"),CP=d("compute"),CT=d(nD),CU=d("recvalid"),CX=d("refined"),CR=[0,d(aU),m7,14],CF=d("solve_eq"),CD=[1,[0,1,0]],CE=[1,[0,1,0]],CB=[0,0,0],CC=d(hH),CY=[1,[0,1,0]],CZ=[1,[0,1,0]],Cz=d(" and cannot be proven by induction. Consider switching to well-founded recursion."),CA=d(ne),Cs=[0,d(aU),777,25],Ct=d("after mut -> nested and mut provable"),Cu=d(mS),Cv=d("splitting nested"),Cw=d("assert mut -> nest first subgoal "),Cq=d("and cannot be proven by induction"),Cr=d(ne),Cn=[0,0,1],Cm=d("Proof of mutual induction principle is not guarded, trying induction"),Co=d("induction on last var"),Cp=d("induction"),Cl=d(nX),Cy=[0,0,0],Cx=[0,d(aU),808,13],Cj=d(hN),Ci=d(mW),Ce=d(cN),Cf=d(hE),Cg=d(hU),Ch=d(hO),Cc=d(cN),Cd=d(hX),Bw=d("solving nested premises of compute rule"),B9=d("Unexpected refinement goal in functional induction proof"),B_=d("clear body"),B$=d("convert concl"),Ca=d("letin"),B7=[0,d(aU),379,21],B5=d("Unexpected goal in functional induction proof"),BU=d(": "),BV=d("Type of induction principle for "),BD=[0,d(aU),463,35],BE=d(" assoc "),BF=d(" type: "),BG=d(hJ),BH=d("Unfolded where "),BY=d("Mismatch between hypotheses in named context and program"),BI=d(mU),BJ=d("New where term"),BK=d("Unfolded where substitution:  "),BZ=d("context "),B0=d(" final term "),B1=d(" subst "),B2=d(hJ),B3=d(" where "),BW=[0,d(aU),495,36],BX=[0,0],BL=d(nG),BM=d("intros"),BN=d("moving section id"),BO=d("one where"),BP=d("Couldn't find associated args of where"),BQ=d(mU),BR=d(hJ),BS=d(" where: "),BT=d("Found path "),B4=[0,d(aU),575,17],Bx=d("solving nested recursive call"),By=d("solving premises of compute rule"),Bz=d("applying compute rule"),BA=d("wheretac"),BB=d("compute "),BC=d("compute empty"),B6=d(nD),B8=d(n6),Cb=d(fU),Bu=[0,d(aU),281,21],Bv=[0,1],Bt=[0,d(aU),276,13],Br=[0,20],Bs=d("eauto with below"),Bq=[0,d(aU),216,4],Bi=d("Fixpoints should be on the same mutual inductive declaration."),Bj=d(" already used in the environment"),Bk=d("Name "),Bl=[0,d("Logic.prim_refiner")],Bm=[0,d(aU),174,29],Bo=d("fix_"),Bb=d(" subgoal"),Bc=d(dl),Bd=d(" index"),Bn=d(" indices"),Be=d(d9),Bf=d(" name"),Bg=d("Cannot apply mutual fixpoint, invalid arguments: "),Bh=[0,d(n5)],A7=d(cN),A8=d("Trying "),A6=d(nd),A9=d("Couldn't rewrite"),A4=d("_wf_obligations"),A3=[0,d(mF),[0,d(fL),[0,d("rec_decision"),0]]],AY=d(fL),AX=d("Helper not found while proving induction lemma."),Ck=d("Equations.Principles_proofs.NotGuarded"),Ds=[0,1],EL=d(mZ),EM=d(nO),EF=d("_mut"),EA=[0,d(nr),[0,d(nH),0]],EB=[0,1],EC=[0,d(nr),[0,d(nH),0]],ED=[0,1],EE=d("_ind"),EJ=d(nG),EK=d("_rect"),EI=[0,1],EG=[0,1],EH=d(hD),Ez=d(mZ),Ey=d("_refinement_"),Ex=d(hD),Ev=d("Typing equation "),Ew=d("Typing constructor "),Et=d(" no alias "),Eu=d(nz),Er=d("alias: "),Es=d(nz),Ep=d("and "),Eq=d("Definining principles of: "),Em=[0,0,0],Eg=d(hL),Ei=d(hL),Eh=d("FunctionalInduction_"),Ef=d(hD),Ee=d("_graph_correct"),El=[0,d(bZ),1246,12],Ej=d(hL),Ek=d("Induction principle could not be proved automatically: "),Ea=d("FunctionalElimination_"),Eb=d("Error while typechecking elimination principle type: "),Ec=d("_elim"),D4=[0,d(bZ),1088,26],D5=[0,0,0],D6=[0,0,0],D7=[0,0,0],D8=d("where_instance: "),D9=[0,0,1],D_=[0,1,1],D3=[0,1],D2=[0,0,0,0],DZ=d(hH),D0=[0,1],D1=[0,0,0],DT=[0,-1],DU=[0,d(bZ),816,17],DV=d("_unfold_eq"),DW=[0,0,0],DX=[0,-1],DY=[0,0,0],DS=[0,1],DR=d("Declaring wf obligation "),DP=d("Replacing variable with "),DQ=d("Fixed hint "),DO=[1,[0,1,0]],DK=d(fE),DL=d(b0),DM=d(nt),DI=[0,1,0],DG=d("More statemsnts than declarations while computing eliminator"),DD=[0,0],DC=d("Heq"),DA=d("refine_eq"),DB=d(fU),Dx=d("abs"),Dy=[0,d(bZ),475,15],Dz=[0,0,0],DE=[0,d(bZ),511,37],DF=[0,d(bZ),512,15],Dw=[0,d(bZ),371,10],Du=[0,d(bZ),209,15],Dt=[0,0,0],Dr=d("Hind"),Dq=[0,0,0],Do=[0,d(bZ),bx,12],Db=d(" does not know how to substitute!"),Dc=d("The object "),C$=d("while rebuilding rewrite rule"),C7=[0,0],C8=[0,1],Df=d("EQUATIONS_REWRITE_RULE"),Dm=d("EQUATIONS_OPACITY"),EY=d(hz),EW=d("target"),EZ=d(hz),EX=d(hz),EV=d(nh),EU=[0,d(mH),167,63],ES=[1,0],ET=[0,0],EP=d("Could not find where clause unfolding lemma "),EQ=d(nO),ER=d("_where_rev"),EN=[0,d(mH),43,65],EO=d("_eq"),Ff=[0,0,0],Fg=[0,d(nV),nf,83],E8=d(hQ),E6=d("0"),E7=d("1"),E9=[0,0,0],E4=d(cr),E5=d(dm),E_=d(dm),E$=d(cr),Fa=[0,0,0],Fb=d("NoConfusionHom_"),Fc=[0,0,0],Fd=[0,0,0],Fe=[0,0],Fh=[0,0,0],E0=d("noConfusionHom_"),E1=d("NoConfusionHomPackage_"),E2=[0,d(nV),77,11],E3=[0,0],Fi=d("NoConfusionHom"),Fp=d("Products do not match"),Fo=d("Second-order matching failed"),Fn=d("Couldn't find a second-order pattern to match"),Fk=[0,d("src/extra_tactics.ml"),19,11],Fl=d("core"),Fj=d("f"),Qe=[2,0],P5=[0,1],PZ=[0,0],OD=d("Not a section variable or hypothesis"),Ov=[0,0,0],Om=[0,0,0],Od=d(dk),Oc=d(dk),NS=[0,[0,[0,d("Classic"),1,0]],1],Nh=[0,0,0],M_=[0,0,0],M4=[0,0,0],Mt=[0,0,0],Mj=[0,1],L7=[0,1],LE=[0,0,0],Lq=[0,1],Lc=[0,0],Hs=[1,0],Ho=[1,1],Hk=[0,1],Hg=[0,0],GI=d("No generalization needed"),Gt=[0,0],Ft=d(by),Fv=d("h'"),Fx=d("h"),Fy=d(mX),FA=d(mX),FD=d("myref"),FE=d(m4),FG=d(m4),FJ=d(nc),FL=d(cn),FM=d(mI),FO=d(mI),FR=d(cn),FS=d("sigma"),FT=d(hZ),FV=d("pattern_sigma"),FX=[0,d(hW),0],F0=d(cn),F1=d(hW),F3=d(hW),F6=d(cn),F7=d("uncurry_hyps"),F9=d("curry_hyps"),Ga=d(nc),Gc=d(cn),Ge=d("c'"),Gg=d(by),Gh=d(mV),Gj=d(mV),Gm=d(by),Gn=d(hZ),Go=d(fG),Gq=d("dependent_pattern"),Gu=d(by),Gv=d("from"),Gw=d(hZ),Gx=d(fG),Gz=d("dependent_pattern_from"),GC=d(by),GD=d(m8),GF=d(m8),GJ=d(cn),GK=d(m5),GM=d(m5),GP=d("tac"),GR=d("destruct"),GS=d(m$),GU=d(m$),GX=d(by),GZ=d(d_),G0=d("simpc"),G3=d(by),G5=d(d_),G6=d(n3),G8=d(n3),G9=d(hP),Hc=d(hP),Hh=d("noind"),Hl=d("ind"),Hp=d(hG),Ht=d("noeqns"),Hx=d(hP),Hy=d(hC),HD=d(hC),HH=d(cO),HJ=d(nK),HO=d(hC),HP=d(hV),HU=d(hV),H0=d(hV),H2=d(nJ),H4=d(nJ),H6=d(n1),H9=d(n1),H_=d(nv),Ib=d(nv),Ic=d(cp),If=d(cp),Ih=d(m1),Ik=d(m1),Il=d(nY),Im=d("identloc"),In=d("equation"),Io=d("pat"),Ip=d(fU),Iq=d("wf_annot"),Ir=d("proto"),Is=d("where_rhs"),It=d("where_clause"),Iu=d("wheres"),Iv=d("local_where_rhs"),Iw=d("local_where"),Ix=d("local_wheres"),Iy=d("rhs"),Iz=d("sub_equations"),ID=[0,d(fQ),d(Q)],IQ=[0,d(Q),d(m2)],IZ=[0,d(Q),d("]")],I1=[0,d(Q),d(dk)],I5=[0,d(Q),d("[")],Jc=[0,d(Q),d(cs)],Je=[0,d(Q),d(m2)],Ji=[0,d(Q),d(cm)],Jk=[0,d(Q),d(nY)],JB=[0,d(Q),d(dk)],JF=[0,d(Q),d(dk)],J0=[0,d(Q),d(fO)],Ka=[0,d(fQ),d("wf")],Kc=[0,d(Q),d(n2)],Km=[0,d(Q),d("struct")],Ko=[0,d(Q),d(n2)],KA=[0,d(Q),d(cl)],KE=[0,d(Q),d(fJ)],KV=[0,d(fQ),d(Q)],KX=[0,d(Q),d(fJ)],K5=[0,d(Q),d(cl)],Lk=[0,d(Q),d(d$)],Ls=[0,d(Q),d(dj)],LO=[0,d(fQ),d(Q)],LQ=[0,d(Q),d(fJ)],LY=[0,d(Q),d(cl)],Md=[0,d(Q),d(d$)],Ml=[0,d(Q),d(dj)],MC=[0,d(Q),d(fP)],MM=[0,d(Q),d(cl)],MR=[0,d(Q),d(eb)],M5=[0,d(Q),d(cl)],M$=[0,d(Q),d(eb)],Ni=[0,d(Q),d(dj)],Nx=[0,d(Q),d(cs)],NA=[0,d(Q),d(cm)],NW=[0,d(hG)],NY=[0,d(m9)],NZ=d(b1),N3=d("Define_equations"),N7=[0,d(hG)],N9=[0,d(m9)],N_=d("Equations?"),Oa=d("Define_equations_refine"),Oe=d(mQ),Oj=d(mQ),On=d(d_),Oo=d(ns),Oq=d(cn),Or=d(mY),Os=d(fG),Ow=d(cn),Ox=d(mY),Oy=d(fG),OA=d("dependent_elimination"),OE=d(cr),OF=d(n0),OH=d(n0),OK=d(by),OL=d(m_),ON=d(m_),OQ=d(h0),OR=d("eqns_specialize_eqs_block"),OU=d(h0),OV=d(nS),OX=d(nS),O0=d(by),O2=d(h0),O3=d(mL),O5=d(mL),O9=[0,d(by)],O_=d("for"),Pa=[0,d("ds")],Pb=d(ny),Pf=d(ny),Pg=d(nq),Pj=d(nq),Pk=d("simplification_rule_located"),Pl=d("simplification_rule"),Pm=d("simplification_step"),Pn=d("direction"),PI=[0,d(Q),d(nx)],PN=[0,d(Q),d(nZ)],PS=[0,d(Q),d(nB)],P0=[0,d(Q),d(ni)],P6=[0,d(Q),d(ng)],P$=[0,d(Q),d(no)],Qf=[0,d(Q),d(hS)],Qk=[0,d(Q),d(cs)],Qn=[0,d(Q),d(cm)],Qp=[0,d(Q),d(hS)],QD=[0,d(Q),d(mT)],QI=[0,d(Q),d(nE)],QM=d(nI),QS=d(nI),QU=[0,d(hY),0],QX=d(d_),QY=d(hY),Q0=d(hY),Q3=d(d_),Q5=d("li"),Q6=d(n5),Q8=d("mutual_fix"),Ra=[0,d("quid")],Rb=d(ns),Rd=[0,d("g")],Re=d(nb),Ri=d(nb),Rj=d(nj),Rk=d(nT),Rl=d(m6),Rm=d(hI),Rn=d(m0),Ro=d(nW),Rp=d(n4),Rq=d(nN),Rr=d(nu),Rs=d(mK),Rt=d(b1),Ru=d("equations_plugin_mod"),Rv=d(nL),qQ=n.Pputils,p0=n.Recordops,oB=n.Summary,tE=n.Patternops,xV=n.UState,xH=n.Evar,xN=n.Proof,yg=n.Implicit_quantifiers,ye=n.Glob_ops,Dj=n.Mod_subst,Dp=n.Stdlib__map,Fm=n.Eauto,Ob=n.Goal;function
n7(d,c,b){return a(d,a(c,b))}function
n8(d,c,b){var
e=b[1],f=a(c,b[2]);return[0,a(d,e),f]}function
n9(a){return a}function
dn(b){var
d=b[1];return[0,d,a(c[2][1],b[2])]}function
ct(d,a){var
e=a[1];return[0,e,b(c[2][2],d,a[2])]}var
b3=[0,0],ec=[0,0];function
n_(b){if(b){var
c=a(e[3],n$);return g(_[6],0,0,c)}b3[1]=b;return 0}var
oc=[0,1,ob,oa,function(a){return 0},n_];b(cu[4],0,oc);function
od(a){b3[1]=a;return 0}var
og=[0,1,of,oe,function(a){return b3[1]},od];b(cu[4],0,og);function
oh(a){b3[1]=a;return 0}var
ok=[0,0,oj,oi,function(a){return b3[1]},oh];b(cu[4],0,ok);function
ol(a){ec[1]=a;return 0}var
oo=[0,0,on,om,function(a){return ec[1]},ol];b(cu[4],0,oo);var
Z=[0,0];function
op(a){Z[1]=a;return 0}var
os=[0,0,or,oq,function(a){return Z[1]},op];b(cu[4],0,os);function
a3(d){var
c=Z[1];if(c){var
e=a(d,0);return b(M[10],0,e)}return c}function
fW(f,d){var
c=a(v[2],0),h=g(f,c,a(o[17],c),d);return b(e[48],h1[7][1],h)}function
fX(d,c,b,a){p(ag[6],d,c,b,a);return 0}function
h2(c,b,a){g(ag[4],c,b,a);return 0}function
fY(j,i,h){try{var
d=function(d,e){h2(e,i,a(x[1][1][3],d));var
f=a(x[1][1][2],d);function
g(b){return fX(e,i,b,a(x[1][1][3],d))}b(L[13],g,f);return b(c[aR],d,e)};g(f[17][16],d,h,j);var
o=0;return o}catch(d){d=w(d);var
k=a(h3[1],d),l=b(c[B],h,j),m=a(A[cR][7],l),n=a(e[49],m);g(fZ[3],ot,n,k);throw d}}function
ou(d){var
b=a(c[8],G[6]);return Rw(P[5],0,0,0,0,0,0,ay[31],o[16],b)[2]}function
F(b){return a(k[71][7],b)}function
aj(a){return b(k[71][1],0,a)}function
h4(c){var
d=[0,c,0];function
e(g,c){var
d=c[1],e=b(f[18],c[2],[0,d,0]);return[0,a(f[17][6],d),e]}return g(f[17][16],e,c,d)[2]}function
h5(e){return function(g,f){var
c=g,a=f;for(;;){if(a){var
h=a[2],d=b(e,c,a[1]);if(d)return d;var
c=c+1|0,a=h;continue}return 0}}}function
ov(a){return g(f[19][7],a,0,a.length-1-1|0)}function
ow(a){return b(f[19][55],a.length-1-1|0,a)}function
ox(e,d){return function(f){var
a=f;for(;;){if(a){var
c=a[1],g=a[2],h=c[1];if(b(e,d,c[2]))return h;var
a=g;continue}throw O}}}function
oy(c,b){var
d=0;function
e(d,b){var
e=a(c,d);function
f(a){return[0,a,b]}return g(L[24],f,b,e)}var
h=g(f[19][18],e,b,d);return a(f[19][12],h)}function
ba(d,c){try{var
j=b(P[9],d,c);return j}catch(d){var
f=a(C[58],c),h=a(e[3],oz),i=b(e[12],h,f);return g(_[3],0,0,i)}}function
aV(a,c){var
b=ba(a[1],c),d=b[2];a[1]=b[1];return d}var
f0=g(oB[4],0,oA,cS[52][1]);function
h6(d){try{var
c=b(cS[52][22],d,f0[1]);return c}catch(c){c=w(c);if(c===O){var
f=a(e[3],d),h=a(e[3],oC),i=b(e[12],h,f);return g(_[6],0,0,i)}throw c}}function
h7(b){var
a=b[2];f0[1]=g(cS[52][4],a[1],a[2],f0[1]);return 0}var
f1=a(cT[1],oD),oE=f1[8];function
oF(a){return[0,a[2]]}var
oG=cT[2];function
oH(a){return[0,a]}var
oI=f1[4];function
oJ(b,a){return h7(a)}var
oK=a(cT[4],[0,f1[1],h7,oJ,oI,oH,oG,oF,oE]);function
h8(d,c){var
e=a(at[28],d),f=a(oK,[0,e,a(ap[9],c)]);return b(cU[7],0,f)}function
f2(a){return h6(b(J[17],oL,a))}function
H(a){return[q,function(b){return f2(a)}]}function
ed(b,a){return aV(a,f2(b))}function
f3(c){var
d=b(at[32],0,c),e=a(ap[10],d);return a(ee[2],e)}var
f4=dp[21];function
oM(d){var
e=b(at[32],0,d),f=a(f4,a(ap[9],e));return a(c[8],f)}function
cV(d,a,c){var
b=p(ag[2],oN,d,a[1],c),e=b[2];a[1]=b[1];return e}function
h9(z,j,i,f,e){var
r=j?j[1]:0,h=a(v[2],0);if(f)var
k=f[1],s=p(ag[2],0,h,i,k)[1],l=p(ag[6],h,s,e,k);else
var
l=p(ag[2],0,h,i,e)[1];var
d=a(o[bN],l),m=g(c[5],0,d,e),t=b(c[5],0,d),n=b(L[16],t,f),u=a(h_[1],m),w=g(L[24],h_[1],aM[2][1],n),x=b(aM[2][7],u,w),q=b(o[hR],d,x),y=[0,b(o[fK],r,q)];return[0,d,q,ax(bb[2],0,0,0,n,y,0,m)]}function
a4(h,n,m,g,l,k){var
f=h9(oO,[0,g],l,m,n),i=f[1],p=f[2],d=z(bb[3],0,0,h,0,[0,[0,f[3]],k]),q=a(j[1][8],h),r=b(J[17],q,oP),s=a(e[3],r),t=M[6];function
u(a){return b(t,0,a)}b(ef[23],u,s);if(g){var
v=a(o[bw],p),w=a(aM[36][4],v),x=[0,d,a(c[2][1],w)];return[0,d,[0,i,a(c[23],x)]]}return[0,d,[0,i,a(c[22],d)]]}function
h$(f,a,e,d,c){var
g=a?a[1]:0,b=h9(f,[0,g],e,d,c);return[0,b[1],b[3]]}function
dq(l,k,j,e,d,i){var
f=b(ac[16],d,i),m=f[2],n=a(L[7],f[1]),o=b(c[38],n,e),g=a4(l,o,[0,b(c[37],m,e)],k,j,oQ),h=g[1],q=g[2],r=p(ac[5],d[1],aJ[4],1,[1,h]);a(ac[6],r);return[0,h,q]}var
dr=H(oR),ds=H(oS),oU=H(oT);function
f5(b){if(0===b){var
c=s(dr),e=t===c?dr[1]:q===c?a(r[2],dr):dr;return a(f4,e)}var
d=s(ds),f=[0,f5(b-1|0)],g=t===d?ds[1]:q===d?a(r[2],ds):ds,h=[0,a(f4,g),f];return a(G[13],h)}function
eg(d){var
b=a(G[26],d);if(9===b[0]){var
c=b[2];if(1===c.length-1)return eg(c[1])+1|0}return 0}function
eh(e,d,c){var
f=a(ay[10],c),g=a(A[77],f),h=a(j[1][10][35],g),i=b(j[1][10][7],e,h);return b(au[27],d,i)}function
oV(d,c,b){return eh(d,c,a(E[8],b))}var
bB=H(oW),bc=H(oX),cv=H(oY),ia=H(oZ),ib=H(o0),aK=[q,function(m){var
e=a(v[2],0),f=s(bc),h=a(o[17],e),i=t===f?bc[1]:q===f?a(r[2],bc):bc,g=b(P[9],h,i),d=g[1],j=z(ad[2],0,0,e,d,g[2]),k=b(c[62],d,j)[2],l=b(c[1][2],d,k);return a(ei[10],l)}],b4=H(o1),cW=H(o2),ic=H(o3),dt=H(o4),id=H(o5),ie=H(o6),ig=H(o7),ih=H(o8),cw=H(o9),cx=H(o_),du=H(o$),ej=H(pa),f6=H(pb),ii=H(pc),ij=H(pd),ik=H(pe),pg=H(pf),dv=H(ph),dw=H(pi),cX=H(pj),f7=H(pk),f8=H(pl),il=H(pm),po=H(pn),im=H(pp),io=H(pq),ip=H(pr),iq=H(ps),ir=H(pt),is=H(pu),it=H(pv),iu=H(pw),iv=H(px),iw=H(py),pA=H(pz),f9=H(pB),ix=H(pC),iy=H(pD);function
aN(d,b){var
c=s(b),e=t===c?b[1]:q===c?a(r[2],b):b;return ba(d,e)}function
av(b,d){var
c=s(b),e=t===c?b[1]:q===c?a(r[2],b):b;return aV(d,e)}function
iz(c,e){var
d=s(c),f=t===d?c[1]:q===d?a(r[2],c):c;return b(aF[11],f,e)}function
f_(b,e){var
d=p(o[hA],0,0,b[1],e),f=d[2];b[1]=d[1];return a(c[13],f)}function
pE(c){var
b=s(aK),d=t===b?aK[1]:q===b?a(r[2],aK):aK;return f_(c,d)}function
cy(h,d,b,g){var
e=s(b),i=t===e?b[1]:q===e?a(r[2],b):b,f=d5(c[cR],0,0,0,h,d[1],i),j=f[2];d[1]=f[1];return a(c[21],[0,j,g])}function
f$(d,a,c){var
b=ax(ek[5],0,pG,0,pF,d,a[1],c),e=b[2];a[1]=b[1];return e}function
b5(b,a,e,d,c){return cy(b,a,bc,[0,f$(b,a,e),d,c])}function
iA(b,a,d,c){return cy(b,a,cv,[0,f$(b,a,d),c])}var
b6=0;function
aG(d,c){try{var
j=[29,[0,b6,[3,[0,b6,[0,b(at[29],0,d),c]]]]],k=a(T[13][26],j);return k}catch(c){c=w(c);if(c===O){var
f=a(e[3],d),h=a(e[3],pH),i=b(e[12],h,f);return g(_[3],0,0,i)}throw c}}function
dx(d,c){var
e=b(ac[11],d,c);return a(L[7],e)[2][1]}function
iB(b){var
a=[0,b],c=ed(pI,a),d=dx(a[1],c);return[0,a[1],d]}function
iC(b){var
a=[0,b],c=ed(pJ,a),d=dx(a[1],c);return[0,a[1],d]}function
iD(a){var
b=ed(pK,a);return dx(a[1],b)}var
bC=H(pL),iE=H(pM),dy=H(pN),a5=H(pO),aW=H(pP),bO=H(pQ),dz=H(pR),dA=H(pS),ga=a(j[1][6],pT),iF=H(pU),cz=H(pV);function
iG(a){return ed(pW,a)}var
pX=[q,function(f){var
b=s(dA),c=0,d=t===b?dA[1]:q===b?a(r[2],dA):dA,e=[0,[0,0,[1,a(aF[8],d)]],c];return a(u[68],e)}],ai=H(pY),ak=H(pZ);function
iH(c){var
d=a(aF[8],c),e=a(p0[8],d),f=a(L[7],e);return b(j[67][2],f,0)}var
aq=[q,function(e){var
b=H(p1),c=s(b),d=t===c?b[1]:q===c?a(r[2],b):b;return iH(d)}],al=[q,function(e){var
b=H(p2),c=s(b),d=t===c?b[1]:q===c?a(r[2],b):b;return iH(d)}];function
p3(e,g){var
a=g;for(;;){var
h=b(A[57],e,a),f=b(A[60],e,h),d=b(c[3],e,f);switch(d[0]){case
6:var
a=d[3];continue;case
8:var
a=d[4];continue;case
9:var
a=d[1];continue;default:return f}}}function
ah(a){var
b=a[3],c=a[2],d=a[1];return c?[1,d,c[1],b]:[0,d,b]}function
el(e,h,d){function
j(e,d){if(d){var
k=d[2],f=a(x[1][1][17],d[1]),l=f[3],m=f[2],n=f[1],o=j(e-1|0,k),p=g(c[i][2],h,e,l),q=b(c[i][2],h,e);return[0,ah([0,n,b(L[16],q,m),p]),o]}return 0}return j(a(x[1][4],d)+e|0,d)}function
aS(b,a){return el(0,b,a)}function
p4(d){var
e=a(c[i][1],1);return b(f[17][69],e,d)}function
gc(r,i,d){var
s=[0,j[1][10][1],j[19][1]];function
t(c,d){var
i=c[2],k=c[1];try{var
q=a(aJ[15],d),f=q}catch(c){c=w(c);if(c!==O)throw c;var
l=a(e[3],d),m=a(e[3],p6),n=b(e[12],m,l),f=g(_[6],0,p7,n)}var
h=a(aJ[14][18],f),o=h[1],p=b(j[19][7],h[2],i);return[0,b(j[1][10][7],o,k),p]}var
p=g(f[17][15],t,s,r),z=i?b(E[19],d,i[1][1]):a(E[7],d),l=a(E[2],d),v=a(E[8],d),x=p[2],y=p[1];function
h(e){var
d=b(c[3],l,e);switch(d[0]){case
1:var
k=d[1];if(b(j[1][10][3],k,y)){var
m=b(ay[42],k,v);return m?[0,1,a(c[8],m[1])]:[0,0,e]}break;case
9:var
n=d[2],p=d[1],q=h(p);if(0===q[1]){var
z=function(f,c,a){var
b=c[2],d=c[1];if(d)return[0,d,[0,a,b]];var
e=h(a);return 0===e[1]?[0,0,[0,a,b]]:[0,1,[0,e[2],b]]},r=g(f[19][46],z,p5,n),A=r[2];if(r[1]){var
B=a(f[17][9],A),C=[0,p,a(f[19][12],B)];return[0,1,a(c[21],C)]}return[0,0,e]}var
D=a(c[21],[0,q[2],n]);return[0,1,b(N[26],o[16],D)];case
10:var
s=d[1],t=s[1],E=s[2];if(b(j[19][3],t,x)){var
F=[0,t,b(c[2][2],l,E)],G=b(ay[62],v,F);return[0,1,a(c[8],G)]}break}var
i=[0,0];function
u(a){if(i[1])return a;var
b=h(a),c=b[2];i[1]=b[1];return c}var
w=g(c[fS],l,u,e);return[0,i[1],w]}var
n=h(z),q=n[2];if(n[1]){if(i){var
A=i[1],B=a(u[47],q),C=g(u[54],0,B,A);return b(k[71][7],C,d)}var
D=b(u[5],q,2);return b(k[71][7],D,d)}var
F=a(e[3],p8);return g(m[24],0,F,d)}function
iI(d){var
c=d;for(;;){var
b=a(G[26],c);switch(b[0]){case
9:var
c=b[1];continue;case
10:var
e=a(j[17][9],b[1][1]);return a(j[6][5],e);default:throw[0,I,p9]}}}var
iJ=a(f[17][69],iI),p$=b(f[17][69],j[1][6],p_),iK=a(j[5][4],p$);function
qa(b){var
c=a(j[6][4],b),d=a(j[5][4],0);return g(j[13][1],[0,iK],d,c)}function
qb(a){return aG(qc,0)}function
iL(a){return aG(qd,0)}function
em(a){return aG(qe,0)}function
iM(a){return aG(qf,0)}function
iN(a){return aG(qg,0)}function
iO(a){return aG(qh,0)}function
iP(a){return aG(qi,0)}function
iQ(a){return aG(qj,0)}function
gd(a){return aG(qk,0)}function
ge(a){return aG(ql,0)}function
en(a){return[2,b(at[32],0,a)]}function
qm(d){var
b=h6(qn);if(1===b[0])return a(j[17][8],b[1]);var
c=a(e[3],qo);return g(_[3],0,0,c)}var
b7=a(dB[3],qm);function
qp(d){var
b=s(b7),c=t===b?b7[1]:q===b?a(r[2],b7):b7;return a(j[10][5],c)}var
eo=a(dB[3],qp);function
cY(d){var
c=s(eo),e=b(J[17],qq,d),f=t===c?eo[1]:q===c?a(r[2],eo):eo;return b(J[17],f,e)}function
gf(a){return aG(qr,[0,en(a),0])}function
qs(a){var
b=[0,en(a),0];return aG(cY(qt),b)}function
qu(a){var
b=[0,en(a),0];return aG(cY(qv),b)}function
qw(a){return aG(cY(qx),0)}function
iR(a){var
b=[0,en(a),0];return aG(cY(qy),b)}function
qz(a){return aG(cY(qA),0)}function
iS(a){return aG(cY(qB),0)}function
gg(b){return a(dp[19],[0,b[1],b[2]])}function
iT(x){var
p=a(j[6][4],qD),e=s(b7),u=j[5][6],v=t===e?b7[1]:q===e?a(r[2],b7):b7,w=g(j[13][1],v,u,p),d=a(j[1][6],qC),f=a(y[6],Y[11]),h=a(ab[3],f),i=gg([0,x,aM[29][1]]),k=a(c[8],i),l=b(ab[1][8],h,k),m=ab[5][1],n=[0,g(j[1][11][4],d,l,j[1][11][1]),m],o=[29,[0,b6,[3,[0,b6,[0,[0,[0,b6,w]],[0,[2,[1,b(aL[1],0,d)]],0]]]]]];return b(T[13][23],n,o)}function
qE(d,e){var
f=a(x[1][1][2],d);if(f)return b(c[i][5],f[1],e);var
g=a(x[1][1][3],d),h=[0,a(x[1][1][1],d),g,e];return a(c[18],h)}function
iU(f,e,d){var
h=a(c[9],1);return g(A[38],f,h,d)?b(c[35],e,d):b(c[i][5],c[14],d)}function
iV(c,b,a){function
d(b,a){return iU(c,a,b)}return g(f[17][15],d,b,a)}function
qF(d,e){var
f=a(x[1][1][2],d);if(f)return b(c[i][5],f[1],e);var
g=a(x[1][1][3],d),h=[0,a(x[1][1][1],d),g,e];return a(c[19],h)}function
iW(j,h,d){var
e=a(x[1][1][17],h),f=e[2],k=e[3],l=e[1];if(f)return b(c[i][5],f[1],d);var
m=a(c[9],1);return g(A[38],j,m,d)?a(c[19],[0,l,k,d]):b(c[i][5],c[14],d)}function
iX(j,h,d){var
e=a(x[1][1][17],h),f=e[2],k=e[3],l=e[1];if(f)return b(c[i][5],f[1],d);var
m=a(c[9],1);return g(A[38],j,m,d)?a(c[18],[0,l,k,d]):b(c[i][5],c[14],d)}function
b8(h,a,e,d){function
i(e,d){var
f=b(c[35],d,e);return b(N[30],a,f)}var
j=g(f[17][15],i,e,d);return g(N[18],h,a,j)}function
gh(j,d,h,e){function
k(f,e){var
g=0===a(x[1][1][1],e)?b(c[i][5],c[14],f):b(c[35],e,f);return b(N[30],d,g)}var
l=g(f[17][15],k,h,e);return g(N[18],j,d,l)}function
dC(d,a){function
e(d,a){return b(c[36],a,d)}var
h=g(f[17][15],e,d,a);return b(N[30],o[16],h)}function
qG(l,e,d){function
h(d,m){var
e=a(x[1][1][17],m),f=e[3],h=e[2],j=e[1];if(h){var
k=h[1];return g(c[i][13],l,1,d)?b(c[i][5],k,d):a(c[20],[0,j,k,f,d])}return a(c[19],[0,j,f,d])}return g(f[17][15],h,e,d)}function
qH(c,b,a){function
d(b,a){return iW(c,a,b)}return g(f[17][15],d,b,a)}function
qI(c,b,a){function
d(b,a){return iX(c,a,b)}return g(f[17][15],d,b,a)}function
gi(e,d){var
g=a(c[i][1],e);return b(f[17][69],g,d)}function
gj(d,g,i,h){var
m=g?g[1]:0;function
e(g,i){var
h=b(c[3],d,i);switch(h[0]){case
1:return b(j[1][10][4],h[1],g);case
9:var
n=h[2],k=b(c[3],d,h[1]);switch(k[0]){case
11:var
l=k[1][1];break;case
12:var
l=k[1][1][1];break;default:return p(c[co],d,e,g,i)}var
o=a(v[28],l)[1],q=m?0:o[6];return p(f[19][52],q,e,g,n);default:return p(c[co],d,e,g,i)}}return e(i,h)}function
gk(j,d,g){var
e=b(c[3],j,d);switch(e[0]){case
11:var
h=e[1][1];break;case
12:var
h=e[1][1][1];break;default:return[0,d,g]}var
k=a(v[28],h)[1][7],i=b(f[19][55],k,g),l=i[2];return[0,a(c[21],[0,d,i[1]]),l]}function
qJ(e,a,d,c){try{var
b=ax(N[84],0,qK,0,e,a[1],d,c)}catch(a){a=w(a);if(a===iY[6])return 0;throw a}return b?(a[1]=b[1],1):0}function
qL(h,f,d){var
e=j[1][10][1];function
i(s,k,i){var
e=a(x[2][1][17],k),l=e[3],m=e[2],n=e[1],o=0;function
q(b){var
e=a(c[8],b);return p(A[34],d,h,f,e)}if(!g(L[24],q,o,m)){var
r=a(c[8],l);if(!p(A[34],d,h,f,r))return i}return b(j[1][10][4],n,i)}return g(ay[43],i,d,e)}var
qM=j[1][10][1];function
qN(c,a){return b(j[1][10][4],a,c)}var
qO=b(f[17][15],qN,qM);function
qP(a){return b(qQ[4],at[27],a)}function
iZ(c){var
b=c[1];return 0===b[0]?a(at[28],b[1]):b[1][1]}function
qR(b){var
c=iZ(b);return a(j[1][6],c)}var
qS=ad[2];function
qT(a){return g(qS,0,0,a)}var
ep=a(E[24],qT);function
i0(c,m){function
d(d){var
h=a(k[67][5],d),n=a(k[67][3],d),o=b(A[43],h,m),p=b(E[42][16],c,d),q=b(A[43],h,p),r=b(j[1][10][9],o,q);function
s(c){var
d=a(x[2][1][1],c);return b(j[1][10][3],d,r)}var
t=a(f[17][9],n),i=b(f[17][co],s,t)[2];if(i)var
l=a(x[2][1][1],i[1]);else
var
v=a(e[3],qU),w=a(j[1][9],c),y=a(e[3],qV),z=b(e[12],y,w),B=b(e[12],z,v),l=g(_[6],0,qW,B);return b(u[81],c,[0,l])}return a(k[67][9],d)}function
az(f,c){return Z[1]?function(h){var
d=g(C[84],0,0,h),i=a(e[3],qX),j=a(e[3],f),l=a(e[3],qY),m=b(e[12],l,j),n=b(e[12],m,i),o=b(e[12],n,d);b(M[10],0,o);function
p(i){var
c=i[1];if(c[1]===bP[29])var
d=c[3],n=c[2],o=g(C[84],0,0,h),j=s(d),p=a(e[3],qZ),u=t===j?d[1]:q===j?a(r[2],d):d,v=a(e[13],0),w=a(e[3],f),x=a(e[3],q0),y=a(e[16],n),z=a(e[3],q1),A=b(e[12],z,y),B=b(e[12],A,x),D=b(e[12],B,w),E=b(e[12],D,v),F=b(e[12],E,u),G=b(e[12],F,p),l=b(e[12],G,o);else{if(c[1]===eq[1])var
J=g(bD[2],c[2],c[3],c[4]),K=a(e[3],q3),m=b(e[12],K,J);else
var
m=a(_[15],i);var
l=m}var
H=a(e[3],q2),I=b(e[12],H,l);b(M[10],0,I);return a(k[16],0)}function
u(c){if(0===c){var
d=a(e[3],q4),h=a(e[3],f),i=b(e[12],h,d);b(M[10],0,i);return a(k[16],0)}return aj(function(c){var
d=g(C[84],0,0,c),f=a(e[3],q5),h=b(e[12],f,d);b(M[10],0,h);return[0,[0,c[1],0],c[2]]})}var
v=b(k[72][1],k[54],u),w=aj(c),x=b(k[18],w,v);return a(F(b(k[23],x,p)),h)}:c}function
bd(h,c){if(Z[1]){var
d=function(d){var
i=a(k[67][4],d),j=a(k[67][5],d),f=a(k[67][2],d),l=g(C[15],i,j,f),m=a(e[3],q6),n=b(C[76],i,j),o=a(e[3],q7),p=a(e[3],h),u=a(e[3],q8),v=b(e[12],u,p),w=b(e[12],v,o),x=b(e[12],w,n),y=b(e[12],x,m),z=b(e[12],y,l);b(M[10],0,z);function
A(l){var
c=l[1];if(c[1]===bP[29])var
f=c[3],p=c[2],u=a(k[67][2],d),v=g(C[15],i,j,u),m=s(f),w=a(e[3],q9),x=t===m?f[1]:q===m?a(r[2],f):f,y=a(e[13],0),z=a(e[3],h),A=a(e[3],q_),B=a(e[16],p),D=a(e[3],q$),E=b(e[12],D,B),F=b(e[12],E,A),G=b(e[12],F,z),H=b(e[12],G,y),I=b(e[12],H,x),J=b(e[12],I,w),n=b(e[12],J,v);else{if(c[1]===eq[1])var
N=g(bD[2],c[2],c[3],c[4]),O=a(e[3],rb),o=b(e[12],O,N);else
var
o=a(_[15],l);var
n=o}var
K=a(e[3],ra),L=b(e[12],K,n);b(M[10],0,L);return a(k[16],0)}function
B(c){if(0===c){var
d=a(e[3],rc),f=a(e[3],h),i=b(e[12],f,d);b(M[10],0,i);return a(k[16],0)}return aj(function(c){var
d=g(C[84],0,0,c),f=a(e[3],rd),h=b(e[12],f,d);b(M[10],0,h);return[0,[0,c[1],0],c[2]]})}var
D=b(k[72][1],k[54],B),E=b(k[18],c,D);return b(k[23],E,A)};return a(k[67][9],d)}return c}function
V(b,a){return g(x[1][15],c[9],b,a)}function
aH(b,a){return g(x[1][14],c[9],b,a)}var
aC=x[1][1][17],cZ=x[2][1][17],re=x[2][1][18];function
i1(a){return b(f[17][69],ah,a)}var
be=x[1][1][3],gl=x[1][1][2],bl=x[1][1][1],gm=x[2][1][3],i2=x[2][1][2];function
rf(b,a){return[0,b,a]}function
ar(c,b,a){return b?[1,c,b[1],a]:[0,c,a]}function
i3(c,b,a){return b?[1,c,b[1],a]:[0,c,a]}var
i4=x[1][7];function
dD(e,m,h){var
n=e?e[1]:0;function
j(o,d){var
h=d[2],j=d[1],p=d[4],q=d[3],r=a(c[i][4],j),e=b(x[1][1][14],r,o),k=a(bl,e),f=k?k[1]:a(m,0),s=a(be,e),t=[0,f,a(gl,e),s],u=a(x[2][1][18],t);if(n)var
g=0;else
if(a(x[1][1][6],e))var
g=0;else
var
l=h,g=1;if(!g)var
l=[0,a(c[10],f),h];return[0,[0,a(c[10],f),j],l,[0,f,q],[0,u,p]]}var
d=g(f[17][16],j,h,rg),k=d[4],l=d[1];return[0,l,a(f[17][9],d[2]),k]}function
er(d){function
e(h,f){var
d=f[2],j=f[1],e=a(cZ,h),g=e[1],k=e[2],l=b(c[i][11],d,e[3]),m=a(c[i][11],d);return[0,[0,ar([0,g],b(L[16],m,k),l),j],[0,g,d]]}return g(f[17][16],e,d,rh)}var
dE=aJ[4];function
i5(c,b){if(0===b[0]){var
d=b[1];return[0,d,a(c,b[2])]}var
e=b[2],f=b[1],g=a(c,b[3]);return[1,f,a(c,e),g]}function
c0(d,e,a){var
h=[0,d,0];function
j(f,a){var
d=a[1],g=a[2];return[0,d+1|0,[0,i5(b(c[i][3],e,d),f),g]]}return g(f[17][16],j,a,h)[2]}function
dF(e,d){function
h(a,f){var
d=a[1],g=a[2];return[0,d+1|0,[0,i5(b(c[i][3],[0,e,0],d),f),g]]}var
j=g(f[17][15],h,ri,d)[2];return a(f[17][9],j)}function
dG(l,k,j){var
e=1,g=0,d=j;for(;;){if(d){var
h=d[2],m=d[1];if(e===l){var
n=a(f[17][9],g),o=c0(0,[0,b(c[i][1],-e|0,k),0],n);return b(f[18],o,h)}var
e=e+1|0,g=[0,m,g],d=h;continue}return 0}}function
i6(m,l,k){var
e=1,g=0,d=k;for(;;){if(d){var
j=d[2],h=d[1];if(e===m){var
n=a(x[1][1][3],h),o=b(c[i][1],-e|0,l),p=[0,[1,a(x[1][1][1],h),o,n],j],q=a(f[17][9],g);return b(f[18],q,p)}var
e=e+1|0,g=[0,h,g],d=j;continue}return 0}}function
b9(b){return a(x[2][1][1],b)}var
es=x[2][9],cB=x[1][8],bm=x[1][1][14],i7=x[2][1][14],rj=x[2][7],rk=x[2][5];function
rl(g,m,l){var
e=0,d=l;for(;;){if(d){var
h=d[2],k=d[1],n=b9(k);if(b(j[1][1],n,g)){var
o=a(f[17][9],e);return b(f[18],o,h)}var
e=[0,b(i7,a(c[i][9],[0,[0,g,m],0]),k),e],d=h;continue}return 0}}function
et(a){return b(M[6],0,a)}function
af(a){return g(_[6],a[1],[0,a[2]],a[3])}function
bf(b){var
c=a(e[3],b);return g(_[6],0,0,c)}function
cC(b,a){return g(_[6],0,[0,b],a)}var
i8=_[4];function
rm(a){return b(_[14],0,a)}var
eu=N[21];function
bE(b,a){return g(_[3],0,b,a)}function
i9(h,f,e,c,d){var
a=b(o[3],h,e),i=c?[0,a[1],a[2],a[3],a[4],c[1],a[6],a[7]]:a;return g(o[22],d,f,i)}function
c1(d,c,b,a){return d6(P[4],b,0,0,0,0,0,0,d,c,a)}function
i_(d,c,b,a){return d7(P[7],b,0,0,0,0,d,c,a)}function
rn(a){return a}function
ro(a){return a}var
ja=i$[7];function
rp(c,b,a){return g(aJ[22],0,[0,a,0],[4,[0,[0,[1,c],0]],b])}function
jb(g,e,d){var
f=a(c[aE][1],d);return b(aF[11],e,f)}function
ev(d,b){var
e=gg(ct(d,b));return a(c[8],e)}function
c2(e,d){var
g=a(c[128],e),h=b(f[17][69],g,d),i=a(A[88],h);return b(f[17][69],c[bx],i)}function
aX(d,a){var
e=b(A[8],d,a);return b(f[19][15],c[8],e)}function
aD(d,b){return a(c[34],[0,d,b])}function
ew(d,c,a){return b(ac[16],c,a)}function
b_(e,d){var
a=b(c[3],e,d);return 9===a[0]?[0,a[1],a[2]]:[0,d,[0]]}function
c3(e){var
d=a(ae[6],e),g=d[1],h=b(f[17][69],c[8],d[2]);return[0,dn(g),h]}var
ex=a(c[5],rq);function
gn(d,g,e){var
h=a(ex,d),i=b(f[19][15],h,e),j=b(ex,d,g),k=b(bg[27],j,i);return a(c[8],k)}function
cD(d,g,e){var
h=a(ex,d),i=b(f[19][15],h,e),j=b(ex,d,g),k=b(iY[23],j,i);return a(c[8],k)}function
dH(d,c,b){var
a=g(ae[71],d,c,b);return[0,a[1],a[2]]}function
jc(r,i,q){var
w=x[1][2];return function(y){var
f=r,h=q,e=w,d=y;for(;;){if(0===h)return[0,e,d];var
j=g(N[29],f,i,d),a=b(c[3],i,j);switch(a[0]){case
5:var
d=a[1];continue;case
6:var
l=a[2],m=a[1],s=a[3],t=b(x[1][3],[0,m,l],e),f=b(c[aR],[0,m,l],f),h=h-1|0,e=t,d=s;continue;case
8:var
n=a[3],o=a[2],p=a[1],u=a[4],v=b(x[1][3],[1,p,o,n],e),f=b(c[aR],[1,p,o,n],f),h=h-1|0,e=v,d=u;continue;default:var
k=g(N[28],f,i,j);if(g(c[95],i,j,k))return[0,e,j];var
d=k;continue}}}}function
go(d,b){var
c=a(d,b[1]),e=c[2];b[1]=c[1];return e}function
aw(e,a,d){var
c=b(e,a[1],d),f=c[2];a[1]=c[1];return f}aB(1215,[0,b3,ec,Z,a3,fW,F,aj,n7,n8,n9,ov,ow,ox,oy,h4,h5,p3,cA,b6,eh,oV,aG,el,aS,p4,gi,ou,fX,h2,fY,qJ,cV,qE,iU,iV,qF,iW,iX,b8,gh,dC,qH,qI,qG,gj,qL,qO,gk,f$,ba,aV,f3,oM,dx,h$,a4,dq,f2,H,aK,bc,cv,ia,ib,dt,id,ie,b4,cW,ic,ig,ih,cw,cx,du,ej,ij,ii,f6,ik,iu,iv,iw,pA,f9,ix,iy,aN,av,iz,ai,ak,aq,al,pg,dv,dw,cX,f7,f8,il,po,im,io,ip,iq,ir,is,it,dr,ds,oU,f5,eg,bB,f_,pE,cy,b5,iA,gb,iB,iC,iD,bC,iE,dy,a5,aW,bO,dz,dA,ga,iF,cz,iG,pX,az,bd,iK,qa,ge,qz,em,qb,iS,iQ,iL,iM,iN,iO,iP,iT,gf,qs,qu,qw,iR,gc,gd,iI,iJ,qP,iZ,qR,ep,i0,V,aH,aC,cZ,ah,re,be,bl,gl,rf,ar,i3,i1,dD,er,c0,b9,gm,i2,i4,es,cB,bm,i7,rj,rk,rn,ro,et,af,bf,cC,i8,rm,bE,eu,dF,dG,i6,rl,i9,c1,i_,dE,ja,rp,dn,ct,jb,ev,c2,aX,aD,ew,b_,c3,gn,cD,dH,aw,go,jc,h8,gg],nj);function
jd(g,f,e){var
c=a(v[2],0),h=a(o[17],c),d=b(P[9],h,e);return p(g,c,d[1],f,d[2])}function
b$(i,f,d){return jd(function(l,d,k,j){var
f=b(c[3],d,j);if(11===f[0]){var
h=f[1];return p(i,l,d,k,[0,h[1],h[2]])}var
m=a(e[3],rr);return g(_[6],0,0,m)},f,d)}var
gp=[0,cS[52][1]];function
ca(a){gp[1]=g(cS[52][4],a[1],a[2],gp[1]);return 0}function
rs(d){try{var
c=b(cS[52][22],d,gp[1]);return c}catch(c){c=w(c);if(c===O){var
f=a(e[3],d),h=a(e[3],rt),i=b(e[12],h,f);return g(_[6],0,0,i)}throw c}}function
je(d,c,a){function
e(a){var
c=a[2];b(ey[12],a[1],c);return c}var
f=b(l[17],e,a);function
g(e){var
a=rs(e);function
c(c){return b(a,d,c)}return b(l[15],c,f)}return b(l[15],g,c)}aB(1218,[0,jd,b$,ca,je],"Ederive");function
bF(d,i){function
c(y,c){if(typeof
c==="number")return a(e[3],ru);else
switch(c[0]){case
0:var
i=c[1],k=c[2]?a(e[3],rv):a(e[7],0),l=a(j[1][9],i);return b(e[12],l,k);case
1:var
g=c[3],h=b(C[62],d,c[1]);if(a(f[17][48],g))return h;var
m=a(e[3],rw),n=bF(d,g),o=a(e[13],0),p=a(e[3],rx),q=b(e[12],p,h),r=b(e[12],q,o),s=b(e[12],r,n);return b(e[12],s,m);default:var
t=c[1],u=a(e[3],ry),v=b(C[42],d,t),w=a(e[3],rz),x=b(e[12],w,v);return b(e[12],x,u)}}var
h=a(W[9],c);return g(e[39],e[13],h,i)}function
rA(b){return et(bF(a(v[2],0),b))}function
gq(d,c){switch(c[0]){case
0:return a(a6[20],c[1]);case
1:return b(C[42],d,c[1]);default:return a(e[3],rB)}}function
jf(d,i){if(i){var
c=i[1];switch(c[0]){case
0:var
k=c[2][1],l=c[1];if(a(f[17][48],k))var
h=a(e[7],0);else
var
Y=function(c){var
f=c[2],g=c[1],h=a(e[3],rK),i=a(gs(d),f),j=a(e[3],rL),k=jg(g),l=b(e[12],k,j),m=b(e[12],l,i);return b(e[12],m,h)},Z=g(e[39],e[5],Y,k),_=a(e[13],0),$=a(e[3],rJ),aa=b(e[12],$,_),h=b(e[12],aa,Z);var
m=a(e[13],0),n=gq(d,l),o=a(e[13],0),p=a(e[3],rC),q=a(e[13],0),r=b(e[12],q,p),s=b(e[12],r,o),t=b(e[12],s,n),u=b(e[12],t,m);return b(e[12],u,h);case
1:var
v=a(j[1][9],c[1][2]),w=a(e[13],0),x=a(e[3],rD),y=a(e[13],0),z=b(e[12],y,x),A=b(e[12],z,w);return b(e[12],A,v);default:var
B=c[2],C=c[1],D=a(e[3],rE),E=a(gs(d),B),F=a(e[3],rF),G=b(e[12],F,E),H=b(e[12],G,D),I=b(e[26],1,H),J=a(e[13],0),K=a(e[3],rG),L=a(e[13],0),M=a6[20],N=function(b){return a(e[3],rH)},O=g(e[39],N,M,C),P=a(e[13],0),Q=a(e[3],rI),R=a(e[13],0),S=b(e[12],R,Q),T=b(e[12],S,P),U=b(e[12],T,O),V=b(e[12],U,L),W=b(e[12],V,K),X=b(e[12],W,J);return b(e[12],X,I)}}return a(e[7],0)}function
jg(c){var
f=c[5],k=c[4],l=c[3],m=c[1][2];if(f){var
d=f[1];if(0===d[0])var
n=d[1],o=function(b){return a(a6[9],b[2])},p=b(e[34],o,n),q=a(e[3],rM),g=b(e[12],q,p);else
var
i=d[1],x=i[1],y=b(e[34],a6[20],i[2]),z=a(a6[20],x),A=a(e[3],rO),B=b(e[12],A,z),g=b(e[12],B,y);var
h=g}else
var
h=a(e[7],0);function
r(c){var
d=a(a6[20],c),f=a(e[3],rN);return b(e[12],f,d)}var
s=b(e[34],r,k),t=a(a6[17],l),u=a(j[1][9],m),v=b(e[12],u,t),w=b(e[12],v,s);return b(e[12],w,h)}function
gr(c,a){var
d=a[2],f=jf(c,a[3]),g=bF(c,d);return b(e[12],g,f)}function
gs(a){function
c(b){return gr(a,b)}return b(e[39],e[5],c)}function
jh(d,f){var
c=f[1],n=f[2];function
m(c){switch(c[0]){case
0:var
f=c[1],h=jj(d,c[2]),i=a(e[13],0),k=gq(d,f),l=a(e[13],0),m=a(e[3],rQ),n=a(e[13],0),o=b(e[12],n,m),p=b(e[12],o,l),q=b(e[12],p,k),r=b(e[12],q,i);return b(e[12],r,h);case
1:var
s=a(j[1][9],c[1][2]),t=a(e[13],0),u=a(e[3],rR),v=a(e[13],0),w=b(e[12],v,u),x=b(e[12],w,t);return b(e[12],x,s);default:var
y=c[2],z=c[1],A=a(e[3],rS),B=a(ji(d),y),C=a(e[3],rT),D=b(e[12],C,B),E=b(e[12],D,A),F=b(e[26],1,E),G=a(e[13],0),H=a(e[3],rU),I=a(e[13],0),J=a6[20],K=function(b){return a(e[3],rV)},L=g(e[39],K,J,z),M=a(e[13],0),N=a(e[3],rW),O=a(e[13],0),P=b(e[12],O,N),Q=b(e[12],P,M),R=b(e[12],Q,L),S=b(e[12],R,I),T=b(e[12],S,H),U=b(e[12],T,G);return b(e[12],U,F)}}var
o=a(a(e[34],m),n);if(0===c[0])var
h=a(a6[20],c[1]);else
var
i=c[1],k=a6[20],l=function(b){return a(e[3],rP)},h=g(e[39],l,k,i);return b(e[12],h,o)}function
ji(a){function
c(b){return jh(a,b)}return b(e[39],e[5],c)}function
jj(h,d){var
c=d[1];if(a(f[17][48],c))return a(e[7],0);function
i(c){var
d=c[2],f=c[1],g=a(e[3],r5),i=a(ji(h),d),j=a(e[3],r6),k=jg(f),l=b(e[12],k,j),m=b(e[12],l,i);return b(e[12],m,g)}var
j=g(e[39],e[5],i,c),k=a(e[13],0),l=a(e[3],r4),m=b(e[12],l,k);return b(e[12],m,j)}function
ez(d,c){var
h=c[3],i=c[2];function
f(c){switch(c[0]){case
0:var
f=c[1],h=jj(d,c[2]),i=a(e[13],0),k=gq(d,f),l=a(e[13],0),m=a(e[3],rX),n=a(e[13],0),o=b(e[12],n,m),p=b(e[12],o,l),q=b(e[12],p,k),r=b(e[12],q,i);return b(e[12],r,h);case
1:var
s=a(j[1][9],c[1][2]),t=a(e[13],0),u=a(e[3],rY),v=a(e[13],0),w=b(e[12],v,u),x=b(e[12],w,t);return b(e[12],x,s);default:var
y=c[2],z=c[1],A=a(e[3],rZ),B=a(dI(d),y),C=a(e[3],r0),D=b(e[12],C,B),E=b(e[12],D,A),F=b(e[26],1,E),G=a(e[13],0),H=a(e[3],r1),I=a(e[13],0),J=a6[20],K=function(b){return a(e[3],r2)},L=g(e[39],K,J,z),M=a(e[13],0),N=a(e[3],r3),O=a(e[13],0),P=b(e[12],O,N),Q=b(e[12],P,M),R=b(e[12],Q,L),S=b(e[12],R,I),T=b(e[12],S,H),U=b(e[12],T,G);return b(e[12],U,F)}}var
k=a(a(e[34],f),h),l=bF(d,i);return b(e[12],l,k)}function
dI(a){function
c(b){return ez(a,b)}return b(e[39],e[5],c)}function
r7(b){return et(gr(a(v[2],0),b))}var
eA=a(y[3],r8);function
gt(d,a){var
c=b(au[26],d,a[1]);a[1]=b(j[1][10][4],c,a[1]);return c}function
eB(f,d,c,b){return a(e[7],0)}function
eC(f,d,c,b){return a(e[7],0)}function
r9(a){if(a){var
b=a[1];if(b)if(0===b[1][0])return 1}return 0}function
gu(a){function
c(a){if(a)if(0!==a[1][0])return 1;return 0}return b(f[17][22],c,a)}function
jk(g,e,f){var
d=b(c[3],g,f);switch(d[0]){case
1:return b(j[1][1],e,d[1]);case
10:var
h=a(j[17][9],d[1][1]),i=a(j[6][7],h);return b(j[1][1],e,i);default:return 0}}var
jl=a(c4[3],r_);function
eD(s,c){var
d=j[1][10][1];function
e(e,d){function
f(i,c,h){var
e=h[1];switch(e[0]){case
0:var
k=e[1];if(a(at[33],k)){var
d=a(at[35],k);if(b(j[1][13][2],d,i))return c;var
o=a(j[1][1],d);if(g(L[24],o,0,s))return c;try{var
p=b(at[32],0,d),l=a(ap[10],p);if(0===l[0])var
q=a(aF[4],l[1])?c:b(j[1][10][4],d,c),m=q;else
var
m=c;return m}catch(a){a=w(a);if(a===O)return b(j[1][10][4],d,c);throw a}}break;case
17:var
n=e[1];if(!n[1])if(!am.caml_string_notequal(n[2],r$))return c;break}function
r(b,a){return[0,b,a]}return z(bn[27],r,f,i,c,h)}var
c=f(0,j[1][10][1],d);return b(j[1][10][7],e,c)}return g(f[17][15],e,d,c)}function
jm(d,c){var
e=c[8],f=c[7],g=c[6],h=a(d,c[5]),i=b(cB,d,c[4]),j=a(d,c[3]);return[0,c[1],c[2],j,i,h,g,f,e]}function
dJ(v,u,i,c){function
k(g,d,c){var
h=a(ae[35],d[1]),i=a(ae[47],d),j=a(f[17][1],c)<i?af([0,g,sb,a(e[3],sa)]):a(f[17][1],c)===(h+i|0)?b(f[17][b2],h,c):a(f[17][1],c)===i?c:af([0,g,sd,a(e[3],sc)]);b(ey[12],g,[3,d]);var
k=a(W[6],l);return[1,d,h,b(f[17][69],k,j)]}function
l(d,c){switch(c[0]){case
0:var
g=c[1];switch(g[0]){case
1:var
l=s(dy),w=t===l?dy[1]:q===l?a(r[2],dy):dy;if(b(j[68][1],g,w))return 0;break;case
3:return k(d,g[1],0)}break;case
1:return[0,c[1],0];case
4:var
m=c[2],n=c[1],o=a(W[1],n);if(0===o[0]){var
h=o[1];switch(h[0]){case
1:var
p=s(a5),B=t===p?a5[1]:q===p?a(r[2],a5):a5;if(b(j[68][1],h,B)){var
D=a(f[17][6],m);return[2,a(f[17][5],D)]}break;case
3:return k(d,h[1],m)}}var
x=a(e[3],sg),y=b(C[42],v,n),z=a(e[3],sh),A=b(e[12],z,y);return af([0,d,si,b(e[12],A,x)]);case
13:var
E=i?i[1]:a(j[1][6],sj);return[0,gt(E,u),1]}return af([0,d,sf,a(e[3],se)])}return b(W[6],l,c)}function
cb(a){return b(c[37],a[5],a[4])}function
gv(k,E,s,i,D){var
l=s?s[1]:[0,j[1][10][1]],n=a(o[17],k),d=a(j[1][10][21],l[1]);function
F(a){return c[14]}var
p=b(f[17][69],F,d);function
G(a){return 0}var
H=b(f[17][69],G,d);try{var
P=ax(a7[3],k,n,0,2,d,p,H),q=P}catch(b){b=w(b);if(b!==O)throw b;var
q=bE(0,a(e[3],sk))}if(i){var
m=i[1][1],t=cb(m);try{var
N=ax(a7[3],k,n,[0,q],0,[0,m[2],0],[0,t,0],[0,m[7],0]),u=N}catch(b){b=w(b);if(b!==O)throw b;var
u=bE(0,a(e[3],sl))}var
x=[0,m[2],d],v=[0,t,p],r=u}else
var
x=d,v=p,r=q;function
I(d,b){return[0,d,a(c[aE][1],b)]}var
y=g(f[17][70],I,x,v),h=b(ay[35],y,k),J=0;function
K(i){var
c=b(ay[35],y,h),d=b(dK[9],c,r);b(f[17][11],d,E);try{var
g=ax(a7[7],1,h,n,[0,r],0,0,D);return g}catch(b){b=w(b);if(b===O)return bE(0,a(e[3],sm));throw b}}var
z=b(dK[14],K,J);try{if(i)var
A=i[1],B=A[1][2],L=A[2],M=function(g,c){if(4===c[0])var
f=c[1],d=c[2];else
var
f=b(W[3],g,c),d=0;function
i(g,f){if(1===f[0])if(b(j[1][1],f[1],B)){var
c=function(b,a){if(b){var
d=b[2],e=b[1];if(a){var
f=a[1],g=c(d,a[2]);return[0,dJ(h,l,f,e),g]}var
i=c(d,0);return[0,dJ(h,l,0,e),i]}return 0};return c(d,L)}var
i=a(j[1][9],B),k=a(e[3],so);return af([0,g,sp,b(e[12],k,i)])}return b(W[9],i,f)},C=b(W[9],M,z);else
var
C=[0,dJ(h,l,0,z),0];return C}catch(b){b=w(b);if(b===O)return bE(0,a(e[3],sn));throw b}}function
gw(m,l,g,d){var
c=[0,j[1][10][1]],i=[0,b(K[5],g[2],sq)],n=g[4];function
o(b){return a(x[1][1][1],b)}var
z=b(f[17][14],o,n),q=g[8];function
u(b){return a(c5[14],b)?[0,a(c5[16],b)]:0}var
A=b(f[17][69],u,q);function
r(a){var
b=[0,c];return function(c,d){return gv(m,a,b,c,d)}}function
s(d,B,u){var
k=u[1],C=u[2];if(0===k[0]){var
l=k[1],D=eD([0,g[2]],[0,l,0]);c[1]=b(j[1][10][7],c[1],D);var
E=a(bn[6],l),m=[0,E,b(r(d),[0,[0,g,z]],l)]}else{var
n=k[1],o=[0,eD(0,n)],p=function(k,c){if(typeof
c==="number")return c;else
switch(c[0]){case
0:var
d=c[1];if(0!==c[2])if(b(j[1][10][3],d,o[1]))return[0,gt(d,o),1];return c;case
1:var
e=c[3],g=c[2],h=c[1],i=a(W[6],p);return[1,h,g,b(f[17][69],i,e)];default:return c}},y=a(W[6],p),q=b(f[17][69],y,B);c[1]=b(j[1][10][7],c[1],o[1]);var
H=a(f[17][5],n),I=a(bn[6],H),J=a(r(d),0),K=b(f[17][69],J,n),M=function(b){return a(f[17][5],b)},w=b(f[17][69],M,K);if(0===q)var
i=function(c,a){if(c){var
d=c[1];if(d){var
e=d[1],f=i(c[2],a);return[0,b(W[3],0,[0,e,0]),f]}var
g=c[2];if(a){var
h=a[1];return[0,h,i(g,a[2])]}return 0}return a},x=i(A,w);else
var
x=b(f[18],q,w);var
m=[0,I,x]}var
v=m[2],F=m[1];function
G(g){switch(g[0]){case
0:var
k=g[2],l=k[2],i=g[1],n=k[1],o=t(c,n,b(f[18],l,d));switch(i[0]){case
0:var
p=i[1],q=function(a,b){return h(d,a,b)},m=b(aL[6],q,p),j=[0,m[1],[0,m[2]]];break;case
1:var
j=[0,0,[1,i[1]]];break;default:var
j=[0,0,[2,i[1]]]}var
r=j[2];return[0,r,[0,b(f[17][57],j[1],o),l]];case
1:return[1,g[1]];default:var
u=g[2],w=g[1],x=function(i){function
j(a,b){return h(d,a,b)}var
c=b(aL[6],j,i),g=c[2];if(1-a(f[17][48],c[1])){var
k=a(e[3],st);af([0,a(bn[6],g),su,k])}return g},y=function(a){return s(d,v,a)},z=b(f[17][69],y,u);return[2,b(f[17][69],x,w),z]}}return[0,F,v,b(L[16],G,C)]}function
k(j,d){var
g=d[2],i=d[1];function
l(d){switch(d[0]){case
0:var
l=d[2],m=l[2],g=d[1],p=l[1],n=b(f[18],m,j),q=t(c,p,n);switch(g[0]){case
0:var
r=g[1],s=function(a,b){return h(n,a,b)},o=b(aL[6],s,r),i=[0,o[1],[0,o[2]]];break;case
1:var
i=[0,0,[1,g[1]]];break;default:var
i=[0,0,[2,g[1]]]}var
u=i[2];return[0,u,[0,b(f[17][57],i[1],q),m]];case
1:return[1,d[1]];default:var
v=d[2],w=d[1],x=function(g){function
i(a,b){return h(j,a,b)}var
c=b(aL[6],i,g),d=c[2];if(1-a(f[17][48],c[1])){var
k=a(e[3],sr);af([0,a(bn[6],d),ss,k])}return d},y=function(a){return k(j,a)},z=b(f[17][69],y,v);return[2,b(f[17][69],x,w),z]}}return[0,i,b(L[16],l,g)]}function
t(e,c,g){function
d(c){var
d=c[1],e=d[1],h=c[2],i=e[1],l=a(j[1][8],e[2]);p(ey[17],[0,i],sw,l,sv);function
m(a){return k(g,a)}return[0,d,b(f[17][69],m,h)]}return b(f[17][69],d,c)}function
h(q,d,g){var
l=d?d[1]:jl,e=[0,0];function
h(r,l,g){var
m=l?l[1]:jl;if(12===g[0]){var
n=g[3];if(n){var
o=n[1],u=a(y[4],eA);if(b(y[9],o,u)){var
v=a(y[4],eA),w=b(y[8],v,o),d=i[1];i[1]=a(K[8],d);c[1]=b(j[1][10][4],d,c[1]);var
x=function(a){return k(q,a)},z=b(f[17][69],x,w);e[1]=[0,[0,[0,[0,m,d],0,0,0,0],z],e[1]];return a(bn[9],d)}}}var
s=b(aL[1],[0,m],g);function
t(b){function
c(a,c){return h(b,a,c)}return a(aL[6],c)}return p(bn[28],j[1][10][4],t,r,s)}var
m=h(c[1],[0,l],g);return[0,e[1],m]}return s(l,0,d)}function
eE(e,c){function
d(a){var
g=a[1];function
i(a){return h(a[2])}var
c=b(f[17][22],i,g);if(c)return c;var
j=a[2];function
d(a){return b(bn[32],e,a[2])}return b(f[17][22],d,j)}function
h(a){return b(f[17][22],g,a)}function
g(m){var
i=m[2];if(i){var
c=i[1];switch(c[0]){case
0:var
g=c[2],j=c[1];switch(j[0]){case
0:var
k=b(bn[32],e,j[1]);return k?k:d(g);case
1:return d(g);default:return d(g)}case
1:break;default:var
n=c[2],o=c[1],p=a(bn[32],e),l=b(f[17][22],p,o);return l?l:h(n)}}return 0}return d(c)}aB(1227,[0,bF,bF,rA,jf,gr,gs,ez,dI,jh,r7,r9,gu,jk,gt,eB,eC,cb,jm,eD,dJ,gv,gw,eA,eE],nW);function
jn(b,a,d){var
e=z(ad[2],0,0,b,a[1],d),c=ax(ek[5],[0,o[d8]],sy,0,sx,b,a[1],e),f=c[2];a[1]=c[1];return f}function
jo(f,d,b){var
e=s(a5),g=[0,jn(f,d,b),b],h=t===e?a5[1]:q===e?a(r[2],a5):a5,i=[0,aV(d,h),g];return a(c[21],i)}function
jp(f,d,b){var
e=s(bO),g=[0,jn(f,d,b),b],h=t===e?bO[1]:q===e?a(r[2],bO):bO,i=[0,aV(d,h),g];return a(c[21],i)}function
cc(d){switch(d[0]){case
0:return a(c[9],d[1]);case
1:var
e=d[2],g=a(c[28],d[1]),h=b(f[17][69],cc,e),i=[0,g,a(f[19][12],h)];return a(c[21],i);case
2:return d[1];default:return a(c[9],d[1])}}function
jq(g,e,d,b){switch(b[0]){case
0:return a(c[9],b[1]);case
1:var
j=b[2],k=a(c[28],b[1]),l=jr(g,e,d,j),m=[0,k,a(f[19][12],l)];return a(c[21],m);case
2:var
h=b[1];if(g)try{var
n=jo(e,d,h);return n}catch(a){return h}return h;default:var
i=b[1];return g?jp(e,d,a(c[9],i)):a(c[9],i)}}function
jr(e,d,c,a){function
g(a){return jq(e,d,c,a)}return b(f[17][69],g,a)}function
gx(a,e,d,c){var
f=a?a[1]:1,b=[0,d],g=jq(f,e,b,c);return[0,b[1],g]}function
js(a,e,d,c){var
f=a?a[1]:1,b=[0,d],g=jr(f,e,b,c);return[0,b[1],g]}function
ju(c){var
d=R[2][1];function
e(c,f){var
d=jt(f),e=b(R[2][8],d,c);if(a(R[2][2],e))return b(R[2][7],d,c);var
g=a(R[2][26],e),h=a(J[22],g),i=b(J[17],h,sz);return bf(b(J[17],sA,i))}return g(f[17][15],e,d,c)}function
jt(b){switch(b[0]){case
0:return a(R[2][5],b[1]);case
1:return ju(b[2]);case
2:return R[2][1];default:return R[2][1]}}function
dL(a){function
c(a){return[2,a]}return b(f[17][69],c,a)}function
jv(c,a){function
d(a){return dM(c,a)}return b(f[17][69],d,a)}function
dM(d,j){var
e=b(c[3],d,j);switch(e[0]){case
0:return[0,e[1]];case
9:var
i=e[2],h=e[1];if(2===i.length-1){var
k=i[2],l=s(a5),p=t===l?a5[1]:q===l?a(r[2],a5):a5;if(g(c[aQ],d,p,h))return[2,k];var
m=s(bO),u=t===m?bO[1]:q===m?a(r[2],bO):bO;if(g(c[aQ],d,u,h))return[3,b(c[66],d,k)]}if(b(c[56],d,h)){var
n=b(c[78],d,h),v=a(ae[35],n[1][1]),o=b(f[19][55],v,i),w=o[1],x=jv(d,a(f[19][11],o[2])),y=dL(a(f[19][11],w));return[1,n,b(f[18],y,x)]}break;case
12:return[1,e[1],0]}return[2,j]}function
jw(c,d,l,e){var
m=c?c[1]:[0,j[1][10][1]],t=[0,m];function
g(e){var
c=t?m:[0,j[1][10][1]];switch(e[0]){case
0:var
n=b(f[17][7],l,e[1]-1|0),o=a(x[1][1][1],n),g=b(au[29],o,c[1]);c[1]=b(j[1][10][4],g,c[1]);return[0,b(W[3],d,[0,g,0])];case
1:var
h=e[1][1],p=e[2],i=a(ae[35],h[1]),q=[1,h,i,jw([0,c],d,l,b(f[17][X],i,p)[2])];return[0,b(W[3],d,q)];case
2:var
r=c[1],s=a(j[1][6],sB),k=b(au[26],s,r);c[1]=b(j[1][10][4],k,c[1]);return[0,b(W[3],d,[0,k,1])];default:return 0}}return b(f[17][66],g,e)}function
jx(c,d,b){var
e=b[2],g=b[1],h=c?c[1]:j[1][10][1],i=jw([0,[0,h]],d,g,e);return a(f[17][9],i)}function
eF(o,n,d){var
e=[0,j[1][10][1],0];function
h(q,i){var
e=i[2],d=i[1],f=a(aC,q),g=f[3],k=f[2],h=f[1];if(h){var
l=b(au[26],h[1],d),r=[0,ar([0,l],k,g),e];return[0,b(j[1][10][4],l,d),r]}var
s=b(c[B],e,o),t=p(au[10],s,n,g,h),m=b(au[26],t,d),u=[0,ar([0,m],k,g),e];return[0,b(j[1][10][4],m,d),u]}return g(f[17][16],h,d,e)[2]}function
jy(c,b,a){return g(C[15],c,b,a)}function
dN(a,d,c){var
b=gx(0,a,d,c);return jy(a,b[1],b[2])}function
eG(d,c,b){var
h=a(f[17][9],b);function
i(a){return dN(d,c,a)}function
j(b){return a(e[3],sC)}return g(e[39],j,i,h)}function
cd(f,i,d){var
h=[0,f,a(e[7],0)];function
j(f,d){var
h=d[1],j=d[2],k=a(c[aE][2],f),l=g(C[74],h,i,k),m=a(e[6],2),n=b(e[12],j,m),o=b(e[12],n,l);return[0,b(c[aR],f,h),o]}var
k=g(x[1][11],j,d,h)[2];return b(e[25],0,k)}function
sD(a){return fW(cd,a)}function
aI(h,g,d){var
i=d[1],j=d[3],k=d[2],l=b(c[B],i,h),m=cd(h,g,i),n=cd(h,g,j),o=a(e[14],0),p=a(e[3],sE),q=a(e[14],0),r=eG(l,g,k),s=a(e[14],0),t=a(e[3],sF),u=a(e[14],0);a(f[17][48],i);var
v=b(e[12],m,u),w=b(e[12],v,t),x=b(e[12],w,s),y=b(e[12],x,r),z=b(e[24],0,y),A=b(e[12],z,q),C=b(e[12],A,p),D=b(e[12],C,o),E=b(e[12],D,n);return b(e[24],0,E)}function
sG(c,b,a){return et(aI(c,b,a))}function
sH(a){return fW(aI,a)}function
jz(g,e,d){var
h=d[3],j=d[1],l=d[2];fY(g,e,j);fY(g,e,h);var
k=b(c[B],j,g),m=[0,e,0];function
n(l,j,d){var
e=d[2],m=d[1],n=a(aC,l)[3],f=gx(sI,k,m,j),g=f[2],h=f[1];fX(k,h,g,b(c[i][4],e,n));return[0,h,[0,g,e]]}p(f[17][20],n,h,l,m);return 0}function
bh(c,h,f,d){var
k=c?c[1]:0;if(Z[1])if(!k)try{jz(h,f,d);return d}catch(c){c=w(c);if(c[1]===eq[1]){var
i=c[4];if(16===i[0]){var
j=c[2],t=g(bD[1],j,f,i[1]),u=a(e[13],0),v=aI(j,f,d),x=a(e[3],sM),y=b(e[12],x,v),z=b(e[12],y,u);return cC(sN,b(e[12],z,t))}}else
if(c[1]===bo){var
A=a(e[3],c[2]),B=a(e[3],sO),C=a(e[13],0),D=aI(h,f,d),E=a(e[3],sP),F=b(e[12],E,D),G=b(e[12],F,C),H=b(e[12],G,B);return cC(sQ,b(e[12],H,A))}if(a(i8,c)){var
l=b(_[14],0,c),m=a(e[3],sJ),n=a(e[13],0),o=aI(h,f,d),p=a(e[3],sK),q=b(e[12],p,o),r=b(e[12],q,n),s=b(e[12],r,m);return cC(sL,b(e[12],s,l))}throw c}return d}function
bp(a,f,e,d,c,b){var
g=a?a[1]:0;return bh([0,g],f,e,[0,d,c,b])}function
jA(e,d){function
g(d){switch(d[0]){case
1:var
f=d[2],g=a(e,a(c[28],d[1])),h=b(c[78],o[16],g);return[1,h,jA(e,f)];case
2:return[2,a(e,d[1])];default:return d}}return b(f[17][69],g,d)}function
bi(c,a){var
d=a[2],e=a[1],f=b(cB,c,a[3]),g=jA(c,d);return[0,b(cB,c,e),g,f]}function
eH(e,d,k,a){function
g(d,a){var
h=b(c[3],e,a);if(0===h[0]){var
j=h[1]-d|0;if(0<j)try{var
l=cc(b(f[17][7],k,j-1|0)),m=b(c[i][1],d,l);return m}catch(b){b=w(b);if(b[1]===gy)return a;throw b}return a}function
n(a){return a+1|0}return z(c[a_],e,n,g,d,a)}return g(d,a)}function
sR(e,d,a){function
c(f,a){var
c=a[1],g=a[2];return[0,c+1|0,[0,b(bm,function(a){return eH(e,c,d,a)},f),g]]}return g(f[17][16],c,a,sS)[2]}function
eI(g,d,c){switch(c[0]){case
0:var
h=c[1];try{var
i=b(f[17][7],d,h-1|0);return i}catch(a){a=w(a);if(a[1]===gy)return c;throw a}case
1:var
j=c[2],k=c[1];return[1,k,a(eJ(g,d),j)];case
2:return[2,bq(g,d,c[1])];default:var
e=b(f[17][7],d,c[1]-1|0);switch(e[0]){case
0:return[3,e[1]];case
1:throw[0,I,sT];case
2:return[2,e[1]];default:return[3,e[1]]}}}function
bq(c,b,a){return eH(c,0,b,a)}function
eJ(c,b){function
d(a){return eI(c,b,a)}return a(f[17][69],d)}function
dO(e,d,a){function
c(f,a){var
c=a[1],g=a[2];return[0,c+1|0,[0,b(bm,function(a){return eH(e,c,d,a)},f),g]]}return g(f[17][16],c,a,sU)[2]}function
S(d,c,b){return bq(d,a(f[8],c),b)}function
gz(j,e,f,d){switch(d[0]){case
0:var
h=d[1];return h===e?[2,f]:e<h?[0,h-1|0]:d;case
1:var
k=d[2],l=d[1];return[1,l,a(jB(j,e,f),k)];case
2:return[2,g(c[i][3],[0,f,0],e-1|0,d[1])];default:var
m=a(c[9],d[1]),n=g(c[i][3],[0,f,0],e-1|0,m);return[3,b(c[66],j,n)]}}function
jB(d,c,b){function
e(a){return gz(d,c,b,a)}return a(f[17][69],e)}function
gA(f,e,d){switch(d[0]){case
0:var
h=d[1];return e<=h?[0,h+f|0]:d;case
1:var
j=d[2],k=d[1];return[1,k,a(gB(f,e),j)];case
2:return[2,g(c[i][2],f,e+1|0,d[1])];default:var
l=a(c[9],d[1]),m=g(c[i][2],f,e+1|0,l);return[3,b(c[66],o[16],m)]}}function
gB(c,b){function
d(a){return gA(c,b,a)}return a(f[17][69],d)}function
cE(b,a){return gA(b,0,a)}function
bG(c,b){return a(gB(c,0),b)}function
jC(e,d,a){switch(d[0]){case
0:if(0===a[0])return d[1]===a[1]?1:0;break;case
1:if(1===a[0]){var
i=a[2],k=d[2],h=b(j[46],d[1][1],a[1][1]);if(h){var
l=function(a,b){return jC(e,a,b)};return g(f[17][23],l,k,i)}return h}break;case
2:if(2===a[0])return g(c[95],e,d[1],a[1]);break;default:if(3===a[0])return d[1]===a[1]?1:0}return 0}function
jD(n,h,m,l){var
o=l[1],d=m[1],s=l[2],t=m[2],k=n?n[1]:a(v[2],0),q=a(f[17][1],d),i=ck(q,0);function
u(f,c){var
h=c-1|0,k=aa(i,h)[h+1];if(k){var
l=k[1];if(f===l)return 0;var
g=function(c,d){var
f=a(bl,b(i4,c,d)),g=a(j[2][8],f),h=a(e[3],sV),i=a(e[16],c),k=b(e[12],i,h);return b(e[12],k,g)},n=g(f,d),p=a(e[3],sW),q=g(l,d),r=a(e[3],sX),s=g(c,o),t=a(e[3],sY),u=b(e[12],t,s),v=b(e[12],u,r),w=b(e[12],v,q),x=b(e[12],w,p),y=b(e[12],x,n),z=a(e[49],y);return a(J[3],z)}var
m=c-1|0;return aa(i,m)[m+1]=[0,f]}function
x(d,c,a){var
e=b(br[7][12],br[8],br[7][2]);return p(N[17],e,d,c,a)}var
y=b(c[B],d,k);function
r(n,m){var
d=n,c=m;for(;;){if(2===c[0])return 0;switch(d[0]){case
0:var
j=d[1];switch(c[0]){case
1:var
i=1;break;case
3:var
i=0;break;default:var
o=c[1];if(j<=q)try{var
p=u(j,o);return p}catch(b){b=w(b);if(b[1]===bo)return a(J[3],sZ);throw b}return 0}break;case
1:var
E=d[2];switch(c[0]){case
1:var
F=c[2],G=function(b,a){return r(b,a)};return g(f[17][17],G,E,F);case
3:var
i=0;break;default:var
i=1}break;case
3:var
d=[0,d[1]];continue;default:var
i=0}if(i){var
s=dN(k,h,c),t=a(e[3],s0),v=dN(k,h,d),z=a(e[3],s1),A=b(e[12],z,v),B=b(e[12],A,t),C=b(e[12],B,s),D=a(e[49],C);return a(J[3],D)}if(3===c[0]){var
c=[0,c[1]];continue}var
l=dM(h,x(y,h,d[1]));if(jC(h,d,l))return a(J[3],s2);var
d=l;continue}}g(f[17][17],r,t,s);function
z(b){return b?[0,b[1]]:a(J[3],s3)}var
A=b(f[19][15],z,i);return bp(0,k,h,d,a(f[19][11],A),o)}function
s4(d,c,b){return bq(d,a(f[8],c),b)}function
dP(c){var
d=a(f[17][1],c);return b(A[9],0,d)}function
gC(d){var
c=a(f[17][1],d);function
e(a){return[0,c-a|0]}return b(D[56],c,e)}function
s5(a){function
c(a){return[0,a+1|0]}return b(D[56],a,c)}var
s6=R[2][1];function
s7(c,a){return b(R[2][4],a,c)}var
jE=b(f[17][15],s7,s6);function
eK(e,d){var
c=b(f[17][X],e,d),a=c[2],g=c[1];if(a)return[0,g,a[1],a[2]];throw[0,bo,s8]}function
jF(g,e){var
d=0,c=g,b=e;for(;;){if(0===c){if(b){var
h=b[2],i=b[1];return[0,h,i,a(f[17][9],d)]}}else
if(b){var
d=[0,b[1],d],c=c-1|0,b=b[2];continue}throw[0,bo,s9]}}function
gD(d,c){var
e=a(f[17][1],d);function
g(a){return c+(a+1|0)|0}return a(jE,b(D[56],e-c|0,g))}function
jG(d,h){var
e=b(c[3],d,h);if(8===e[0]){var
f=s(bB),i=e[2],j=t===f?bB[1]:q===f?a(r[2],bB):bB;return g(c[aQ],d,j,i)}return 0}function
jH(d,c){var
e=R[2][1],g=1;function
h(f,c,e){return jG(d,a(be,e))?b(R[2][4],f,c):c}return p(f[17][87],h,g,e,c)}function
jI(k,j,h,e,d,g){var
l=a(aC,b(f[17][7],e,d-1|0)),n=l[3],o=l[2],p=a(c[i][1],d),m=b(L[16],p,o),q=b(c[i][1],d,n),r=m?bQ(k,j,h,e,m[1],g):R[2][1],s=bQ(k,j,h,e,q,g),t=b(R[2][7],r,s),u=a(R[2][5],d);return b(R[2][7],u,t)}function
bQ(i,h,c,l,f,e){var
j=b(A[37],c,f);if(i)if(b(R[2][3],e,j))var
m=g(eu,h,c,f),k=b(A[37],c,m),d=1;else
var
d=0;else
var
d=0;if(!d)var
k=j;var
n=R[2][1];function
o(b){var
d=jI(i,h,c,l,b,e);return a(R[2][7],d)}return g(R[2][15],o,k,n)}function
s_(h,a,e){var
d=R[2][1],j=1;function
k(d,a,f){var
j=f[3],k=b(c[i][1],-d|0,e);return g(A[38],h,k,j)?a:b(R[2][4],d,a)}return p(f[17][87],k,j,d,a)}function
jJ(h,e,d){var
j=[0,e,1,0];function
k(j,d){var
f=d[2],g=d[1],k=d[3],e=a(aC,j),l=e[3],m=e[2],n=e[1],o=a(c[9],f),q=[0,ar(n,m,p(A[51],h,g,o,l)),k];return[0,b(c[i][1],1,g),f+1|0,q]}return g(f[17][16],k,d,j)[3]}function
gE(x,w,v,d,n,e,u){var
G=x?x[1]:1,H=w?w[1]:0,I=jH(d,n),J=bQ(1,v,d,n,u,e),K=b(R[2][7],J,I),L=G?gD(n,e):a(R[2][5],e),p=b(R[2][7],L,K),M=1;function
N(a,c){if(b(R[2][3],a,p))if(a<e){var
f=e-a|0;return b(bm,function(a){var
c=b(A[37],d,a);return b(R[2][3],f,c)?g(eu,v,d,a):a},c)}return c}var
q=g(f[17][73],N,M,n),O=a(f[17][1],q),y=a(R[2][20],p),m=1,r=1,l=0,o=1,k=0,j=0,h=q,P=O-y|0;for(;;){if(h){var
z=h[2],B=h[1];if(b(R[2][3],m,p)){var
m=m+1|0,Q=[0,[0,r],j],r=r+1|0,l=[0,B,l],k=dF(a(c[9],(y+P|0)-(o-1|0)|0),k),j=Q,h=z;continue}var
m=m+1|0,l=dF(c[14],l),S=[0,[1,o],j],o=o+1|0,k=[0,B,k],j=S,h=z;continue}var
s=a(f[17][9],k),C=a(f[17][9],l),t=a(f[17][1],s),D=a(f[17][9],j),T=1,U=function(b,a){return 0===a[0]?[0,a[1]+t|0,b]:[0,a[1],b]},V=g(f[17][73],U,T,D),W=function(a){return 0===a[0]?[0,a[1]+t|0]:[0,a[1]]},E=b(f[17][69],W,D);if(H)var
X=bq(d,E,u),Y=jJ(d,b(c[i][1],-t|0,X),s),F=b(f[18],Y,C);else
var
F=b(f[18],s,C);return[0,[0,F,E,q],V]}}function
gF(m,e,o,l,t,F){var
G=t?t[1]:gD(o,l),H=bQ(1,m,e,o,F,l),p=b(R[2][7],G,H),I=1;function
J(a,c){if(b(R[2][3],a,p))if(a<l){var
d=l-a|0;return b(bm,function(a){var
c=b(A[37],e,a);return b(R[2][3],d,c)?g(eu,m,e,a):a},c)}return c}var
n=g(D[73],J,I,o),u=a(x[1][4],n),v=u-a(R[2][20],p)|0,q=ck(u,s$),d=1,s=0,r=0,h=1,k=0,j=n;for(;;){if(j){var
w=j[2],K=j[1],y=b(bm,a(c[i][1],d),K);if(b(R[2][3],d,p)){var
z=(d+v|0)-h|0;aa(q,z)[z+1]=[0,d];var
L=[0,[0,((v+d|0)-h|0)+1|0],k],d=d+1|0,s=[0,y,s],k=L,j=w;continue}var
B=h-1|0;aa(q,B)[B+1]=[0,d];var
d=d+1|0,r=[0,y,r],M=[0,[0,h],k],h=h+1|0,k=M,j=w;continue}var
C=a(D[9],k),N=b(f[18],s,r),O=a(D[9],N),P=a(f[19][11],q),Q=1,S=function(d,a){return b(bm,function(f){var
a=bq(e,C,f);return b(c[i][1],-d|0,a)},a)},E=g(D[73],S,Q,O),T=bp(0,m,e,E,C,n);return[0,T,bp(0,m,e,n,P,E)]}}function
gG(b){var
c=gC(b);return a(f[17][9],c)}function
$(a){return[0,a,gG(a),a]}function
jK(h,d,j,i){try{var
s=[0,h,1],t=function(l,o,k){var
m=k[2],f=k[1];if(m){var
h=a(be,l),i=a(be,o),n=g(c[95],d,h,i);if(n)var
j=n;else
var
y=b(P[30],d,h),B=b(P[30],d,i),j=z(N[80],0,f,d,y,B);if(0===j){var
q=g(A[hR],tb,0,d),r=a(e[49],q),s=a(c[aE][1],i),t=g(C[7],f,d,s),u=a(e[49],t),v=a(c[aE][1],h),w=g(C[7],f,d,v),x=a(e[49],w);p(fZ[3],tc,x,u,r)}return[0,b(c[aR],l,f),j]}return[0,f,m]},u=p(f[17][20],t,j,i,s)[2];return u}catch(d){d=w(d);if(d[1]===bo)return 0;var
k=a(h3[1],d),l=b(c[B],i,h),m=a(A[cR][7],l),n=a(e[49],m),o=b(c[B],j,h),q=a(A[cR][7],o),r=a(e[49],q);p(fZ[3],ta,r,n,k);throw d}}function
jL(d,c,g,f){if(jK(d,c,g[3],f[1]))return 0;var
h=aI(d,c,f),i=a(e[3],td),j=aI(d,c,g),k=a(e[3],te),l=b(e[12],k,j),m=b(e[12],l,i);return cC(tf,b(e[12],m,h))}function
an(g,f,e,c,b){var
j=b[3],k=b[2],l=c[2],m=c[1],h=g?g[1]:0,d=e?e[1]:o[16],i=Z[1],n=i?1-h:i;if(n)jL(f,d,c,b);return bp([0,h],f,d,m,a(eJ(d,l),k),j)}function
jM(e,c,a){var
d=a[2],g=a[3],h=a[1],i=b(bm,function(a){return bq(e,d,a)},c),j=[0,c,g],k=1;function
l(a){return cE(k,a)}return[0,[0,i,h],[0,tg,b(f[17][69],l,d)],j]}function
gH(d,a,c,b){function
e(c,b){return jM(a,c,b)}return bh(0,d,a,g(f[17][16],e,b,c))}function
c6(o,k,e,d,n,h){var
q=o?o[1]:0,j=cc(n),u=a(c[9],d);if(g(c[95],e,j,u))return $(h);if(p(c[i][14],e,1,d,j)){var
v=dG(d,j,h),w=function(b){var
a=b+1|0;return a===d?cE(-1,n):d<a?[0,a-1|0]:[0,a]},x=a(f[17][1],h);return bp([0,q],k,e,v,b(D[56],x,w),h)}var
l=gE(0,0,k,e,h,d,j)[1],m=l[2],y=l[3],z=l[1],r=b(f[17][7],m,d-1|0),s=0===r[0]?r[1]:bf(th),t=bq(e,m,j),A=dG(s,t,z),B=1;function
C(d,a){return gz(e,s,b(c[i][1],-1,t),a)}return bp([0,q],k,e,A,g(f[17][73],C,B,m),y)}function
ce(e,d){var
f=a(bl,b(c[nn],d,e));return a(K[10][8],f)}function
jN(d,c){var
e=b(f[17][7],c,d-1|0);return a(x[1][1][7],e)}function
eL(a){var
c=a[1],d=a[2];function
e(a){switch(a[0]){case
0:if(jN(a[1],c))return 0;break;case
3:if(jN(a[1],c))return 0;break}return[0,a]}return b(D[66],e,d)}aB(1233,[0,jo,jp,cc,gx,js,jt,ju,dL,jv,dM,jx,jy,dN,eG,cd,sD,aI,sG,sH,ce,eF,jz,bh,bp,bi,eH,sR,eI,bq,eJ,dO,S,gz,jB,gA,gB,cE,bG,jD,s4,dP,gC,s5,jE,eK,jF,gD,jG,jH,jI,bQ,s_,jJ,gE,gF,gG,$,jK,jL,an,jM,gH,c6,eL],"Context_map");function
jO(b,e){var
d=s(b),f=t===d?b[1]:q===d?a(r[2],b):b,g=[0,a(aF[10],f),e];return a(c[28],g)}function
jP(b,e){var
d=s(b),f=t===d?b[1]:q===d?a(r[2],b):b,g=[0,a(aF[8],f),e];return a(c[23],g)}function
bR(e,d,b){var
f=[0,aV(e,d),b];return a(c[21],f)}function
ti(d,c,b){return bR(d,c,a(f[19][12],b))}function
tj(f,b){var
d=b[2],e=s(ai),g=[0,d,a(c[19],[0,b[1],d,b[3]])],h=t===e?ai[1]:q===e?a(r[2],ai):ai;return bR(f,h,g)}function
eM(f,d,e,h){function
j(l,h,k,u){var
i=b(c[3],d[1],k);if(9===i[0]){var
e=i[2],m=s(ak),v=i[1],w=t===m?ak[1]:q===m?a(r[2],ak):ak;if(g(c[aQ],d[1],w,v))if(4===e.length-1){var
x=aa(e,1)[2],y=z(ad[2],0,0,l,d[1],x),f=b(c[3],d[1],y);if(6===f[0]){var
n=f[1],o=s(aq),A=f[3],B=f[2],C=t===o?aq[1]:q===o?a(r[2],aq):aq,p=s(al),D=a(c[24],[0,C,h]),E=t===p?al[1]:q===p?a(r[2],al):al,F=a(c[24],[0,E,h]),G=aa(e,3)[4],H=ah([0,n,0,B]),I=j(b(c[aR],H,l),F,G,A),J=aa(e,0)[1];return[0,[0,n,aa(e,2)[3],D,J],I]}throw[0,bo,tk]}}return[0,[0,0,k,h,u],0]}return j(f,h,e,z(ad[2],0,0,f,d[1],e))}function
eN(d,j){var
h=s(ai),k=t===h?ai[1]:q===h?a(r[2],ai):ai,e=b(c[3],d,j);if(9===e[0]){var
f=e[2],i=e[1];if(g(c[aQ],d,k,i))if(2===f.length-1){var
l=b(c[77],d,i)[2],m=aa(f,1)[2];return[0,[0,l,aa(f,0)[1],m]]}}return 0}function
tl(j,d,g){var
e=b(c[3],j,d);switch(e[0]){case
11:var
h=e[1][1];break;case
12:var
h=e[1][1][1];break;default:return[0,d,g]}var
k=a(v[28],h)[1][7],i=b(f[19][55],k,g),l=i[2];return[0,a(c[21],[0,d,i[1]]),l]}function
tm(k,d,f,e){function
l(e,w){var
x=g(N[28],k,d,w),h=b(c[3],d,x);if(9===h[0]){var
i=h[2];if(2===i.length-1){var
m=i[2],y=i[1],z=b(c[77],d,h[1])[2],f=b(c[3],d,m);if(7===f[0])var
o=f[2],p=f[1],F=f[3],G=b(c[aR],[0,p,o],k),H=[0,p,o,g(N[28],G,d,F)],j=a(c[19],H);else
var
j=m;var
A=[0,j,[0,a(c[9],e),0]],B=l(e-1|0,b(N[56],d,A)),n=s(ak),C=[0,y,j,a(c[9],e),B],D=t===n?ak[1]:q===n?a(r[2],ak):ak,u=[0,D,b(c[2][2],d,z)],v=a(dp[19],u),E=[0,a(c[8],v),C];return a(c[21],E)}}return a(c[9],e)}return l(f,e)}function
jQ(k,b,h){function
g(d){if(d){var
f=d[2],e=d[1];if(f){var
h=a(be,e),k=g(f),l=[0,a(bl,e),h,k],i=s(dw),m=[0,h,a(c[19],l)],n=t===i?dw[1]:q===i?a(r[2],dw):dw;return bR(b,n,m)}var
j=s(dv),o=[0,a(be,e)],p=t===j?dv[1]:q===j?a(r[2],dv):dv;return bR(b,p,o)}throw[0,bo,tn]}var
d=g(a(f[17][9],h)),e=s(cX),i=[0,d],j=t===e?cX[1]:q===e?a(r[2],cX):cX;return[0,d,bR(b,j,i)]}function
c7(d,h){if(h){var
e=h[2],j=h[1];if(e){var
k=a(aC,j),l=k[3],w=k[1],x=a(f[17][1],e)+1|0,y=[0,l,0],z=function(e,k){var
l=e[2],m=e[1],f=a(aC,k),g=f[3],h=a(c[19],[0,f[1],g,m]),i=s(ai),n=[0,g,h],o=t===i?ai[1]:q===i?a(r[2],ai):ai,j=bR(d,o,n),p=b(c[74],d[1],j)[1];return[0,j,[0,[0,b(c[77],d[1],p)[2],h],l]]},m=g(f[17][15],z,y,e),n=m[2],A=m[1],B=[0,a(c[9],1),2],C=function(g,f){var
e=f[2],k=f[1],l=g[1],h=b(c[i][1],e,g[2]),m=b(c[72],d[1],h)[2],j=s(ak),n=[0,m,h,a(c[9],e),k],o=t===j?ak[1]:q===j?a(r[2],ak):ak,p=[0,ev(d[1],[0,o,l]),n];return[0,a(c[21],p),e+1|0]},D=g(f[17][16],C,n,B)[1],E=[0,a(c[9],1),1,0],F=a(f[17][9],n),G=function(y,l,d){var
e=d[2],f=d[1],m=d[3],h=a(aC,l),j=s(aq),n=h[3],o=h[1],p=t===j?aq[1]:q===j?a(r[2],aq):aq,k=s(al),u=a(c[24],[0,p,f]),v=t===k?al[1]:q===k?a(r[2],al):al,w=a(c[24],[0,v,f]),x=[0,ah([0,o,[0,u],g(c[i][2],1,e,n)]),m];return[0,b(c[i][1],1,w),e+1|0,x]},o=p(f[17][20],G,F,e,E),H=o[3],J=o[1];return[0,A,[0,ah([0,w,[0,J],g(c[i][2],1,x,l)]),H],D]}var
u=a(aC,j),v=u[3],K=u[1],L=a(c[9],1),M=b(c[i][1],1,v);return[0,v,[0,ah([0,K,[0,a(c[9],1)],M]),0],L]}throw[0,I,to]}function
gI(ah,u,d,m,l){var
v=b(c[B],m,u),E=z(ad[2],0,0,v,d[1],l),F=g(N[69],v,d[1],E)[1],n=c7(d,c2(d[1],F)),h=n[2],k=n[1],G=n[3],e=a(f[17][1],h),H=aX(0,e),I=[0,b(c[i][1],e+1|0,l),H],J=a(c[21],I),K=b(c[37],J,h),M=[0,[0,a(j[1][6],tp)],k,K],w=a(c[19],M),x=s(ai),O=[0,k,w],P=t===x?ai[1]:q===x?a(r[2],ai):ai,y=s(aq),Q=bR(d,P,O),R=t===y?aq[1]:q===y?a(r[2],aq):aq,A=s(al),S=t===A?al[1]:q===A?a(r[2],al):al;function
T(b){var
c=a(aC,b),d=a(f[8],c);return a(L[7],d)}var
U=h4(b(f[17][69],T,h));function
V(d){var
e=a(f[17][5],d),g=a(f[17][6],d);return b(c[i][4],g,e)}var
W=b(f[17][14],V,U),p=b(c[i][1],e+1|0,k),X=el(1,e+1|0,h),Y=aX(0,e),Z=[0,b(c[i][1],2*(e+1|0)|0,l),Y],_=a(c[21],Z),$=b(c[37],_,X),aa=[0,[0,a(j[1][6],tq)],p,$],ab=a(c[19],aa);b(c[i][1],e,p);var
ac=a(c[9],1),C=s(ak),ae=[0,p,ab,b(c[i][1],1,G),ac],af=t===C?ak[1]:q===C?a(r[2],ak):ak,ag=bR(d,af,ae),D=b(c[38],w,m);cV(u,d,D);d[1]=a(o[bN],d[1]);return[0,k,D,m,W,R,S,ag,Q]}function
jR(b){return a(ap[41],[2,b])}function
dQ(m,i,d){var
n=a(v[28],d[1])[1],o=ct(i,d),p=a(ae[41],o),g=c2(i,b(f[17][69],c[bx],p)),q=n[7],j=a(f[17][1],g)-q|0;if(0===j)af([0,0,ts,a(e[3],tr)]);var
k=b(f[17][X],j,g)[2],r=V(0,k),s=[0,a(c[26],d),r],t=a(c[21],s),u=V(0,g),w=[0,a(c[26],d),u],l=[0,i],x=a(c[21],w),h=gI(0,m,l,k,t);return[0,l[1],h[2],h[3],x,h[7],g,j,h[1]]}function
jS(c,a){return b(P[30],c,a)}function
gJ(m,A,e,l){var
n=l[1],d=dQ(m,A,[0,n,l[2]]),p=d[7],g=d[6],q=d[1],B=d[8],C=d[5],D=d[4],E=d[3],F=d[2],f=jR(n),G=b(bs[9],m,q),h=a(o[bN],q),r=jS(h,D),H=jS(h,B),s=a4(b(K[5],f,tt),F,0,e,h,tu)[2],I=s[2],J=s[1],t=b(K[5],f,tv),M=[0,ah([0,[0,b(K[5],f,tw)],0,r]),g],u=a4(t,a(G,b(c[38],C,M)),0,e,J,tx)[2],N=u[2],O=u[1];if(e)var
w=O;else
var
X=a(v[2],0),w=a(o[17],X);var
j=aN(w,f9),k=j[1],y=b(ac[11],k,j[2]),z=a(L[7],y)[2][1],x=b(K[5],f,ty),P=[0,N,V(0,g)],Q=[0,a(c[21],P),0],R=[0,I,V(p,E)],S=[0,a(c[21],R),Q],T=dq(x,e,k,g,z,[0,r,[0,b(c[i][1],p,H),S]]),U=[0,b(at[32],0,t),0];b(gK[2][88],1,U);var
W=[0,b(at[32],0,x),0];b(gK[2][88],1,W);return T}function
tz(d,c,b,a){gJ(d,c,b,a);return 0}ca([0,tA,function(a,b){return b$(tz,a,b)}]);function
jT(h,d,j){try{var
s=i_(h,d,[0,[0,b6,0]],o[d8]),k=s[2][1],Y=s[1];b(c[76],d,k);var
t=aN(Y,f9),Z=t[1],u=a(c[21],[0,t[2],[0,j,k]]),v=p(ac[30],0,h,Z,u),x=v[2],n=v[1],_=b(c[74],d,u)[1],y=b(c[77],d,_)[2],$=[0,jP(ix,y),[0,j,k,x]],aa=a(c[21],$),ab=[0,jP(iy,y),[0,j,k,x]],ad=a(c[21],ab),ae=b(P[30],n,ad),af=[0,n,b(P[30],n,aa),ae];return af}catch(k){k=w(k);if(k===O){var
z=g(c[5],0,d,j),q=b(bH[1],h,z),m=q[1],A=q[2],l=dQ(h,d,dn(m)),i=l[1],B=l[6],D=l[5],E=l[2],F=a(e[3],tB),G=g(C[66],h,i,m),H=a(e[3],tC),I=g(C[66],h,i,m),J=a(e[3],tD),K=b(e[12],J,I),L=b(e[12],K,H),Q=b(e[12],L,G),R=b(e[12],Q,F);b(M[8],0,R);var
S=[0,ah([0,0,0,j]),B],T=b(c[38],D,S),r=b(f[17][69],c[8],A),U=b(N[56],i,[0,T,r]),V=b(P[30],i,U),W=[0,E,a(f[19][12],r)],X=a(c[21],W);return[0,i,b(P[30],i,X),V]}throw k}}function
jU(j,t,s,i,r){var
e=[0,r],h=eM(i,e,t,a(c[10],s));if(j)var
d=h;else{if(h)if(h[2])var
q=h[1],E=eM(i,e,q[2],q[3]),d=b(f[18],E,h),n=1;else
var
n=0;else
var
n=0;if(!n)var
d=h}if(j)var
l=d;else{if(d)if(d[2])var
p=d[1],D=eM(i,e,p[2],p[3]),l=b(f[18],d,D),o=1;else
var
o=0;else
var
o=0;if(!o)var
l=d}function
v(b){var
f=b[2],h=a(u[47],b[3]),d=g(c[5],0,e[1],f);return[0,g(tE[8],i,e[1],d),h]}var
w=b(f[17][69],v,l);function
x(a){return F(g(u[71],[0,a[1]],a[2],dR[6]))}var
y=j?f[17][14]:f[17][69],z=b(y,x,w),A=a(m[7],z),B=a(bP[11],e[1]),C=b(m[5],B,A);return b(k[71][1],0,C)}function
tF(s,d,p,j){function
k(o,B,n,m,l,f,k){var
e=b(c[72],d,f),p=e[3],q=[0,ah([0,e[1],0,e[2]]),0],h=[0,ah([0,n,0,p]),q],r=a(c[9],1),s=a(c[9],2),t=b(c[i][1],2,f),u=[0,b(c[i][1],2,l),t,s,r],v=[0,jO(ak,m),u],j=a(c[21],v),w=[0,b(c[i][1],2,o),[0,j]],x=a(c[21],w),y=b(c[38],x,h),z=g(c[i][2],2,2,k),A=b(c[i][5],j,z);return[0,y,b(c[37],A,h)]}function
l(i,e){var
g=b(c[3],d,e);if(6===g[0]){var
t=g[3],u=g[1],m=eN(d,g[2]);if(m){var
j=m[1],n=k(i,e,u,j[1],j[2],j[3],t),o=n[2],p=n[1],h=b(c[3],d,o);if(6===h[0]){var
q=h[2],r=h[1],v=h[3],w=b(c[72],d,p),s=l(a(f[9],w),v),x=s[1],y=a(c[18],[0,r,q,s[2]]);return[0,a(c[19],[0,r,q,x]),y]}return[0,p,o]}return[0,i,e]}return[0,i,e]}var
e=b(c[3],d,j);if(6===e[0]){var
q=e[3],r=e[1],m=eN(d,e[2]);if(m){var
h=m[1],n=k(p,j,r,h[1],h[2],h[3],q),o=l(n[1],n[2]);return[0,[0,o[1],o[2]]]}return 0}return 0}function
gL(d,h,e){function
m(z,e){var
n=eN(d,e);if(n){var
j=n[1],h=j[3],o=j[2],A=j[1];if(b(c[52],d,h))var
p=b(c[72],d,h),k=p[1],u=p[3];else
var
I=[0,h,[0,a(c[9],1)]],k=0,u=a(c[21],I);var
v=m(k,u),w=v[1],B=v[2],l=a(f[17][1],w),C=a(c[9],l+1|0),D=b(c[i][1],l+1|0,h),E=[0,b(c[i][1],l+1|0,o),D,C,B],F=[0,jO(ak,A),E],G=a(c[21],F),H=[0,ah([0,k,0,o]),0];return[0,b(f[18],w,H),G]}var
x=s(cw),J=t===x?cw[1]:q===x?a(r[2],cw):cw;if(g(c[aQ],d,J,e)){var
y=s(cx),K=b(c[77],d,e)[2],L=t===y?cx[1]:q===y?a(r[2],cx):cx;return[0,0,ev(d,[0,L,K])]}var
M=a(c[9],1);return[0,[0,ah([0,z,0,e]),0],M]}return m(h,e)}function
jV(m){function
d(d){var
n=a(k[67][3],d),o=a(k[67][4],d),h=a(k[67][5],d);function
v(b){var
d=s(cz),f=a(gm,b),i=t===d?cz[1]:q===d?a(r[2],cz):cz,e=g(c[aQ],h,i,f);if(e)return e;var
j=b9(b);return a(A[a_],j)}var
w=b(f[17][co],v,n)[1];function
x(d,v){var
w=d[3],x=d[2],y=d[1],h=a(cZ,v),i=h[3],j=h[1],k=s(ak),z=t===k?ak[1]:q===k?a(r[2],ak):ak,l=ba(y,z),m=l[2],n=l[1],A=b(c[78],n,m)[2],o=[0,i,g(c[39],j,i,w)],B=[0,a(c[10],j),x],C=[0,m,b(f[19][5],o,B)],e=s(ai),D=a(c[21],C),p=t===e?ai[1]:q===e?a(r[2],ai):ai,u=[0,a(aF[9],p),A],E=[0,a(c[26],u),o];return[0,n,D,a(c[21],E)]}var
i=aN(h,cx),y=i[2],j=aN(i[1],cw),e=g(es,x,[0,j[1],y,j[2]],w),l=e[2],B=e[3],C=p(ag[2],0,o,e[1],l)[1],D=z(u[bw],0,[0,m],l,[0,B],cA),E=a(k[65][1],C);return b(k[72][2],E,D)}return a(k[67][9],d)}function
jW(e,d,y,x){var
A=b(c[83],d,y)[2],o=b(c[83],d,x),C=o[2],D=o[1],E=a(f[17][1],A),q=b(f[17][X],E,C),r=q[2],s=a(c[34],[0,D,q[1]]),F=z(ad[2],0,0,e,d,s),G=g(N[69],e,d,F)[1];function
k(g,f,a){if(f){var
e=f[1];if(0!==e[0])return[0,e,k(g,f[2],a)];if(a){var
h=a[2],i=f[2],j=a[1],l=e[1];if(b(c[49],d,e[2])){var
m=z(ad[2],0,0,g,d,j);return[0,[0,l,m],k(g,i,h)]}return[0,e,k(g,i,h)]}}else
if(a)throw[0,I,tG];return 0}var
H=k(e,a(f[17][9],G),r),l=[0,d],m=c7(l,a(f[17][9],H)),t=m[2],h=m[1],J=m[3],K=a(f[17][9],r),L=b(c[i][4],K,J),M=[0,s,aX(0,a(f[17][1],t))],u=dC(a(c[21],M),t),n=[0,a(j[1][6],tH)],O=l[1],P=b(c[B],[0,[0,n,h],0],e),Q=z(ad[2],0,0,P,O,u),v=aN(l[1],ak),R=v[2],S=v[1],T=a(c[9],1),U=[0,R,[0,h,a(c[19],[0,n,h,Q]),T,u]],w=a(c[21],U),V=b(c[B],[0,[0,n,h],0],e);return[0,p(ag[2],0,V,S,w)[1],L,w,h]}function
jX(e,d,m,P,Z){var
aI=eK(P-1|0,m)[2],aJ=a(x[1][1][3],aI),aK=b(c[i][1],P,aJ),aL=a(G[1],P),aM=g(c[5],tI,d[1],aK),_=b(bH[2],e,aM),C=_[1],aN=_[2],aa=a(v[29],C),h=aa[2],ab=aa[1],ac=b(f[17][X],ab[6],aN),ad=ac[1],aO=b(f[18],ac[2],[0,aL,0]),aP=a(ae[5],[0,C,ad]),q=p(ae[65],e,d[1],1,aP),aQ=a(f[17][9],q),af=b(f[17][69],c[8],ad),ag=b(f[17][69],c[8],aO),t=0,H=ag,s=aQ,F=0,Q=0,E=0;for(;;){if(H){if(s){var
u=H[1],aR=s[2],aS=s[1],aT=H[2];if(b(c[44],d[1],u)){var
aU=b(A[38],d[1],u);if(b(f[17][22],aU,af))var
y=0,K=0;else{var
aW=b(A[38],d[1],u);if(b(f[17][22],aW,t))var
y=0,K=0;else
var
ah=b(c[66],d[1],u),aY=a(x[1][1][3],aS),aZ=b(A[37],d[1],aY),a0=1,a1=function(i){return function(e,c){if(c)try{var
g=b(f[17][7],i,e-1|0),h=a(L[2],g);return h}catch(a){a=w(a);if(a[1]!==gy)if(a[1]!==bo)throw a;var
d=1}else
var
d=c;return d}}(F),a2=[0,ah],a3=g(R[2][15],a1,aZ,a0)?[0,ah]:0,y=a3,K=a2}}else
var
y=0,K=0;var
aV=a(L[2],y)?E:E+1|0,t=[0,u,t],H=aT,s=aR,F=[0,y,F],Q=[0,K,Q],E=aV;continue}}else
if(!s){var
l=0,n=F,k=Q,j=t,T=E;for(;;){if(n){var
ai=n[1];if(ai){if(k)if(j){var
l=[0,[0,ai[1]],l],n=n[2],k=k[2],j=j[2];continue}}else
if(k){var
aj=k[1];if(aj){if(j){var
ak=j[2],al=k[2],U=aj[1],am=n[2],a4=[0,[0,0,Z],b(D[cq],U-1|0,m)],a5=0,a6=function(l,m){return function(e,i){var
j=a(x[1][1][3],i),k=a(c[9],m-e|0),h=1-g(A[38],d[1],k,j);return h?h:b(f[17][25],[0,e],l)}}(l,U);if(g(D[50],a6,a5,a4)){var
l=[0,[0,U],l],n=am,k=al,j=ak,T=T-1|0;continue}var
l=[0,0,l],n=am,k=al,j=ak;continue}}else
if(j){var
l=[0,0,l],n=n[2],k=k[2],j=j[2];continue}}}else
if(!k)if(!j){var
V=a(f[17][9],l),a7=$(b(f[18],q,m)),a8=[0,a7,$(b(f[18],q,m))],a9=function(i,g){var
j=i[2],f=i[1],l=f[1];if(g){var
m=g[1],n=a(c[9],1),o=S(d[1],f,n),p=a(c[9],(m+h[6]|0)+1|0),q=S(d[1],f,p),r=b(c[66],d[1],q),k=gF(e,d[1],l,r,0,o),s=k[2],t=an(0,e,[0,d[1]],k[1],f);return[0,t,an(0,e,[0,d[1]],j,s)]}return[0,f,j]},ao=g(f[17][15],a9,a8,V),ap=ao[2],aq=ao[1],a_=a(c[9],1),a$=S(d[1],aq,a_),ar=b(c[66],d[1],a$)-1|0,ba=a(f[9],ap),M=b(f[17][b2],(ar+h[6]|0)+1|0,ba),bb=a(f[8],ap),bc=b(f[17][b2],(ar+h[6]|0)+1|0,bb),bd=bG(-(h[6]+1|0)|0,bc),as=bp(0,e,d[1],m,bd,M),be=0,bf=function(i,g,f){var
j=f[2],k=f[1];if(g){var
l=g[1],m=eI(d[1],j,[0,(h[6]+1|0)-i|0]),n=a(c[9],(l+h[6]|0)+1|0),o=S(d[1],f,n),p=b(c[66],d[1],o),q=c6(tL,e,d[1],p,m,k);return an(tM,e,[0,d[1]],q,f)}return f},r=p(D[86],bf,be,V,aq),bg=a(c[9],1),bh=S(d[1],r,bg),o=b(c[66],d[1],bh)-1|0,bi=$(m),bj=a(f[8],bi),bk=bG(h[6]+1|0,bj),bl=b(f[18],q,m),bm=bp(0,e,d[1],bl,bk,m),O=an(tN,e,[0,d[1]],r,bm),at=S(d[1],O,Z),bn=a(f[7],r),au=b(f[17][X],o,bn),B=au[1],av=b(f[17][cq],h[6]+1|0,au[2]),bq=function(g){var
j=a(f[8],r),e=-h[6]|0,d=j;for(;;){if(d){var
i=d[1];if(0===i[0])if(g===i[1])return 0===b(f[17][7],B,g-1|0)[0]?[0,a(c[9],e)]:0;var
e=e+1|0,d=d[2];continue}return a(J[3],tO)}},br=function(a){return a+1|0},bs=b(D[56],o,br),bt=b(D[69],bq,bs),bu=a(f[17][9],bt),bv=function(a){return a},aw=b(D[66],bv,bu);if(0===T)var
az=at,ay=0,ax=0;else
var
bZ=a(f[8],O),b0=dO(d[1],bZ,q),b1=d[1],b3=function(a){return S(b1,O,a)},b4=b(f[17][69],b3,t),b6=[0,o+1|0,0,0,0],b7=function(b,k,j,i){var
e=b[4],f=b[3],g=b[2],d=b[1];return i?[0,d+1|0,dF(a(c[9],(o+h[6]|0)+1|0),g),f,e]:[0,d+1|0,[0,k,g],[0,j,f],[0,a(c[9],d),e]]},Y=z(D[89],b7,b6,b0,b4,V),b8=Y[4],b9=Y[3],aD=c7(d,a(f[17][9],Y[2])),aE=aD[3],aF=aD[1],b_=a(f[17][9],b9),aG=b(c[i][4],b_,aE),b$=a(f[17][9],b8),ca=b5(e,d,aF,b(c[i][4],b$,aE),aG),cb=[0,0,ca,b(c[i][1],1,at)],cc=a(c[18],cb),aH=function(a){var
e=b(c[38],a,B),g=b(c[38],e,av),h=S(d[1],as,g),i=[0,h,b(f[18],ag,aw)];return b(N[56],d[1],i)},cd=aH(aG),az=cc,ay=[0,iA(e,d,aH(aF),cd),0],ax=1;var
bw=d[1],by=function(a){return S(bw,O,a)},bz=b(f[17][69],by,af),bA=a(c[i][1],-((o+h[6]|0)+1|0)|0),bB=b(f[17][69],bA,bz),bC=b(A[14],az,B),aA=b(c[38],bC,av),bD=b(c[5],tP,d[1]),aB=b(f[17][69],bD,bB),bE=g(c[5],tQ,d[1],aA),bF=p(bH[22],C,[0,ab,h],aB,bE),bI=a(ae[5],[0,C,aB]),aC=b(ae[60],e,bI),W=$(m),bJ=W[3],bK=W[1],bL=bG(h[6]+1|0,W[2]),bM=b(f[18],q,bK),bN=bp(0,e,d[1],bM,bL,bJ),bO=an(tR,e,[0,d[1]],r,bN),bP=$(M),bQ=a(f[8],bP),bR=$(B),bS=a(f[8],bR),bT=function(g){var
j=g[5],k=a(f[19][12],g[2]),l=b(f[19][15],c[8],k),m=b(f[19][15],c[8],j),n=dn(g[1]),p=[0,a(c[28],n),l],q=a(c[21],p),s=b(c[i][1],g[3],q),t=[0,s,aX(0,g[3])],u=[0,a(c[21],t),0],v=a(f[19][11],m),w=b(f[18],v,u),x=b(f[17][69],c[bx],g[4]),y=a(f[17][9],w),z=d[1];function
A(a){return dM(z,a)}var
C=b(f[17][69],A,y),D=bG(g[3],bQ),h=b(f[18],C,D),E=dO(d[1],h,B),F=bG(o,h),G=b(f[18],bS,F),H=a(f[7],r),I=b(f[18],x,M),J=b(f[18],E,I),K=bp(0,e,d[1],J,G,H);return an(tS,e,[0,d[1]],K,bO)},bU=b(f[19][15],bT,aC),bV=function(a){return a[3]},bW=b(f[19][15],bV,aC),bX=function(e,d,b){return[0,a(c[8],e),d,b]},bY=p(f[19][60],bX,bF,bW,bU);return[0,M,aA,bY,o,as,b(f[18],aw,ay),ax]}throw[0,I,tK]}}throw[0,I,tJ]}}function
tT(d){function
f(f){var
r=a(cZ,b(E[18],f,d)),s=r[2],G=r[3],n=a(E[2],f),H=a(E[8],f),h=b(c[3],n,G);if(6===h[0])var
x=h[3],o=gL(n,h[1],h[2]),p=o[2],q=o[1],y=[0,a(c[10],d),[0,p]],A=a(c[21],y),B=b(c[i][5],p,x),C=g(N[19],H,n,B),D=b(c[37],C,q),l=[0,[0,b(c[38],A,q),D]];else
var
l=0;if(l){var
t=l[1],v=t[2],w=t[1];if(s){var
I=b(c[i][9],[0,[0,d,s[1]],0],w),J=F(z(u[bw],0,[0,d],I,[0,v],cA)),K=F(a(u[75],[0,d,0]));return g(m[5],K,J,f)}var
L=[0,a(c[aE][1],w)],M=a(bP[8],L),O=b(u[mJ],d,v),P=a(k[71][7],O);return a(b(m[9],P,M),f)}var
Q=a(j[1][9],d),R=a(e[3],tU),S=b(e[12],R,Q);return g(m[24],0,S,f)}return b(k[71][1],0,f)}function
tV(h){var
p=a(k[67][4],h),l=a(k[67][5],h),n=a(k[67][2],h),d=b(c[3],l,n);if(6===d[0]){var
u=d[2],v=d[1],P=d[3],w=function(h){var
l=gL(h,v,u),d=l[1],A=b(c[i][5],l[2],P),B=b(c[38],A,d),C=[0,B,V(0,d)],D=a(c[21],C),E=b(c[37],D,d);function
k(b){var
c=s(b),d=t===c?b[1]:q===c?a(r[2],b):b,e=a(j[67][6],d);return a(br[7][8],e)}var
w=[0,br[7][1],[0,br[7][4],0]],x=[0,k(al),w],y=[0,k(aq),x],z=a(br[7][15],y),F=g(a(N[16],z),p,h,E);function
m(h,l,d){var
b=d[2],e=d[1];if(h)return[0,[0,b,e],b];var
f=s(aq),i=t===f?aq[1]:q===f?a(r[2],aq):aq,g=s(al),j=a(c[24],[0,i,b]),k=t===g?al[1]:q===g?a(r[2],al):al;return[0,[0,j,e],a(c[24],[0,k,b])]}if(d){var
n=d[2],G=d[1];if(n)var
H=[0,0,a(c[9],1)],I=0,J=function(a,b){return m(I,a,b)},e=m(1,G,g(f[17][16],J,n,H))[1];else{a(c[9],1);var
e=[0,a(c[9],1),0]}}else{a(c[9],1);var
e=[0,a(c[9],1),0]}var
o=c1(p,h,0,F),K=o[2],L=o[1],M=[0,K,a(a8[72],e)],O=[0,v,u,a(c[21],M)];return[0,L,a(c[19],O)]};return b(cF[2],1,w)}var
o=a(e[3],tW);return b(m[66][4],0,o)}var
tX=a(k[67][9],tV);function
tY(m,l,e,j){function
d(f){var
g=a(k[67][4],f),d=jW(g,a(k[67][5],f),m,l),h=d[2],n=d[4],o=d[3],q=p(ag[2],0,g,d[1],h)[1],r=a(c[10],e),s=b(c[i][5],r,o),t=z(u[bw],0,[0,j],s,0,cA),v=z(u[bw],0,[0,e],h,[0,n],cA),w=a(k[65][1],q),x=b(k[72][2],w,v);return b(k[72][2],x,t)}return a(k[67][9],d)}function
tZ(f,h){function
d(d){var
i=a(k[67][4],d),j=a(k[67][5],d);try{var
g=jT(i,j,b(E[42][16],f,d)),n=g[3],o=g[1],p=[0,n,[0,a(c[10],f)]],q=a(c[21],p),r=z(u[bw],0,[0,h],q,0,cA),s=a(k[65][1],o),t=b(k[72][2],s,r);return t}catch(c){c=w(c);if(c===O){var
l=a(e[3],t0);return b(m[66][4],0,l)}throw c}}return a(k[67][9],d)}var
c8=[0,tT,tX,tY,function(d){function
c(c){var
e=a(k[67][4],c),f=a(k[67][5],c),g=a(i2,b(E[42][15],d,c));return jU(1,a(L[7],g),d,e,f)}return a(k[67][9],c)},tZ];aB(1241,[0,bR,ti,tj,eM,eN,tl,tm,jQ,c7,gI,jR,gJ,jT,jU,tF,dQ,jV,gL,jW,jX,c8],nT);function
gM(b){var
c=s(b);return t===c?b[1]:q===c?a(r[2],b):b}function
c9(b){return[q,function(d){var
c=gM(b);return a(aF[9],c)}]}function
jY(b){return[q,function(d){var
c=gM(b);return a(aF[10],c)}]}function
dS(b){return[q,function(d){var
c=gM(b);return a(aF[8],c)}]}var
dT=c9(bc),t1=jY(cv),t2=dS(iw),cf=c9(b4),cg=c9(dt),eO=dS(ie),t3=dS(cW),t4=dS(ic),t5=c9(bC),t6=c9(iE);function
aO(a){return dS(H(b(J[17],t7,a)))}var
eP=aO(t8),eQ=aO(t9),eR=aO(t_),eS=aO(t$),eT=aO(ua),ch=aO(ub),eU=aO(uc),eV=aO(ud),eW=aO(ue),eX=aO(uf);aO(ug);var
eY=aO(uh),uj=aO(ui),ul=aO(uk),un=aO(um),up=aO(uo),uq=c9(ai),dU=jY(ak);function
eZ(b,d){var
c=s(b),e=t===c?b[1]:q===c?a(r[2],b):b;return aV(d,[2,e])}function
cG(b,d){var
c=s(b),e=t===c?b[1]:q===c?a(r[2],b):b;return aV(d,[1,e])}function
jZ(b,d){var
c=s(b),e=t===c?b[1]:q===c?a(r[2],b):b;return aV(d,[3,e])}function
ur(a){return eZ(uq,a)}function
us(a){return jZ(dU,a)}function
ut(a){return eZ(dT,a)}function
j0(a){return jZ(t1,a)}function
j1(a){return cG(t2,a)}function
j2(a){return eZ(t5,a)}function
uu(a){return eZ(t6,a)}function
uv(a){return cG(uj,a)}function
uw(a){return cG(ul,a)}function
ux(a){return cG(un,a)}function
uy(a){return cG(up,a)}var
U=[cQ,uz,cM(0)];function
cH(f,d,l,e,k){var
g=e[2],h=e[1],m=l[1],n=b(c[B],h,f),i=aw(d7(P[4],0,0,0,0,0,0,0,n),d,g),j=a(k,i),o=b(c[B],m,f);aw(b(ag[2],0,o),d,j);return[0,[0,[0,[0,h,g],b(c[76],d[1],i)]],j]}function
j3(d,f,K,H,g,m){switch(g[0]){case
0:throw[0,I,uA];case
1:var
t=g[1],n=aw(b(o[nC],0,d),f,t),u=a(G[16],n),h=[0,u,b(j4[28],d,n)];break;case
2:var
B=g[1],r=aw(b(o[mP],0,d),f,B),C=a(G[19],r),h=[0,C,b(ae[1],d,r)];break;default:var
E=g[1],s=aw(b(o[161],0,d),f,E),F=a(G[21],s),h=[0,F,b(ae[2],d,s)]}var
v=h[2],z=a(c[8],h[1]),k=a(c[8],v),j=m;for(;;){var
e=b(c[3],f[1],k);switch(e[0]){case
5:var
k=e[1];continue;case
6:if(j){var
p=j[1],w=e[3],x=e[2];if(p){var
y=j[2],k=b(c[i][5],p[1],w),j=y;continue}var
q=x,l=1}else
var
l=0;break;case
8:var
k=b(c[i][5],e[2],e[4]);continue;default:var
l=0}if(!l)var
q=a(J[3],uB);var
A=b(N[25],f[1],q);return[0,function(d){var
e=a(L[25],d),f=b(D[69],e,m),g=[0,z,a(aY[12],f)];return a(c[21],g)},A]}}function
c_(d,c,b,a,i,h){var
e=b[2],f=b[1],g=j3(d,c,[0,f,e],a,i,h);return cH(d,c,[0,f,e],[0,a,g[2]],g[1])}var
uC=a(dV[15],j[60]);function
gN(g,f,e,d,a){var
h=b(c[B],e,g);return ax(N[84],0,0,0,h,f,d,a)?1:0}function
j5(A,d,h,f){var
j=h[1],n=f[2],q=f[1],r=h[2];if(j){var
k=j[1],e=k[2][1],s=k[1][1],t=b(o[23],d[1],e),u=a(o[5],t),v=a(l[1],s),w=b(D[cq],v,u),y=function(b){var
d=a(x[2][1][1],b);return a(c[10],d)},z=b(l[17],y,w),m=b(c[i][4],z,n);d[1]=g(o[31],e,m,d[1]);d[1]=p(ek[16],d[1],e,m,uC);return[0,q,r]}throw[0,I,uD]}function
j6(d,c,b,a){var
e=a[2],f=b[2],g=j5(d,c,b[1],a[1]);return[0,g,an(0,d,[0,c[1]],e,f)]}function
uE(j,e,a,d){var
f=d[2],h=d[1],i=g(j,e,a,[0,h,f]),k=i[1][2],l=b(c[B],h,e);a[1]=p(ag[6],l,a[1],k,f);return i}function
bS(h,f,b,a,e){var
c=g(f,b,a,e),d=c[1][1];return d?j6(b,a,c,g(h,b,a,d[1][1])):c}function
bt(c,b,a){var
d=$(a[1]);return[0,cH(c,b,a,a,function(a){return a}),d]}function
uF(h,d,c,b){function
e(c,b,i){var
a=g(h,c,b,i),f=a[1][1];if(f){var
j=f[1][1];try{var
d=[0,b[1]],k=j6(c,d,a,e(c,d,j));b[1]=d[1];return k}catch(b){b=w(b);if(b[1]===U)return a;throw b}}return a}try{var
a=e(d,c,b);return a}catch(a){a=w(a);if(a[1]===U)return bt(d,c,b);throw a}}function
dW(d,a){return b(c[aQ],d,[2,a])}function
j7(d,a){return b(c[aQ],d,[1,a])}function
bu(g,f){try{var
d=b(c[71],g,f)}catch(b){b=w(b);if(b===G[54])throw[0,U,a(e[3],uG)];throw b}return[0,d[1],d[2],d[3]]}function
j8(g,f){try{var
d=b(c[73],g,f)}catch(b){b=w(b);if(b===G[54])throw[0,U,a(e[3],uH)];throw b}return[0,d[1],d[2],d[3],d[4]]}function
bT(w,d,v,k,j,i,u){var
l=k?k[1]:0,m=j?j[1]:0,n=i?i[1]:0,o=b_(d,u),f=o[2],p=s(dT),x=o[1],y=t===p?dT[1]:q===p?a(r[2],dT):dT;if(1-a(dW(d,y),x))throw[0,U,a(e[3],uI)];var
z=aa(f,0)[1],g=aa(f,2)[3],h=aa(f,1)[2],A=l?1-gN(w,d,v,h,g):l;if(A)throw[0,U,a(e[3],uJ)];var
B=m?1-b(c[44],d,h):m;if(B)throw[0,U,a(e[3],uK)];var
C=n?1-b(c[44],d,g):n;if(C)throw[0,U,a(e[3],uL)];return[0,z,h,g]}function
e0(e,h){var
f=b_(e,h),d=f[2],g=s(dU),i=f[1],j=t===g?dU[1]:q===g?a(r[2],dU):dU;if(a(b(c[aQ],e,[3,j]),i)){var
k=aa(d,3)[4],l=aa(d,2)[3],m=aa(d,1)[2];return[0,[0,aa(d,0)[1],m,l,k]]}return 0}function
gO(m,e,d,h){var
n=h[2],f=h[1];try{var
s=[0,d[1]],I=g(m,e,s,[0,f,n]);d[1]=s[1];return I}catch(h){h=w(h);if(h[1]===U){var
i=function(a){var
h=b(c[B],f,e);return g(bs[11],h,d[1],a)},o=i(n);try{var
j=b(c[71],d[1],o),t=j[3],u=j[1],v=[0,u,i(j[2]),t],q=a(c[18],v);try{var
k=bu(d[1],q),x=k[3],y=k[1],l=bT(e,d[1],f,0,0,0,k[2]),z=l[3],A=l[1],C=i(l[2]),D=[0,A,C,i(z)],E=[0,ut(d),D],F=[0,y,a(c[21],E),x],H=a(c[18],F),r=H}catch(a){a=w(a);if(a[1]!==U)throw a;var
r=q}var
p=r}catch(a){a=w(a);if(a!==G[54])throw a;var
p=o}return g(m,e,d,[0,f,p])}throw h}}function
e1(C,B,d,z){var
D=z[2],h=z[1],E=C?C[1]:0,x=bu(d[1],D),f=x[3],y=x[2],F=x[1],H=bT(B,d[1],h,0,0,0,y),R=H[3],I=e0(d[1],H[2]),J=e0(d[1],R);if(I)if(J){var
K=J[1],j=K[4],k=K[3],l=I[1],m=l[4],n=l[3],o=l[2],p=l[1];if(g(c[i][13],d[1],1,f))try{var
M=b(c[72],d[1],o)[3];if(!g(c[i][13],d[1],1,M))throw G[54];var
N=s(eU),V=t===N?eU[1]:q===N?a(r[2],eU):eU,X=a(A[47],M),W=[1,V],Y=[0,[0,p],[0,[0,X],[0,[0,a(A[47],f)],[0,[0,n],[0,[0,k],[0,[0,m],[0,[0,j],uO]]]]]]],v=W,u=Y}catch(b){b=w(b);if(b!==G[54])throw b;if(E)throw[0,U,a(e[3],uM)];var
L=s(eV),S=t===L?eV[1]:q===L?a(r[2],eV):eV,v=[1,S],u=[0,[0,p],[0,[0,o],[0,[0,a(A[47],f)],[0,[0,n],[0,[0,k],[0,[0,m],[0,[0,j],uN]]]]]]]}else
try{var
P=b(c[72],d[1],o)[3];if(!g(c[i][13],d[1],1,P))throw G[54];var
Q=s(eW),_=t===Q?eW[1]:q===Q?a(r[2],eW):eW,ab=a(A[47],P),aa=[1,_],ac=[0,[0,p],[0,[0,ab],[0,[0,n],[0,[0,k],[0,[0,m],[0,[0,j],[0,[0,a(c[19],[0,F,y,f])],uR]]]]]]],v=aa,u=ac}catch(b){b=w(b);if(b!==G[54])throw b;if(E)throw[0,U,a(e[3],uP)];var
O=s(eX),Z=t===O?eX[1]:q===O?a(r[2],eX):eX,v=[1,Z],u=[0,[0,p],[0,[0,o],[0,[0,n],[0,[0,k],[0,[0,m],[0,[0,j],[0,[0,a(c[19],[0,F,y,f])],uQ]]]]]]]}var
T=$(h);return[0,c_(B,d,[0,h,D],h,v,u),T]}throw[0,U,a(e[3],uS)]}function
uT(a){var
b=0;return function(c,d){return e1(b,a,c,d)}}function
uU(a,b,c){return uF(uT,a,b,c)}function
j9(M,h,d,o){var
j=o[2],f=o[1],k=bu(d[1],j),l=k[3],m=k[2],p=k[1],u=bT(h,d[1],f,uV,0,0,m),n=u[1],y=u[2],v=$(f);if(g(c[i][13],d[1],1,l)){var
z=function(d){var
e=[0,p,m,b(c[i][1],1,d)];return a(c[19],e)};return[0,cH(h,d,[0,f,j],[0,f,a(A[47],l)],z),v]}var
D=a(c[19],[0,p,m,l]);try{if(b3[1]){var
x=s(eY),H=t===x?eY[1]:q===x?a(r[2],eY):eY,I=[0,j1(d),[0,n]],J=a(c[21],I),K=b(c[B],f,h),L=[0,c_(h,d,[0,f,j],f,[1,H],[0,[0,n],[0,[0,aw(b(ac[30],0,K),d,J)],[0,[0,y],[0,[0,D],uX]]]]),v];return L}throw O}catch(i){i=w(i);if(i===O){var
E=b(c[B],f,h),F=g(C[15],E,d[1],n),G=a(e[3],uW);throw[0,U,b(e[12],G,F)]}throw i}}function
j_(Y,k,d,B){var
q=B[2],n=B[1],o=0===Y?1:0,r=bu(d[1],q),C=r[2],E=r[1],Z=r[3],s=bT(k,d[1],n,0,[0,o],[0,1-o],C),F=s[3],G=s[2],_=s[1];if(o)var
t=G,p=F;else
var
t=F,p=G;var
u=b(c[66],d[1],t),$=bQ(1,k,d[1],n,p,u);if(b(R[2][3],u,$))throw[0,U,a(e[3],uY)];var
H=gF(k,d[1],n,u,0,p),h=H[1],v=h[1],aa=H[2],w=S(d[1],h,t),f=b(c[66],d[1],w),j=S(d[1],h,p),l=S(d[1],h,_),I=S(d[1],h,C),J=eK(f-1|0,v),y=J[1],K=a(x[1][1][1],J[2]),m=g(c[i][13],d[1],1,Z),ab=0===o?0===m?uy:ux:0===m?uw:uv,L=ab(d),ac=S(d[1],h,q),M=b(c[71],d[1],ac)[3];if(m)var
N=a(A[47],M),ad=b(c[37],N,y),ae=[0,K,b(c[i][1],-f|0,l),ad],O=a(c[19],ae),z=N;else
var
aq=g(c[i][2],1,f+1|0,M),ar=a(c[9],f),W=b(c[i][5],ar,aq),as=aS(1,y),at=b(c[37],W,as),au=[0,E,b(c[i][1],1-f|0,I),at],av=a(c[19],au),aw=[0,K,b(c[i][1],-f|0,l),av],O=a(c[19],aw),z=W;var
P=b(c[i][1],f,O),af=V(1,y),Q=dG(f,j,v),ag=b(D[X],f-1|0,Q)[1];if(m)var
ah=[0,b(c[i][1],-f|0,j),0],T=g(c[i][3],ah,f-1|0,z);else
var
al=[0,j0(d),[0,l,j]],am=a(c[21],al),ao=[0,b(c[i][1],-f|0,j),0],ap=[0,b(c[i][1],-f|0,am),ao],T=g(c[i][3],ap,f-1|0,z);var
ai=dM(d[1],j),aj=c6(0,k,d[1],f,ai,v),ak=an(0,k,[0,d[1]],aj,h);return[0,cH(k,d,[0,n,q],[0,Q,T],function(g){var
h=b(c[38],g,ag),e=b(c[i][1],f,h),k=m?a(c[21],[0,L,[0,l,P,j,e,w]]):a(c[21],[0,L,[0,l,j,P,e,w]]),n=b(c[i][1],1,k),o=[0,n,[0,a(c[9],1)]],p=[0,a(c[21],o),af],q=[0,E,I,a(c[21],p)],r=a(c[19],q);return S(d[1],aa,r)}),ak]}function
c$(a,d){var
e=e0(a,d),f=e?e[1][4]:d,g=b(c[83],a,f)[1];return b(c[56],a,g)}function
u0(h,d,y){var
j=y[2],f=y[1],n=bu(d[1],j),z=n[2],Q=n[3],R=n[1],o=bT(h,d[1],f,0,0,0,z),E=o[3],F=o[2],p=o[1],G=c$(d[1],F),S=G?c$(d[1],E):G;if(1-S)throw[0,U,a(e[3],u1)];try{var
T=d[1],V=b(c[B],f,h),W=g(ae[71],V,T,p)}catch(b){b=w(b);if(b===O)throw[0,U,a(e[3],u2)];throw b}var
H=a(ae[12],W),I=H[2],aa=H[1];if(a(D[48],I))return bt(h,d,[0,f,j]);var
X=[0,j2(d),[0,p]],Y=a(c[21],X),Z=b(c[B],f,h);try{aw(b(ac[30],0,Z),d,Y);var
_=1,J=_}catch(a){a=w(a);if(a!==O)throw a;var
J=0}if(J)return bt(h,d,[0,f,j]);var
K=c3(aa),M=K[2],k=K[1],m=dQ(h,d[1],k),ab=m[8],ad=m[5],af=m[2];d[1]=m[1];var
ag=a(l[9],M),u=b(c[i][4],ag,ab),ah=[0,j1(d),[0,u]],ai=a(c[21],ah),v=b(c[B],f,h);try{var
ap=aw(b(ac[30],0,v),d,ai)}catch(c){c=w(c);if(c===O){var
aj=b(C[63],v,k[1]),ak=a(e[3],u3),al=g(C[15],v,d[1],u),am=a(e[3],u4),an=b(e[12],am,al),ao=b(e[12],an,ak);throw[0,U,b(e[12],ao,aj)]}throw c}if(1-b3[1]){var
x=b(c[B],f,h),aq=a(e[3],u5),ar=a(e[3],u6),as=b(C[63],x,k[1]),at=a(e[3],u7),au=b(C[63],x,k[1]),av=a(e[3],u8),ax=a(e[13],0),ay=a(e[3],u9),az=g(C[15],x,d[1],p),aA=a(e[3],u_),aB=b(e[12],aA,az),aC=b(e[12],aB,ay),aD=b(e[12],aC,ax),aE=b(e[12],aD,av),aF=b(e[12],aE,au),aG=b(e[12],aF,at),aH=b(e[12],aG,as),aI=b(e[12],aH,ar);throw[0,U,b(e[12],aI,aq)]}var
aJ=e0(d[1],ad),aK=a(L[7],aJ)[3],aL=a(A[47],aK),aM=a(D[9],I),P=s(eS),aN=b(c[i][4],aM,aL),aO=t===P?eS[1]:q===P?a(r[2],eS):eS,aP=b(N[56],d[1],[0,af,M]),aQ=b(c[B],f,h),aR=g(bs[9],aQ,d[1],aP),aS=[0,[0,u],[0,[0,ap],[0,[0,aR],[0,[0,aN],[0,[0,F],[0,[0,E],[0,[0,a(c[19],[0,R,z,Q])],u$]]]]]]],aT=$(f);return[0,c_(h,d,[0,f,j],f,[1,aO],aS),aT]}function
va(h,d,l){var
m=l[2],f=l[1],i=bu(d[1],m),n=i[2],y=i[3],z=i[1],j=bT(h,d[1],f,0,0,0,n),o=j[3],p=j[2],k=j[1],u=c$(d[1],p),A=u?c$(d[1],o):u;if(1-A)throw[0,U,a(e[3],vb)];var
D=[0,j2(d),[0,k]],E=a(c[21],D),v=b(c[B],f,h);try{var
H=aw(b(ac[30],0,v),d,E)}catch(c){c=w(c);if(c===O){var
F=g(C[15],v,d[1],k),G=a(e[3],vc);throw[0,U,b(e[12],G,F)]}throw c}var
x=s(eP),I=t===x?eP[1]:q===x?a(r[2],eP):eP,J=[0,[0,k],[0,[0,H],[0,[0,p],[0,[0,o],[0,[0,a(c[19],[0,z,n,y])],vd]]]]],K=$(f);return[0,c_(h,d,[0,f,m],f,[1,I],J),K]}function
j$(i,d,m){var
j=m[2],h=m[1];try{var
y=function(a){var
e=b(c[B],h,i);return g(bs[11],e,d[1],a)},n=function(h){var
b=b_(d[1],h),c=b[2],f=b[1],g=s(ch),i=t===g?ch[1]:q===g?a(r[2],ch):ch,j=1-a(j7(d[1],i),f),k=j||1-(8===c.length-1?1:0);if(k)throw[0,U,a(e[3],ve)];return[0,f,c]};try{var
M=n(j)[2],f=M,p=j}catch(a){a=w(a);if(a[1]!==U)throw a;var
o=y(j),f=n(o)[2],p=o}var
k=aa(f,0)[1],z=aa(f,1)[2],l=aa(f,2)[3],u=aa(f,3)[4],v=aa(f,4)[5],A=aa(f,6)[7],C=aa(f,7)[8],D=[0,ur(d),[0,k,l]],E=a(c[21],D),F=[0,us(d),[0,k,l,u,v]],G=[0,E,a(c[21],F)],H=[0,j0(d),G],I=a(c[21],H);if(1-gN(i,d[1],h,C,I))throw[0,U,a(e[3],vf)];var
x=s(eT),J=t===x?eT[1]:q===x?a(r[2],eT):eT,K=$(h),L=[0,c_(i,d,[0,h,p],h,[1,J],[0,[0,k],[0,[0,z],[0,[0,l],[0,[0,u],[0,[0,v],[0,[0,A],vg]]]]]]),K];return L}catch(a){a=w(a);if(a[1]===U)return bt(i,d,[0,h,j]);throw a}}function
gP(a,b,c){return bS(va,u0,a,b,c)}function
ka(h,d,l){var
m=l[2],f=l[1],i=bu(d[1],m),n=i[2],E=i[3],F=i[1],j=bT(h,d[1],f,0,0,0,n),o=j[3],p=j[2],k=j[1],u=c$(d[1],p),G=c$(d[1],o),H=u||G;if(1-H)throw[0,U,a(e[3],vh)];var
I=[0,uu(d),[0,k]],J=a(c[21],I),v=b(c[B],f,h);try{var
M=aw(b(ac[30],0,v),d,J)}catch(c){c=w(c);if(c===O){var
K=g(C[15],v,d[1],k),L=a(e[3],vi);throw[0,U,b(e[12],L,K)]}throw c}if(u)var
x=s(eR),N=t===x?eR[1]:q===x?a(r[2],eR):eR,y=[1,N];else
var
D=s(eQ),Y=t===D?eQ[1]:q===D?a(r[2],eQ):eQ,y=[1,Y];var
z=j3(h,d,[0,f,m],f,y,[0,[0,k],[0,[0,M],[0,[0,p],[0,[0,o],[0,[0,a(c[19],[0,F,n,E])],vj]]]]]),A=z[2],P=z[1],Q=$(f);try{var
W=b(c[B],f,h),X=[0,[0,0,a(P,aw(b(ac[30],0,W),d,A))],Q];return X}catch(i){i=w(i);if(i===O){var
R=d[1],S=b(c[B],f,h),T=g(C[15],S,R,A),V=a(e[3],vk);throw[0,U,b(e[12],V,T)]}throw i}}function
kb(n,d,m){var
h=m[2],f=m[1],j=bu(d[1],h),k=j[3],l=j[2],o=j[1],p=s(cg),w=t===p?cg[1]:q===p?a(r[2],cg):cg;if(1-a(dW(d[1],w),l))throw[0,U,a(e[3],vl)];var
u=$(f);if(g(c[i][13],d[1],1,k)){var
x=function(d){var
e=[0,o,l,b(c[i][1],1,d)];return a(c[19],e)};return[0,cH(n,d,[0,f,h],[0,f,a(A[47],k)],x),u]}var
v=s(eO),y=a(c[19],[0,o,l,k]),z=t===v?eO[1]:q===v?a(r[2],eO):eO;return[0,c_(n,d,[0,f,h],f,[1,z],[0,[0,y],vm]),u]}function
kc(u,d,j){var
k=j[1],f=bu(d[1],j[2]),h=f[3],l=f[2],m=s(cf),v=f[1],w=t===m?cf[1]:q===m?a(r[2],cf):cf;if(1-a(dW(d[1],w),l))throw[0,U,a(e[3],vn)];var
x=$(k);if(g(c[i][13],d[1],1,h))var
y=a(A[47],h),o=y,n=cG(t3,d);else
var
C=a(c[19],[0,v,l,h]),o=C,n=cG(t4,d);var
p=a(c[21],[0,n,[0,o]]),z=b(c[B],k,u);aw(b(ag[2],0,z),d,p);return[0,[0,0,p],x]}function
e2(E,C,i,d,n){var
o=n[2],j=n[1],p=s(ch),F=b_(d[1],o)[1],G=t===p?ch[1]:q===p?a(r[2],ch):ch;if(a(j7(d[1],G),F))return 0;var
k=bu(d[1],o)[2],u=s(cf),H=t===u?cf[1]:q===u?a(r[2],cf):cf;if(a(dW(d[1],H),k))return 3;var
v=s(cg),I=t===v?cg[1]:q===v?a(r[2],cg):cg;if(a(dW(d[1],I),k))return 2;var
l=bT(i,d[1],j,0,0,0,k),f=l[3],h=l[2],J=l[1];function
x(b,a){return mD(b,a)?0:1}if(C){if(b(c[44],d[1],h))if(b(c[44],d[1],f)){var
K=b(c[66],d[1],f);return[1,x(b(c[66],d[1],h),K)]}if(b(c[44],d[1],h))return vo;if(b(c[44],d[1],f))return vp;throw[0,U,a(e[3],vq)]}function
m(f,e){var
a=b(c[66],d[1],f),g=bQ(1,i,d[1],j,e,a);return 1-b(R[2][3],a,g)}if(b(c[44],d[1],h))if(b(c[44],d[1],f))if(m(h,f)){var
L=b(c[66],d[1],f);return[1,x(b(c[66],d[1],h),L)]}if(b(c[44],d[1],h))if(m(h,f))return vr;if(b(c[44],d[1],f))if(m(f,h))return vs;function
M(a){var
e=b(c[83],d[1],a)[1];try{var
f=g(c[5],0,d[1],e);b(bH[1],i,f);var
h=1;return h}catch(a){a=w(a);if(a===O)return 0;throw a}}function
y(a){var
e=b(c[83],d[1],a)[1],f=b(c[B],j,i),h=g(bs[11],f,d[1],e);return b(c[56],d[1],h)}if(M(J))if(y(h))if(y(f))return[2,[0,[0,E,2],0]];if(gN(i,d[1],j,h,f))return vt;function
z(e,i){function
a(e){if(g(c[95],d[1],e,i))throw A[28];var
f=b(c[83],d[1],e),j=f[2],h=b(c[56],d[1],f[1]);return h?b(D[11],a,j):h}try{a(e);var
f=0;return f}catch(a){a=w(a);if(a===A[28])return 1;throw a}}if(!z(h,f))if(!z(f,h))throw[0,U,a(e[3],vu)];return 1}function
da(b,e,d,a,c){var
f=a[1];try{var
h=g(b,d,a,c);return h}catch(b){b=w(b);if(b[1]===U){a[1]=f;return g(e,d,a,c)}throw b}}function
kd(b,f,d,a,c){var
e=a[1];try{var
j=g(b,d,a,c);return j}catch(b){b=w(b);if(b[1]===U){var
h=b[2];a[1]=e;try{var
i=g(f,d,a,c);return i}catch(b){b=w(b);if(b[1]===U){a[1]=e;throw[0,U,h]}throw b}}throw b}}var
ke=[cQ,vv,cM(0)];function
vw(f,b,d){var
e=s(aW),h=j8(b[1],d[2])[2],i=t===e?aW[1]:q===e?a(r[2],aW):aW;if(g(c[aQ],b[1],i,h))throw ke;return bt(f,b,d)}function
vx(h,e,d){var
f=d[2],a=d[1],g=j8(e[1],f),j=g[4],k=g[2],l=$(a);function
m(a){return a}return[0,cH(h,e,[0,a,f],[0,a,b(c[i][5],k,j)],m),l]}function
vy(i,d,f){var
j=f[2],k=f[1];try{b(c[73],d[1],j);var
h=bt(i,d,f);return h}catch(h){h=w(h);if(h===G[54])try{var
l=b(c[B],k,i),m=g(N[28],l,d[1],j);b(c[71],d[1],m);throw[0,U,a(e[3],vz)]}catch(a){a=w(a);if(a===G[54])return bt(i,d,f);throw a}throw h}}function
gQ(c,b,a){function
d(a){var
b=0;return function(c,d){return e1(b,a,c,d)}}return da(gP,function(a,b,c){return bS(gQ,d,a,b,c)},c,b,a)}function
kf(d){if(typeof
d==="number")switch(d){case
0:return j$;case
1:return ka;case
2:return kb;default:return kc}else
switch(d[0]){case
0:var
f=d[1];return function(a,b,c){return j9(f,a,b,c)};case
1:var
w=d[1],h=function(h,d,x){var
l=x[2],f=x[1],i=0===w?1:0,m=bu(d[1],l),r=m[2],y=m[3],z=m[1],n=bT(h,d[1],f,0,[0,i],[0,1-i],r),s=n[3],t=n[2],u=n[1];if(i)var
j=t,o=s;else
var
j=s,o=t;var
k=b(c[66],d[1],j),A=a(c[21],[0,u,[0,o]]),C=bQ(0,h,d[1],f,A,k);if(b(R[2][3],k,C)){var
D=d[1],E=b(c[B],f,h),p=g(N[28],E,D,u),F=d[1],G=b(c[B],f,h),q=g(N[28],G,F,o),H=a(c[21],[0,p,[0,q]]),I=bQ(1,h,d[1],f,H,k);if(b(R[2][3],k,I))throw[0,U,a(e[3],uZ)];var
J=function(a){return a},v=b_(d[1],r)[1],K=i?a(c[21],[0,v,[0,p,j,q]]):a(c[21],[0,v,[0,p,q,j]]),L=a(c[18],[0,z,K,y]),M=$(f);return[0,cH(h,d,[0,f,l],[0,f,L],J),M]}return bt(h,d,[0,f,l])},i=function(a,b,c){return j_(w,a,b,c)};return function(a,b,c){return bS(i,h,a,b,c)};default:var
j=e3(d[1]),k=function(a,b,c){return bS(j,gP,a,b,c)};return function(a,b,c){return bS(j$,k,a,b,c)}}}function
vA(e){var
a=e[2],b=e[1];function
c(e,d,c,a){try{var
f=g(e,d,c,a);return f}catch(a){a=w(a);if(a[1]===U)return af([0,b,vB,a[2]]);throw a}}function
d(d){function
a(c,b,a){return g(kf(g(d,c,b,a)),c,b,a)}function
b(b,c,d){return bS(a,uU,b,c,d)}function
c(c,d,e){return da(a,b,c,d,e)}return function(a,b,d){return gO(c,a,b,d)}}function
f(a){var
b=d(a);return function(a,d,e){return c(b,a,d,e)}}if(typeof
a==="number")switch(a){case
0:var
i=function(a,b,c){return e1(vC,a,b,c)},j=0,k=d(function(a,c,d){return e2(b,j,a,c,d)}),l=function(a,b,c){return kd(k,i,a,b,c)},m=function(a,b,c){return gO(gQ,a,b,c)},n=function(a,b,c){return da(m,l,a,b,c)};return function(a,b,d){return c(n,a,b,d)};case
1:var
o=1;return f(function(a,c,d){return e2(b,o,a,c,d)});default:var
h=function(f,e,c){var
a=0,g=d(function(c,d,e){return e2(b,a,c,d,e)});function
i(a,b,c){return gO(gQ,a,b,c)}function
j(a,b,c){return da(i,g,a,b,c)}function
k(a,b,c){return da(vw,j,a,b,c)}try{var
l=bS(function(a,b,c){return da(vy,h,a,b,c)},k,f,e,c);return l}catch(a){a=w(a);if(a===ke)return vx(f,e,c);throw a}},p=function(a,b,c){return e1(vD,a,b,c)},q=function(a,b,c){return kd(h,p,a,b,c)};return function(a,b,d){return c(q,a,b,d)}}var
r=a[1];return f(function(c,b,a){return r})}function
e3(c){var
a=b(l[19],vA,c);return a?g(l[20],bS,a[1],a[2]):bt}function
gR(j){function
d(d){var
m=a(k[67][4],d),n=a(ay[46],m),o=a(k[67][3],d);function
p(b){var
c=a(x[2][1][1],b);return a(A[a_],c)}var
e=b(D[co],p,o),q=e[1],r=b(c[b2],e[2],n),f=er(q),h=f[1],s=f[2],t=dD(vF,function(a){throw[0,I,vE]},h)[2],u=a(k[67][2],d),v=b(c[i][11],s,u);function
w(e){var
d=[0,e],f=g(e3(j),r,d,[0,h,v])[1][2],k=a(l[9],t),m=b(c[i][4],k,f);return[0,d[1],m]}return b(cF[2],1,w)}return a(k[67][9],d)}function
vP(d){var
c=d[2];if(typeof
c==="number")switch(c){case
0:return a(e[3],vQ);case
1:return a(e[3],vR);default:return a(e[3],vS)}var
b=c[1];if(typeof
b==="number")switch(b){case
0:return a(e[3],vG);case
1:return a(e[3],vH);case
2:return a(e[3],vI);default:return a(e[3],vJ)}else
switch(b[0]){case
0:return 0===b[1]?a(e[3],vK):a(e[3],vL);case
1:return 0===b[1]?a(e[3],vM):a(e[3],vN);default:return a(e[3],vO)}}var
dX=b(e[39],e[13],vP);aB(1245,[0,U,j5,uE,bS,j9,j_,gP,ka,kb,kc,bt,kf,e2,e3,gR,dX],"Simplify");function
bI(d,h){var
i=d?d[1]:0,c=a(f[17][9],h);if(c){var
e=c[1],k=c[2],l=i?b(K[5],e,vT):e,m=function(d,c){var
e=a(j[1][8],c),f=b(K[5],d,vU);return b(K[5],f,e)};return g(f[17][15],m,l,k)}throw[0,I,vV]}function
kg(f,e){var
c=f,a=e;for(;;){if(c){var
g=c[2],h=c[1];if(a){var
i=a[2],d=b(j[1][2],h,a[1]);if(0===d){var
c=g,a=i;continue}return d}return-1}return a?1:0}}var
ao=a(f[21][1],[0,kg]),vW=ao[1],vX=ao[2],vY=ao[3],vZ=ao[4],v0=ao[5],v1=ao[6],v2=ao[7],v3=ao[8],v4=ao[9],v5=ao[10],v6=ao[11],v7=ao[12],v8=ao[13],v9=ao[14],v_=ao[15],v$=ao[16],wa=ao[17],wb=ao[18],wc=ao[19],wd=ao[20],we=ao[21],wf=ao[22],wg=ao[23],wh=ao[24];function
aT(b){return a(c[34],[0,b[1][5],b[3]])}function
kh(a){switch(a[0]){case
0:return a[1];case
1:return a[1];case
2:return a[1];case
3:return a[1];default:return a[1]}}function
wi(f){var
c=j[1][9];function
d(b){return a(e[3],wj)}return b(e[39],d,c)}function
ki(f,e){var
c=f,a=e;for(;;){if(c){if(a){var
g=a[2],h=c[2],d=b(j[1][1],c[1],a[1]);if(d){var
c=h,a=g;continue}return d}}else
if(!a)return 1;return 0}}function
bU(a){return a[1][2]}function
gS(a){return cb(a[1])}function
e4(a){return a[1][4]}function
kj(a){return a[1][7]}function
wk(a){return a[1][6]}function
kk(a){return a[1][5]}function
bJ(a){return a[1][1][2]}function
e5(a){function
c(a){var
b=a[7],c=[0,aT(a)];return ar([0,bJ(a)],c,b)}return b(f[17][69],c,a)}function
wl(a){return gS(a[1])}function
gT(f){var
g=f[6];if(g){var
h=g[1];if(0===h[0]){var
c=h[1];if(typeof
c==="number")var
d=a(e[3],wm);else
if(0===c[0]){var
l=c[1];if(l)var
s=a(e[16],l[1][1]),t=a(e[3],wo),m=b(e[12],t,s);else
var
m=a(e[3],wp);var
d=m}else{var
n=c[1];if(n)var
u=a(e[16],n[1][1]),v=a(e[3],wq),o=b(e[12],v,u);else
var
o=a(e[3],wr);var
d=o}var
i=d}else
var
i=a(e[3],ws);var
k=i}else
var
k=a(e[3],wt);var
p=a(e[3],wn),q=a(j[1][9],f[2]),r=b(e[12],q,p);return b(e[12],r,k)}function
ci(i,d,h,k){var
m=h?h[1]:0;function
n(b){return m?b:a(e[7],0)}function
o(d,c,b){return eG(d,c,a(f[8],b))}function
l(h){switch(h[0]){case
0:var
r=h[4],y=h[2],p=h[1],D=h[3],E=a(f[7],p),q=b(c[B],E,i),F=function(c){try{var
s=l(c[1][4]),t=a(e[5],0),u=a(e[3],wx),v=a(e[3],wy),w=a(e[16],c[6]),x=a(e[3],wz),y=a(e[3],wA),z=g(C[15],i,d,c[1][1][5]),A=a(e[3],wB),B=a(e[3],wC),D=aT(c),E=g(C[15],i,d,D),F=a(e[3],wD),G=a(e[13],0),H=a(e[3],wE),I=bI(0,c[4]),J=a(j[1][9],I),K=a(e[3],wF),L=gT(c[1][1]),M=a(e[3],wG),N=wl(c),O=g(C[15],i,d,N),P=a(e[3],wH),Q=b(e[12],P,O),R=b(e[12],Q,M),S=b(e[26],1,R),T=g(C[15],q,d,c[7]),U=b(e[12],T,S),V=b(e[12],U,L),W=b(e[12],V,K),X=b(e[12],W,J),Y=b(e[12],X,H),Z=b(e[12],Y,G),_=b(e[12],Z,F),$=b(e[12],_,E),aa=b(e[12],$,B),ab=b(e[12],aa,A),ac=b(e[12],ab,z),ad=b(e[12],ac,y),ae=b(e[12],ad,x),af=b(e[12],ae,w),ag=b(e[12],af,v),ah=b(e[12],ag,u),ai=b(e[12],ah,t),aj=b(e[12],ai,s),f=aj}catch(b){var
f=a(e[3],wu)}var
h=a(e[3],wv),k=bJ(c),m=a(j[1][9],k),n=a(e[3],ww),o=b(e[12],n,m),p=b(e[12],o,h),r=b(e[12],p,f);return b(e[26],2,r)},G=g(e[39],e[5],F,y),H=e5(y),s=b(c[B],H,q),I=aI(i,d,p),J=a(e[3],wI),K=n(b(e[12],J,I)),L=a(e[5],0);if(0===r[0])var
M=r[1],N=g(C[15],s,d,D),O=a(e[3],wJ),P=n(b(e[12],O,N)),Q=g(C[15],s,d,M),R=a(e[3],wK),T=o(q,d,p),U=b(e[12],T,R),V=b(e[12],U,Q),z=b(e[12],V,P);else
var
Y=ce(s,r[1]),Z=a(e[3],wL),_=o(q,d,p),$=b(e[12],_,Z),z=b(e[12],$,Y);var
W=b(e[12],z,L),X=b(e[12],W,G);return b(e[12],X,K);case
1:var
t=h[1],aa=h[4],ab=h[3],ac=h[2],ad=a(f[7],t),u=b(c[B],ad,i),ae=a(e[7],0),af=function(f,c){if(c)var
d=l(c[1]);else
var
h=a(e[5],0),i=a(e[3],wM),d=b(e[12],i,h);var
g=b(e[23],2,d);return b(e[12],f,g)},ag=g(f[19][17],af,ae,aa),ah=a(e[13],0),ai=aI(i,d,t),aj=a(e[3],wN),ak=g(C[15],u,d,ab),al=a(e[3],wO),am=b(e[12],al,ak),an=b(e[12],am,aj),ao=b(e[12],an,ai),ap=n(b(e[12],ao,ah)),aq=a(e[5],0),ar=ce(u,ac),as=a(e[3],wP),at=o(u,d,t),au=b(e[12],at,as),av=b(e[12],au,ar),aw=b(e[12],av,aq),ax=b(e[12],aw,ap);return b(e[12],ax,ag);case
2:var
ay=h[1],az=l(h[2]),aA=a(e[5],0),aB=aI(i,d,ay),aC=a(e[3],wQ),aD=b(e[12],aC,aB),aE=b(e[12],aD,aA),aF=b(e[12],aE,az);return b(e[26],2,aF);case
3:var
aG=h[2],aH=l(h[4]),aJ=a(e[5],0),aK=a(j[1][9],aG),aL=a(e[3],wR),aM=b(e[12],aL,aK),aN=b(e[12],aM,aJ),aO=b(e[12],aN,aH);return b(e[26],2,aO);default:var
k=h[2],v=h[1],w=k[8],A=k[7],x=k[1],aP=h[3],aQ=k[10],aR=k[3],aS=k[2],aU=x[3],aV=x[2],aW=x[1],aX=a(f[7],v),m=b(c[B],aX,i),aY=l(aP),aZ=a(e[14],0),a0=aI(i,d,k[9]),a1=a(e[3],wS),a2=a(e[13],0),a3=aI(i,d,A),a4=a(e[3],wT),a5=a(e[13],0),a6=aR[2],a7=a(f[7],w),a8=ce(b(c[B],a7,i),a6),a9=a(e[3],wU),a_=a(e[13],0),a$=a(e[13],0),ba=a(f[7],w),bb=b(c[B],ba,i),bc=g(C[15],bb,d,aQ),bd=a(e[3],wV),be=aI(i,d,w),bf=a(e[3],wW),bg=k[6],bh=b(C[15],m,d),bi=g(e[39],e[13],bh,bg),bj=a(e[3],wX),bk=g(C[15],m,d,k[5]),bl=a(e[3],wY),bm=a(e[13],0),bn=aI(i,d,v),bo=a(e[3],wZ),bp=a(e[3],w0),bq=g(C[15],m,d,aS),br=a(e[3],w1),bs=g(C[15],m,d,aU),bt=a(e[3],w2),bu=b(e[12],bt,bs),bv=b(e[12],bu,br),bw=b(e[12],bv,bq),bx=b(e[12],bw,bp),by=b(e[12],bx,bo),bz=b(e[12],by,bn),bA=b(e[12],bz,bm),bB=b(e[12],bA,bl),bC=b(e[12],bB,bk),bD=b(e[12],bC,bj),bE=b(e[12],bD,bi),bF=b(e[12],bE,bf),bG=b(e[12],bF,be),bH=b(e[12],bG,bd),bK=b(e[12],bH,bc),bL=b(e[12],bK,a$),bM=b(e[12],bL,a_),bN=b(e[12],bM,a9),bO=b(e[12],bN,a8),bP=b(e[12],bO,a5),bQ=b(e[12],bP,a4),bR=b(e[12],bQ,a3),bS=b(e[12],bR,a2),bT=b(e[12],bS,a1),bU=b(e[12],bT,a0),bV=n(b(e[12],bU,aZ)),bW=a(e[14],0),bX=S(d,A,aV),bY=g(C[15],m,d,bX),bZ=a(e[3],w3),b0=a(j[1][9],aW),b1=a(e[3],w4),b2=o(m,d,v),b3=b(e[12],b2,b1),b4=b(e[12],b3,b0),b5=b(e[12],b4,bZ),b6=b(e[12],b5,bY),b7=b(e[12],b6,bW),b8=b(e[12],b7,bV),b9=b(e[12],b8,aY);return b(e[26],2,b9)}}try{var
p=l(k);return p}catch(b){return a(e[3],w5)}}function
kl(c,b,a){function
d(a){return ci(c,b,0,a[4])}return g(e[39],e[5],d,a)}function
w6(d){var
c=a(v[2],0),f=ci(c,a(o[17],c),0,d);return b(e[48],h1[9][1],f)}function
km(c,d){var
f=d[6];if(0===f[0])var
e=f[1],i=a(c,e[3]),j=a(c,e[2]),h=[0,[0,a(c,e[1]),j,i]];else
var
g=f[1],h=[1,[0,g[1],g[2]]];var
k=d[5],l=a(c,d[4]),m=b(cB,c,d[3]),n=b(cB,c,d[2]);return[0,bi(c,d[1]),n,m,l,k,h]}function
e6(d,c){var
e=a(d,c[5]),f=gU(d,c[4]),g=c[3];function
h(a){return km(d,a)}var
i=b(L[16],h,g),j=bi(d,c[2]);return[0,jm(d,c[1]),j,i,f,e]}function
gU(c,d){function
g(d){switch(d[0]){case
0:var
h=d[4],j=d[3],k=d[2],l=d[1];if(0===h[0]){var
m=h[1],n=function(d){var
e=a(c,d[7]),g=d[6],h=d[5],i=d[4],j=b(f[17][69],c,d[3]),k=d[2];return[0,e6(c,d[1]),k,j,i,h,g,e]},o=b(f[17][69],n,k),p=bi(c,l),q=[0,a(c,m)];return[0,p,o,a(c,j),q]}var
r=bi(c,l);return[0,r,k,a(c,j),h];case
1:var
s=d[4],t=d[3],u=d[2],v=bi(c,d[1]),w=a(L[16],g),x=b(f[19][15],w,s);return[1,v,u,a(c,t),x];case
2:var
y=d[2],z=bi(c,d[1]);return[2,z,g(y)];case
3:var
A=d[3],B=d[2],C=d[1],D=g(d[4]),E=km(c,A);return[3,bi(c,C),B,E,D];default:var
e=d[2],F=d[3],G=bi(c,d[1]),i=e[1],H=i[3],I=i[2],J=i[1],K=e[6],M=g(F),N=a(c,e[10]),O=bi(c,e[9]),P=bi(c,e[8]),Q=bi(c,e[7]),R=b(f[17][69],c,K),S=a(c,e[5]),T=e[4],U=e[3],V=a(c,e[2]),W=a(c,H);return[4,G,[0,[0,J,a(c,I),W],V,U,T,S,R,Q,P,O,N],M]}}return g(d)}function
gV(d){var
a=d[6];if(a){var
b=a[1];if(0===b[0]){var
c=b[1];if(typeof
c==="number")return 1;else
if(0!==c[0])return 1}}return 0}function
gW(D,n,u,m){function
T(a){return 1-gV(a[1])}var
h=b(f[17][61],T,m);function
U(f){var
b=f[1][6];if(b){var
c=b[1];if(0===c[0]){var
a=c[1];if(typeof
a==="number")var
e=0;else
if(1===a[0])var
e=0;else{var
d=a[1];if(d)return[0,d[1][1]];var
e=1}}}return 0}var
W=b(f[19][54],U,h),X=a(f[17][1],h),o=a(f[17][1],m)-X|0,z=a(f[17][1],h),l=0,d=0,k=m;a:for(;;){if(k){var
s=k[2],F=k[1],G=F[2],e=F[1],H=e[6];if(H){var
J=H[1];if(0===J[0]){var
B=J[1];if(typeof
B==="number"){var
ax=aX(0,a(f[17][1],h)),ay=[0,a(u,G),ax],l=l+1|0,d=[0,[0,1,a(c[21],ay)],d],k=s;continue}else
if(0!==B[0]){var
K=B[1],aA=K?K[1][1]:a(f[17][1],e[4])-1|0,aB=function(a){return cb(a[1])},aC=b(f[17][69],aB,s),q=a(f[17][1],e[4]),Y=aX(q+(o-1|0)|0,a(f[17][1],h)),Z=[0,a(u,G),Y],_=a(c[21],Z),v=(o-1|0)-l|0,$=b(c[i][1],1,_),ac=ck(1,[0,e[2]]),ad=ck(1,cb(e)),ae=b(A[9],q+1|0,l),an=[0,a(c[9],q+1|0),0],x=[0,b(f[18],ae,an),0],w=0,r=aC,ab=[0,ck(1,aA),0];for(;;){var
E=x[2],y=x[1];if(w===v){var
ao=[0,b(c[i][1],v,$),y],ap=a(c[34],ao),aq=[0,ap,V(v,e[4])],ar=a(c[21],aq),as=b(c[38],ar,E),at=b(c[38],as,e[4]),au=function(a){return[0,0,c[14]]},av=b(f[17][56],o-1|0,au),aw=a(c[31],[0,ab,[0,ac,ad,ck(1,at)]]),l=l+1|0,d=[0,[0,1,b(c[38],aw,av)],d],k=s;continue a}if(r){var
af=r[2],ag=r[1],ah=[0,a(c[9],q+o|0),y],ai=a(c[34],ah),aj=[0,[1,[0,a(j[1][6],w7)],ai,ag],E],ak=[0,a(c[9],1),0],al=a(c[i][1],1),am=b(f[17][69],al,y),x=[0,b(f[18],am,ak),aj],w=w+1|0,r=af;continue}throw[0,I,w8]}}}}var
az=[0,[0,0,a(c[9],z)],d],z=z-1|0,d=az,k=s;continue}var
aD=a(f[17][9],d),aF=function(a){return a[1]},L=b(f[17][30],aF,aD),aG=L[2],aH=L[1],aI=0,aJ=function(d,b){return[0,a(c[34],[0,d[2],b]),b]},aK=g(f[17][16],aJ,aH,aI),aL=0,aM=function(b,d){var
e=[0,d,a(f[17][9],b)];return[0,a(c[34],e),b]},aN=g(f[17][15],aM,aL,aK),aO=b(N[18],D,n[1]),M=b(f[17][14],aO,aN),aP=function(a){return a[2]},aQ=b(f[17][69],aP,aG),aR=function(g){var
d=g[1],h=g[2],j=[0,d[2]],k=cb(d),e=d[4],l=a(f[19][12],M),m=a(f[19][12],aQ),o=b(f[19][5],m,l),p=a(u,h),q=cD(n[1],p,o),r=V(0,e),s=a(f[17][1],e),t=b(c[i][1],s,q),v=cD(n[1],t,r),w=aS(1,e);return[0,j,k,b(c[38],v,w)]},aT=b(f[17][69],aR,h),C=a(f[17][mM],aT),aU=C[2],aV=C[1],aW=a(f[19][12],C[3]),aY=a(f[19][12],aU),t=[0,a(f[19][12],aV),aY,aW],aZ=function(a){return gV(a[1])},O=b(f[17][30],aZ,m),P=t[3],Q=t[2],R=n[1],a0=O[2],a1=O[1],a2=t[1],a3=function(a,k,d){if(a)return[0,a[1],0];var
e=b(A[66],R,d),h=g(c[92],R,e,d)[1],i=0;function
j(a,b){return a}return g(f[17][73],j,i,h)},a4=p(f[19][60],a3,W,P,Q),a5=b(f[19][15],c[aE][1],Q),a6=[0,a2,a5,b(f[19][15],c[aE][1],P)],a7=a(f[19][11],a4),S=p(gX[2],0,D,a7,a6),a8=function(d,e){var
b=e[1],f=e[2],g=aa(S,d)[d+1],h=[0,b[1],b[2],b[3],b[4],b[5],[0,[0,[0,[0,[0,g,0]]]]],b[7],b[8]];return[0,h,f,a(c[31],[0,[0,S,d],t])]},a9=b(f[17][13],a8,a0),a_=function(a,b){return[0,a[1],a[2],b]};return[0,a9,g(f[17][70],a_,a1,M)]}}function
w9(e,d,i,h,g){var
b=p(P[58],w_,i,e,h),j=b[3],k=i9(b[1],d,b[2],[0,g],e),l=[0,d,a(f[19][12],j)];return[0,k,a(c[12],l)]}function
cj(j,l,h){function
m(d,n,aw){var
j=aw;for(;;)switch(j[0]){case
0:var
v=j[4],E=j[3],Q=j[2],R=j[1],w=R[1];if(0===v[0]){var
az=v[1],aA=function(b,a){var
c=a[3],d=a[2],e=a[1],f=b[7],g=[0,aT(b)];return[0,e,d,[0,ar([0,bJ(b)],g,f),c]]},F=g(f[17][16],aA,Q,[0,d,n,w]),T=F[3],U=F[2],aB=F[1],aC=b(c[38],az,T);return[0,U,aC,b8(aB,U,E,T)]}var
aD=v[2],aF=v[1];if(a(f[17][48],Q)){var
W=s(b4),aG=t===W?b4[1]:q===W?a(r[2],b4):b4,Y=ba(n,aG),_=m(d,Y[1],[1,R,aF,Y[2],aD]),$=s(cW),aH=_[2],aI=_[1],aJ=t===$?cW[1]:q===$?a(r[2],cW):cW,aa=ba(aI,aJ),G=aa[1],aK=aa[2],aL=[0,aK,[0,E,cD(G,aH,V(0,w))]],aM=a(c[21],aL),aO=b(c[38],aM,w);return[0,G,aO,b8(d,G,E,w)]}throw[0,I,w$];case
1:var
ab=j[3],y=j[2],ac=j[1],A=ac[1],aP=j[4],af=aN(n,aW),ah=af[2],H=af[1],aQ=b(c[i][1],1,ab),aR=[0,0,ah,z(ad[2],0,0,d,H,ah),aQ],h=[0,H],k=jX(d,h,A,y,a(c[20],aR)),ai=k[7],aj=k[5],D=k[1],aS=k[6],aU=k[4],aV=k[3],aX=k[2],aY=ai?b(e3(xa),d,h):function(a){return bt(d,h,a)},aZ=function(r,q){var
P=r[3],v=g(c[92],l[1],r[2],r[1]),Q=v[2],j=g(au[20],d,l[1],v[1]),R=b(f[18],j,D),w=b(c[B],R,d),y=a(jc(w,l[1],aU),Q),z=y[2],k=y[1];if(ai)var
T=h[1],U=b(c[B],k,w),s=g(bs[11],U,T,z);else
var
s=z;if(Z[1]){var
W=b(f[18],j,D),A=b(f[18],k,W),Y=a(e[3],xb);b(M[6],0,Y);var
_=b(c[B],A,d),$=g(C[15],_,h[1],s);b(M[6],0,$);var
aa=a(e[3],xc);b(M[6],0,aa);var
ab=cd(d,h[1],A);b(M[6],0,ab);var
ad=a(e[3],xd);b(M[6],0,ad);var
ae=a(c[d8],d),af=a(c[aE][4],ae),ag=g(C[75],d,h[1],af);b(M[6],0,ag)}var
ah=b(f[18],j,D),E=a(aY,[0,b(f[18],k,ah),s]),F=E[1],t=F[2],G=F[1],aj=E[2];if(Z[1]){var
ak=a(e[3],xe);b(M[10],0,ak);var
al=b(f[18],j,D),am=b(f[18],k,al),ao=b(c[B],am,d),ap=g(C[15],ao,h[1],t);b(M[6],0,ap)}var
aq=an(xf,d,[0,h[1]],P,ac),ar=an(xg,d,[0,h[1]],aj,aq);if(G)if(q){var
H=q[1],I=G[1],K=I[2],as=I[1][1],L=m(d,h[1],H),n=L[1],at=L[2],N=jD([0,d],n,ar,kh(H)),av=V(0,a(f[9],N)),aw=S(n,N,cD(h[1],at,av)),ax=b(o[24],n,K[1]),az=a(o[5],ax),aA=function(b){var
d=a(x[2][1][1],b);return a(c[10],d)},aB=b(f[17][69],aA,az),aC=a(f[17][1],as),O=b(f[17][X],aC,aB),aD=O[2],aF=O[1],aG=a(ay[10],d),aH=function(c,b){return[0,a(x[2][1][1],c),b]},aI=g(f[17][70],aH,aG,aD),aJ=b(c[i][9],aI,aw),aK=b(c[i][4],aF,aJ);h[1]=g(o[31],K[1],aK,n);var
u=t,p=1}else
var
p=0;else
if(q)var
p=0;else
var
u=t,p=1;if(!p)var
u=a(J[3],xh);var
aL=b(f[18],k,j);return b(c[38],u,aL)},a0=g(f[19][58],aZ,aV,aP),a1=S(h[1],aj,aX),a2=h[1],a3=function(a){return S(a2,aj,a)},a4=b(f[19][15],a3,a0),K=function(k){var
d=k;for(;;){var
e=b(c[3],h[1],d);if(8===e[0]){var
f=e[2],j=s(aW),l=e[4],m=t===j?aW[1]:q===j?a(r[2],aW):aW;if(jb(h[1],m,f)){var
d=b(c[i][5],f,l);continue}}return g(c[fS],h[1],K,d)}},a5=K(a1),a6=b(f[19][15],K,a4),a7=eK(y-1|0,A)[2],a8=a(x[1][1][3],a7),a9=b(c[i][1],y,a8),a_=a(c[9],y),ak=g(ae[72],d,h[1],a9),al=ak[1],a$=ak[2],bb=g(ae[76],d,al[1],4),bc=[0,ct(h[1],al),a$],bd=a(ae[5],bc),be=ax(ae[77],d,h[1],bd,bb,a5,a_,a6),bf=[0,be,a(f[19][12],aS)],bg=a(c[21],bf),bh=b(c[38],bg,A),am=b8(d,H,ab,A),ao=b(P[30],h[1],bh);h[1]=p(ag[6],d,h[1],ao,am);return[0,h[1],ao,am];case
2:var
ap=j[1],aq=ap[1],bi=ap[2],L=m(d,n,j[2]),u=L[1],bj=L[3],bk=L[2],bl=js(xi,d,u,bi)[2],as=a(f[19][72],bl),bm=a(c[21],[0,bk,as]),bn=b(N[25],u,bm),bo=b(c[38],bn,aq);return[0,u,bo,b8(d,u,gn(u,bj,as),aq)];case
3:var
j=j[4];continue;default:var
O=j[2],at=j[1][1],bp=O[6],bq=O[5],br=O[2],av=m(d,n,j[3])[1],bu=a(c[34],[0,bq,bp]),bv=b(c[38],bu,at);return[0,av,bv,b8(d,av,br,at)]}}var
d=m(j,l[1],h),k=d[3],n=d[2];l[1]=d[1];return[0,n,k]}function
gY(g,k,a,j,h){var
d=gW(k,a,function(c){return b(j,a,c)},h),l=d[2],m=d[1];function
n(c){var
b=c[1],f=c[2],d=a4(b[2],c[3],[0,b[3]],g[1],a[1],xj),e=d[2],h=e[2],i=d[1];a[1]=e[1];p(c5[26],0,[1,i],0,[0,b[7],0]);return[0,b,f,h]}var
e=b(f[17][69],n,m);function
o(a){return a[3]}var
q=b(f[17][14],o,e);function
r(e){var
d=e[1],j=e[2],k=d[3],l=b(c[i][4],q,e[3]),f=a4(d[2],l,[0,k],g[1],a[1],xk),h=f[2],m=h[2],n=f[1];a[1]=h[1];p(c5[26],0,[1,n],0,[0,d[7],0]);return[0,d,j,m]}return[0,e,b(f[17][69],r,l)]}function
kn(v,n,d,r,h,m){if(m){var
e=m[1],w=cj(v,n,h)[1],o=$(e[2]),s=e[6];if(0===s[0]){var
K=s[1],L=V(0,a(f[7],o)),M=[0,cD(n[1],w,L)],N=a(c[21],[0,K[1],M]),O=3===h[0]?h:[3,o,d[2],e,h],P=a(f[7],o);return[0,d,r,m,O,b(c[38],N,P)]}var
x=s[1],y=V(0,e[2]),Q=cD(n[1],w,y),z=b(D[X],e[5],e[3]),R=z[1],A=b(D[X],x[2],z[2]),S=A[2],T=A[1],U=a(f[19][11],y),W=a(f[17][9],U),Y=function(a){return c[14]},Z=b(f[17][69],Y,T),B=b(f[17][57],Z,W),_=c0(0,B,R),aa=g(c[i][3],B,e[5],e[4]),j=[0,d[1],d[2],d[3],_,aa,d[6],d[7],d[8]],k=3===h[0]?h:[3,o,d[2],e,h],C=d[6];if(C){var
E=C[1];if(0===E[0]){var
l=E[1];if(typeof
l==="number")var
p=0;else
if(1===l[0])if(l[1])var
p=0;else{switch(k[0]){case
1:var
J=k[2],I=k[1],q=1;break;case
3:var
t=k[4];if(1===t[0])var
J=t[2],I=t[1],q=1;else
var
q=0;break;default:var
q=0}if(q)var
ab=x[2],ac=a(f[7],I),H=[1,[0,[0,(a(f[17][1],ac)-J|0)-ab|0,0]]];else
var
H=l;var
F=H,p=1}else
var
p=0;if(!p)var
F=l;var
G=[0,j[1],j[2],j[3],j[4],j[5],[0,[0,F]],j[7],j[8]],u=1}else
var
u=0}else
var
u=0;if(!u)var
G=j;return[1,G,r,e,k,S,Q]}return[0,d,r,m,h,cj(v,n,h)[1]]}function
e7(n,d,m,s,A){var
t=s?s[1]:0;function
C(a){return kn(n,d,a[1],a[2],a[3],a[4])}var
o=b(f[17][69],C,A);if(o){var
h=o[1];if(0===h[0])if(!o[2]){var
l=h[1],R=h[4],S=h[3],T=h[2],w=g(N[18],n,d[1],h[5]);if(t){var
x=a4(l[2],w,[0,l[3]],m[1],d[1],xq),y=x[2],U=y[2],V=x[1];d[1]=y[1];p(c5[26],0,[1,V],0,[0,l[7],0]);a(bb[7],l[2]);var
z=U}else
var
z=w;return[0,[0,l,T,S,R,z],0]}}function
D(d){if(0===d[0]){var
f=d[1],g=a(e[3],xl),h=a(j[1][9],f[2]),k=a(e[3],xm),l=b(e[12],k,h),m=b(e[12],l,g);return af([0,[0,f[1]],xn,m])}var
n=d[5],o=d[4],p=d[3],q=d[2],r=d[1];return[0,r,[0,q,p,o,n,b(c[i][1],1,d[6])]]}var
k=b(f[17][69],D,o);if(t){if(1<a(f[17][1],k)){var
E=function(a){return gV(a[1])};if(b(f[17][22],E,k))var
r=0;else
var
L=function(e){var
a=e[2],f=a[4],g=e[1],i=a[3],j=a[2],k=a[1],l=b(c[38],a[5],f),n=d[1],o=m[1],h=a4(b(K[5],g[2],xp),l,0,o,n,xo)[2],p=h[2];d[1]=h[1];return[0,g,[0,k,j,i,f,p]]},M=b(f[17][69],L,k),O=function(f,b){var
d=b[5],e=[0,d,aH(0,b[4])];return a(c[34],e)},q=gY(m,a(v[2],0),d,O,M),r=1}else
var
r=0;if(!r)var
F=function(d,a){return b(c[38],a[5],a[4])},q=gY(m,a(v[2],0),d,F,k)}else
var
P=a(f[17][5],k)[2][4],Q=b(c[B],P,n),q=gW(Q,d,function(a){return a[5]},k);var
G=q[2],H=q[1];function
u(k){var
i=k[2],r=i[4],h=i[3],e=i[2],a=k[1],u=i[1],v=b(c[38],k[3],r),w=g(N[18],n,d[1],v),m=a[6],o=e[6];if(m){var
p=m[1];if(0===p[0]){var
s=p[1];if(0===o[0])var
j=0;else
var
q=[0,e[1],e[2],e[3],e[4],e[5],[1,[0,s,o[1][2]]]],t=3===h[0]?[3,h[1],h[2],q,h[4]]:h,l=[0,q,t],j=1}else
var
j=0}else
var
j=0;if(!j)var
l=[0,e,h];var
x=l[2],y=l[1],z=a[8],A=a[7],B=a[6],C=a[5],D=b(f[18],a[4],r);return[0,[0,a[1],a[2],a[3],D,C,B,A,z],u,[0,y],x,w]}var
I=b(f[17][69],u,H),J=b(f[17][69],u,G);return b(f[18],I,J)}function
db(h,g,f,e,d,c,b){var
a=e7(h,g,f,0,[0,[0,e,d,c,b],0]);if(a)if(!a[2])return a[1];throw[0,bo,xr]}function
dc(e,a){var
c=a[3],d=a[2],g=a[1];function
h(a){if(1===a[0]){var
c=a[1];if(c){var
d=c[1],g=a[3];try{var
h=[1,[0,d],b(f[17][31],d,e),g];return h}catch(b){b=w(b);if(b===O)return a;throw b}}}return a}return[0,b(f[17][69],h,g),d,c]}function
xs(e,h,d){function
i(d,a){var
f=b(c[B],d,e);p(ag[2],0,f,h,a);return 0}function
m(f,d,a){var
g=b(c[B],f,e);p(ag[6],g,h,d,a);return 0}function
k(q){var
d=q;for(;;)switch(d[0]){case
0:var
l=d[4],n=d[3],o=d[1],r=d[2];bh(0,e,h,o);var
C=function(g,d){var
j=d[1];i(0,gS(j));bh(0,e,h,j[2]);var
n=j[3];if(n){var
l=n[1];i(0,b(c[38],l[4],l[3]));var
o=l[6];if(0===o[0])i(0,o[1][1])}k(j[4]);i(g,d[7]);var
p=d[7];m(g,a(c[34],[0,d[1][5],d[3]]),p);var
q=a(f[17][1],g);if(d[6]===q){var
r=d[7],s=[0,aT(d)];return[0,ar([0,bJ(d)],s,r),g]}throw[0,I,xt]},D=a(f[7],o),p=g(f[17][15],C,D,r);i(p,n);if(0===l[0])m(p,l[1],n);return 0;case
1:var
j=d[1],s=d[4],t=d[3],u=d[2];bh(0,e,h,j);var
v=a(f[7],j),w=b(c[B],v,e);b(c[nn],u,w);i(a(f[7],j),t);var
x=a(L[13],k);return b(f[19][13],x,s);case
2:var
y=d[2];bh(0,e,h,d[1]);var
d=y;continue;case
3:var
z=d[4];bh(0,e,h,d[1]);var
d=z;continue;default:var
A=d[3];bh(0,e,h,d[1]);var
d=A;continue}}return k(d)}function
ko(l,j,c,m,h){if(a(o[20],c[1]))throw[0,I,xu];var
i=[0,0];function
e(j,h,c){switch(c[0]){case
0:var
k=c[4],n=c[3],p=c[2],q=c[1];if(0===k[0]){var
C=k[1],D=function(c,j){var
d=j[3],k=c[1],w=j[4],x=j[2],y=j[1],z=c[4],A=dc(d,k[2]),B=k[4];function
g(a){switch(a[0]){case
0:var
c=a[4],e=a[3],h=a[2];return[0,dc(d,a[1]),h,e,c];case
1:var
i=a[4],j=a[3],k=a[2],l=a[1],m=function(a){return b(L[16],g,a)},n=b(f[19][15],m,i);return[1,dc(d,l),k,j,n];case
2:var
o=a[1],p=g(a[2]);return[2,dc(d,o),p];case
3:var
q=a[3],r=a[2],s=a[1],t=g(a[4]);return[3,dc(d,s),r,q,t];default:var
u=a[2],v=a[1],w=g(a[3]);return[4,dc(d,v),u,w]}}var
n=e(y,x,g(B)),C=n[2],D=n[1],p=a(v[2],0),q=[0,b(o[nF],D,p)],h=db(p,q,l,k[1],A,C,k[3]),E=h[5],F=q[1],G=l[1],r=a4(bI([0,m],z),E,0,G,F,xv),s=r[2],H=s[2],I=s[1];i[1]=[0,[0,r[1],xw],i[1]];var
t=a(v[2],0),J=b(o[nF],I,t),u=[0,[0,h[1],h[2],h[3],h[4],H],c[2],c[3],c[4],c[5],c[6],c[7]],K=aT(u);return[0,t,J,[0,[0,bJ(c),K],d],[0,u,w]]},r=g(f[17][16],D,p,[0,j,h,0,0]);return[0,r[2],[0,q,r[4],n,[0,C]]]}return[0,h,[0,q,p,n,[1,k[1],k[2]]]];case
1:var
E=c[4],F=c[3],G=c[2],H=c[1],I=function(b,a){if(a){var
c=e(j,b,a[1]);return[0,c[1],[0,c[2]]]}return[0,b,0]},s=g(a8[64],I,h,E);return[0,s[1],[1,H,G,F,s[2]]];case
2:var
J=c[1],t=e(j,h,c[2]);return[0,t[1],[2,J,t[2]]];case
3:var
K=c[3],M=c[2],N=c[1],u=e(j,h,c[4]);return[0,u[1],[3,N,M,K,u[2]]];default:var
d=c[2],O=c[1],w=e(j,h,c[3]),x=w[2],y=[0,w[1]],z=cj(a(v[2],0),y,x),P=z[1],Q=y[1],R=l[1],S=[0,z[2]],A=a4(bI([0,m],d[4]),P,S,R,Q,xx),B=A[2],T=B[2],U=B[1];i[1]=[0,[0,A[1],d[3]],i[1]];return[0,U,[4,O,[0,d[1],d[2],d[3],d[4],T,d[6],d[7],d[8],d[9],d[10]],x]]}}var
d=e(j,c[1],h),k=d[2];c[1]=d[1];return[0,i[1],k]}function
kp(e,j,d,c,i){var
k=c?c[1]:0,l=0;function
m(g,a){var
c=ko(e,j,d,k,a[4]),h=[0,a[1],a[2],a[3],c[2],a[5]];return[0,b(f[18],g,c[1]),h]}var
h=g(f[17][91],m,l,i),n=h[2],o=h[1],p=a(v[2],0);function
q(a){return[0,a[1],a[2],a[4],a[3]]}return[0,o,e7(p,d,e,xy,b(f[17][69],q,n))]}function
xz(f,d,b){if(d){var
g=d[1];if(typeof
b!=="number"&&0===b[0]){var
e=b[1];if(1===e[0]){var
h=a(c[22],e[1]);return jk(f,g[2],h)}}return 0}return 0}function
dd(b){return a(f[8],b[4])}function
kq(f){var
c=a(e[3],xD),d=a(e[3],xE);return af([0,0,xF,b(e[12],d,c)])}function
gZ(d,e){function
h(e){var
k=a(c[aE][1],e),i=a(G[26],k);switch(i[0]){case
3:return b(P[30],d,e);case
9:var
j=i[1],l=i[2];if(3===a(G[26],j)[0]){var
m=a(c[8],j),n=b(P[30],d,m),o=[0,n,b(f[19][53],c[8],l)];return b(N[56],d,o)}return g(c[fS],d,h,e);default:return g(c[fS],d,h,e)}}return h(e)}function
xZ(c){if(c){var
a=c[1];if(0===a[0]){var
d=a[1],e=function(a){return a[1]};return b(f[17][69],e,d)}return[0,a[1][2],0]}return 0}var
x0=a(D[78],xZ);function
ks(H,l,aR,aY,h,U,s,F){var
aS=U?U[1]:0;function
V(k,c,i,g,f,e,d){var
l=[0,f,h[1],0],m=bU(c),n=a(j[1][8],m);return b(d,c,[0,e,g,n,l,i,k,j[1][10][1]])}function
t(r,q,p){var
t=a(o[bN],p),c=a(P[40],t);if(Z[1]){var
u=kl(H,c,s),v=a(e[3],x1),w=b(e[12],v,u);b(M[10],0,w)}function
x(a){return gZ(c,a)}function
y(a){return e6(x,a)}var
i=b(f[17][69],y,s);if(Z[1]){var
A=kl(H,c,i),B=a(e[3],x2),C=b(e[12],B,A);b(M[10],0,C)}var
k=[0,c],l=kp(h,H,k,[0,aS],i),m=l[1],d=k[1],D=l[2];function
E(a){return gZ(d,a)}function
F(a){return e6(E,a)}var
G=b(f[17][69],F,D),I=a(P[30],d);function
J(a){return e6(I,a)}var
n=b(f[17][69],J,G),K=a(o[bz],d);function
L(c){var
d=c[1],e=bU(a(f[17][5],n)),b=[0,a(j[1][8],e),0];return g(aJ[22],1,b,[3,[0,[1,d],0]])}b(f[17][11],L,m);return z(r,q,m,K,2,n)}a(x0,aR);if(0===F[0]){var
aT=F[1];if(a(o[20],l[1]))throw[0,I,x3];var
aU=function(i,h,g,j,e){var
d=a(f[17][5],e);return V(i,d,h,g,2,[1,b(c[75],l[1],d[5])[1]],aT)};return t(aU,0,l[1])}var
aV=F[1];function
L(h,g,e,j,d){function
i(f,d){var
i=b(c[75],l[1],d[5])[1];return V(h,d,g,e,2,[1,i],a(aV,f))}return b(f[17][12],i,d)}if(a(o[20],l[1])){if(h[2]){var
n=l[1],W=bU(a(f[17][5],s)),Y=[0,1,h[1],xG],_=a(o[37],n),$=a(xH[8][17],_),q=a(v[2],0),aa=function(h){var
d=h[2],k=h[1];if(Z[1]){var
l=g(C[15],q,n,d[1]),m=a(e[3],xI),p=b(e[12],m,l);b(M[10],0,p)}var
r=a(c[d8],q),s=a(f[17][1],r),i=a(o[5],d),t=a(f[17][1],i)-s|0,j=b(f[17][X],t,i)[1],u=b(A[17],d[1],j);return[0,q,k,d,j,g(N[18],q,n,u)]},y=b(f[17][69],aa,$),r=[0,n],B=[0,0],O=function(e,h){if(e){var
d=e[1],i=e[2],j=d[5],k=d[4],l=d[2],m=b(P[37],r[1],d[1]),n=function(e,d){var
h=r[1],j=b(x[2][13],c[10],k),m=[0,d,a(f[17][9],j)],n=a(c[34],m);r[1]=g(o[31],l,n,h);B[1]=[0,d,B[1]];return O(i,e)};return[1,m,h,b(P[30],r[1],j),n]}return[0,h]},ab=a(o[bz],n),ac=O(y,a(o[18],ab)),ad=function(h){if(0===h[0])return af([0,0,xK,a(e[3],xJ)]);var
i=h[3];if(Z[1]){var
j=a(e[3],xL);b(M[10],0,j)}var
E=[0,1],d=a(f[9],i[3]),ac=0===d[0]?[1,d[1]]:[2,d[1]],u=[0,n],k=i[2],l=a(f[17][9],B[1]),m=b(D[aQ],l,y);function
p(ad,j){var
D=ad[2],F=D[4],H=D[2],L=b(o[53],H,n);if(L)var
M=L[1];else{var
aj=E[1];E[1]++;var
ak=a(J[22],aj),al=b(J[17],xM,ak),M=b(K[5],W,al)}var
A=j[4];if(A){var
V=A[1],B=a(e8[17],j[1]),C=B[1],X=B[2],Y=C[2],Z=C[1],l=a(f[17][1],F),q=Z,p=V,k=0;for(;;){if(0===l){var
m=k,i=F,h=q,e=p,s=0;for(;;){if(m){if(i){var
v=i[2],w=m[2],t=m[1],Q=i[1];if(b(aA[3],1,h))if(b(aA[3],1,e)){var
R=b(aA[14],G[6],h),m=w,i=v,h=R,e=b(aA[14],G[6],e);continue}var
S=b(bg[9],t,h),T=b(bg[5],t,e);if(a(x[1][1][6],t))var
U=a(x[2][1][1],Q),y=[0,a(G[2],U),s];else
var
y=s;var
m=w,i=v,h=S,e=T,s=y;continue}}else
if(!i){var
_=j[7],$=j[5],aa=j[3],ab=j[2],ae=[0,[0,[0,b(e8[6],0,[0,[0,h,Y],X]),ab,aa,[0,e],$,0,_]],ac],N=z(bb[3],0,0,M,0,ae),O=b(P[9],u[1],[1,N]),af=O[2],ag=O[1],ah=[0,af,b(f[17][69],c[8],s)],ai=a(c[34],ah);u[1]=g(o[31],H,ai,ag);return N}throw[0,I,xB]}}var
d=a(G[26],q),r=a(G[26],p);switch(d[0]){case
7:if(6===r[0]){var
l=l-1|0,q=d[3],p=r[3],k=[0,[0,d[1],d[2]],k];continue}break;case
8:if(8===r[0]){var
l=l-1|0,q=d[4],p=r[4],k=[0,[1,d[1],d[2],d[3]],k];continue}break}throw[0,I,xA]}}throw[0,I,xC]}var
q=g(D[70],p,m,k);return t(L,q,u[1])},ae=function(c){var
d=u[16],e=a(f[17][1],c[4]);return b(m[66][31],e,d)},ag=b(f[17][69],ae,y),ah=a(dY[12],ad);z(dY[15],W,0,Y,ac,ah);var
ai=function(d,b){var
c=a(k[37],ag);return z(kr[8],0,1,0,c,b)[1]};a(dY[25],ai);var
aj=function(d,b){var
c=a(m[66][24],aZ[6][1]);return z(kr[8],0,1,0,c,b)[1]};a(dY[25],aj);var
ak=a(dY[10],0);if(a(xN[7],ak))return h[2]?kq(0):b(bK[12],0,xO);if(h[2])return 0;var
al=a(e[3],xP),am=a(e[3],xQ),an=a(e[3],xR),ao=b(e[12],an,am);return af([0,0,xS,b(e[12],ao,al)])}var
aW=l[1],aX=bU(a(f[17][5],s)),ap=[0,1,h[1],0],aq=a(v[2],0),Q=aN(aW,id),ar=Q[2],R=aN(Q[1],dt),as=R[2],at=a(o[bN],R[1]),au=a(o[nQ],at)[1],d=a(P[40],au),S=b(K[5],aX,xT),av=[0,0,ar],aw=function(h,g,e){var
j=e[2],k=e[1],l=a(o[6],g);function
m(b){var
c=a(x[2][1][1],b);return a(A[a_],c)}var
d=b(f[17][co],m,l)[1];function
n(b){var
d=a(x[2][1][1],b);return a(c[10],d)}var
i=b(f[17][69],n,d),p=[0,h,a(f[19][12],i)],q=a(c[12],p),r=b(A[19],q,d),s=a(o[4],g),t=[0,0,r,b(A[17],s,d),j];return[0,[0,[0,h,i],k],a(c[20],t)]},T=g(o[28],aw,d,av),az=T[2],aB=T[1],aC=g(c[5],0,d,as),aD=g(c[5],0,d,az),E=ax(aZ[5],aq,S,d,0,xU,aD,aC),aE=E[4],aG=E[3],aH=E[1],aI=function(y,l,h){var
m=b(o[fV],d,h),n=a(xV[16],h),p=a(aM[36][4],n),q=[0,a(aF[8],l),p],r=a(v[2],0),s=b(ay[62],r,q);function
j(a,d,l){var
e=b(c[3],a,l);if(d){if(8===e[0]){var
f=d[1],m=e[4],n=d[2],p=f[2],q=f[1],h=b(c[85],a,e[2])[2],r=b(c[i][4],p,h),s=b(N[25],a,r),k=j(g(o[31],q,s,a),n,m);return[0,[0,h,k[1]],k[2]]}throw[0,I,xW]}return[0,0,a]}var
k=j(m,aB,a(c[8],s)),e=k[2],u=k[1];function
x(a){var
d=b(c[83],e,a)[1],f=b(c[85],e,d)[2],g=b(c[83],e,f)[1];try{var
h=b(c[75],e,g)[1];return h}catch(a){a=w(a);if(a===G[54])throw[0,I,xX];throw a}}return t(L,b(f[17][69],x,u),e)},aK=a(bK[1],aI),aL=[0,aK],aO=[0,function(b){var
e=br[10],f=a(c[8],b),h=a(v[2],0),i=p(N[16],e,h,d,f);return g(c[5],0,d,i)}],aP=a(o[bz],d);bY(aZ[7],S,[0,aG],aE,aP,0,0,[0,ap],0,aO,aL,xY,aH);return 0}return h[2]?kq(0):t(L,0,l[1])}function
kt(g,f,e,d,c,a,b){var
h=a?a[1]:0;return ks(g,f,e,d,c,[0,h],[0,b,0],[0,function(b,a){return[0,b,a]}])}function
g0(h,g,f,e,d,a,c,b){var
i=a?a[1]:0;return ks(h,g,f,e,d,[0,i],c,[1,b])}function
ku(g,d,c){if(0===c[0])return[0,S(g,d,c[1])];var
h=c[2],i=c[1];try{var
j=a(f[8],d),e=b(f[17][7],j,i-1|0);if(0===e[0]){var
k=[1,e[1],h];return k}throw[0,I,x5]}catch(a){a=w(a);if(a===O)throw[0,I,x4];throw a}}function
g1(d,c,b){if(0===b[0])return[0,a(d,b[1])];var
e=b[2];return[1,a(c,b[1]),e]}var
a9=[0,vW,vX,vY,vZ,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v_,v$,wa,wb,wc,wd,we,wf,wg,wh];aB(1256,[0,bI,[0,kg],a9,bJ,aT,bU,gS,e4,kk,kj,wk,wi,ki,ci,w6,e5,gT,kh,xs,w9,cj,kn,e7,db,ko,kp,xz,dd,gW,gY,g0,kt,ku,g1,gU,gZ],nN);var
bj=[cQ,x6,cM(0)],aP=[cQ,x7,cM(0)];function
dZ(c,b,e,h,g,d){if(g){if(d){var
k=d[2],l=d[1],m=g[2],n=g[1];try{var
j=g2(c,b,e,h,n,l),t=j[2],u=j[1],v=function(a){return bq(b,t,a)},p=a(f[17][69],v),x=a(p,m),y=an(0,c,[0,b],dZ(c,b,e,u,x,a(p,k)),j);return y}catch(d){d=w(d);if(d===aP){var
i=dZ(c,b,e,h,m,k),q=a(f[8],i),o=function(a){return bq(b,q,a)},r=o(n),s=o(l);return an(0,c,[0,b],g2(c,b,e,a(f[7],i),r,s),i)}throw d}}}else
if(!d)return $(h);throw bj}function
g2(j,a,h,f,e,d){if(g(c[95],a,e,d))return $(f);var
m=b(c[3],a,e);if(0===m[0]){var
k=m[1];if(!b(c[44],a,d))if(!g(c[i][13],a,k,d))throw bj;if(b(R[2][3],k,h))return c6(0,j,a,k,[2,d],f);throw aP}var
n=b(c[3],a,d);if(0===n[0]){var
l=n[1];if(g(c[i][13],a,l,e)){if(b(R[2][3],l,h))return c6(0,j,a,l,[2,e],f);throw aP}throw bj}var
o=b(c[83],a,e),p=o[1],s=o[2],q=b(c[83],a,d),r=q[1],t=q[2];if(b(c[56],a,p))if(b(c[56],a,r)){if(g(c[95],a,p,r))return dZ(j,a,h,f,s,t);throw bj}throw aP}function
kv(c,a){var
d=[0,1,R[2][1]];function
e(c,e,f){var
d=c[2],a=c[1];return 2===e[0]?[0,a+1|0,b(R[2][4],a,d)]:[0,a+1|0,d]}return p(f[17][19],e,d,c,a)[2]}function
kx(a){var
c=R[2][1];function
d(c,a){var
d=kw(a);return b(R[2][7],c,d)}return g(f[17][15],d,c,a)}function
kw(b){switch(b[0]){case
0:return a(R[2][5],b[1]);case
1:return kx(b[2]);default:return R[2][1]}}function
e9(a){return 3===a[0]?1:0}function
e_(e,a){if(e){if(a){var
i=a[2],j=a[1],k=e[2],l=e[1];try{var
x=[0,ky(l,j)],g=x}catch(a){a=w(a);if(a!==aP)throw a;var
g=0}try{var
v=[0,e_(k,i)],h=v}catch(a){a=w(a);if(a!==aP)throw a;var
h=0}if(g)if(h){var
c=h[1],d=g[1],m=c[3],n=c[2],o=c[1],p=d[3],q=d[2],r=d[1],s=b(f[18],d[4],c[4]),t=b(f[18],p,m),u=b(f[18],q,n);return[0,b(f[18],r,o),u,t,s]}throw aP}}else
if(!a)return x8;throw bj}function
ky(e,c){var
d=a(W[1],e);if(typeof
d!=="number")switch(d[0]){case
0:var
i=d[2],k=d[1];if(2!==c[0])return[0,[0,[0,[0,k,i],c],0],0,0,0];break;case
1:var
l=d[3],m=d[2],n=d[1];switch(c[0]){case
1:var
o=c[2];if(b(j[46],n,c[1][1]))return e_(l,b(f[17][X],m,o)[2]);throw bj;case
2:break;default:throw aP}break;default:return[0,0,[0,[0,d[1],c],0],0,0]}if(2===c[0])return[0,0,0,[0,[0,e,c[1]],0],0];var
g=0;function
h(a,b){return[0,a,c]}return[0,0,0,0,[0,b(W[9],h,e),g]]}function
kz(d,c){var
e=c[2];try{var
g=a(f[17][9],e),h=function(a){return 1-e9(a)},i=[0,e_(d,b(f[17][61],h,g))];return i}catch(a){a=w(a);if(a===bj)return 0;if(a===aP)return 1;throw a}}function
e$(c,a){if(c){if(a){var
i=a[2],j=a[1],k=c[2],l=c[1];try{var
q=[0,kA(l,j)],d=q}catch(a){a=w(a);if(a!==aP)throw a;var
d=0}try{var
p=[0,e$(k,i)],e=p}catch(a){a=w(a);if(a!==aP)throw a;var
e=0}if(d)if(e){var
g=e[1],h=d[1],m=g[1],n=h[1],o=b(f[18],h[2],g[2]);return[0,b(f[18],n,m),o]}throw aP}}else
if(!a)return x_;throw bj}function
kA(d,e){var
c=a(W[1],e);switch(d[0]){case
0:return[0,[0,[0,d[1],c],0],0];case
1:var
g=d[2],h=d[1][1];if(typeof
c!=="number")switch(c[0]){case
2:break;case
0:return[0,0,[0,[0,c[1],d],0]];default:var
i=c[3],k=c[2];if(b(j[46],c[1],h))return e$(b(f[17][X],k,g)[2],i);throw bj}break;case
2:return x9}throw aP}function
kB(d,c){var
e=d[2];try{var
g=a(f[17][9],e),h=function(a){return 1-e9(a)},i=[0,e$(b(f[17][61],h,g),c)];return i}catch(a){a=w(a);if(a===bj)return 0;if(a===aP)return 1;throw a}}function
kC(g,e){var
c=b(f[17][X],g,e)[2],d=a(f[17][1],c);function
h(b){return a(x[1][1][7],b)}return[0,d,d-b(f[17][81],h,c)|0]}function
g3(m,q,d,l){if(l)var
t=0,u=function(d,c){var
e=a(f[17][1],c[2]);return b(J[6],d,e)},n=g(f[17][15],u,t,l);else
var
n=a(x[1][6],d[4]);var
h=q,p=n,o=0,j=b8(m,q,d[5],d[4]);for(;;){if(0===p)var
k=[0,h,o,j];else{var
v=g(N[28],m,h,j),i=b(c[3],h,v);switch(i[0]){case
3:var
r=b(i$[11],h,i[1]),h=r[1],j=r[2];continue;case
6:var
p=p-1|0,o=[0,[0,i[1],i[2]],o],j=i[3];continue;default:var
k=af([0,0,ya,a(e[3],x$)])}}var
s=k[1],w=k[3],y=k[2],z=function(b){var
c=b[1];return a(f[17][1],b[2])<n?af([0,c,yc,a(e[3],yb)]):0};b(f[17][11],z,l);var
A=eF(m,s,y);return[0,s,[0,d[1],d[2],d[3],A,w,d[6],d[7],d[8]]]}}function
g4(k,e,o,h){var
p=b(c[B],e,k),l=[0,0,0,0,0,j[1][10][1]];function
m(a,h){var
f=h[2],d=h[1],k=a[5],e=a[4],l=a[3],m=a[2],n=a[1],g=cc(f);if(0===f[0]){var
q=f[1];return[0,n,m,[0,[0,q,d],l],e,b(j[1][10][4],d,k)]}var
r=cV(p,o,g),s=b(j[1][10][4],d,k),t=b(c[i][1],e,r);return[0,[0,ar([0,d],[0,b(c[i][1],e,g)],t),n],[0,g,m],l,e+1|0,s]}var
d=g(f[17][15],m,l,h),n=d[5],q=d[3],r=d[2],s=d[1],t=[0,n,a(f[17][1],e),0];function
u(m,c){var
h=c[3],d=c[2],e=c[1],g=a(aC,m),i=g[3],k=g[2],n=g[1];try{var
p=[0,e,d-1|0,[0,ar([0,b(f[17][31],d,q)],k,i),h]];return p}catch(a){a=w(a);if(a===O){var
l=b(au[29],n,e),o=[0,ar([0,l],k,i),h];return[0,b(j[1][10][4],l,e),d-1|0,o]}throw a}}return[0,r,s,g(f[17][16],u,e,t)[3]]}function
fa(o,k,j,n,h){var
p=b(c[B],k,j);function
q(f,d){var
e=d[2],g=d[1],h=a(aC,f)[2],j=a(L[7],h);return[0,[0,b(c[i][1],-e|0,j),g],e+1|0]}var
l=g(f[17][16],q,h,yd),d=l[2],r=l[1];function
s(a){var
b=a[1];return[0,b,cE(d,a[2])]}var
t=b(f[17][69],s,n),e=g4(j,b(f[18],h,k),o,t),m=e[2],u=e[3],v=e[1],w=a(f[17][1],m),x=b(f[18],m,u),y=a(c[i][1],-d|0),z=b(f[17][69],y,v);return[0,x,p,w+d|0,b(f[18],z,r)]}function
kD(e,c){if(gu(e)){var
a=function(c){function
g(c){return b(ye[13],a,c)}function
d(k,d){if(4===d[0]){var
h=d[1],m=d[2],i=function(i,d){if(1===d[0]){var
n=d[1],k=function(a){if(a){var
c=a[1];if(0!==c[0])return b(j[1][1],n,c[1][2])}return 0};if(b(f[17][22],k,e)){var
o=[0,b(W[3],i,[13,[3,yf],0,0]),0],l=b(f[17][69],a,m),p=[4,h,b(f[17][57],l,o)];return b(W[3],i,p)}}return g(c)};return b(W[9],i,h)}return g(c)}return b(W[9],d,c)};return a(c)}return c}function
kE(h,g,i,f,d,e){var
a=kD(f[1],e);b(yg[9],[0,0===d?1:0],a);var
c=z(gX[7],0,h,g,[0,d],a);return[0,c[1],c[2]]}function
kF(a){return a?[0,a[1]]:1}function
g5(h,g,d,e,k,j){var
l=0;function
m(H){var
C=b(f[17][69],c[aE][2],d),D=e[5],E=e[4],F=b(ay[22],C,h),G=b(dK[9],F,E);b(f[17][11],G,D);switch(k[0]){case
0:var
q=k[1],m=b(c[B],d,h),n=kF(j);return kE(m,g,d,e,n,ax(a7[7],n,m,g,[0,e[4]],0,0,q));case
1:var
r=k[1],s=b(c[B],d,h);return kE(s,g,d,e,kF(j),r);default:var
t=k[1],u=a(ay[11],h),v=b(ay[47],u,h),o=b(c[B],d,v),w=0,x=1,y=function(f,b,e){var
d=a(bl,e);if(d){var
g=d[1];return[0,[0,g,a(c[9],f)],b]}return b},z=p(f[17][87],y,x,w,d),l=b(c[i][9],z,t),A=j?p(ag[6],o,g,l,j[1]):p(ag[2],0,o,g,l)[1];return[0,A,l]}}return b(dK[14],m,l)}function
g6(e,a,m,d,f,l,k){var
h=d[4],n=d[1],s=d[3],t=d[2];if(k){var
u=b(c[i][1],s,k[1]),o=b(P[30],a[1],u),p=g5(e,a[1],n,m,l,[0,o]),v=p[2];a[1]=ax(ac[29],0,[0,ac[17]],0,0,0,e,p[1]);var
w=g(c[i][3],h,f,v),x=b(P[30],a[1],w),y=g(c[i][3],h,f,o);return[0,x,b(P[30],a[1],y)]}var
q=g5(e,a[1],n,m,l,0),A=q[1],B=g(c[i][3],h,f,q[2]),j=ax(ac[29],0,[0,ac[17]],0,0,0,e,A),r=b(P[30],j,B);a[1]=j;return[0,r,z(ad[2],0,0,t,j,r)]}function
g7(c,j,b,i,h,g,f,d){try{var
k=g6(c,b,i,fa(b,j,c,g,f),0,d,h);return k}catch(b){b=w(b);if(b[1]===ek[15])return bE(0,a(e[3],yh));throw b}}function
kG(j,d,e,Q,t,s){try{var
u=a(A[76],s),v=a(A[76],e),k=[0,b(f[18],v,u)],x=function(c){var
d=a(a7[27],c);return d?d:b(f[17][25],c,k[1])},l=function(c){var
a=b(au[25],c,x);k[1]=[0,a,k[1]];return a},h=b(c[B],e,j),y=b(P[30],d[1],t),m=dH(h,d[1],y),z=m[2],n=c3(m[1]),o=n[1],C=n[2],D=b(bs[10],h,d[1]),q=b(f[17][69],D,z),E=b(bs[10],h,d[1]),r=b(f[17][69],E,C),F=b(f[18],r,q),G=[0,a(c[26],o),F],H=a(c[34],G),I=ct(d[1],o),J=b(ae[3],h,I),K=function(k,t){var
u=a(c[8],t),v=g(A[58],d[1],u,r),m=b(c[91],d[1],v),w=m[2],x=m[1],y=0;function
z(k,e){var
f=a(aC,k),g=f[3],i=f[2],j=f[1];if(j)return[0,ar([0,l(j[1])],i,g),e];var
m=d[1],n=b(c[B],e,h);return[0,ar([0,l(p(au[10],n,m,g,0))],i,g),e]}var
e=g(f[17][16],z,x,y),n=b(c[B],e,j),o=dH(n,d[1],w),C=o[2],q=c3(o[1]),s=q[2],i=q[1],D=dP(e),E=b(f[18],s,D),F=[0,a(c[29],[0,i,k+1|0]),E],G=a(c[34],F),H=dL(dP(e)),I=gC(e),J=dL(s),K=b(f[18],J,I);return[0,n,e,G,[1,[0,[0,i[1],k+1|0],i[2]],K],H,C]},L=b(f[19][16],K,J),M=function(g){var
k=g[2],m=g[6],n=g[5],o=g[4],p=g[3];a(f[17][1],e);var
h=a(f[17][1],k),l=b(f[18],k,e);try{var
r=a(c[i][1],h),s=b(f[17][69],r,q),t=bG(h,dL(dP(e))),u=kv(b(f[18],t,n),l),v=[0,[0,dZ(j,d[1],u,l,s,m),h,p,o]];return v}catch(a){a=w(a);if(a===bj)return 0;if(a===aP)return 1;throw a}},N=[0,[0,H,b(f[19][15],M,L)]];return N}catch(a){a=w(a);if(a===O)return 0;throw a}}function
kH(d,c){var
e=c[2];function
k(l,q){var
d=q;for(;;){if(l){if(d){var
e=d[1],r=l[2],s=l[1];if(3===e[0]){var
d=d[2];continue}var
t=k(r,d[2]),h=a(W[1],s);if(typeof
h==="number")var
c=0;else
switch(h[0]){case
0:var
g=0,c=2;break;case
1:var
m=h[3],n=h[2],o=h[1];switch(e[0]){case
0:var
c=0;break;case
1:var
p=e[2];if(b(j[46],o,e[1][1]))var
g=k(m,b(f[17][X],n,p)[2]),c=2;else
var
g=0,c=2;break;default:var
c=1}break;default:var
g=0,c=2}switch(c){case
0:if(0===e[0])var
g=[0,e[1],0],i=1;else
var
i=0;break;case
1:var
i=0;break;default:var
i=1}if(!i)var
g=0;return b(f[18],g,t)}}else
if(!d)return 0;return 0}}return k(d,a(f[17][9],e))}function
yi(e,d,k,a){function
g(d,a){var
h=b(c[3],e,a);if(0===h[0]){var
j=h[1]-d|0;if(0<=j)try{var
l=b(f[17][31],j,k),m=b(c[i][1],d,l);return m}catch(b){b=w(b);if(b===O)return a;throw b}return a}function
n(a){return a+1|0}return z(c[a_],e,n,g,d,a)}return g(d,a)}function
yj(a){var
c=a[2];function
d(a){switch(a[0]){case
0:return 1;case
1:return 0;default:return 1}}return b(f[17][21],d,c)}function
fb(l,k,r){var
c=l[2],d=l[1],g=jF(k-1|0,r),h=g[3],m=g[2],e=g[1],i=a(aC,m),j=i[1],s=i[2],n=kG(d,c,e,j,i[3],h);function
t(a){if(typeof
a==="number"){if(0===a)return 0;throw[0,I,yk]}var
g=a[1],i=g[1],j=i[2],k=g[2],l=i[1],n=eI(c[1],j,g[4]),o=eF(d,c[1],l),p=[0,o,[0,n,b(f[17][X],k,j)[2]],[0,m,e]],q=bh(0,d,c[1],p);return[0,gH(d,c[1],q,h)]}if(n){var
o=n[1],p=o[2],q=o[1],u=function(a){return 1===a?1:0};if(b(f[19][32],u,p))return[0,[1,j,e,q]];var
v=[0,ar(j,s,q),e],w=b(f[18],h,v),x=b(f[19][15],t,p);return[0,[0,[0,k,eF(d,c[1],w),x]]]}return 0}function
g8(h,g,f){var
c=fb(h,f,g);if(c){var
d=c[1];if(0===d[0]){var
a=d[1],e=a[3],i=a[2],j=a[1],k=function(a){return 0===a?1:0};if(b(a8[34],k,e)){var
l=function(a){return 0};return[0,[0,j,i,b(a8[15],l,e)]]}return 0}return 0}return 0}function
kI(e,c){function
g(a){return a+1|0}var
h=a(f[17][1],c),i=b(D[56],h,g);function
j(a){return g8(e,c,a)}var
d=b(f[17][66],j,i);return d?[0,d[1]]:0}function
kJ(c){function
e(d,c){function
h(d,c){switch(d[0]){case
0:return[0,[0,d[1],0],c];case
1:var
g=e(0,a(f[17][9],d[2]));return b(f[18],g,c);case
2:return c;default:return[0,[0,d[1],1],c]}}return g(f[17][16],h,c,d)}var
d=e(0,c);function
h(b,a){return b[1]-a[1]|0}return b(f[17][39],h,d)}function
yl(a){var
b=a[1];return a[2]?[3,b]:[0,b]}var
ym=a(f[17][69],yl);function
yn(e,d){return b(bm,a(c[i][1],e),d)}function
fc(e){var
h=1;return function(i){var
c=h,a=i;for(;;){if(a){var
d=a[1],f=a[2],g=b9(d);if(b(j[1][1],e,g))return[0,c,d];var
c=c+1|0,a=f;continue}throw O}}}function
kK(w,l,k,d){var
h=dD(0,function(a){throw[0,bo,yo]},k),i=h[3],m=h[1],n=a(c[68],l),j=b(f[17][69],n,m),o=0;function
p(d,c){return[0,a(fc(b(f[17][7],j,d[1]-1|0)),i)[2],c]}var
e=g(f[17][16],p,d,o),q=1;function
r(g,f){var
c=a(fc(f),e)[1];function
h(a){var
b=a[2];if(a[1]===g){var
d=b?[3,c]:[0,c];return[0,d]}return 0}return b(D[fR],h,d)}var
s=g(f[17][73],r,q,j),t=1;function
u(c,e){var
f=a(fc(a(cZ,e)[1]),i)[1];function
g(a){var
b=a[2];if(a[1]===f){var
d=b?[3,c]:[0,c];return[0,d]}return 0}return b(D[fR],g,d)}var
v=g(f[17][73],u,t,e);return[0,er(e)[1],s,v]}function
kL(f,d,e){if(0===a(c[d8],d))return b(c[B],f,d);var
g=av(cz,e),h=i3(ga,[0,av(iF,e)],g),i=b(c[cq],h,d);return b(c[B],f,i)}function
yp(e,d){function
h(b){var
d=a(gm,b);return iz(cz,g(c[5],0,e,d))}return b(f[17][co],h,d)}function
cI(m,i,h,g){var
d=g[1],n=g[2],o=b(c[B],d,i),k=cd(i,h,d);if(a(f[17][48],d))var
l=k;else
var
u=a(e[5],0),v=a(e[3],yr),w=a(e[5],0),x=b(e[12],w,v),y=b(e[12],x,u),l=b(e[12],y,k);var
p=eG(o,h,n),q=a(e[3],yq),r=a(j[1][9],m[2]),s=b(e[12],r,q),t=b(e[12],s,p);return b(e[12],t,l)}function
ys(d,c){var
e=b(f[17][7],d,c-1|0),g=a(f[7],e);return a(K[10][16],g)}var
yt=a(f[17][16],c[cq]);function
kM(h,g){function
i(a){return 0===a[2][2]?1:0}var
c=b(f[17][61],i,g);if(c){var
d=c[1][1],j=d[1],k=ez(h,d),l=a(e[3],yu);return af([0,j,yv,b(e[12],l,k)])}return 0}function
fd(d,c){function
i(a){return a[6]?0:1}if(b(f[17][21],i,c))return[0,0,d];function
j(b){var
a=b[6];if(a)if(0!==a[1][0])return 0;return 1}if(b(f[17][21],j,c)){var
k=function(a){var
b=a[6];if(b){var
c=b[1];if(0!==c[0])throw[0,I,yw];var
d=c[1]}else
var
d=0;return[0,a[2],d]};return[0,[0,[0,b(f[17][69],k,c)]],d]}if(1!==a(f[17][1],c))af([0,0,yy,a(e[3],yx)]);var
g=a(f[17][5],c)[6];if(g){var
h=g[1];if(0!==h[0])return[0,[0,[1,h[1][3]]],d]}throw[0,I,yz]}function
kN(d){var
c=Z[1];if(c){var
f=g(e[39],e[5],gT,d),h=a(e[3],yA),i=b(e[12],h,f);return b(M[10],0,i)}return c}function
kO(h,d,m,l){function
n(a){var
d=b(c[37],a[5],a[4]);return[0,a[2],d,a[7]]}var
o=b(f[17][69],n,l),e=a(f[17][mM],o),i=e[2],j=e[1],p=ax(a7[3],h,d[1],m,0,j,i,e[3]);function
q(e){var
b=av(bB,d),f=[0,0,b,z(ad[2],0,0,h,d[1],b),e];return a(c[20],f)}var
k=b(f[17][69],q,i);function
r(b,a){return ah([0,[0,b],0,a])}var
s=g(f[17][70],r,j,k);return[0,p,a(f[17][9],s),k]}function
g9(t,d,ai,T,S,R,C){var
h=C[1],D=h[5],E=h[4],n=h[2],F=h[1],i=F[2],k=F[1],U=h[3],G=aw(p(a7[25],0,0,0,t),d,U)[2],H=G[1],V=G[2],W=H[2],X=H[1],Y=E?E[1]:b(aL[1],[0,k],yG),Z=a(a7[19],X),I=aw(function(a){return b(Z,a,0)},d,Y),_=I[1],q=b(f[18],V,I[2]),g=b(P[36],d[1],W),l=b(P[30],d[1],_);function
u(b,a){if(b)if(0===b[1])return[1,a];return[0,a]}if(D){var
v=D[1];if(0===v[0]){var
J=v[1];if(J){var
r=J[1];try{var
ag=b(A[7],r[2],g)[1],ah=[0,[0,u(n,[0,[0,a(f[17][1],g)-ag|0,[0,r]]])]],K=ah}catch(c){c=w(c);if(c!==O)throw c;var
$=a(e[3],yB),aa=a(j[1][9],r[2]),ab=a(e[3],yC),ac=b(e[12],ab,aa),ad=b(e[12],ac,$),K=af([0,[0,r[1]],yD,ad])}var
L=K}else
var
L=[0,[0,u(n,0)]];var
M=L}else
var
Q=v[1],M=[0,[1,[0,Q[1],Q[2]]]];var
m=M}else
if(T){if(mE(n,yE))if(eE(i,[0,[0,C,0],R]))var
B=0;else
var
m=yF,B=1;else
var
B=0;if(!B)var
m=[0,[0,u(n,0)]]}else
var
m=0;var
ae=b(c[38],l,g);if(1-S)p(gX[13],t,o[16],d[1],ae);var
s=b(c[37],l,g),x=z(c5[20],t,d[1],s,0,q);d[1]=a(o[bN],d[1]);if(m){var
y=m[1];if(0===y[0])return[0,k,i,s,g,l,[0,[0,y[1]]],q,x];var
N=y[1];return[0,k,i,s,g,l,[0,[1,[0,N[1],N[2],[0,k,i]]]],q,x]}return[0,k,i,s,g,l,0,q,x]}function
g_(c){function
d(c){var
d=a(x[1][1][1],c),e=[0,a(K[10][16],d),0];return b(W[3],0,e)}return b(f[17][14],d,c)}function
g$(e,d,l,L,K,J,H){var
n=jQ(e,d,l),o=n[1],M=n[2],O=b(c[38],L,l),u=cy(e,d,f7,[0,o,K,J,H]),P=cy(e,d,f6,[0,M,u]),v=c1(e,d[1],0,P),Q=v[2];d[1]=ax(ac[29],0,0,0,0,0,e,v[1]);var
w=cy(e,d,f8,[0,o,u,Q,O]),y=p(ag[2],0,e,d[1],w),R=y[2];d[1]=y[1];var
z=s(aq),S=br[13],T=j[59],U=j[18][1],V=t===z?aq[1]:q===z?a(r[2],aq):aq,W=a(j[67][6],V),A=s(al),X=b(j[18][7],W,U),Y=t===A?al[1]:q===A?a(r[2],al):al,Z=a(j[67][6],Y),_=b(j[18][7],Z,X);function
$(e,c){var
d=s(c),f=t===d?c[1]:q===d?a(r[2],c):c,g=a(aF[8],f);return b(j[18][7],g,e)}var
aa=g(f[17][15],$,_,[0,cX,[0,f7,[0,f8,[0,im,[0,il,[0,io,[0,ip,[0,iq,[0,ir,[0,is,[0,it,0]]]]]]]]]]]),C=b(br[7][13],S,[0,T[1],aa]);function
D(b){var
c=g(fe[1],C,b,d[1]);return a(fe[2],c)}var
ab=a(D(e),R),m=b(c[3],d[1],ab);if(6===m[0]){var
ad=m[2];b(c[i][5],c[14],m[3]);var
ae=a(D(e),w),af=a(x[1][6],l),E=p(N[67],e,d[1],af,ad),h=E[1],ah=E[2],ai=d[1],aj=b(c[B],h,e),ak=g(N[28],aj,ai,ah),k=b(c[3],d[1],ak);if(6===k[0]){var
am=k[3],an=k[2],ao=k[1],ap=d[1],ar=b(c[B],h,e),F=g(N[69],ar,ap,an),as=F[2],at=F[1],au=d[1],av=b(c[B],h,e),aw=g(fe[1],C,av,au),ay=a(fe[2],aw),G=a(ay,b(c[37],as,at)),az=a(c[18],[0,ao,G,am]);return[0,G,b(c[37],az,h),ae]}throw[0,I,yI]}throw[0,I,yH]}function
kP(k,h,J,d,x,o,n){var
m=b(c[B],d,k),q=p(a7[14],m,h[1],0,o),r=q[2],j=q[1],y=z(ad[2],0,0,m,j,r),s=g(N[21],m,j,y),A=a(f[17][1],d);if(p(c[i][14],j,1,A,s))var
C=-a(f[17][1],d)|0,l=b(c[i][1],C,s);else
var
I=a(e[3],yJ),l=af([0,a(bn[6],o),yK,I]);var
t=b(c[38],r,d),D=c[14],E=[0,0,b(c[i][1],1,l),D],F=[0,0,l,a(c[18],E)],u=a(c[18],F),v=n?z(a7[15],k,j,0,n[1],u):c1(k,j,0,u),w=v[2];h[1]=v[1];var
G=g$(k,h,d,x,l,t,w),H=b(P[30],h[1],w);return[0,b(P[30],h[1],t),H,G]}function
ha(M,L,h,e,J,d){var
u=d[6];if(u){var
l=u[1];if(0===l[0]){var
v=l[1];if(typeof
v==="number")var
N=h[2],O=function(c){var
e=a(x[1][1][1],c),f=a(K[10][16],e);return 1-b(j[1][1],f,d[2])},w=b(f[17][61],O,N),y=a(f[17][1],w),P=aS(y,d[4]),A=y,z=b(f[18],P,w);else
var
D=a(f[17][1],h[2]),Z=h[2],_=aS(D,d[4]),A=D,z=b(f[18],_,Z);var
B=h[1],Q=d[2];if(B){var
s=B[1];if(s){var
t=s[1];if(0===t[0])var
H=t[1],I=function(a){var
c=a[1];if(typeof
a[2]==="number")if(b(j[1][1],c,Q))return 0;return[0,b(W[3],0,[0,c,0])]},m=b(f[17][66],I,H),k=1;else
var
k=0}else
var
k=0}else
var
k=0;if(!k)var
m=0;var
C=b(f[18],z,e),R=g_(e),S=b(f[18],R,m),T=[1,[0,v,a(f[17][1],m)]],U=a(f[17][1],d[4]),V=d[5],X=a(f[17][1],d[4])+1|0,Y=g(c[i][2],A,X,V),n=[0,$(C),e,C,Y,U,T];return[0,d,n[1],n[4],S,[0,n]]}var
E=l[1],o=kP(M,L,J,d[4],d[5],E[1],E[2]),F=o[3],aa=F[3],ab=F[1],ac=o[2],ad=o[1],ae=g_(e),af=a(f[17][1],d[4]),ag=ar([0,d[2]],0,ab),p=b(f[18],d[4],e),G=[0,ag,p],ah=[0,G,[0,yL,bG(1,gG(p))],G],q=[0,ah,e,p,b(c[i][1],1,d[5]),af,[0,[0,aa,ad,ac]]],ai=d[8],aj=d[7],ak=d[6],al=d[5],am=b(f[18],d[4],e);return[0,[0,d[1],d[2],d[3],am,al,ak,aj,ai],q[1],q[4],ae,[0,q]]}var
an=d[8],ao=d[7],ap=d[6],aq=d[5],as=b(f[18],d[4],e),r=[0,d[1],d[2],d[3],as,aq,ap,ao,an],at=g_(e),au=r[5];return[0,r,$(r[4]),au,at,0]}var
kQ=[cQ,yM,cM(0)];function
ff(h,d,m,P,bl,bk,aB,k,F,u,l){var
G=bl,v=bk;for(;;){var
bm=k[3],bn=k[2];if(Z[1]){var
bo=bF(h,F),bp=a(e[3],yN),br=cI(m,h,d[1],k),bt=a(e[3],yO),bu=function(a){return a[1]},bv=b(f[17][69],bu,v),bw=a(dI(h),bv),bx=a(e[3],yP),by=b(e[12],bx,bw),bz=b(e[12],by,bt),bA=b(e[12],bz,br),bB=b(e[12],bA,bp),bC=b(e[12],bB,bo);b(M[10],0,bC)}if(v){var
ad=v[2],ae=v[1],aC=ae[2],V=aC[2],ag=aC[1],ah=ae[1],Y=ah[3],y=ah[2],D=ah[1];if(Z[1]){var
bD=cI(m,h,d[1],k),bE=a(e[3],yQ),bH=bF(h,b(f[18],F,y)),bI=a(e[3],yR),bJ=b(e[12],bI,bH),bK=b(e[12],bJ,bE),bL=b(e[12],bK,bD);b(M[10],0,bL)}var
ai=kz(b(f[18],F,y),k);if(typeof
ai==="number"){if(0===ai){if(Z[1]){var
bM=a(e[3],yS);b(M[10],0,bM)}var
G=[0,ae,G],v=ad;continue}if(Z[1]){var
bN=a(e[3],yT);b(M[10],0,bN)}var
aE=kH(b(f[18],F,y),k);a3(function(n){var
d=a(f[7],k),i=b(c[B],d,h);function
j(a){return ce(i,a)}var
l=g(e[39],e[13],j,aE),m=a(e[3],yU);return b(e[12],m,l)});var
aF=function(z,y){var
q=z,i=y;for(;;){if(i){var
j=i[2],n=i[1];a3(function(j){return function(l){var
d=a(f[7],k),g=ce(b(c[B],d,h),j),i=a(e[3],yV);return b(e[12],i,g)}}(n));var
o=fb([0,h,d],n,a(f[7],k));if(o){var
r=o[1];if(0===r[0]){var
p=r[1],s=p[1],A=p[3],C=p[2];a3(function(j){return function(l){var
d=a(f[7],k),g=ce(b(c[B],d,h),j),i=a(e[3],yW);return b(e[12],i,g)}}(s));var
t=[0,C,bn,bm];try{var
D=function(s){return function(i,g){if(g){var
b=g[1],k=a(f[8],b),n=bq(d[1],k,l),o=a(f[8],b),p=dO(d[1],o,u),c=ff(h,d,m,P,0,i,aB,an(0,h,[0,d[1]],b,s),F,p,n);if(c){var
j=c[1],q=j[2],r=j[1];a3(function(b){return a(e[3],yZ)});return[0,r,[0,q]]}throw O}return[0,i,0]}}(t),E=a(f[17][9],G),H=b(f[18],E,v),x=g(f[19][64],D,H,A),I=[0,[0,[0,x[1],[1,t,s,l,x[2]]]]];return I}catch(b){b=w(b);if(b===O){a3(function(b){return a(e[3],yX)});var
i=j;continue}if(b[1]===kQ){a3(function(b){return a(e[3],yY)});var
i=j;continue}throw b}}a3(function(j){return function(l){var
d=a(f[7],k),g=ce(b(c[B],d,h),j),i=a(e[3],y0);return b(e[12],i,g)}}(n));var
J=q||o,q=J,i=j;continue}var
i=j;continue}return 0}}(0,aE);if(aF){var
Q=aF[1];if(0===Q[0]){var
aG=Q[1];return[0,[0,aG[1],aG[2]]]}var
bO=Q[3],bP=Q[2],bQ=Q[1],bR=a(e[3],y1),bS=a(e[5],0),bT=a(e[3],y2),bU=d[1],bV=b(c[B],bP,h),bW=g(C[15],bV,bU,bO),bX=a(e[3],y3),bY=a(K[10][8],bQ),bZ=a(e[3],y4),b0=b(e[12],bZ,bY),b1=b(e[12],b0,bX),b2=b(e[12],b1,bW),b3=b(e[12],b2,bT),b4=b(e[12],b3,bS);return af([0,D,y5,b(e[12],b4,bR)])}return 0}var
$=ai[1],aJ=$[4],b5=$[3],b7=$[2],b8=$[1];if(Z[1]){var
b9=a(e[3],y6);b(M[10],0,b9)}var
b_=function(a){return[0,a[1][1],a[2]]},H=b(f[17][69],b_,b8);if(0===V)var
aK=y7;else
var
cy=a(J[22],V),aK=b(J[17],zi,cy);var
b$=a(J[22],ag),ca=b(J[17],b$,aK),cd=b(J[17],y8,ca),cf=a(j[1][6],cd);if(aJ){var
aL=aJ[1],aj=aL[2],ak=aL[1];if(Y)return af([0,D,y_,a(e[3],y9)]);if(0===aj[0]){var
cg=aj[1],aM=g8([0,h,d],a(f[7],k),cg);if(aM){var
aN=aM[1],ch=[0,k,0,l,[1,aN[1],aN[3]]],ci=a(f[17][9],G);return[0,[0,b(f[18],ci,[0,[0,[0,ak,y,Y],[0,ag,V+1|0]],ad]),ch]]}var
ck=cI(m,h,d[1],k),cl=a(e[5],0),cm=a(e[3],y$),cn=b(e[12],cm,cl);return af([0,ak,za,b(e[12],cn,ck)])}var
co=dN(h,d[1],aj),cp=a(e[5],0),cq=a(e[3],zb),cr=b(e[12],cq,cp);return af([0,ak,zc,b(e[12],cr,co)])}if(Y){var
n=Y[1],al=[0,cf,aB],p=k[1],cH=k[2],R=kL(p,h,d),cL=function(j){var
c=j[2],f=j[1],i=g7(h,p,d,P,0,H,u,[1,f])[1];if(2===c[0]){var
k=c[1],l=z(dV[6],R,0,d[1],i,k);if(l){d[1]=l[1];return 0}var
A=function(c,r){var
f=g(C[15],R,d[1],k),h=a(e[3],zp),j=a(e[13],0),l=g(C[15],R,d[1],i),m=a(e[3],zq),n=b(e[12],m,l),o=b(e[12],n,j),p=b(e[12],o,h),q=b(e[12],p,f);return g(_[6],c,zr,q)};return b(W[9],A,f)}var
m=cc(c),n=g(C[15],R,d[1],m),o=a(e[3],zn),q=a(e[13],0),r=g(C[15],R,d[1],i),s=a(e[3],zo),t=b(e[12],s,r),v=b(e[12],t,q),w=b(e[12],v,o),x=b(e[12],w,n);function
y(a,b){throw[0,kQ,[0,a,x]]}return b(W[9],y,f)},cM=function(f){var
l=f[2],j=f[1];function
k(j,f){if(a(L[3],j))return 0;if(typeof
f!=="number"&&0===f[0])return 0;var
k=fa(d,p,h,H,u),m=k[1],n=g(c[i][3],k[4],0,l),o=d[1],q=b(c[B],m,h),r=g(C[15],q,o,n),s=a(e[3],zs),t=b(e[12],s,r);return g(_[6],j,zt,t)}return b(W[9],k,j)};b(f[17][11],cL,b7);b(f[17][11],cM,b5);switch(n[0]){case
0:var
aT=n[2],cN=n[1],aa=fa(d,p,h,H,u),az=aa[4],bi=aa[2],aA=aa[1],U=aT[2],dM=aa[3],dQ=function(l,p){var
z=p[1],A=z[1],k=A[2],m=l[5],B=l[3],u=l[2],e=l[1],Q=p[2],R=z[4],S=A[1],T=l[4],C=g9(m,d,0,eE(k,[0,[0,p,0],U]),1,U,p),V=b(f[18],e[5],U);function
W(a){return gw(m,V,C,a)}var
v=b(f[17][69],W,Q),D=g3(m,d[1],C,v),w=D[2];d[1]=D[1];var
q=cb(w),X=fd(e[1],[0,w,0]),r=[0,X,[0,[0,[0,k],q],0],e[3],e[4],e[5]],n=ha(m,d,r,u,az,w),E=n[5],x=n[4],F=n[3],s=n[2],j=n[1],G=ax(a7[3],m,d[1],[0,e[4]],0,[0,k,0],[0,q,0],[0,j[7],0]),Y=b(f[18],e[5],U),H=[0,r[1],r[2],r[3],G,Y],Z=b(f[18],e[5],U),y=[0,e[1],e[2],e[3],G,Z],t=[0,k,al],_=aH(0,u),$=b(c[i][3],az,B),I=b(f[17][69],$,_);function
J(b){return[0,b,j,I,t,t,a(f[17][1],x),q]}if(R)var
aa=de(0,h,d,j,H,v,t,s,x,F),K=db(h,d,y[3],j,s,aa,E),ab=K[5],ac=J(K),M=a(dB[4],ac),L=ab;else{var
ad=cb(j),O=c1(h,d[1],[0,[0,[0,S],[3,[0,zK,[0,k],0]]]],ad),P=O[2];d[1]=O[1];var
ae=b(c[76],d[1],P),af=function(c){var
b=de(0,h,d,j,H,v,t,s,x,F),a=db(h,d,y[3],j,s,b,E);d[1]=g(o[31],ae[1],a[5],d[1]);return J(a)},M=a(dB[3],af),L=P}var
N=ar([0,k],[0,aD(L,I)],q);return[0,y,[0,N,u],B+1|0,[0,M,T],b(c[aR],N,bi)]},dR=aT[1],dS=[0,P,aA,0,0,b(c[B],aA,h)],ac=g(f[17][15],dQ,dS,dR),bj=ac[3],dT=ac[4],dU=ac[2],dW=ac[1];b(c[B],aA,h);var
aU=g6(h,d,dW,[0,dU,bi,dM,az],bj,cN,[0,b(c[i][1],bj,l)]),cO=aU[2],cP=aU[1],cQ=function(b){var
c=s(b);return t===c?b[1]:q===c?a(r[2],b):b},N=[0,[0,k,b(f[17][69],cQ,dT),cO,[0,cP]]];break;case
1:var
aV=n[1],am=aV[2],aW=aV[1],aQ=b(f[17][31],am,H);if(0===aQ[0])var
aX=aQ[1];else
var
cJ=a(j[1][9],am),cK=a(e[3],zl),aX=af([0,[0,aW],zm,b(e[12],cK,cJ)]);var
aY=g8([0,h,d],a(f[7],k),aX);if(aY)var
aZ=aY[1],N=[0,[0,k,0,l,[1,aZ[1],aZ[3]]]];else
var
cR=a(e[3],zu),cS=a(j[1][9],am),cT=a(e[3],zv),cU=b(e[12],cT,cS),N=af([0,[0,aW],zw,b(e[12],cU,cR)]);break;default:var
a0=n[2],ao=n[1];if(!ao)throw[0,I,zJ];var
cV=ao[2],cW=ao[1],a1=cV||0,a2=g7(h,p,d,P,0,H,u,[0,cW]),a4=a2[2],ap=a2[1],aq=kJ(cH),a5=kK(h,d[1],p,aq),a6=a5[2],ab=a5[1],T=bh(0,h,d[1],[0,ab,a6,p]),cX=a(A[76],ab),cY=a(j[1][10][35],cX),cZ=a(j[1][6],zx),as=b(au[26],cZ,cY),a8=[0,[0,[0,as],S(d[1],T,a4)],ab],c0=S(d[1],T,ap),at=b(c[i][1],1,c0),a9=gE(zz,zy,h,d[1],a8,1,at),av=a9[2],E=a9[1],c2=function(a){return 1===a[2]?1:0},a_=b(f[17][27],c2,av)[1],c3=a(f[7],E),c4=i6(a_,S(d[1],E,at),c3),c5=bG(1,a6),c6=a(f[8],E),c7=[0,c4,a(eJ(d[1],c6),c5),ab],a$=an(0,h,[0,d[1]],c7,T),aw=a(f[7],E),c8=dP(aw),c9=function(g){var
e=b(c[66],d[1],g),h=a(f[8],a$);function
i(a){return 3===a[0]?e===a[1]?1:0:0}return b(f[17][22],i,h)?[3,e]:[0,e]},ay=[0,aw,b(f[17][14],c9,c8),aw],c_=b(c[B],a8,h),c$=S(d[1],T,l),da=b(c[i][1],1,c$),dc=g(bs[9],c_,d[1],da),dd=g(A[50],d[1],at,dc),ba=S(d[1],E,dd),df=function(b,a){return a[1]-b[1]|0},dg=b(f[17][39],df,av),dh=function(a){return a[2]},di=b(f[17][14],dh,dg);if(a1)var
dj=[0,b(W[3],D,[0,as,1]),0],bb=[0,[0,D,b(f[18],y,dj),[0,[2,a1,a0]]],0];else
var
bb=a0;var
bc=function(c,l){var
i=[0,-1],m=a(j[1][6],zA);function
t(d){i[1]++;var
c=a(J[22],i[1]);return b(K[5],m,c)}function
n(i){var
j=i[3],l=i[2],p=i[1],u=a(f[17][1],l)-c|0,q=b(f[17][X],u,l),m=q[2],v=q[1];if(m){var
x=m[2],y=m[1],r=kB(k,b(f[18],F,v));if(typeof
r==="number"){var
z=bF(h,l),A=a(e[13],0),B=a(e[3],zB),C=a(e[5],0),D=aI(h,d[1],k),E=a(e[13],0),G=a(e[3],zC),H=a(e[16],c),J=a(e[5],0),K=a(e[3],zD),L=b(e[12],K,J),M=b(e[12],L,H),N=b(e[12],M,G),P=b(e[12],N,E),Q=b(e[12],P,D),R=b(e[12],Q,C),S=b(e[12],R,B),T=b(e[12],S,A),U=b(e[12],T,z);return g(_[6],p,zE,U)}var
V=r[1][1],Y=function(a){if(1===a)return[0,y];function
c(b){var
c=b[1]===(a-1|0)?1:0,d=b[2],e=c?d:c;return e}if(b(f[17][22],c,aq))return 0;try{var
e=b(f[17][31],a-1|0,V),g=[0,b(W[3],0,e)];return g}catch(a){a=w(a);if(a===O){var
d=[0,t(0),1];return[0,b(W[3],0,d)]}throw a}},Z=b(f[17][66],Y,di);if(j){var
n=j[1];if(2===n[0])var
aa=n[1],s=[0,[2,aa,bc(c+1|0,n[2])]],o=1;else
var
o=0}else
var
o=0;if(!o)var
s=j;var
$=a(f[17][9],Z);return[0,[0,p,b(f[18],$,x),s]]}throw[0,I,zF]}return b(f[17][66],n,l)},bd=bc(1,bb),dk=function(b,a){return a[1]-b[1]|0},dl=b(f[17][39],dk,av),dm=function(e){var
d=e[2];if(1===d)return ap;var
g=b(f[17][7],aq,(d-1|0)-1|0)[1];return a(c[9],g)},dn=b(f[17][69],dm,dl),dp=function(e){if(b(c[44],d[1],e)){var
g=b(c[66],d[1],e)-1|0,h=a(f[7],k),i=b(f[17][7],h,g);return a(x[1][1][7],i)?0:[0,e]}return[0,e]},dq=b(f[17][66],dp,dn),dr=a(f[17][1],u),ds=g4(h,p,d,H)[2],dt=aS(1,u),du=aS(dr+1|0,ds),dv=b(f[18],du,dt),dw=a(f[8],E),dx=dO(d[1],dw,dv),dy=function(b,a){return[0,a,[0,b+1|0,0]]},be=ff(h,d,m,P,0,b(f[17][13],dy,bd),al,ay,0,dx,ba);if(be){var
bf=be[1],bg=bf[2];kM(h,bf[1]);var
dz=cj(h,d,bg)[1],N=[0,[4,k,[0,[0,as,ap,a4],l,kC(a_,a(f[7],E)),al,dz,dq,T,ay,a$,ba],bg]]}else
var
dA=a(dI(h),bd),dC=a(e[3],zG),dD=a(e[5],0),dE=cI(m,h,d[1],ay),dF=a(e[5],0),dG=a(e[3],zH),dH=b(e[12],dG,dF),dJ=b(e[12],dH,dE),dK=b(e[12],dJ,dD),dL=b(e[12],dK,dC),N=cC(zI,b(e[12],dL,dA))}if(N){var
cs=N[1],ct=a(f[17][9],G);return[0,[0,b(f[18],ct,[0,[0,[0,D,y,[0,n]],[0,ag,V+1|0]],ad]),cs]]}var
cu=a(e[3],zd),cv=ez(h,[0,D,y,[0,n]]),cw=a(e[3],ze),cx=b(e[12],cw,cv);return af([0,b6,zf,b(e[12],cx,cu)])}return af([0,D,zh,a(e[3],zg)])}var
aO=kI([0,h,d],a(f[7],k));if(aO){var
aP=aO[1],cz=[0,k,0,l,[1,aP[1],aP[3]]],cA=a(f[17][9],G);return[0,[0,b(f[18],cA,v),cz]]}var
cB=cI(m,h,d[1],k),cD=a(e[5],0),cE=a(e[3],zj),cF=b(e[12],cE,cD),cG=b(e[12],cF,cB);return af([0,[0,m[1]],zk,cG])}}function
de(i,c,h,g,p,o,n,d,m,l){var
q=i?i[1]:1;function
r(b,a){return[0,a,[0,b+1|0,0]]}var
j=ff(c,h,g,p,0,b(f[17][13],r,o),n,d,m,0,l);if(j){var
k=j[1],s=k[2],t=k[1];if(q)kM(c,t);return s}var
u=cI(g,c,h[1],d),v=a(e[5],0),w=a(e[3],zL),x=b(e[12],w,v);return cC(zM,b(e[12],x,u))}function
kR(h,e,d,i,a){function
j(l,r){var
m=d[5],n=b(c[B],d[2],h);function
o(a){return gw(n,m,l,a)}var
i=b(f[17][69],o,r),j=g3(h,e[1],l,i),g=j[2];e[1]=j[1];var
a=ha(h,e,d,0,0,g),k=a[2],p=a[5],q=a[1];return[0,q,k,de(0,h,e,g,d,i,[0,g[2],0],k,a[4],a[3]),p]}var
k=g(f[17][70],j,i,a);return e7(h,e,d[3],0,k)}aB(1260,[0,bj,aP,g2,dZ,kv,kw,kx,e9,ky,e_,kz,kA,e$,kB,g4,kD,g5,g6,g7,kG,kH,yi,yj,fb,kI,kJ,ym,yn,fc,kK,kL,yp,cI,ys,yt,kC,fa,ff,de,g3,fd,kN,kO,g$,kP,ha,g9,kR],n4);function
kS(aB,d,w,m){var
F=m[2],G=m[1],A=a(v[28],G),aF=A[1],aG=A[2][2],aI=b(c[2][2],d,F),U=b(aA[25],aI,aG),W=s(aK),aE=1,aL=t===W?aK[1]:q===W?a(r[2],aK):aK;switch(aL){case
0:var
H=c[14];break;case
1:var
H=c[15];break;default:var
c7=[0,A,b(c[2][2],d,F)],c8=b(bH[10],aB,c7),c9=a(bg[51],c8)[2],H=a(c[13],c9)}var
aM=a(f[17][1],U),n=aF[6],l=aM-n|0,aN=b(f[17][69],c[bx],U),Y=b(f[17][X],l,aN),k=Y[2],e=Y[1],aO=V(0,k),aP=[0,a(c[26],m),aO],M=a(c[21],aP);function
Z(a){var
e=b(c[83],d,a)[2];return b(f[17][X],n,e)[2]}var
aQ=ct(d,m),aR=b(bH[18],aQ,A);function
aT(h,r){var
s=a(c[8],r),l=b(c[91],d,s),e=l[1],t=l[2],o=a(f[17][1],e),p=o-n|0,q=b(f[17][X],p,e)[1];function
u(e,k){var
l=a(aC,k)[3],f=b(c[91],d,l),g=f[2],m=f[1],n=b(c[83],d,g)[1],h=b(c[3],d,n);if(11===h[0])if(b(j[37],h[1][1],G)){var
o=Z(b(c[i][1],e+1|0,g));return[0,[0,m,e,a(c[9],e+1|0),o]]}return 0}var
v=b(D[67],u,q),w=V(0,e),x=[0,a(c[29],[0,m,h+1|0]),w],y=a(c[21],x),z=Z(t),A=1;function
B(j,e){var
g=e[1],l=e[4],m=e[3],n=e[2],d=a(f[17][1],g),r=[0,b(c[i][1],d,y),0],s=V(0,g),t=[0,b(c[i][1],d,m),s],u=[0,a(c[21],t),r],v=a(c[i][1],d),w=b(f[17][69],v,z),x=b(f[18],w,u),A=b(f[18],l,x),B=aH(p+d|0,k),C=b(f[18],B,A),D=a(f[19][12],C),E=[0,a(c[9],(o+1|0)+d|0),D],F=a(c[21],E),G=aS(n+1|0,g),H=b(c[37],F,G);return[0,h,j,b(c[37],H,q)]}return g(f[17][73],B,A,v)}var
aU=b(f[19][16],aT,aR),aV=0;function
aW(c,a){return b(f[18],c,a)}var
_=g(f[19][18],aW,aU,aV),$=aS(l,e),aX=aS(l,$);function
N(d){var
f=V(d+am.caml_mul(2-d|0,l)|0,e),g=[0,b(c[i][1],(3*l|0)+d|0,M),f];return a(c[21],g)}var
aY=N(0),a0=[0,[0,[0,a(j[1][6],zN)],0,aY],0],a1=N(1),a2=[0,[0,[0,a(j[1][6],zO)],0,a1],a0],a3=N(2),a5=[0,[0,[0,a(j[1][6],zP)],0,a3],a2],a6=b(f[18],$,e),a7=b(f[18],aX,a6),a8=i1(a5),u=3*(l+1|0)|0,a9=b(f[18],a8,a7),a_=[0,a(c[9],2),0],a$=[0,a(c[9],3),a_],ba=aH(l+3|0,e),bb=b(f[18],ba,a$),bc=aH((2*l|0)+3|0,e),bd=b(f[18],bc,bb),be=aH(u,k),bf=b(f[18],be,bd),bh=a(f[19][12],bf),bi=[0,a(c[9],(u+1|0)+n|0),bh],bj=a(c[21],bi),bk=[0,a(c[9],1),0],bl=[0,a(c[9],2),bk],bm=aH(3,e),bn=b(f[18],bm,bl),bo=aH(l+3|0,e),bp=b(f[18],bo,bn),bq=aH(u,k),br=b(f[18],bq,bp),bt=a(f[19][12],br),bu=[0,a(c[9],(u+1|0)+n|0),bt],bv=a(c[21],bu),bw=[0,a(c[9],1),0],by=[0,a(c[9],3),bw],bA=aH(3,e),bB=b(f[18],bA,by),bC=aH((2*l|0)+3|0,e),bD=b(f[18],bC,bB),bE=aH(u,k),bF=b(f[18],bE,bD),bG=a(f[19][12],bF),bI=[0,a(c[9],(u+1|0)+n|0),bG],bJ=a(c[21],bI),bL=b(c[i][1],2,bJ),bM=[0,0,b(c[i][1],1,bv),bL],bO=[0,0,bj,a(c[18],bM)],bP=a(c[18],bO);b(c[37],bP,a9);var
b8=b(o[m3],w,d),bQ=a(ap[41],[2,m[1]]),aa=b(K[5],bQ,zQ),b9=0;function
bR(a){return g(c[5],0,d,a[3])}var
bS=b(f[17][69],bR,_);function
bT(c){var
d=c[1],e=a(J[22],c[2]),f=b(J[17],zR,e),g=a(J[22],d),h=b(J[17],g,f),i=b(J[17],zS,h);return b(K[5],aa,i)}var
bU=b(f[17][69],bT,_),O=a(f[17][1],e),bV=aS(O,e),bW=b(f[18],bV,e),bX=V(2*O|0,k),bZ=[0,a(c[26],m),bX],ab=a(c[21],bZ),b0=[0,ab,V(0,e)],b1=a(c[21],b0),b2=[0,0,b(c[i][1],1,b1),H],b3=a(c[18],b2),b4=[0,ab,V(O,e)],b5=[0,0,a(c[21],b4),b3],b6=a(c[18],b5),b7=b(c[37],b6,bW),ad=[0,[0,aa,g(c[5],0,d,b7),0,bU,bS],b9],b_=0;function
b$(h){var
b=a(aC,h),e=b[2],f=b[1],i=b[3];if(e){var
j=[0,g(c[5],0,d,e[1])];return[0,a(K[10][16],f),j]}var
k=[1,g(c[5],0,d,i)];return[0,a(K[10][16],f),k]}var
ca=[0,0,0,b(f[17][69],b$,k),ad,b8,b_],Q=g(kU[2],ca,kT[5],0),R=a(v[2],0),cb=a(o[17],R),ae=p(o[mP],0,R,cb,[0,Q,0]),cc=ae[1];gJ(R,cc,w,dn(ae[2]));var
af=a(c[26],[0,[0,Q,0],F]),cd=0;function
ce(b,a){var
c=a[5],d=1;function
e(a,c){return[0,dE,w,1,0,[0,[3,[0,[0,Q,b],a]]]]}return g(f[17][73],e,d,c)}var
cf=g(f[17][73],ce,cd,ad),cg=[0,a(f[17][58],cf)];g(aJ[22],0,[0,gb,0],cg);var
ch=a(ap[41],[2,G]),ag=b(K[5],ch,zT),ah=b(K[6],zU,ag),h=[0,d],C=a(v[2],0),ai=dx(d,av(f6,h));if(a(f[17][48],e))var
ci=[0,af,V(0,k)],y=k,x=M,aj=a(c[21],ci);else
var
z=gI(0,C,h,k,M),T=z[8],as=z[6],at=z[5],au=z[3],cI=z[4],ay=b(c[B],au,C),cJ=b(c[i][2],2,2),az=b(f[17][69],cJ,cI),cK=[0,at,a(c[9],1)],cL=a(c[24],cK),cM=a(c[i][5],cL),cN=b(f[17][69],cM,az),cO=[0,at,a(c[9],2)],cP=a(c[24],cO),cQ=a(c[i][5],cP),cR=b(f[17][69],cQ,az),cS=[0,as,a(c[9],1)],cT=[0,a(c[24],cS),0],cU=[0,as,a(c[9],2)],cW=[0,a(c[24],cU),cT],cX=b(f[18],cN,cW),cY=b(f[18],cR,cX),cZ=aH(2,k),c0=aD(af,b(f[18],cZ,cY)),c1=b(c[i][1],1,T),c2=[0,[0,a(j[1][6],zZ)],c1,c0],c3=a(c[19],c2),c4=[0,[0,a(j[1][6],z0)],T,c3],c5=a(c[19],c4),c6=g(bs[9],ay,h[1],T),y=au,x=c6,aj=g(bs[9],ay,h[1],c5);var
cj=[0,av(ik,h),[0,x,aj]],ck=a(c[21],cj),cl=b(c[38],ck,y),cm=[0,av(ij,h),[0,x]],cn=a(c[21],cm),co=b(c[37],cn,k),ak=a4(ag,cl,[0,co],w,h[1],zV),al=ak[2],cp=al[2],cq=ak[1];h[1]=al[1];g(aJ[22],0,[0,gb,0],[3,[0,[1,cq],0]]);var
cr=[0,cp,V(0,k)],an=a(c[21],cr),cs=b(c[B],y,C),cu=[0,av(ii,h),[0,x,an]],cv=a(c[21],cu),cw=[0,x,[0,an,[0,aw(d7(P[4],0,0,0,0,0,0,0,cs),h,cv),0]]],ao=ew(h[1],ai,cw),cx=ao[1],aq=b(c[37],ao[2],y),cy=a(L[7],cx),ar=b(c[38],cy,y);function
cz(e,b,d){if(1===b[0]){var
c=p(ac[5],ai[1],dE,aE,[1,b[1]]);return a(ac[6],c)}throw[0,I,zW]}cV(a(v[2],0),h,ar);cV(a(v[2],0),h,aq);var
E=a(o[bN],h[1]),cA=g(c[5],zX,E,aq),cB=g(c[5],zY,E,ar),S=ax(aZ[5],C,ah,E,0,0,cB,cA),cC=S[4],cD=S[3],cE=S[1],cF=a(o[bz],E),cG=[0,a(bK[1],cz)],cH=[0,iQ(0)];bY(aZ[7],ah,[0,cD],cC,cF,0,0,[0,[0,2,w,10]],cH,0,cG,0,cE);return 0}ca([0,z1,function(a,b){return b$(kS,a,b)}]);function
kV(x,T,t,s){var
h=s[1],d=[0,T],U=s[2],y=a(v[28],h),q=y[2],u=y[1],n=u[6],z=q[6],k=q[7],B=b(f[17][69],c[bx],q[2]),W=V(0,B),Y=[0,a(c[26],s),W],Z=a(c[21],Y),C=[0,ah([0,[0,a(j[1][6],z2)],0,Z]),B],E=b(f[17][X],k+1|0,C),F=E[2],l=E[1],G=go(a(P[8],[0,o[115]]),d),H=b(c[37],G,l),_=b(c[38],G,l),$=b(c[i][1],k+1|0,_),aa=aX(k+1|0,n),I=V(0,b(D[cq],k+1|0,C)),e=a(j[1][6],z3),J=[0,[0,e],H],ab=b(c[i][1],1,H),w=a(j[1][6],z4),m=a(j[1][6],z5),r=a(j[1][6],z6),ac=[0,a(c[10],e)],ad=b(f[19][5],aa,ac),M=b(f[19][5],ad,I),af=[0,a(c[10],r),M],N=a(c[21],af),ag=b(c[37],N,l),ai=b(c[i][1],2,ag),aj=b(c[38],N,l),ak=b(c[i][1],k+1|0,aj),al=q[9];function
am(C,B){var
j=a(c[8],B),l=a(ae[47],[0,h,C+1|0]),o=a(c[10],m),D=b(c[2][2],d[1],U),E=g(bH[5],h[1],u,D),F=b(f[17][69],c[8],E),G=b(c[i][4],F,j),q=b(c[91],d[1],G)[1],H=a(f[17][1],q)-n|0,s=b(f[17][X],H,q)[1],I=u[4]-h[2]|0,J=aX(-n|0,n),K=[0,a(c[9],I),J],M=a(c[21],K),N=p(A[51],d[1],M,o,j),t=b(c[91],d[1],N)[1],O=a(f[17][1],t)-n|0,P=b(f[17][X],O,t)[1];function
Q(e,f){var
d=e[2],g=e[1],h=a(be,f),j=b(c[i][1],d,h);return[0,[0,[0,a(c[9],d),j],g],d+1|0]}var
v=g(f[17][15],Q,z7,P)[1];function
x(h,e){var
i=0;function
j(e,f){if(e){var
i=e[1],j=i[2],k=i[1],l=b(h,function(b){var
e=b[2],f=b[1],g=[0,av(du,d),[0,e,j]],h=a(c[21],g),i=[0,av(ej,d),[0,e,j,f,k]];return[0,a(c[21],i),h]},f),m=function(a){return[0,a]};return g(L[24],m,e,l)}return b(h,function(a){return a},f)}var
k=g(f[17][15],j,i,e),l=av(cw,d),m=[0,av(cx,d),l];function
n(a){return a}return g(L[24],n,m,k)}function
y(p,n,j){var
q=j[1],k=b(c[91],d[1],j[2]),e=k[1],l=b(c[83],d[1],k[2]),r=l[2];if(g(c[95],d[1],l[1],o)){var
h=a(f[17][1],e),s=aX(0,h),t=[0,b(c[i][1],h,q),s],u=[0,a(c[21],t),0],v=b(f[18],r,u),m=b(p,a(f[19][12],v),h),w=m[2],x=b(c[38],m[1],e);return[0,a(n,[0,x,b(c[37],w,e)])]}return 0}var
z=av(du,d);function
R(b,j){var
d=[0,a(c[10],m),b],f=a(c[21],d),g=[0,a(c[10],e),b],h=[0,z,[0,a(c[21],g),f]],i=a(c[21],h);return[0,a(c[9],0),i]}var
S=x(function(a,b){return y(R,a,b)},v)[2];function
T(g,j){var
k=[0,a(c[10],m),g],h=a(c[21],k),o=[0,a(c[10],e)],p=b(f[19][5],o,g),q=aX(l+j|0,n),i=b(f[19][5],q,p),s=[0,a(c[10],w),g],t=[0,a(c[21],s),[0,h]],u=a(c[21],t),v=[0,a(c[10],r),i],x=a(c[21],v),y=[0,a(c[10],e),g],A=[0,a(c[21],y),x,u,h],B=[0,av(ej,d),A],C=a(c[21],B),D=[0,a(c[10],r),i],E=a(c[21],D),F=[0,a(c[10],e),g],G=[0,z,[0,a(c[21],F),E]];return[0,C,a(c[21],G)]}var
V=x(function(a,b){return y(T,a,b)},v)[1],W=b(c[38],S,s),Y=b(c[i][1],k+1|0,W),Z=b(c[38],V,s);return[0,l,Y,b(c[i][1],k+1|0,Z)]}var
O=b(f[19][16],am,al),an=b(f[19][15],f[8],O),ao=a(c[9],1),aq=[0,g(ae[76],x,h,4),$,ao,an],ar=a(c[30],aq),as=b(f[19][15],f[9],O),at=a(c[9],1),au=[0,g(ae[76],x,h,4),ak,at,as],aw=a(c[30],au),ax=b(c[38],aw,l),ay=b(c[i][1],3,ax),az=b(c[38],ar,l),aA=b(c[i][1],2,az),aB=[0,[0,[0,z],0],[0,[0,[0,m]],[0,ab],[0,b(c[i][11],[0,m,[0,e,0]],aA)]]],aC=a(c[31],aB),aD=b(c[38],aC,[0,J,F]),aE=a(ap[41],[2,h]),aF=b(K[6],z8,aE),Q=a4(aF,aD,0,t,d[1],z9)[2],R=Q[2],aG=Q[1],aH=[0,[0,[0,z],0],[0,[0,[0,m]],[0,ai],[0,b(c[i][11],[0,m,[0,w,0]],ay)]]],aI=a(c[31],aH),aJ=a(c[i][1],1),aK=b(f[19][15],aJ,I),aL=[0,a(c[10],e),aK],aM=a(c[21],aL),aN=[0,0,a(c[21],[0,R,M]),aM],aO=a(c[18],aN),aP=b(c[37],aO,l),aQ=[0,[0,w],b(c[i][1],1,aP)],aR=b(c[36],aQ,aI),aS=b(c[i][11],[0,e,0],aR),aT=b(c[38],aS,[0,J,F]),aU=b(c[i][9],[0,[0,r,R],0],aT),aV=a(ap[41],[2,h]),aW=b(K[6],z_,aV);if(t)var
S=aG;else
var
aY=a(v[2],0),S=a(o[17],aY);a4(aW,aU,0,t,S,z$);return 0}ca([0,Aa,function(a,b){return b$(kV,a,b)}]);aB(1263,[0,kS,kV],hI);var
kW=a(f[17][69],c[bx]);function
kX(d,l){var
m=l[2],n=l[1],e=n[1],o=a(v[28],n)[1],q=o[8],r=b(c[2][2],d,m),s=a(kW,b(aA[25],r,q)),h=dD(0,function(b){return a(j[1][6],Ab)},s),k=h[3],p=h[1],t=h[2],u=a(f[17][1],k),w=a(v[2],0),x=g(f[17][16],c[cq],k,w);function
y(h,j){var
k=[0,[0,e,h],m],l=c0(0,p,a(kW,b(f[17][X],j[6],j[2])[1])),n=a(ap[41],[2,[0,e,h]]),o=[0,a(c[26],k),t],q=a(c[34],o),r=ct(d,k),s=b(ae[4],x,r);function
w(e){var
f=a(c[8],e),h=g(c[92],d,u,f)[2],j=b(c[i][4],p,h);return b(c[91],d,j)}var
y=b(f[19][15],w,s);return[0,n,q,l,y,function(f,d,b){var
i=a(v[2],0),j=[0,g(ae[76],i,[0,e,h],4),d,f,b];return a(c[30],j)}]}return[0,k,b(f[19][16],y,o[1])]}function
kY(c){var
d=av(iu,c),e=b(ac[11],c[1],d);return a(L[7],e)}function
kZ(a){return av(iv,a)}function
k0(d){function
e(b){var
d=b9(b);return a(c[10],d)}var
g=b(f[17][69],e,d);return a(f[19][12],g)}function
k1(x,e,q,n){var
k=kX(e,n),h=k[1],d=[0,e],l=kY(d)[2][1],r=a(f[19][11],k[2]);function
s(e){var
m=V(0,e[3]),k=a(c[21],[0,e[2],m]),n=[0,kZ(d),[0,k]],p=a(c[21],n),r=[0,a(j[1][6],Ac)],s=[0,a(j[1][6],Ad)],t=a(c[9],1),u=[0,a(c[9],2),t],w=[0,b(c[i][1],2,p),u],x=a(c[21],w),y=[0,s,b(c[i][1],1,k),x],z=[0,r,k,a(c[18],y)],B=a(c[18],z),C=b(c[37],B,e[3]),D=b(A[17],C,h);return[0,e,[0,D,function(j){var
n=k0(h),p=b(f[19][5],n,m),r=a(dB[4],j),s=[0,k,[0,cy(a(v[2],0),d,r,p),0]],i=b(ac[16],l,s),t=i[2],u=e[3],w=a(L[7],i[1]),x=b(c[38],w,u),y=b(A[19],x,h),z=b(o[fK],q,d[1]),B=b(c[37],t,e[3]),C=b(A[17],B,h),D=[0,g(c[5],0,d[1],C)],E=k2[6],F=aM[40][1],G=[0,[0,g(c[5],0,d[1],y),F],E];return[0,b(e8[6],0,G),0,0,D,z,0,0]}]]}var
m=b(f[17][69],s,r);function
t(g,d,e){function
c(c){var
e=c[1],f=[0,[0,a(c[2][2],d)],Ae],g=b(K[5],e[1],Af),h=[1,z(bb[3],0,0,g,0,f)],i=p(ac[5],l[1],aJ[4],1,h);return a(ac[6],i)}return b(f[17][11],c,m)}var
u=a(bK[1],t);function
w(e){var
f=e[2][1],h=b(K[5],e[1][1],Ag),i=[0,iO(0)],j=a(o[bz],d[1]),k=g(c[5],0,d[1],f);bY(aZ[7],h,0,k,j,0,0,0,i,0,[0,u],0,[0]);return 0}return b(f[17][11],w,m)}ca([0,Ah,function(a,b){return b$(k1,a,b)}]);aB(1265,[0,kX,kY,kZ,k0,k1],m6);function
hb(h,d,k,r,q){var
l=dH(h,d,z(ad[2],0,0,h,d,k))[1],m=c3(l),e=m[1],s=m[2],t=e[1][1],n=a(v[28],e[1]),j=n[2],o=n[1],u=o[1];function
w(b,d){return a(c[26],[0,[0,t,b],e[2]])}var
x=b(f[19][16],w,u),y=a(f[19][11],x),A=a(f[17][9],y);a(f[17][1],j[2]);var
p=o[6],B=g(ae[76],h,e[1],4),C=j[9],D=j[4];function
E(n,m,l){var
o=a(c[8],l),r=b(c[i][4],A,o),g=b(c[91],d,r),h=g[1],t=g[2],u=a(f[17][1],h)-p|0,v=b(f[17][X],u,h)[1],w=b(c[37],t,v),x=a(f[17][9],s),y=b(c[i][4],x,w),j=b(c[91],d,y),k=j[1],z=d5(q,e,n,m,p,k,j[2]);return b(c[38],z,k)}var
F=g(f[19][59],E,D,C);return ax(ae[77],h,d,l,B,r,k,F)}function
k3(u,az,Q,k){var
R=k[1],d=[0,az],aB=k[2],S=a(v[28],R),T=S[2],aC=S[1],aD=T[2],aE=b(c[2][2],d[1],aB),aF=b(aA[25],aE,aD),aG=b(f[17][69],c[bx],aF),U=c2(d[1],aG),aH=a(f[17][1],U),V=aC[6],aI=T[6],W=aX(0,aH),X=b(f[19][55],V,W)[2];if(0===X.length-1)var
aJ=a(c[9],1),aL=[0,a(c[26],k),W],m=a(c[21],aL),y=aJ,w=U,l=0;else{var
h=dQ(u,d[1],k),as=h[3],bw=h[8],by=h[7],bA=h[6],bB=h[2];d[1]=h[1];var
at=s(ai),bD=t===at?ai[1]:q===at?a(r[2],ai):ai,au=b(P[9],d[1],bD),bE=au[2];d[1]=au[1];var
bF=g(c[5],0,d[1],bB),bG=a(f[17][1],as),bH=b(bg[34],bG,bF)[2],bI=[0,bE,[0,bw,a(c[8],bH)]],bJ=a(c[21],bI),bL=b(f[17][cq],by,bA),ay=s(al),bM=a(c[9],1),bN=t===ay?al[1]:q===ay?a(r[2],al):al,bO=a(c[24],[0,bN,bM]),m=g(N[20],u,d[1],bJ),y=bO,w=as,l=bL}var
aM=av(dt,d),aN=av(b4,d),Y=a(j[1][6],Ai),A=a(j[1][6],Aj),C=[0,ah([0,[0,Y],0,m]),w],Z=s(aK),aO=[0,ah([0,[0,A],0,b(c[i][1],1,m)]),C],aP=t===Z?aK[1]:q===Z?a(r[2],aK):aK,aQ=o[hA],aR=aw(function(a){return g(aQ,0,0,a)},d,aP),D=a(c[13],aR),aT=b(c[37],D,aO),_=b(c[B],C,u),$=g(x[1][15],c[9],0,w);function
aa(a){return b(c[i][1],a,m)}function
ab(d){var
e=a(c[i][1],d),g=b(f[19][15],e,$),h=b(f[19][5],g,X),j=[0,a(c[26],k),h];return a(c[21],j)}var
ae=a(f[17][1],l),aU=aa(aI+2|0),aW=aS(1,l),aY=[0,ah([0,[0,Y],0,ab(ae+1|0)]),aW],a0=ah([0,0,0,aU]),a1=b(c[35],a0,D),a2=b(c[38],a1,aY);function
a3(w,o,v,u,e,t){var
g=[0,[0,A],0,aa(a(f[17][1],e)+1|0)],j=[0,ah(g),e],h=b(c[B],j,_),k=aS(a(f[17][1],e)+2|0,l),m=[0,[0,A],0,ab((a(f[17][1],e)+ae|0)+2|0)];function
n(t,p,s,r,j,q){if(o===p){if(0===a(f[17][1],e))return aM;var
k=b(c[B],j,h),l=c7(d,e)[3],m=c7(d,j)[3],n=a(f[17][1],e)+1|0,g=b(c[i][1],n,l);return b5(k,d,z(ad[2],0,0,k,d[1],g),g,m)}return aN}var
p=[0,ah(m),k],q=b(c[38],D,p),r=hb(h,d[1],y,q,n),s=ah(g);return b(c[36],s,r)}var
a4=hb(_,d[1],y,a2,a3),a5=b(c[38],a4,C),a6=h$(0,[0,Q],d[1],[0,aT],a5)[2],E=a(ap[41],[2,R]),a7=b(K[6],Ak,E),a8=b(K[6],Al,E),a9=b(K[6],Am,E),a_=z(bb[3],0,0,a7,0,[0,[0,a6],An]),af=a(v[2],0),e=[0,a(o[17],af)],aj=s(bC),a$=t===aj?bC[1]:q===aj?a(r[2],bC):bC,F=a(ac[8],a$),ak=aV(e,[1,a_]),ba=aV(e,F[2]),bc=b(c[77],e[1],ba)[2],bd=a(c[21],[0,ak,$]),be=z(ad[2],0,0,af,e[1],ak),am=g(c[92],e[1],V,be),G=am[1],an=b(c[3],e[1],am[2]);if(6===an[0]){var
ao=ew(e[1],[0,F,bc],[0,an[2],[0,bd,0]])[1],bf=a(v[2],0),n=b(c[B],G,bf),bk=a(L[7],ao),bl=z(ad[2],0,0,n,e[1],bk),J=a(L[7],ao),H=bl;for(;;){var
M=b(c[3],e[1],H);if(6===M[0]){var
bh=M[3],bi=M[2],aq=aw(d7(P[4],0,0,0,0,0,0,0,n),e,bi),bj=b(c[i][5],aq,bh),J=a(c[21],[0,J,[0,aq]]),H=bj;continue}var
ar=b(c[38],J,G),bm=b(c[37],H,G);aw(b(ag[2],0,n),e,ar);var
bn=function(e,b,d){var
c=p(ac[5],F,dE,1,b);return a(ac[6],c)},bo=g(c[5],0,e[1],bm),bp=g(c[5],Ap,e[1],ar),O=ax(aZ[5],n,a8,e[1],0,0,bp,bo),bq=O[4],br=O[3],bs=O[1],bt=[0,a(bK[1],bn)],bu=[0,iM(0)],bv=a(o[bz],e[1]);bY(aZ[7],a9,[0,br],bq,bv,0,0,[0,[0,2,Q,0]],bu,0,bt,0,bs);return 0}}throw[0,I,Ao]}ca([0,Aq,function(a,b){return b$(k3,a,b)}]);aB(1266,[0,hb,k3],nu);function
Ar(k,i,d,h,c){if(a(j[1][10][2],c))return 0;var
e=[0,c,0];return g(es,function(f,e){var
d=f[2],a=f[1],c=b9(e);if(b(j[1][10][3],c,h))return[0,a,d];if(b(j[1][10][3],c,a))return[0,a,[0,c,d]];var
l=g(A[99],k,i,e),m=j[1][10][1],n=b(j[1][10][9],l,a);return b(j[1][10][11],n,m)?[0,a,d]:[0,b(j[1][10][4],c,a),[0,c,d]]},e,d)[2]}var
hc=[cQ,As,cM(0)];function
k4(e,d,c){var
a=[0,d];try{var
h=function(c){var
d=gj(e,At,j[1][10][1],c),f=a[1];function
h(c,a){if(b(j[1][10][3],c,a))throw hc;return b(j[1][10][4],c,a)}a[1]=g(j[1][10][15],h,d,f);return 0};b(f[19][13],h,c);var
i=1;return i}catch(a){a=w(a);if(a===hc)return 0;throw a}}function
k5(e,h){var
d=e[2];b(E[20],h,e);var
i=a(cZ,b(E[18],e,h)),k=i[2],q=i[3];if(k)var
l=b(c[83],d,k[1]),m=l[1],g=l[2];else
var
p=b(c[83],d,q),m=p[1],g=p[2];if(0===g)return 0;var
n=gk(d,m,a(f[19][12],g)),o=n[2];if(k4(d,gj(d,Au,j[1][10][1],n[1]),o)){var
r=function(e){var
f=1-b(c[45],d,e);if(f)return f;var
g=b(c[68],d,e);return a(A[a_],g)};return b(f[19][32],r,o)}return 1}function
hd(l,e,d){var
r=l?l[1]:1,h=d[2],m=b(E[17],d,e),i=b(c[3],h,m);if(9===i[0])var
C=gk(h,i[1],i[2])[2],n=a(f[19][11],C);else
var
n=0;function
o(e){var
f=b(c[3],h,e);if(1===f[0])return f[1];var
i=a(E[2],d),k=a(E[8],d),l=g(au[9],k,i,e),m=a(j[1][6],l);return b(E[20],m,d)}var
s=a(E[8],d);function
t(f,b){var
h=b[3],i=b[2],j=b[1],k=f[1],l=a(E[2],d),e=z(k6[6],s,l,Av,j,k),m=e[2];return[0,g(c[39],i,h,e[1]),m]}function
v(a){var
c=b(ep,d,a);return[0,a,o(a),c]}var
p=b(f[17][14],v,n),q=r?[0,[0,e,o(e),m],p]:p,w=a(E[2],d),x=[0,a(E[7],d),w],y=g(f[17][15],t,x,q)[1],A=aD(y,b(f[17][14],f[7],q)),B=b(u[5],A,2);return b(k[71][7],B,d)}function
k7(C,d){var
D=d[1],E=a(ap[41],[2,d]),t=a(v[28],d),k=t[2],u=t[1],F=u[1];function
G(b,d){return a(c[25],[0,D,b])}var
H=b(f[19][16],G,F),I=a(f[19][11],H),L=a(f[17][9],I),m=u[6],h=b(f[17][69],c[bx],k[2]),M=a(f[17][1],h)-m|0,w=b(f[17][X],M,h),x=w[2],l=w[1],n=a(f[17][1],l),N=V(0,h),O=[0,a(c[25],d),N],q=a(c[21],O),Q=a(v[2],0),e=[0,a(o[17],Q)],R=[0,[0,0,q],l],S=P[8],T=go(function(a){return b(S,0,a)},e),U=b(c[37],T,R),r=k[9].length-1,W=k[9],Y=k[4];function
Z(g,G,o){var
p=a(c[8],o),q=b(c[i][4],L,p),k=b(c[91],e[1],q),l=k[1],s=b(c[83],e[1],k[2])[2],t=b(f[17][X],m,s)[2],h=a(f[17][1],l)-m|0,n=aS(g+1|0,b(f[17][X],h,l)[1]),u=V(0,n),v=V((h+g|0)+1|0,x),w=b(f[19][5],v,u),y=[0,a(c[27],[0,d,g+1|0]),w],z=[0,a(c[21],y),0],A=b(f[18],t,z),B=aD(a(c[9],(h+g|0)+1|0),A),C=a(c[9],(1+r|0)-g|0),D=b(c[37],B,n),E=a(J[22],g),F=b(J[17],Aw,E);return[0,[0,[0,a(j[1][6],F)],D],C]}var
s=g(f[19][59],Z,Y,W),_=a(v[2],0),$=g(ae[76],_,d,4);function
y(e){var
g=V(0,l),h=V((n+r|0)+e|0,x),i=b(f[19][5],h,g),j=[0,a(c[25],d),i];return a(c[21],j)}var
A=[0,[0,0,y(2+n|0)],l],aa=V(0,A),ab=[0,a(c[9],(n+r|0)+3|0),aa],ac=a(c[21],ab),ad=b(c[38],ac,A);function
af(a){return a[2]}var
ag=b(f[19][15],af,s),ah=[0,$,ad,a(c[9],1),ag],ai=a(c[30],ah),aj=y(1),ak=e[1],al=a(v[2],0),am=p(au[11],al,ak,aj,0),an=1+(s.length-1)|0,ao=[0,[0,[0,a(j[1][6],Ax)],U],h];function
aq(a){return a[1]}var
ar=b(f[19][15],aq,s),as=a(f[19][11],ar),at=a(f[17][9],as),av=b(f[18],at,ao),aw=[0,[0,am,b(c[i][1],an,q)],av],ay=b(c[38],ai,aw),az=b(o[fK],C,e[1]),aA=g(c[5],0,e[1],ay),aB=ax(bb[2],0,0,0,0,[0,az],0,aA),aC=b(K[5],E,Ay),aE=[1,z(bb[3],0,0,aC,0,[0,[0,aB],Az])],B=a(v[2],0);return[0,B,a(o[17],B),h,q,aE]}function
k8(w,l,f,k){var
g=k[1],d=k7(f,g),h=d[3],m=d[5],n=d[4],o=d[2],p=d[1],q=a(ap[41],[2,g]),e=[0,o],r=b(K[6],AA,q),s=iD(e),i=aV(e,m),t=z(ad[2],0,0,p,e[1],i),j=V(0,h),u=[0,a(c[21],[0,i,j]),0],v=[0,n,[0,gn(l,t,j),u]];return dq(r,f,e[1],h,s,v)}function
AB(d,c,b,a){k8(d,c,b,a);return 0}ca([0,AC,function(a,b){return b$(AB,a,b)}]);function
k9(i,e,d){var
q=i?i[1]:1,l=a(E[8],d),h=a(E[2],d),r=b(ep,d,e),s=a(E[9],d),t=a(A[77],s),v=a(j[1][10][35],t),m=b(c[3],h,e),w=9===m[0]?a(f[19][11],m[2]):0;function
n(d){var
e=b(c[3],h,d);if(1===e[0])return e[1];var
f=g(au[9],l,h,d),i=a(j[1][6],f);return b(au[26],i,v)}function
x(e,b){var
f=b[3],h=b[2],i=b[1],j=a(E[2],d),k=z(k6[6],l,j,AD,i,e)[1];return g(c[39],h,f,k)}function
y(a){var
c=b(ep,d,a);return[0,a,n(a),c]}var
o=b(f[17][14],y,w),p=q?[0,[0,e,n(e),r],o]:o,B=a(E[7],d),C=g(f[17][15],x,B,p),D=aD(C,b(f[17][14],f[7],p)),F=b(u[5],D,2);return b(k[71][7],F,d)}function
d0(d,h,f){if(!b(c[45],d,f))if(!b(c[44],d,f)){var
i=b(c[3],d,h),j=b(c[3],d,f);if(9===i[0])if(9===j[0]){var
k=j[2],l=i[2],m=i[1];if(g(c[96],d,m,j[1]))if(b(c[56],d,m)){var
o=b(c[78],d,m)[1],e=a(ae[47],o);if(e<=l.length-1)if(e<=k.length-1){var
q=g(a8[7],l,l.length-1-e|0,e),r=g(a8[7],k,k.length-1-e|0,e),s=function(a,b){return d0(d,a,b)};return g(a8[35],s,q,r)}var
t=function(a,b){return d0(d,a,b)};return p(c[na],d,t,h,f)}}var
n=function(a,b){return d0(d,a,b)};return p(c[na],d,n,h,f)}return 1}function
AF(Z,D,x){var
M=a(E[8],x),aj=b(E[19],x,D),d=[0,a(E[2],x)],aK=a(c[10],D),aL=0,aM=0,aN=0,aO=Z?0:1,w=aO,o=aN,k=aM,h=aL,n=aK,l=aj;for(;;){var
v=b(c[3],d[1],l);switch(v[0]){case
6:var
N=v[3],G=v[2],$=v[1];if(!w){var
aG=[0,a(c[9],1)],aH=[0,b(c[i][1],1,n),aG],aI=a(c[21],aH),w=0,k=[0,ar($,0,G),k],n=aI,l=N;continue}var
O=b(c[3],d[1],G);if(9===O[0]){var
H=O[2];if(3===H.length-1){var
aa=O[1],I=H[2],Q=H[3],ab=s(bc),at=H[1],au=t===ab?bc[1]:q===ab?a(r[2],bc):bc;if(g(c[aQ],d[1],au,aa)){var
av=a(f[17][1],h);if(p(c[i][14],d[1],1,av,I))var
V=1;else{var
aF=a(f[17][1],h);if(p(c[i][14],d[1],1,aF,Q))var
V=1;else
var
U=1,V=0}if(V){var
A=b(c[3],d[1],aa);switch(A[0]){case
10:var
W=A[1],R=[0,[1,W[1]],W[2]];break;case
11:var
X=A[1],R=[0,[2,X[1]],X[2]];break;case
12:var
Y=A[1],R=[0,[3,Y[1]],Y[2]];break;default:throw[0,bo,AE]}var
ax=R[2],ay=a(f[17][1],h);if(p(c[i][14],d[1],1,ay,I))var
J=I,S=Q;else
var
J=Q,S=I;var
ac=s(cv),az=t===ac?cv[1]:q===ac?a(r[2],cv):cv,aA=[0,ev(d[1],[0,az,ax]),[0,at,J]],ad=a(c[21],aA);if(d0(d[1],J,S)){var
aB=b(c[B],k,M),ak=dC(J,h),al=dC(S,h),_=z(dV[6],aB,0,d[1],al,ak),aD=_?(d[1]=_[1],1):0;if(aD){var
aE=b(c[i][5],ad,N),o=1,n=a(c[21],[0,n,[0,ad]]),l=aE;continue}}var
K=[0,n,o,k,h,l],L=1,U=0}}else
var
U=1;if(U)var
L=0}else
var
L=0}else
var
L=0;if(!L){if(!o){var
am=dC(G,h),an=b(c[B],k,M),ao=aw(d7(P[4],0,0,0,0,0,0,0,an),d,am),ap=[0,a(c[9],1)],aq=[0,b(c[i][1],1,n),ap],as=a(c[21],aq),o=0,h=[0,ar($,[0,ao],G),h],n=as,l=N;continue}var
K=[0,n,o,k,h,l]}break;case
8:var
y=v[4],ae=v[3],T=v[2],af=v[1],ag=s(aW),aJ=t===ag?aW[1]:q===ag?a(r[2],aW):aW;if(!g(c[aQ],d[1],aJ,T)){if(w){var
h=[0,ar(af,[0,T],ae),h],l=y;continue}var
k=[0,ar(af,[0,T],ae),k],l=y;continue}if(!Z){var
w=1,l=b(c[i][5],c[14],y);continue}if(!w){var
w=1,l=b(c[i][5],c[14],y);continue}var
K=[0,n,o,k,h,b(c[i][5],c[14],y)];break;default:var
K=[0,n,o,k,h,l]}var
aP=K[5],aR=b(P[36],d[1],h),aS=function(f){var
e=a(aC,f),g=e[2],h=e[3],i=e[1];if(g)if(b(c[47],d[1],g[1]))return[0,i,h];return f},ah=b(f[17][69],aS,aR),aT=b(c[37],aP,ah),aU=b(c[38],n,ah),aV=b(c[37],aT,k),aX=b(c[38],aU,k),ai=b(P[30],d[1],aV),aY=b(P[30],d[1],aX);if(o){var
aZ=F(a(u[42],aY)),a0=F(b(u[mJ],D,ai));return g(m[9],a0,aZ,x)}var
a1=g(C[15],M,d[1],ai),a2=a(j[1][9],D),a3=a(e[3],AG),a4=b(e[12],a3,a2),a5=b(e[12],a4,a1);return g(m[24],0,a5,x)}}function
he(f,c,b){try{a(F(a(u[75],[0,c,0])),b);var
i=0,d=i}catch(b){b=w(b);if(!a(_[18],b))throw b;var
d=1}if(d){var
h=a(e[3],AH);return g(m[24],0,h,b)}return AF(f,c,b)}function
d1(r,d){function
h(l){var
t=a(k[67][4],l),u=a(ay[46],t),v=a(k[67][3],l),n=d[2],s=d[1];function
y(b){var
c=a(x[2][1][1],b);return a(A[a_],c)}var
p=b(D[co],y,v),o=p[1],h=b(c[b2],p[2],u);function
z(A){var
t=er(o),d=t[1],B=t[2],D=dD(AJ,function(a){throw[0,I,AI]},d)[2],E=a(k[67][2],l),F=a(k[67][5],l),p=b(c[i][11],B,E),u=[0,[0,b(aL[1],0,AK)],AL];function
G(k){if(Z[1]){var
l=a(dI(h),k),m=a(e[5],0),n=a(e[3],AM),o=b(e[12],n,m),q=b(e[12],o,l);b(M[6],0,q)}var
r=[0,AO,0,AN,a7[1],0],t=b(c[37],p,d),u=[0,s,a(j[1][6],AP),t,d,p,0,0,0],v=$(d),w=g(x[1][14],c[9],0,d);function
y(l){var
d=[0,l],m=[0,cj(h,d,de(0,h,d,u,r,k,0,v,0,p))[1],w],n=b(N[56],d[1],m),o=a(f[17][9],D),j=b(c[i][4],o,n);if(Z[1]){var
q=g(C[15],h,d[1],j),s=a(e[3],AQ),t=b(e[12],s,q);b(M[10],0,t)}return[0,d[1],j]}return b(cF[2],1,y)}if(r)var
H=r[1],J=function(c,e){function
d(f){var
d=a(x[2][1][1],f);return b(j[1][1],d,n)?b(W[3],c,e):b(W[3],0,[0,d,0])}return[0,c,b(f[17][14],d,o),[0,u]]},K=a(W[9],J),O=b(f[17][69],K,H),v=a(k[16],O);else{var
w=fb([0,h,[0,F]],A,d);if(w){var
y=w[1];if(0===y[0])var
P=a(f[19][11],y[1][3]),Q=a(L[31][2],P),R=function(a){return jx(0,0,a)},S=b(f[17][69],R,Q),T=function(a){return[0,[0,s],a,[0,u]]},U=b(f[17][69],T,S),z=a(k[16],U),q=1;else
var
q=0}else
var
q=0;if(!q)var
V=a(j[1][9],n),X=a(e[3],AR),Y=b(e[12],X,V),z=b(m[66][5],0,Y);var
v=z}return b(k[72][1],v,G)}try{var
G=function(f,e){var
d=f,c=e;for(;;){if(c){var
g=c[2],h=a(x[2][1][1],c[1]);if(b(j[1][1],n,h))return d;var
d=d+1|0,c=g;continue}throw O}}(1,o),H=a(k[16],G),q=H}catch(c){c=w(c);if(c!==O)throw c;var
B=a(j[1][9],n),E=a(e[3],AS),F=b(e[12],E,B),q=b(m[66][5],0,F)}return b(k[72][1],q,z)}return a(k[67][9],h)}aB(1268,[0,Ar,hc,k4,k5,hd,k7,k8,k9,he,d0,d1,function(c,g){function
d(h){var
i=a(k[67][4],h);if(c)var
d=c[1],j=[0,eD(0,d)],l=function(b){var
c=gv(i,0,[0,j],0,b);return a(f[17][5],c)},e=[0,b(f[17][69],l,d)];else
var
e=0;return d1(e,g)}return a(k[67][9],d)}],m0);function
k_(R,ao){function
aq(c){var
f=c[2],d=b(at[32],0,c[1]);try{var
k=a(ap[11],d);return k}catch(c){c=w(c);if(c===O){var
h=a(e[3],AW),i=a(at[27],d),j=b(e[12],i,h);return g(_[6],f,0,j)}throw c}}var
S=b(f[17][69],aq,ao),d=a(v[2],0),V=a(o[17],d);function
W(f,e){var
a=p(o[nC],0,d,f,e),c=a[2],g=a[1];return[0,g,[0,c,b(j4[28],d,c)]]}var
t=g(D[91],W,V,S),l=t[2],m=t[1],B=a(f[17][5],l)[2],i=a(bg[31],B)[1],u=a(f[17][5],i)[2],x=a(G[26],u);if(9===x[0])var
y=a(G[68],x[1]),C=[0,i,y,b(bH[4],d,y[1])[2][6]];else
var
C=[0,i,a(G[68],u),0];var
X=C[3];function
Y(b){var
e=a(c[8],b[2]);return 0===z(ad[4],0,0,d,m,e)?1:0}if(b(f[17][21],Y,l))var
F=ig,E=ih;else
var
F=du,E=ej;var
Z=a(c[8],B),n=b(A[66],m,Z)-(X+1|0)|0,H=aN(m,F),$=H[2],I=aN(H[1],E),q=I[1],aa=I[2],ab=b(A[8],0,n);function
ac(c){var
d=c[1],e=b(bg[33],n,c[2])[2],f=[0,a(G[16],d),ab];return[0,a(G[13],f),e]}var
r=b(f[17][14],ac,l);function
ae(d,b){var
e=b[2],f=d[2],h=b[1],i=d[1],j=[0,g(c[5],0,q,$),[0,e,f]],k=a(G[13],j),l=[0,g(c[5],0,q,aa),[0,e,f,h,i]];return[0,a(G[13],l),k]}var
K=r?g(f[17][15],ae,r[1],r[2]):b(_[9],0,AU),af=K[2],ah=K[1];function
ai(a){return[0,a[1],a[2]]}var
k=0,j=0,h=b(f[17][14],ai,i);for(;;){if(h){var
s=h[2],U=h[1];if(k!==n){var
k=k+1|0,j=[0,U,j],h=s;continue}var
L=[0,j,s]}else
var
L=a(J[3],AT);var
M=L[1],aj=function(c,a){return b(bg[6],a,c)},N=g(f[17][15],aj,af,M),P=b(A[16],ah,M),ak=a(c[8],N),al=a(c[8],P),Q=p(ag[6],d,q,al,ak),ar=b(e8[6],0,[0,[0,P,aM[40][1]],k2[6]]),as=[1,a(f[17][5],S)],au=a(v[45],as),T=R[1],av=[0,N],am=a(bb[3],[0,2]),an=au?[1,a(o[bw],Q)]:[0,a(o[fT],Q)];p(am,0,T,0,[0,[0,[0,ar,0,0,av,an,0,0]],AV]);a(bb[7],T);return b(bb[9],0,[0,R[1],0])}}aB(1269,[0,k_],"Combined_scheme");function
k$(d,c){try{var
g=d[5],h=function(d){var
e=d[1],f=a(aF[16],c);return b(j[68][1],[1,e],f)},i=b(f[17][27],h,g);return i}catch(b){b=w(b);if(b===O)return bE(0,a(e[3],AX));throw b}}function
la(c){var
b=a(aJ[15],AY);return a(aJ[14][14],b)}function
AZ(b){return F(a(u[62],[0,b,0]))}var
A0=a(m[53],AZ),A1=F(u[61]),A2=b(m[5],A1,A0);function
fg(c,a){var
d=b(f[18],a,A3),e=[0,la(0)];return F(z(hf[7],0,e,0,c,d))}function
hg(a){return b(J[17],a[3],A4)}function
A5(b){var
c=fg(0,b),d=[0,a(m[21],c),0],e=g(cJ[2],0,m[66][2],b),f=[0,a(k[71][7],e),d],h=a(m[7],f);return a(m[17],h)}function
hh(c,b){var
d=fg(0,b),e=[0,a(m[21],d),0],f=g(cJ[6],0,b,c),h=[0,a(k[71][7],f),e],i=a(m[7],h);return a(m[17],i)}function
lb(b){var
c=g(cJ[2],0,m[66][2],[0,b,0]),d=a(k[71][7],c);return a(m[17],d)}function
fh(c){var
f=a(cJ[4],c);function
d(c){if(c){var
f=c[1],l=c[2],h=a(aF[16],f[1]),n=f[5]?df[3]:df[4],o=function(a){return b(n,0,a)},p=a(m[66][61],h),i=b(k[17],p,o),q=function(f){if(Z[1]){var
c=a(e[3],A6);b(M[10],0,c)}return d(l)};if(Z[1])var
r=function(c){var
d=a(k[67][2],c),f=a(k[67][5],c),j=a(k[67][4],c),l=g(C[15],j,f,d),m=a(e[3],A7),n=a(C[58],h),o=a(e[3],A8),p=b(e[12],o,n),q=b(e[12],p,m),r=b(e[12],q,l);b(M[10],0,r);return i},j=a(k[67][9],r);else
var
j=i;return b(k[22],j,q)}var
s=a(e[3],A9);return b(m[66][4],0,s)}var
h=d(f);return a(k[71][7],h)}function
A_(b){var
c=[0,b9(a(E[42][17],b)),0];return a(u[83],c)}var
hi=a(k[67][9],A_);function
lc(c,b,a){if(a){var
f=a[2],d=d6(P[4],0,0,0,0,0,0,0,c,b,a[1]),g=d[2],e=lc(c,d[1],f);return[0,e[1],[0,g,e[2]]]}return[0,b,0]}function
ld(o,h,n,m){var
f=o,j=n,i=m;for(;;){var
p=b(A[60],h,i),d=b(c[3],h,p);switch(d[0]){case
6:var
k=d[2],t=d[3],u=d[1];if(1===j)try{var
l=g(ae[72],f,h,k)[1],v=[0,l[1][1],l[2]];return v}catch(a){a=w(a);if(a===O)return bf(Ba);throw a}var
f=b(c[aR],[0,u,k],f),j=j-1|0,i=t;continue;case
8:var
x=d[4],f=b(c[aR],[1,d[1],d[2],d[3]],f),i=x;continue;default:var
q=g(C[15],f,h,i),r=a(e[3],A$),s=b(e[12],r,q);return g(_[6],0,0,s)}}}function
fi(v,q){function
d(F){function
d(d){function
h(av){var
r=b(f[17][69],k[10],av);function
H(c){var
e=b(o[23],d,c);return a(o[4],e)}var
l=b(f[17][69],H,r);function
K(e){var
f=b(o[23],d,e),g=a(o[5],f);return a(c[aE][4],g)}var
L=b(f[17][69],K,r),y=a(f[17][fH],L),z=y[1],M=y[2];function
N(a){return g(x[2][6],G[74],z,a)}var
m=b(f[17][21],N,M)?b(ay[35],z,F):F;if(v)var
h=b(f[17][69],j[1][6],v);else
var
au=function(f,e){var
c=b(o[53],e,d);if(c)return c[1];var
g=a(J[22],f),h=b(J[17],Bo,g);return a(j[1][6],h)},h=b(f[17][13],au,r);var
n=a(f[17][1],h),s=a(f[17][1],q),t=a(f[17][1],l),A=n===s?1:0,P=A?n===t?1:0:A;if(1-P){var
Q=b(f[15][46],t,Bb),R=a(e[3],Q),S=a(e[16],t),T=a(e[3],Bc),U=1===s?Bd:Bn,V=a(e[3],U),W=a(e[16],s),X=a(e[3],Be),Y=b(f[15][46],n,Bf),Z=a(e[3],Y),$=a(e[16],n),aa=a(e[3],Bg),ab=b(e[12],aa,$),ac=b(e[12],ab,Z),ad=b(e[12],ac,X),ae=b(e[12],ad,W),af=b(e[12],ae,V),ag=b(e[12],af,T),ah=b(e[12],ag,S),ai=b(e[12],ah,R);g(_[6],0,Bh,ai)}function
aj(c,b,a){return[0,c,b,a]}var
B=p(D[75],aj,h,q,l),C=a(f[17][5],B),ak=ld(m,d,C[2],C[3])[1];function
al(p,o){var
h=p,f=o;for(;;){if(f){var
i=f[1],l=i[3],k=i[1],q=f[2],r=ld(m,d,i[2],l)[1];if(1-b(j[23][13],ak,r))bf(Bi);try{b(x[2][5],k,h);var
z=1,n=z}catch(a){a=w(a);if(a!==O)throw a;var
n=0}if(n){var
s=a(e[3],Bj),t=a(a6[9],k),u=a(e[3],Bk),v=b(e[12],u,t),y=b(e[12],v,s);g(_[6],0,Bl,y)}var
h=[0,[0,k,g(c[5],0,d,l)],h],f=q;continue}return h}}var
am=al(a(ay[10],m),B);function
an(a){return a-1|0}var
E=b(f[19][54],an,q);function
ao(a){return[0,a]}var
ap=b(f[19][54],ao,h),u=[0,function(a){throw[0,I,Bm]}];function
aq(e){var
g=a(ay[30],am),d=lc(b(ay[47],g,m),e,l),j=d[2],k=d[1],n=a(f[17][9],h),o=a(c[i][11],n),p=b(f[19][54],o,j),q=[0,ap,a(f[19][12],l),p];u[1]=function(b){return a(c[31],[0,[0,E,b],q])};return[0,k,a(u[1],0)]}var
ar=b(cF[2],0,aq);function
as(c){function
d(b){return[0,b,a(u[1],c+1|0)]}return b(cF[2],0,d)}var
at=[0,ar,b(f[17][56],E.length-1-1|0,as)];return a(k[37],at)}return b(k[72][1],k[65][6],h)}return b(k[72][1],k[55],d)}return b(k[72][1],k[56],d)}function
Bp(i,c){var
j=a(f[17][5],i),l=a(k[10],j);try{var
d=b(o[23],c,l),e=d[3];if(e){var
m=e[1],n=a(o[12],d);g(ae[81],n,c,m);var
h=1}else
var
h=1;return h}catch(a){a=w(a);if(a[1]===fj[1])return 0;throw a}}function
fk(f,e,d){var
a=k$(f,e),b=a[2],c=b[2],g=a[1],h=aa(d,c)[c+1];return[0,g,b[2],h]}function
le(d,h,i,g){function
e(h,g){var
j=a(f[17][1],g);if(a(f[17][1],h)===j){var
k=0,l=function(g,a,j){if(g)return g;var
f=b(c[83],d,j);switch(a[0]){case
0:var
h=f[1];if(a[1]===i)return[0,b(c[68],d,h)];break;case
1:return e(a[2],f[2])}return 0};return p(f[17][19],l,k,h,g)}throw[0,I,Bq]}var
j=e(a(f[17][9],h),g);return a(L[7],j)}function
d2(d){var
e=a(E[7],d),f=a(E[2],d);switch(b(c[3],f,e)[0]){case
6:var
h=F(u[16]);return g(m[5],h,d2,d);case
8:var
i=F(u[58]);return g(m[5],i,d2,d);default:return a(m[1],d)}}function
hj(d){var
c=[0,F(b(u[aR],0,0)),0];return a(m[7],c)}function
lf(a){return 1===a[0]?[0,a[4]]:0}function
lg(a){return 4===a[0]?[0,a[3]]:0}function
lh(a){return 0===a[0]?[0,[0,a[1],a[2]]]:0}function
fl(c,b){return b?a(c,b[1]):0}function
fm(a){var
b=[0,hg(a),0];return bd(Bs,aj(fg(Br,[0,a[3],b])))}function
li(h,g,b,e){var
d=a(f[19][8],g);aa(d,b)[b+1]=e;return a(c[21],[0,h,d])}function
lj(a){function
c(d){function
c(a){return b(v[53],[0,a[1]],1)}return b(f[17][11],c,a)}return[0,c,function(d){function
c(a){return b(v[53],[0,a[1]],a[2])}return b(f[17][11],c,a)}]}function
lk(d){var
a=d[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
b=a[1];if(b)return[0,b[1][1]+1|0];throw[0,I,Bu]}var
c=a[1];return c?[0,c[1][1]+1|0]:Bv}}function
fn(h,J,U,n,l,d){function
r(q,l,s,W){var
d=W;for(;;)switch(d[0]){case
0:var
n=d[2],Y=d[4],an=d[1][1],P=fl(lh,l);if(P)var
Q=P[1],_=Q[2],ab=Q[1],ac=function(a){return[0,a]},ae=b(f[17][69],ac,_),R=a(f[7],ab),t=ae;else
var
aJ=function(a){return 0},R=0,t=b(f[17][69],aJ,n);if(a(f[17][48],n))var
y=m[1];else{var
ay=function(d,n,l,y,H){var
p=H[2],J=l[1],ao=H[1];if(y){var
t=y[1];try{var
N=b(a9[22],t[4],h[3])}catch(a){a=w(a);if(a===O)throw[0,I,BD];throw a}var
aq=N[2],ar=N[1];if(Z[1]){var
as=g(C[15],d,n,ar),au=a(e[3],BE),av=g(C[15],d,n,t[7]),aw=a(e[3],BF),ax=aT(t),ay=g(C[15],d,n,ax),az=a(e[3],BG),aA=a(e[3],BH),aB=b(e[12],aA,az),aC=b(e[12],aB,ay),aE=b(e[12],aC,aw),aF=b(e[12],aE,av),aG=b(e[12],aF,au),aJ=b(e[12],aG,as);b(M[10],0,aJ)}var
P=t[1],aK=a(f[17][1],R),aL=a(f[17][1],P[1][4])-aK|0,Q=b(f[17][X],aL,P[1][4]),aM=Q[1],aN=a(f[17][1],Q[2]),E=aN<=a(f[17][1],p)?p:bE(0,a(e[3],BY)),aO=aT(t),S=b(c[i][4],E,aO),T=c0(0,E,aM);if(Z[1]){var
aP=cd(d,o[16],T),aQ=a(e[3],BI),aR=g(C[15],d,n,S),aS=a(e[3],BJ),aU=b(C[15],d,n),aV=g(e[39],e[13],aU,E),aW=a(e[3],BK),aX=b(e[12],aW,aV),aY=b(e[12],aX,aS),aZ=b(e[12],aY,aR),a0=b(e[12],aZ,aQ),a1=b(e[12],a0,aP);b(M[10],0,a1)}var
W=0,z=T,x=S,V=-1,U=[0,aq,s]}else{var
bG=aT(l),F=J[1][4],bH=a(f[17][1],p),bI=a(f[17][1],F)-bH|0,al=b(f[17][X],bI,F),bK=al[2],bL=al[1],am=b(c[i][4],p,bG),bM=c0(0,p,bL);if(Z[1]){var
bN=cd(d,n,F),bO=a(e[3],BZ),bP=g(C[15],d,n,am),bQ=a(e[3],B0),bR=b(C[15],d,n),bS=g(e[39],e[13],bR,p),bT=a(e[3],B1),bU=aT(l),bV=g(C[15],d,n,bU),bW=a(e[3],B2),bX=a(e[3],B3),bY=b(e[12],bX,bW),bZ=b(e[12],bY,bV),b0=b(e[12],bZ,bT),b1=b(e[12],b0,bS),b2=b(e[12],b1,bQ),b3=b(e[12],b2,bP),b4=b(e[12],b3,bO),b5=b(e[12],b4,bN);b(M[10],0,b5)}var
b6=q[1],W=a(f[17][1],bK),z=bM,x=am,V=b6,U=s}var
Y=l[2][6],a2=[0,V,q[2]];if(Y){var
_=Y[1];if(0===_[0]){var
A=_[1];if(typeof
A==="number")var
B=0,D=1;else
if(0===A[0]){var
ah=A[1];if(!ah)throw[0,I,BW];var
ai=ah[1][1],D=0}else{var
ak=A[1];if(ak)var
ai=ak[1][1],D=0;else
var
B=BX,D=1}if(!D)var
B=[0,ai];if(B)var
aa=B[1],a3=b(K[5],J[1][2],BL),a4=y?aa:aa+a(f[17][1],an)|0,a5=[0,b(u[8],a3,a4+1|0),0],ab=a(m[66][22],a5);else
var
ab=m[66][2];var
ac=ab,G=1}else
var
G=0}else
var
G=0;if(!G)var
ac=m[66][2];var
a7=0,a8=l[1][4];function
a_(a){return a[1][4]}var
a$=[0,aj(r(a2,b(L[16],a_,y),U,a8)),a7],ba=[0,ac,[0,bd(BM,u[28]),a$]],bb=b(u[81],ga,1),bc=[0,bd(BN,a(m[66][24],bb)),ba],be=[0,b(m[66][31],W,hi),bc],bg=bd(BO,a(m[66][22],be));try{var
bF=b(a9[22],l[4],h[2]),ad=bF}catch(a){a=w(a);if(a!==O)throw a;var
ad=bf(BP)}var
ae=ad[1];if(Z[1]){var
af=a(v[2],0),bh=$(z),bi=aI(af,o[16],bh),bj=a(e[3],BQ),bk=g(C[15],af,o[16],x),bl=a(e[3],BR),bm=bJ(l),bn=a(a6[9],bm),bo=a(e[3],BS),bp=a(j[1][8],ae),bq=a(e[3],bp),br=a(e[3],BT),bs=b(e[12],br,bq),bt=b(e[12],bs,bo),bu=b(e[12],bt,bn),bv=b(e[12],bu,bl),bw=b(e[12],bv,bk),bx=b(e[12],bw,bj),by=b(e[12],bx,bi);b(M[10],0,by)}var
bz=b(at[32],0,ae),bA=a(ap[9],bz);function
ag(d){var
e=b(c[83],o[16],x)[2];function
g(a){return b(c[45],o[16],a)}var
h=b(f[17][61],g,e),a=aH(0,z),i=b(f[17][57],h,a),j=[0,aD(x,a),0],k=aD(d,b(f[17][57],i,j));return b(c[37],k,z)}function
bB(c){if(Z[1]){var
d=a(v[2],0),f=ag(c),h=g(C[15],d,o[16],f),i=a(e[3],BU),k=bJ(l),m=a(j[1][8],k),n=a(e[3],m),p=a(e[3],BV),q=b(e[12],p,n),r=b(e[12],q,i),s=b(e[12],r,h);b(M[10],0,s)}var
t=ag(c),w=[0,bJ(l)];return g(u[hM],w,t,bg)}var
bC=a(m[66][61],bA),bD=b(k[17],bC,bB);return[0,b(m[66][3],ao,bD),[0,x,p]]},aA=a(f[17][1],t);if(a(f[17][1],n)!==aA)throw[0,I,B4];var
aB=function(e){var
j=a(k[67][4],e),d=a(k[67][5],e),l=a(k[67][2],e),o=b(c[83],d,l)[2],q=a(f[17][i],o);function
h(a,k){var
e=b(c[83],d,k),l=e[2],i=b(c[3],d,e[1]);switch(i[0]){case
1:var
j=i[1];return b(f[17][25],j,a)?a:[0,j,a];case
12:return g(f[17][15],h,a,l);default:return a}}var
r=g(f[17][15],h,0,q);function
s(b){return 1-a(A[a_],b)}var
u=b(f[17][61],s,r),v=b(f[17][69],c[10],u),w=[0,m[66][2],v];function
x(a,b,c){return ay(j,d,a,b,c)}return p(f[17][20],x,n,t,w)[1]},aC=a(k[67][9],aB),aE=fh(h[1][3]),aF=[0,a(m[21],aE),0],aG=[0,F(aC),aF],y=a(m[7],aG)}if(0===Y[0]){var
af=0,V=function(n){var
e=a(k[67][5],n),y=a(k[67][2],n),o=b(c[3],e,y);if(9===o[0]){var
R=a(f[19][42],o[2]),S=b(c[83],e,R)[1];try{var
T=b(c[75],e,S)[1],V=a(j[17][9],T),W=a(j[6][7],V),X=function(a){return b(j[1][1],a[1],W)},Y=[0,b(D[27],X,U)],x=Y}catch(a){a=w(a);if(a!==G[54])if(a!==O)throw a;var
x=0}var
i=x}else
var
i=0;if(i){var
p=i[1],d=p[3],q=d[3],z=p[2];if(q){var
s=q[1],t=s[6],A=s[5];if(0===t[0])var
l=0;else
var
K=t[1],M=[0,b(m[66][31],A,u[16]),0],N=lk(K),P=b(L[25],1,N),Q=[0,b(u[8],d[1][2],P),M],v=a(m[66][22],Q),l=1}else
var
l=0;if(!l)var
v=a(k[16],0);var
B=aj(r(J,0,0,d[4])),C=b(m[66][3],v,B),E=aj(az(Bw,F(fm(h[1])))),H=[0,bU(d)],I=g(u[hM],H,z,C);return b(m[66][3],I,E)}return a(k[16],0)},ah=az(Bx,F(a(k[67][9],V))),ai=az(By,F(fm(h[1]))),ak=a(m[22],ai),al=[0,b(m[4],ak,ah),af],am=function(c){var
d=df[3];function
e(a){return b(d,0,a)}var
f=f3(c),g=a(m[66][61],f),h=F(b(k[17],g,e));return a(m[21],h)},ao=b(m[30],am,s),aq=F(u[28]),ar=[0,b(m[5],aq,ao),al],as=[0,az(Bz,hj(h[1])),ar],au=[0,az(BA,y),as],av=fh(h[1][3]),aw=[0,d2,[0,a(m[21],av),au]];return az(BB,a(m[7],aw))}var
ax=[0,d2,[0,y,[0,F(iS(0)),0]]];return az(BC,a(m[7],ax));case
1:var
aK=d[2],aL=d[1],aM=a(f[19][11],d[4]),aN=function(a){return a},aO=b(f[17][66],aN,aM),S=fl(lf,l);if(S)var
aP=a(f[19][11],S[1]),aQ=function(a){return a},T=[0,b(f[17][66],aQ,aP)];else
var
T=0;var
aR=function(a){var
c=b(f[17][7],aO,a-1|0);function
d(c){return b(f[17][7],c,a-1|0)}return r(q,b(L[16],d,T),s,c)},aS=function(d){var
r=a(E[7],d),s=a(E[2],d),k=b(c[3],s,r);if(9===k[0]){var
u=a(f[19][11],k[2]),n=a(f[17][i],u),v=0<=q[1]?b(f[17][X],q[1],n)[2]:n;if(l){var
h=l[1];if(1===h[0])var
y=h[2],p=eL(h[1]),o=y,j=1;else
var
j=0}else
var
j=0;if(!j)var
w=eL(aL),x=function(a){return 1-e9(a)},p=b(f[17][61],x,w),o=aK;return a(F(gf(le(a(E[2],d),p,o,v))),d)}var
t=a(e[3],B5);return g(m[24],0,t,d)};return az(B6,b(m[8],aS,aR));case
2:var
d=d[2];continue;case
3:var
H=d[3],aU=d[4],aV=d[1],aW=function(d){var
h=a(E[8],d),o=a(E[2],d),j=H[6];if(0===j[0]){var
k=j[1],q=a(E[7],d),l=g(c[92],o,H[5],q),n=l[2],e=l[1],r=function(d){var
l=[0,d],m=b(c[83],d,n),v=m[2],w=m[1],x=a(f[7],aV),y=a(f[17][1],x),A=z(ad[2],0,0,h,d,w);function
j(k,a,f){if(0===f)return 0;var
e=b(c[3],d,k);switch(e[0]){case
6:if(a){var
g=a[1],l=a[2];return[0,g,j(b(c[i][5],g,e[3]),l,f-1|0)]}break;case
8:var
h=e[2];return[0,h,j(b(c[i][5],h,e[4]),a,f-1|0)]}throw[0,I,Bt]}var
u=j(A,v,y),C=k[2],D=a(f[17][9],u),o=b(c[i][4],D,C),E=aD(o,aH(0,e)),q=b(c[B],e,h),F=p(ag[2],0,q,d,E)[2],r=g(N[21],q,d,F),G=a(f[17][1],e);if(p(c[i][14],d,1,G,r)){var
H=-a(f[17][1],e)|0,J=b(c[i][1],H,r),s=g$(h,l,e,n,J,o,k[3]),K=s[3],t=c1(h,l[1],0,s[2]),L=t[1];return[0,L,a(c[21],[0,K,[0,t[2]]])]}throw[0,I,B7]};return a(F(b(cF[2],0,r)),d)}return a(m[1],d)},aX=r(q,l,s,aU),aY=F(u[28]),aZ=b(m[5],aY,aX),a0=[0,az(B8,b(m[5],aW,aZ)),0],a1=F(hi),a2=[0,b(m[26],H[5],a1),a0];return a(m[7],a2);default:var
a3=d[3],a4=d[2],a5=fl(lg,l),a7=a(f[7],a4[1]),a$=function(d){var
i=a(k[67][5],d),F=a(k[67][2],d),n=b(c[3],i,F);if(9===n[0]){var
o=n[2],H=n[1],p=b(a8[55],o.length-1-1|0,o),I=p[1],J=aa(p[2],0)[1],t=b(c[74],i,J),v=t[2],w=t[1],K=g(c[5],0,i,w),y=fk(h[1],K,v),A=y[2],L=y[3],l=b(E[42][11],a7,d),M=a(k[67][3],d),B=j[1][10][1],C=function(d,c){var
e=a(x[2][1][1],c);return b(j[1][10][4],e,d)},D=g(f[17][15],C,B,M),N=a(j[1][10][21],D),O=function(a){return[0,[0,0,a],0]},P=[0,[0,b(f[17][69],O,N)],1],Q=li(w,v,A,a(c[10],l)),R=a(c[10],l),S=[0,li(H,I,A-q[2]|0,R),[0,Q]],T=a(c[21],S),U=[0,aj(r(q,a5,s,a3)),0],V=[0,bd(B_,a(u[76],[0,l,0])),U],W=[0,bd(B$,b(u[5],T,2)),V],X=[0,bd(Ca,z(u[fK],1,0,[0,l],[0,i,L],P)),W];return a(m[66][22],X)}var
G=a(e[3],B9);return b(m[66][4],0,G)},ba=[0,fm(h[1]),0],bb=[0,a(k[67][9],a$),0],bc=a(m[66][35],bb),be=aj(hj(h[1])),bg=aj(fh(h[1][3])),bh=a(m[66][24],bg),bi=b(m[66][3],bh,be),bj=[0,b(m[66][19],bi,bc),ba];return F(bd(Cb,a(m[66][22],[0,u[28],bj])))}}return r(J,n,l,d)}function
fo(d,c){if(Z[1]){var
h=function(i){function
h(h){function
i(j){var
i=b(f[17][69],k[10],j),l=d6(C[85],0,0,0,0,h,0,0,0,0,i),m=a(e[3],Cc),n=a(e[3],d),o=a(e[3],Cd),p=b(e[12],o,n),u=b(e[12],p,m),v=b(e[12],u,l);b(M[10],0,v);function
w(g){var
f=g[1];if(f[1]===bP[29])var
c=f[3],m=f[2],n=d6(C[85],0,0,0,0,h,0,0,0,0,i),j=s(c),o=a(e[3],Ce),p=t===j?c[1]:q===j?a(r[2],c):c,u=a(e[13],0),v=a(e[3],d),w=a(e[3],Cf),x=a(e[16],m),y=a(e[3],Cg),z=b(e[12],y,x),A=b(e[12],z,w),B=b(e[12],A,v),D=b(e[12],B,u),E=b(e[12],D,p),F=b(e[12],E,o),l=b(e[12],F,n);else
var
l=a(_[15],g);var
G=a(e[3],Ch),H=b(e[12],G,l);b(M[10],0,H);return a(k[16],0)}function
x(c){if(0===c){var
f=a(e[3],Ci),h=a(e[3],d),i=b(e[12],h,f);b(M[10],0,i);return a(k[16],0)}return aj(function(c){var
d=g(C[84],0,0,c),f=a(e[3],Cj),h=b(e[12],f,d);b(M[10],0,h);return[0,[0,c[1],0],c[2]]})}var
y=b(k[72][1],k[54],x),z=b(k[18],c,y);return b(k[23],z,w)}return b(k[72][1],k[65][6],i)}return b(k[72][1],k[55],h)};return b(k[72][1],k[56],h)}return c}var
dg=[cQ,Ck,cM(0)];function
hk(c){function
d(d){function
e(e){function
c(c){return Bp(d,c)?a(k[16],0):b(k[21],0,dg)}return b(k[72][1],k[55],c)}return b(k[72][1],c,e)}return b(k[72][1],k[65][6],d)}function
ll(H,aA,d,aC,o,h){function
aB(d){var
c=d[1];if(c[1]===eq[1]){var
e=g(bD[2],c[2],c[3],c[4]);b(M[8],0,e);return a(f[33],d)}return a(f[33],d)}if(H){var
q=H[1];if(q){var
r=q[1];if(0===r[0])var
s=r[1],K=function(b){var
a=b[2];if(typeof
a!=="number"&&0===a[0])return 1;return 0},t=b(f[17][30],K,s),v=t[2],w=t[1],N=function(c){var
a=c[2];if(typeof
a!=="number"&&0===a[0]){var
b=a[1];if(b)return b[1][1]+1|0}return-1},i=b(f[17][69],N,w),O=function(d){var
a=d[1][1][6];if(a){var
b=a[1];if(0===b[0]){var
c=b[1];if(typeof
c!=="number"&&1!==c[0])return 1}}return 0},y=b(f[17][30],O,h),A=y[2],l=y[1],P=hf[7],Q=[0,Cl,[0,d[1][3],0]],n=function(c){if(c){var
d=c[2];if(d){var
e=[0,n(d),0],f=[0,a(k[16],0),e],g=a(k[37],f),h=a(u[b2],0);return b(k[72][2],h,g)}}return a(k[16],0)},B=function(c){function
e(e){var
c=d[1],g=e[1],h=d[3],i=d[2],j=c[7],k=c[6],l=b(f[18],d[1][5],e[3][2][5]),m=[0,[0,c[1],c[2],c[3],c[4],l,k,j],i,h],n=g[4];return aj(fn(m,[0,0,a(f[17][1],s)],o,0,0,n))}var
g=b(f[17][69],e,c),h=a(k[37],g);return b(k[72][2],u[28],h)},R=B(A),S=function(d){var
c=d[2],e=d[1];if(typeof
c!=="number"&&1===c[0]){var
f=c[1];return f?b(u[8],e,f[1][1]+1|0):b(u[8],e,1)}return a(k[16],0)},T=b(f[17][69],S,v),U=a(k[37],T),V=b(k[72][2],U,R),W=B(l),X=gd(0),Y=fi(0,i),_=b(k[72][2],Y,X),C=b(k[72][2],_,W),$=function(q){var
r=q[1],F=q[2];if(r===dg){var
G=function(c){var
d=c[1],f=c[2];if(d===dg){var
g=a(e[3],Cq),h=a(e[3],Cr),i=b(e[12],h,g);b(M[6],0,i);return m[66][2]}return b(k[21],[0,f],d)};if(i)if(i[2])var
c=0;else{var
s=i[1];if(Z[1]){var
t=a(e[3],Cm);b(M[10],0,t)}if(h)if(h[2])var
l=0;else{var
n=h[1][1][4];if(1===n[0])var
C=a(f[19][11],n[4]),E=function(a){return a},p=[0,b(D[66],E,C)];else
var
p=0;var
g=p,l=1}else
var
l=0;if(!l)var
g=0;if(g)var
v=g[1],w=0,y=function(c){function
e(a){return aj(fn(d,Cn,o,0,0,a))}var
g=b(f[17][69],e,v),h=a(k[37],g),i=gd(0),j=u[28],l=iR(a(x[2][1][1],c)),m=b(k[72][2],l,j),n=b(k[72][2],m,i);return b(k[72][2],n,h)},z=bd(Co,a(m[66][47],y)),A=b(m[66][31],s,u[16]),B=[0,b(k[72][2],A,z),w],j=bd(Cp,a(k[37],B)),c=1;else
var
j=b(k[21],0,dg),c=1}else
var
c=0;if(!c)var
j=b(k[21],0,dg);var
H=hk(j);return b(k[23],H,G)}return b(k[21],[0,F],r)},aa=0<a(f[17][1],v)?C:hk(C),ab=b(k[23],aa,$),ac=a(f[17][1],l),ad=function(m){function
h(g,d){var
n=a(k[67][5],m),e=b(c[3],n,g);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
i=f[1],j=f[2],o=e[1];if(1===d)return[0,i,[0,j]];var
l=h(j,d-1|0),p=l[2];return[0,a(c[21],[0,o,[0,i,l[1]]]),p]}}if(1===d)return[0,g,0];throw[0,I,Cs]}var
d=h(a(k[67][2],m),ac),e=d[2],i=d[1],o=fo(Ct,z(P,0,0,0,0,Q)),p=[0,a(k[16],0),0],q=a(f[17][1],w),r=g(k[32],1,q,ab),s=n(l),t=[0,fo(Cu,b(k[72][2],s,r)),p],v=a(k[37],t),x=b(u[nm],0,i);if(e)var
y=e[1],B=[0,a(k[16],0),0],C=fo(Cv,n(A)),D=b(k[72][2],u[16],C),E=[0,fo(Cw,b(k[72][2],D,V)),B],F=a(k[37],E),G=a(c[18],[0,0,i,y]),H=b(u[nm],0,G),j=b(k[72][2],H,F);else
var
j=a(k[16],0);var
J=em(0),K=b(k[72][2],J,j),L=b(k[72][2],K,x),M=b(k[72][2],L,v);return b(k[72][2],M,o)},J=a(k[67][9],ad),j=1;else
var
j=0}else
var
j=0}else
var
j=0;if(!j){var
ae=d[1][5],af=function(a){return a[1]},ag=b(f[17][69],af,ae),ah=[0,a(G[66],aA)[1],ag],ai=function(a){return[0,a,0]},E=lj(b(f[17][69],ai,ah)),ak=E[2],al=E[1];if(h)if(h[2])var
p=0;else{var
F=h[1],am=F[2],an=F[1],ao=function(a){return a[4]},ap=b(L[16],ao,am),aq=an[4];a(al,0);var
ar=function(b){a(ak,0);return a(k[16],b)},as=[0,aj(fn(d,Cy,o,ap,0,aq)),0],at=[0,u[28],as],au=[0,em(0),at],av=a(m[66][22],au),aw=a(m[66][34],av),ax=b(k[17],aw,ar),ay=function(c){var
d=c[1],f=c[2];if(d===dg){var
g=a(e[3],Cz),h=a(e[3],CA),i=b(e[12],h,g);b(M[6],0,i);return a(k[16],0)}return b(k[21],[0,f],d)},az=hk(ax),J=b(k[23],az,ay),p=1}else
var
p=0;if(!p)throw[0,I,Cx]}return b(k[23],J,aB)}function
lm(A,aA,l,h,T,S,C){if(Z[1]){var
o=M[10],U=a(E[8],C),W=a(E[2],C);b(o,0,a(e[3],C0));b(o,0,ci(U,W,C1,T));var
aB=a(e[5],0),aC=a(e[3],C2),aD=a(e[5],0),aE=b(e[12],aD,aC);b(o,0,b(e[12],aE,aB));b(o,0,ci(U,W,C3,S))}try{var
D=function(b){return d1(0,[0,a(c4[3],CB),b])},Y=A[5],_=function(a){return a[1]},$=[0,l,[0,h,b(f[17][69],_,Y)]],aa=function(a){return[0,a,0]},ab=b(f[17][69],aa,$),G=s(dz),ac=d3[3],ad=t===G?dz[1]:q===G?a(r[2],dz):dz,H=lj([0,[0,a(aF[8],ad),ac],ab]),d=H[2],P=H[1],Q=function(c,b){a(P,0);var
e=a(c,b);a(d,0);return e},y=function(a){return Q(F(iP(0)),a)},ae=F(u[61]),af=function(a){return Q(ae,a)},ag=[0,a(N[16],br[14]),2],ah=F(a(u[50],ag)),ai=0,ak=[0,b(J[17],A[3],CC),0],al=function(a){return gc(ak,ai,a)},am=b(m[5],al,ah),an=0,ao=[0,A[3],0],aq=function(a){return gc(ao,an,a)},B=b(m[5],aq,am),ar=function(k,d){var
l=a(E[7],d),n=a(E[2],d),i=b(c[3],n,l);if(9===i[0]){var
h=i[2];if(3===h.length-1){var
o=h[2],p=h[3],e=a(E[2],d),q=b(c[83],e,o)[1],r=b(c[83],e,p)[1];try{var
s=function(a){var
d=a[2],b=g(c[aQ],e,[1,a[1]],q);return b?g(c[aQ],e,[1,d],r):b},j=b(f[17][27],s,k),t=a(u[68],[0,[0,CE,[1,j[1]]],[0,[0,CD,[1,j[2]]],0]]),v=[0,y,[0,F(iL(0)),0]],x=[0,F(t),v],z=b(m[7],x,d);return z}catch(b){b=w(b);if(b===O)return a(F(u[ea]),d);throw b}}}return a(F(u[ea]),d)},R=function(c){function
e(a){return ar(c,a)}var
f=F(u[ea]);function
g(c){a(d,0);var
b=a(f,c);a(P,0);return b}return az(CF,b(m[4],g,e))},n=function(l,o,J){var
d=J;for(;;){switch(o[0]){case
0:var
r=o[2];if(0===o[4][0])switch(d[0]){case
0:var
s=d[2],M=d[1][1];if(0===d[4][0]){var
N=function(K,J,t){try{var
d=b(a9[22],t[4],aA)}catch(a){a=w(a);if(a===O)throw[0,I,CG];throw a}var
j=d[2],L=d[1];return function(o){var
N=a(E[8],o),d=[0,a(E[2],o)],p=J[1],e=t[1],h=e[1][4],O=a(f[17][1],M),v=a(f[17][1],h)-O|0,P=V(0,b(f[17][X],v,h)[1]),Q=[0,b(c[i][1],v,L),P],R=a(c[21],Q),S=V(0,h),T=a(c[21],[0,e[5],S]),U=b5(N,d,e[1][5],R,T),W=b(c[37],U,h);function
w(e){var
a=b(c[83],d[1],e)[1];if(b(c[55],d[1],a))return b(c[75],d[1],a)[1];throw[0,I,CH]}var
x=w(p[5]),y=w(e[5]);function
q(b){return a(F(a(u[68],[0,[0,CJ,[1,x]],[0,[0,CI,[1,y]],0]])),b)}var
r=p[4];if(3===r[0]){var
s=r[3],C=s[6],ak=r[4];if(0===C[0])var
al=[0,ge(0),0],am=[0,aj(q),al],D=a(m[66][22],am);else{var
G=lk(C[1]);if(G)var
an=G[1],ao=[0,aj(q),0],ap=[0,b(m[66][31],s[5],u[16]),ao],aq=[0,bd(CN,fi(0,[0,an,0])),ap],ar=[0,b(m[66][31],s[5],hi),aq],H=a(m[66][22],ar);else
var
H=a(k[16],0);var
D=H}var
B=ak,A=[0,[0,x,y],l],z=D}else
var
Y=aj(q),B=p[4],A=l,z=Y;var
Z=[0,az(CK,n(A,B,e[4])),0],_=[0,az(CL,F(z)),Z],$=[0,az(CM,F(u[28])),_],aa=aj(a(m[7],$)),ab=F(g(u[nQ],0,[0,j],aa)),ac=F(a(u[78],0)),ad=aj(b(m[5],ac,ab)),ae=g(u[hM],[0,j],W,ad),af=a(c[10],j),ag=[0,F(b(df[3],0,af)),[0,K,0]],ah=[0,F(ae),ag],ai=[0,a(bP[11],d[1]),ah];return b(m[7],ai,o)}},P=a(f[17][1],s);if(a(f[17][1],r)===P){var
Q=p(f[17][19],N,m[1],r,s),S=[0,y,[0,R(l),0]],T=[0,Q,[0,az(CO,a(m[21],B)),S]],U=[0,F(u[28]),T];return az(CP,a(m[7],U))}throw[0,I,CQ]}var
q=1;break;case
2:var
h=0,q=0;break;default:var
h=1,q=0}else
var
q=1;if(q)switch(d[0]){case
0:var
t=d[4],W=d[1][1];if(0!==t[0]){var
Y=a(bl,b(f[17][7],W,t[1]-1|0));return F(D(a(K[10][16],Y)))}var
h=1;break;case
2:var
h=0;break;default:var
h=1}break;case
1:var
v=o[4];switch(d[0]){case
1:var
C=d[4],Z=d[2],_=d[1][1];return az(CT,function(d){var
r=a(E[2],d),i=a(E[8],d),o=b(f[17][7],_,Z-1|0),p=c3(dH(i,r,a(x[1][1][3],o))[1])[1][1],q=b(bH[4],i,p);if(a(bH[14],q)){var
s=a(f[19][40],C),t=a(L[7],s),u=a(f[19][40],v);return a(n(l,a(L[7],u),t),d)}var
w=a(E[7],d),z=a(E[2],d),j=b(c[3],z,w);if(9===j[0]){var
k=j[2];if(3===k.length-1){var
B=k[3],h=a(E[2],d),G=b(c[83],h,B)[1],H=b(c[79],h,G)[3],I=m[1],J=b(c[83],h,H)[1],K=b(c[68],h,J),M=a(f[19][11],v),N=function(a){return a},O=b(f[17][66],N,M),P=a(f[19][11],C),Q=function(a){return a},R=b(f[17][66],Q,P),S=function(c){var
d=b(f[17][7],O,c-1|0),e=[0,I,[0,y,[0,n(l,d,b(f[17][7],R,c-1|0)),0]]];return a(m[7],e)},T=F(D(K));return a(F(aj(b(m[8],T,S))),d)}}var
A=a(e[3],CS);return g(m[24],0,A,d)});case
2:var
h=0;break;default:var
h=1}break;case
3:var
$=n(l,o[4],d),aa=F(ge(0));return az(CU,b(m[5],aa,$));case
4:var
ab=o[3];switch(d[0]){case
2:var
h=0;break;case
4:var
G=d[2],ac=d[3],ad=a(f[7],G[1]),H=function(d){var
t=a(E[7],d),v=a(E[2],d),i=b(c[3],v,t);if(9===i[0]){var
h=i[2];if(3===h.length-1){var
x=h[2],C=h[3],f=a(E[2],d),D=b(c[83],f,G[5])[1],I=b(c[75],f,D)[1],o=b(c[74],f,x),J=o[2],K=o[1],p=b(c[74],f,C),L=p[2],M=p[1],N=fk(A,g(c[5],0,f,K),J)[3],q=fk(A,g(c[5],0,f,M),L),r=q[3],O=q[1],s=b(E[20],ad,d);if(b(j[17][13],O,I)){var
P=[0,B,[0,n(l,ab,ac),0]],Q=a(u[76],[0,s,0]),S=[0,a(k[71][7],Q),P],T=[0,az(CW,F(z(u[bw],0,[0,s],r,0,dR[4]))),S],U=[0,R(l),0],V=aj(a(m[7],U)),W=[0,F(g(df[13],N,r,V)),T];return b(m[7],W,d)}return b(m[7],[0,B,[0,y,[0,H,0]]],d)}}var
w=a(e[3],CV);return g(m[24],0,w,d)},ae=[0,y,[0,az(CX,H),0]],af=[0,F(u[28]),ae];return F(aj(a(m[7],af)));default:var
h=1}break;default:var
h=0}if(!h)if(2===d[0]){var
d=d[2];continue}throw[0,I,CR]}};try{var
as=a(u[68],[0,[0,CZ,[1,l]],[0,[0,CY,[1,h]],0]]),at=0,au=[0,af,[0,function(c){b(v[53],[0,l],1);b(v[53],[0,h],1);return a(n([0,[0,l,h],0],T,S),c)},at]],av=[0,F(as),au],aw=[0,F(u[28]),av],ax=[0,F(em(0)),aw],ay=b(m[7],ax,C);a(d,0);return ay}catch(b){b=w(b);a(d,0);throw b}}catch(a){a=w(a);if(a[1]===ap[1])throw a;throw a}}function
ln(A,l,y,d,x){var
n=hf[7],o=[0,C4,[0,d[3],0]];function
h(a){return z(n,0,0,0,a,o)}function
j(l,e){function
d(n){var
r=a(k[67][4],n),s=a(k[67][5],n),B=a(k[67][2],n),o=b(c[3],s,B);if(0===l){var
q=aD(A,a(f[17][9],e)),t=p(ag[2],0,r,s,q),d=t[1],C=t[2];if(1===y){var
v=g(N[18],r,d,q),D=0,z=function(b){var
c=a(k[67][5],b),d=a(k[67][4],b),e=p(ag[2],0,d,c,v)[1],f=[0,h(0),0],g=[0,u[61],f],i=[0,a(u[86],v),g],j=[0,a(k[65][1],e),i];return a(m[66][22],j)},E=[0,a(k[67][9],z),D],F=[0,u[61],[0,u[28],E]],G=[0,a(k[65][1],d),F];return a(m[66][22],G)}var
H=b(c[91],d,C)[2],J=h(0),K=u[61],L=a(u[86],q),M=b(k[72][2],L,K),O=b(k[72][2],M,J),P=[0,a(m[66][8],O),0],Q=h(0),R=a(u[fT],0),S=a(m[66][61],x),T=b(k[72][1],S,R),U=b(k[72][2],T,Q),V=b(k[72][2],u[16],U),W=[0,a(m[66][8],V),P],X=a(k[37],W),Y=a(u[143],H),Z=u[28],_=u[61],$=a(k[65][1],d),aa=b(k[72][2],$,_),ab=b(k[72][2],aa,Z),ac=b(k[72][2],ab,Y);return b(k[72][2],ac,X)}switch(o[0]){case
6:var
ad=0,ae=function(b){return j(l-1|0,[0,a(c[10],b),e])},af=[0,a(m[66][45],ae),ad];return a(m[66][22],[0,u[16],af]);case
8:var
w=o[2],ah=o[4],ai=[0,j(l-1|0,[0,w,e]),0],aj=b(c[i][5],w,ah),ak=[0,b(u[5],aj,2),ai];return a(m[66][22],ak);default:throw[0,I,C5]}}return a(k[67][9],d)}try{var
r=j(l,0);return r}catch(c){var
q=a(e[3],C6);return b(m[66][4],0,q)}}aB(1275,[0,k$,la,A2,fg,hg,A5,hh,lb,fh,fi,fk,le,d2,hj,lf,lg,lh,fl,fm,fn,ll,lm,ln],"Principles_proofs");function
lo(d){var
a=d[6];if(a){var
b=a[1];if(0===b[0]){var
c=b[1];if(typeof
c==="number")return C7;else
if(0!==c[0])return C8}}return 0}function
lp(a){return typeof
a==="number"?0===a?1:0:1}function
lq(a){return a[1]}function
C9(a){return a[2]}function
C_(c){var
d=c[2],f=c[1];try{var
i=a(v[2],0),j=[0,g(dp[15],0,i,d),1,0],k=[0,b(aL[1],0,j),0],l=b(cJ[1],f,k);return l}catch(b){b=w(b);if(b===O){var
h=a(e[3],C$);return g(_[3],0,0,h)}throw b}}function
Da(a){var
c=a[2],d=c[1];return[0,d,b(aF[14],a[1],c[2])]}function
lr(h,f,d,n){var
c=a(cT[1],h),i=c[8],j=d?d[1]:function(k){var
c=a(e[3],Db),d=a(e[3],h),f=a(e[3],Dc),i=b(e[12],f,d),j=b(e[12],i,c);return g(_[3],0,0,j)},k=a(L[2],d)?function(a){return[0,a]}:function(a){return[1,a]},l=c[4];function
m(c,b){return a(f,b)}return[0,c[1],f,m,l,k,j,n,i]}function
Dd(c){var
b=c[2],d=a(cU[59],b[2]);return[0,[0,b[1],d]]}var
De=[0,Da],Dg=lr(Df,function(a){return C_(a[2])},De,Dd),Dh=a(cT[4],Dg);function
Di(a){return b(Dj[42],a[1],a[2])}function
Dk(b){return[0,a(cU[57],b[2])]}var
Dl=[0,Di],Dn=lr(Dm,function(a){return b(v[53],[0,a[2]],1)},Dl,Dk),ls=a(cT[4],Dn);function
lt(f,d,b){function
e(h){var
a=h;for(;;){if(a<b.length-1){if(a<d.length-1){var
i=aa(b,a)[a+1],j=aa(d,a)[a+1];if(g(c[95],f,j,i))return[0,a,e(a+1|0)];var
a=a+1|0;continue}var
a=a+1|0;continue}return[0,a,0]}}return e(0)}function
fp(b,a){function
d(h,c,g){var
b=h,a=g;for(;;){if(c)if(a){var
e=a[2],f=c[1],i=a[1],j=c[2];if(b<f){var
b=b+1|0,a=e;continue}if(b===f)return[0,i,d(b+1|0,j,e)];throw[0,I,Do]}return a}}return d(0,b,a)}var
a0=a(Dp[1],[0,G[80]]);function
lu(l,k,i){var
a=k;for(;;){if(a){var
d=a[1];if(d){var
e=d[1];if(0===e[0]){var
m=a[2],n=e[1];try{var
o=function(a){var
c=a[2];return b(j[1][1],l,a[1])?[0,c]:0},f=b(D[fR],o,n);if(typeof
f==="number")var
c=0;else{var
h=f[1];if(h)var
g=[0,h[1][1]<i?1:0],c=1;else
var
c=0}if(!c)var
g=Ds;return g}catch(b){b=w(b);if(b===O){var
a=m;continue}throw b}}}var
a=a[2];continue}return 0}}function
lv(c,b){var
d=a0[1];function
e(e,d,b){var
f=a(c,e);return g(a0[4],f,d,b)}return g(a0[13],e,b,d)}function
hl(c,a){function
d(e,c,a){if(c){var
d=c[1];return a?[0,b(J[5],d,a[1])]:c}return a?a:0}return g(a0[8],d,c,a)}function
lw(d,c){function
e(a,e){var
c=a[1],f=a[2],g=b(aA[12],[0,d,0],c);return[0,c+1|0,[0,b(x[1][1][14],g,e),f]]}var
f=g(l[20],e,Dt,c)[2];return a(l[9],f)}function
lx(N,M,d,m,e){function
f(f){var
o=f[6],p=f[5],q=f[4],r=f[3],s=f[2][2],O=a(c[aE][1],f[1]);if(b(G[74],O,m)){var
P=a(G[66],m)[1],Q=a(l[1],e),R=a(j[17][9],P),t=lu(a(j[6][7],R),N,Q);if(t)if(0===t[1])return 0;var
u=a(l[1],p),k=fp(s,e);if(u<=a(l[1],k))var
v=b(D[X],u,k),w=v[2],S=v[1],T=a(l[1],w),K=a(l[9],e),L=b(D[b2],T,K),z=0,y=[0,a(l[9],L),S,w];else{var
U=b(l[17],c[aE][2],p),d=a(l[9],U),h=k;for(;;){if(h){if(d){var
n=d[1],F=h[2],H=h[1];if(0===n[0]){var
d=lw(H,d[2]),h=F;continue}var
d=lw(n[2],d[2]);continue}throw[0,I,Du]}var
i=a(l[9],d),B=a(l[1],i),V=g(x[1][14],G[1],0,i),W=a(aA[8],B),Y=b(l[17],W,k),Z=b(J[26],Y,V),_=g(x[1][14],G[1],0,i),$=a(aA[8],B),aa=b(l[17],$,e),z=i,y=[0,b(J[26],aa,_),Z,0];break}}return[0,[0,q,o,s,z,y]]}if(r){var
C=r[1],ab=C[2],ac=b(A[69],M,C[1])[1],ad=a(c[aE][1],ac),E=a(G[64],ad)[1];return b(G[74],E,m)?[0,[0,q,o,ab,0,[0,e,e,0]]]:0}return 0}try{var
h=[0,b(D[fR],f,d)];return h}catch(a){a=w(a);if(a===O)return 0;throw a}}function
hm(w,V,k,U,r,v,I){var
W=k?k[1]:1;function
J(a){return a[2][1]}var
y=b(l[17],J,v),z=[0,0];function
f(e,h,k,m){var
d=a(G[26],m);switch(d[0]){case
6:var
H=d[3],at=d[2],au=d[1];if(b(aA[3],1,H)){var
I=f(e,h,k,at),av=I[2],aw=I[1],J=f(e,h,aw,b(aA[14],G[6],H)),ax=J[1],ay=[0,au,av,b(aA[8],1,J[2])];return[0,ax,a(G[10],ay)]}break;case
7:var
K=d[2],L=d[1],az=f(e+1|0,[0,[0,L,0,K],h],a0[1],d[3])[1];return[0,hl(k,lv(function(b){return a(G[10],[0,L,K,b])},az)),m];case
8:var
M=d[3],t=d[2],N=d[1],aB=d[4],aC=f(e,h,k,t)[1],aD=f(e+1|0,[0,[0,N,[0,t],M],h],a0[1],aB)[1];return[0,hl(aC,lv(function(b){return a(G[12],[0,N,t,M,b])},aD)),m];case
13:var
O=d[4],aF=d[2],aG=d[1],P=f(e,h,k,d[3]),aH=P[2],aI=P[1],aJ=function(b,a){return f(e,h,b,a)[1]},aK=g(aY[17],aJ,aI,O),aL=a(G[23],[0,aG,aF,aH,O]),aM=a(c[8],aL),aN=g(c[i][3],y,r+1|0,aM);return[0,aK,a(c[aE][1],aN)];case
16:var
aO=d[1],Q=f(e,h,k,d[2]),aP=Q[1];return[0,aP,a(G[17],[0,aO,Q[2]])]}var
B=a(G[65],m),o=B[2],p=B[1],X=a(c[8],p),u=b(c[3],w,X);if(10===u[0])var
R=a(j[17][9],u[1][1]),S=a(j[6][7],R),C=b(j[1][10][3],S,V);else
var
C=0;if(C){if(W)var
Y=a(c[8],m),Z=g(c[i][3],y,r+e|0,Y),D=a(c[aE][1],Z);else
var
D=m;return[0,k,D]}var
E=lx(U,w,v,p,a(aY[11],o));if(E){var
q=E[1],s=q[5],n=q[4],_=s[3],$=s[2],aa=s[1],ab=q[3],ac=q[1],ad=function(g,d,j){var
a=ab;for(;;){if(a){var
c=a[1],i=a[2];if(mD(g,c))var
b=1;else{if(!mE(g,c)){var
a=i;continue}var
b=0}}else
var
b=0;return b?d:f(e,h,d,j)[1]}},ae=g(a8[46],ad,k,o),af=[0,p,a(aY[12],aa)],ag=a(G[13],af),F=b(A[16],ag,n),ah=g(x[1][14],G[1],0,n),ai=a(l[1],n),aj=b(aA[8],ai,F),ak=[0,b(bg[12],aj,ah)],al=a(aY[12],$),am=(((ac+1|0)+r|0)+e|0)+a(l[1],n)|0,an=[0,a(G[1],am),al],ao=[0,a(G[13],an),ak],ap=a(G[13],ao),aq=b(bg[22],ap,n),T=hl(b(a0[6],aq,z[1]),ae);z[1]++;return[0,T,a(bg[11],[0,F,_])]}function
ar(b,a){return f(e,h,b,a)[1]}var
as=g(aY[17],ar,k,o);return[0,as,a(G[13],[0,p,o])]}var
K=a(c[aE][1],I),m=f(0,0,a0[1],K),n=m[2],o=m[1];function
p(b,d){var
c=a(G[41],b);return c?c:a(G[40],b)}var
d=b(a0[17],p,o),q=d[2],s=d[1];function
t(d,e,c){var
f=a(bg[35],d),h=f[2],i=a(l[1],f[1]);if(g(aA[4],1,i,h)){var
j=b(aA[8],-i|0,h);return b(a0[3],j,c)?c:g(a0[4],d,e,c)}return g(a0[4],d,e,c)}var
u=g(a0[13],t,s,q),B=a(a0[19],u);function
C(b,a){return am.caml_int_compare(b[2],a[2])}var
D=b(l[48],C,B);function
E(d,f){var
e=d[1],g=d[2],h=a(c[8],f[1]),k=b(c[i][1],e,h);return[0,e+1|0,[0,[0,[0,a(j[1][6],Dr)],k],g]]}var
e=g(l[20],E,Dq,D),h=e[1],F=e[2],H=a(c[8],n);return[0,F,h,b(c[i][1],h,H)]}function
ly(d,h,j,e){function
k(f,e,b){var
d=0<b.length-1?g(aY[7],b,0,b.length-1-1|0):b;return a(c[21],[0,h,d])}function
f(e,a){var
h=b(c[3],d,a);switch(h[0]){case
1:if(g(c[95],d,j,a))return k(e,a,[0]);break;case
9:var
i=h[1],m=h[2];if(g(c[95],d,j,i)){var
n=function(a){return a+1|0},o=p(c[a_],d,n,f,e);return k(e,i,b(aY[15],o,m))}break}function
l(a){return a+1|0}return z(c[a_],d,l,f,e,a)}return f(0,e)}function
Dv(d,c,b,a){return gU(function(a){return ly(d,c,b,a)},a)}function
lz(e,q,d){function
f(r){var
h=r;for(;;){var
d=b(c[3],e,h);switch(d[0]){case
6:var
k=d[3],m=d[2],s=d[1],o=b(c[91],e,m)[2],l=b(c[83],e,o)[1];if(b(c[46],e,l))var
p=b(c[77],e,l)[1][1],n=b(j[23][13],p,q);else
var
n=0;if(n){var
t=a(c[9],1);if(g(A[38],e,t,k))throw[0,I,Dw];var
h=b(c[i][5],c[14],k);continue}var
u=[0,s,m,f(k)];return a(c[18],u);case
8:var
v=d[3],w=d[2],x=d[1],y=[0,x,w,v,f(d[4])];return a(c[20],y);default:return h}}}return b(cB,f,d)}function
lA(g,f){var
d=a(G[26],g);if(0===d[0]){var
e=d[1],h=a(be,b(l[7],f,e-1|0));return b(c[i][1],e,h)}return c[14]}function
lB(m,f,U,T,S,R,v,k,Q,u,P,O){var
w=b(c[91],f[1],O),x=w[2],y=w[1];function
W(a){return lp(a[2][8][1])}var
Y=b(l[35],W,k),z=a(l[1],Y);if(1===z)var
Z=a(l[1],u)+2|0,r=b(D[b2],Z,y);else
var
r=y;if(1===z)var
_=b(c[i][4],[0,c[14],[0,P,0]],x),s=b(c[37],_,u);else
var
K=function(h,g,e){var
d=b(c[91],f[1],h),j=d[2],k=b(D[b2],2,d[1]),l=[0,e,V(0,g)],m=[0,a(c[21],l),0],n=b(c[i][4],[0,c[14],m],j);return b(c[37],n,k)},M=function(m,l){var
e=m,d=l;for(;;){var
g=b(c[3],f[1],e);if(d){var
i=d[1][2][8][1];if(typeof
i==="number")if(0!==i){var
d=d[2];continue}if(9===g[0]){var
h=g[2];if(2===h.length-1){var
k=d[1][2],o=g[1],p=h[1],q=k[4],r=k[1][1],s=M(h[2],d[2]),t=[0,o,[0,K(p,q,r),s]];return a(c[21],t)}}var
j=d[1][2],n=d[2],e=K(e,j[4],j[1][1]),d=n;continue}return e}},s=M(x,k);var
t=lz(f[1],R,r);if(1===v){var
$=b(c[37],s,t);return[0,a(l[1],t),$]}var
aa=f_(f,2),ab=a(l[1],r)-v|0,B=b(D[X],ab,t),ac=B[1],C=a(D[fH],B[2]),ad=C[2],ae=C[1];function
af(J,t,s){var
e=s[2],u=e[7],v=e[6],w=e[5],n=e[4],x=e[3],K=s[1];if(1===e[8][1]){var
L=a(aC,t)[1],o=a(l[1],n),y=[0,ah([0,0,0,w]),n],M=0,O=function(k,e){var
d=(o-e[2][1]|0)+1|0,f=e[1],g=a(be,b(l[7],n,(d-1|0)-1|0)),h=b(c[i][1],d,g),j=a(c[9],d);return[0,d,h,b(c[i][1],1,f),j]},h=g(D[73],O,M,u),d=a(l[1],h),P=av(ia,f),z=function(e,d,l,k,h,g){var
m=b(c[i][1],1,g),n=a(c[9],1),o=b(c[i][1],1,d),q=p(A[51],f[1],o,n,m),r=[0,[0,a(j[1][6],Dx)],e,q],s=[0,P,[0,e,d,a(c[19],r),h,l,k]];return a(c[21],s)};if(h){if(h[2])throw[0,I,Dy];var
r=h[1],q=r[3],B=r[2],Q=r[4],R=function(e,k){var
l=k[2],m=k[1],h=b(c[i][1],d,Q),s=lA(g(c[5],0,f[1],e),n),j=b(c[i][1],d+1|0,s);if(g(A[38],f[1],h,j)){if(b(c[44],f[1],q))var
o=b(c[i][1],d+1|0,e);else
var
y=b(c[i][1],d+1|0,e),C=a(c[9],1),D=b(c[i][1],d,q),o=z(b(c[i][1],d,B),h,D,C,y,j);if(b(c[44],f[1],q))var
r=b(c[i][1],d+3|0,e);else
var
t=b(c[i][1],2,j),u=b(c[i][1],d+3|0,e),v=a(c[9],1),w=a(c[9],2),x=b(c[i][1],2,h),r=z(b(c[i][1],d+2|0,B),x,w,v,u,t);return[0,[0,o,m],[0,[0,h,r],l]]}var
E=b(c[i][1],d+1|0,e),F=b(c[i][1],d,q);return[0,[0,p(A[51],f[1],F,h,E),m],l]},C=g(l[21],R,v,Dz),F=C[1],E=C[2]}else
var
ao=a(c[i][1],d+1|0),F=b(l[17],ao,v),E=0;var
V=b(c[i][1],(d*2|0)+1|0,w),W=[0,a(c[9],d+1|0),V],X=function(o,e){var
q=e[4],k=e[3],n=e[2],h=o[2],r=o[1],s=e[1]+(2*d|0)|0,t=a(c[9],s);if(g(A[38],f[1],t,h)){var
u=b(c[i][1],d+1|0,q),v=a(c[9],1),w=b5(m,f,b(c[i][1],d+1|0,n),v,u),x=a(c[9],1),y=a(c[9],s),z=p(A[51],f[1],y,x,h),B=b(c[i][1],1,z),C=function(b,a){return p(A[51],f[1],a[1],a[2],b)},D=g(l[20],C,B,E),F=av(ib,f),G=a(c[9],1),H=b(c[i][1],d,k),I=[0,[0,a(j[1][6],DA)],w,D],J=a(c[19],I),K=b(c[i][1],d,n),L=[0,[0,a(j[1][6],DB)],K,J],M=a(c[19],L),N=b(c[i][1],d,q),O=[0,F,[0,b(c[i][1],d,n),N,M,r,H,G]],P=a(c[21],O);return[0,P,b(c[i][5],k,h)]}return[0,r,b(c[i][5],k,h)]},Y=g(l[20],X,W,h)[1];if(x){var
Z=x[2],_=1,G=b(h5(function(b,a){return ki(a[2][3],Z)?[0,(K+1|0)-b|0]:0}),_,k);if(G){var
$=a(c[9],G[1]),ab=aD(aD(b(c[i][1],(o+1|0)+d|0,$),F),[0,Y,0]),ac=function(a){return b5(m,f,a[2],a[3],a[4])},ad=b(l[17],ac,h),ag=function(a){var
d=b(c[i][1],1,a[1]),e=g(N[18],m,f[1],d);return el(o+2|0,-(J-1|0)|0,hm(f[1],U,DD,T,o,S,e)[1])},ai=b(l[17],ag,u),H=a(l[13],ai),aj=aS(d,H),ak=a(l[1],H),al=b(c[i][1],ak,ab),am=gh(m,f[1],al,aj),ae=function(d,b){var
e=[0,[0,a(j[1][6],DC)],d,b];return a(c[18],e)},af=g(l[21],ae,ad,am),an=b(c[38],af,y);return ah([0,L,[0,an],b(c[37],aa,y)])}throw[0,I,DE]}throw[0,I,DF]}return t}var
ag=a(l[6],k),ai=a(l[9],ag),E=p(D[74],af,1,ad,ai),h=Q,d=a(l[9],ac),o=0,n=0;for(;;){if(h){var
F=h[1],G=F[1];if(typeof
G==="number")if(1===G)if(d){var
aj=h[2],h=aj,d=dF(c[14],d[2]),o=o+1|0;continue}if(!F[4]){var
h=h[2];continue}if(d){var
h=h[2],ak=[0,d[1],n],d=d[2],n=ak;continue}var
q=bE(0,a(e[3],DG))}else
if(d)var
al=a(l[9],d),q=[0,o,b(J[26],al,n)];else
var
q=[0,o,n];var
H=q[2],am=q[1],an=b(J[26],E,[0,ae,0]),ao=b(J[26],H,an),ap=b(c[i][1],-am|0,s),aq=b(c[37],ap,ao),ar=function(b){var
c=a(gl,b);return a(L[3],c)},as=b(l[35],ar,E),at=a(l[1],as);return[0,(a(l[1],H)+at|0)+1|0,aq]}}function
DH(e,a){function
d(f,a){var
d=a[1],g=a[2];return[0,d+1|0,[0,b(bm,b(c[i][10],d,e),f),g]]}return g(l[21],d,a,DI)}function
DJ(f,d,i,c){var
k=c[7],h=b(ay[22],i,f),l=ci(f,d,0,c[1][4]),m=a(e[5],0),n=aI(f,d,c[1][2]),o=a(e[5],0),p=a(e[3],DK),q=g(C[15],h,d,k),r=a(e[3],DL),s=bJ(c),t=a(j[1][9],s),u=a(e[3],DM),v=a(e[5],0),w=aT(c),x=g(C[15],h,d,w),y=b(e[12],x,v),z=b(e[12],y,u),A=b(e[12],z,t),B=b(e[12],A,r),D=b(e[12],B,q),E=b(e[12],D,p),F=b(e[12],E,o),G=b(e[12],F,n),H=b(e[12],G,m);return b(e[12],H,l)}function
DN(a){function
c(a){return aT(a)}return b(l[17],c,a)}function
hn(c,a){return b(A[69],c,a)[2]}function
lC(e,d){var
f=[0,[0,DO,[1,b(c[75],e,d)[1]]],0];return F(a(u[68],f))}function
ho(o,f,e,n){if(f){var
g=f[1],d=b(c[91],o,n)[1],p=a(l[1],d),h=b(A[9],0,p);if(-1===g)var
k=a(D[i],h),j=0;else
var
m=b(D[X],g-1|0,h),t=m[1],k=t,j=a(l[6],m[2]);var
q=b(J[26],k,j),r=a(l[1],d),s=aD(b(c[i][1],r,e),q);return b(c[38],s,d)}return e}function
lD(r,e,d){function
h(s,d){var
e=s;for(;;){var
j=d[3],t=d[2],u=d[1];if(e){var
k=e[2],m=e[1],n=m[2],v=n[2],x=n[1],y=m[1];try{var
o=b(A[7],y,j),f=o[1],p=ho(r,x,v,b(c[i][1],f,o[3])),z=dG(f,p,j),q=b(D[X],f-1|0,t),B=q[1],C=a(l[6],q[2]),E=function(f,h){return function(a){var
b=a[2],d=b[1],e=a[1];return[0,e,[0,d,g(c[i][3],[0,h,0],f,b[2])]]}}(f,p),F=b(l[17],E,k),G=h(F,[0,u,b(l[11],B,C),z]);return G}catch(a){a=w(a);if(a===O){var
e=k;continue}throw a}}return d}}return h(e,$(d))}function
lE(d,a,j,h,e){var
k=$(e[1]);function
m(f,g){var
h=g[2],j=f[1],m=h[2],n=h[1],o=g[1];try{var
k=b(A[7],o,j),l=k[1],p=b(c[i][1],l,k[3]),q=an(0,d,[0,a],c6(0,d,a,l,[2,ho(a,n,S(a,e,m),p)],j),f);return q}catch(a){a=w(a);if(a===O)return f;throw a}}var
f=g(l[20],m,k,h);return[0,f,an(0,d,[0,a],an(0,d,[0,a],f,e),j)]}function
lF(D,h){var
q=[0,0],d=a(v[2],0),u=a(o[17],d),m=d5(c[cR],0,0,0,d,u,h),r=m[2],n=m[1],x=z(ad[2],0,0,d,n,r);function
k(h,f,m,E){var
d=b(c[3],f,E);switch(d[0]){case
6:var
s=d[3],o=d[2],n=d[1];try{if(n){var
K=n[1],L=function(c){var
d=a(j[17][9],c),e=a(j[6][7],d);return b(j[1][1],e,K)},M=b(l[33],L,D),u=b(c[91],f,o)[1],N=a(l[6],u),v=d5(c[cR],0,0,0,h,f,[1,M]),x=v[1],P=v[2],Q=[0,P,V(1,N)],R=a(c[21],Q),p=b(c[38],R,u);a3(function(f){var
c=g(C[15],h,x,p),d=a(e[3],DP);return b(e[12],d,c)});q[1]=1;var
S=k(h,x,[0,p,m],b(c[i][5],p,s));return S}throw O}catch(d){d=w(d);if(d===O){var
G=a(c[i][1],1),H=b(l[17],G,m),I=[0,a(c[9],1),H],t=k(b(c[aR],[0,n,o],h),f,I,s),J=t[1];return[0,J,a(c[19],[0,n,o,t[2]])]}throw d}case
8:var
y=d[3],z=d[2],A=d[1],T=d[4],U=a(c[i][1],1),W=b(l[17],U,m),B=k(b(c[aR],[1,A,z,y],h),f,W,T),X=B[1];return[0,X,a(c[20],[0,A,z,y,B[2]])];default:var
F=[0,r,a(a8[72],m)];return[0,f,a(c[21],F)]}}var
s=k(d,n,0,x),f=s[2],t=s[1];if(q[1]){a3(function(i){var
c=g(C[15],d,t,f),h=a(e[3],DQ);return b(e[12],h,c)});var
y=p(ag[2],0,d,t,f)[1],A=a(o[bN],y);return[1,f,a(o[fT],A)]}return[0,h]}function
hp(d,c,a){function
e(a){var
b=a[2],e=b[1],f=a[1];return[0,f,[0,e,S(d,c,b[2])]]}return b(l[17],e,a)}function
lG(m,f,e){var
y=[0,a9[1]],d=[0,f];function
o(b,a){return lD(d[1],b,a)}function
k(c,b,a){return lE(m,d[1],c,b,a)}function
F(q,e,p,h,f){function
r(d,f){var
e=d[1][6];if(e){var
g=0===e[1][0]?0:DT,h=a(l[1],d[2][1])-p|0,j=[0,g,b(c[i][1],h,f)];return[0,[0,d[1][2],j]]}return 0}var
s=g(l[23],r,h,f),u=a(l[9],s);function
v(a){return a}var
j=b(D[66],v,u);function
w(d){var
e=d[2],f=e[2],g=e[1],h=d[1],k=a(l[1],j);return[0,h,[0,g,b(c[i][1],k,f)]]}var
x=b(l[17],w,e),n=b(J[26],j,x);function
y(a,l){var
c=a[4],p=3===c[0]?c[4]:c,f=a[3],r=f?f[1][1]:a[2],b=a[1],g=o(e,b[4]),h=k(g,e,$(b[4]))[1],s=b[8],u=b[7],v=S(d[1],h,b[5]),i=[0,b[1],b[2],b[3],h[1],v,0,u,s],w=[0,i,a[2],a[3],a[4],a[5]],x=[0,a[1][2],q],j=t(o(n,r[1]),n,w,l,x,p),y=cj(m,d,j)[1];return[0,i,$(g[3]),0,j,y]}return g(l[23],y,h,f)}function
t(as,ar,v,u,aq,ap){var
n=as,f=ar,s=aq,e=ap;for(;;)switch(e[0]){case
0:var
G=e[2],r=e[1],H=r[1],at=e[4],av=e[3],M=k(n,f,r),z=M[2],O=M[1],aw=hp(d[1],r,f),P=e5(G),al=r[3],am=r[2],an=r[1],ao=bG(a(l[1],P),am),Q=k(n,f,[0,b(J[26],P,an),ao,al])[1],ax=function(b){return a(L[2],b[2][1])},ay=b(l[28],ax,f),az=function(e,v){var
w=v[1],p=e[4],A=e[3],h=e[1],M=v[2],N=e[7],P=e[5],B=e5(w),Q=o(f,O[3]),q=d[1],J=[0,O,Q];function
L(c,f){var
e=f[2],d=f[1],g=d[3],h=d[2],i=d[1],j=b(bm,function(a){return S(q,e,a)},c),k=a(x[1][1][3],c),n=gH(m,q,e,[0,[0,a(x[1][1][1],c),k],0]),o=[0,j,g],p=1;function
r(a){return cE(p,a)}return[0,[0,[0,c,i],[0,DS,b(l[17],r,h)],o],n]}var
n=bh(0,m,q,g(l[21],L,B,J)[1]),R=a(l[1],H),T=a(l[1],B)+R|0,U=S(d[1],n,N);function
V(d){var
e=d[2],f=e[2],g=e[1],j=d[1],k=a(l[1],H),m=a(l[1],h[2][1])-k|0;return[0,j,[0,g,b(c[i][1],m,f)]]}var
W=b(l[17],V,aw),r=F(s,W,T,[0,h,0],[0,aT(e),0]);if(r)if(!r[2]){var
k=r[1];if(ay)var
t=0;else{var
E=h[3];if(E)if(0===E[1][6][0])var
G=1,u=1;else
var
u=0;else
var
u=0;if(!u)var
G=0;if(G)var
t=0;else
var
ag=S(d[1],n,h[5]),ah=d[1],ai=function(a){return S(ah,n,a)},aj=b(l[17],ai,A),D=[0,k[1],k[2],k[3],k[4],ag],C=aj,t=1}if(!t){var
X=bI(0,p),Y=b(K[5],X,DV),Z=b(au[28],Y,j[1][10][1]),_=S(d[1],n,h[5]),$=d[1],aa=function(a){return S($,n,a)},ab=b(l[17],aa,A),ac=y[1],ad=k[4],ae=[0,aD(_,ab),Z,ad];y[1]=g(a9[4],p,ae,ac);var
D=k,C=aH(0,z[1])}var
af=a(l[1],z[1]);return[0,[0,[0,D,h[1],C,p,P,af,U],w],[0,e,M]]}throw[0,I,DU]},aA=g(l[21],az,G,DW)[1],aB=ku(d[1],Q,at),aC=function(a){return a},aE=g1(b(N[18],m,d[1]),aC,aB);return[0,z,aA,S(d[1],Q,av),aE];case
1:var
aF=e[4],aG=e[3],aI=e[2],R=k(n,f,e[1]),T=R[1],aJ=R[2],aK=a(c[9],aI),aL=S(d[1],T,aK),aM=b(c[66],d[1],aL),aN=function(a){return t(n,f,v,u,s,a)},aO=a(L[16],aN),aP=b(aY[15],aO,aF);return[1,aJ,aM,S(d[1],T,aG),aP];case
2:var
aQ=e[2],aR=k(n,f,e[1])[2];return[2,aR,t(n,f,v,u,s,aQ)];case
3:var
U=e[4],V=e[2],W=v[1][6],aS=e[3];if(W)if(0!==W[1][0]){var
aU=aS[1],Y=[0,[0,V,[0,DX,b(c[i][1],1,u)]],f],n=o(Y,aU[1]),f=Y,s=[0,V,s],e=U;continue}var
e=U;continue;default:var
h=e[2],A=e[1],Z=h[8],_=h[7],$=h[6],B=h[3],C=h[1],aV=e[3],aW=h[10],aX=h[5],aZ=h[2],a0=C[3],a1=C[2],a2=C[1],w=hp(d[1],A,f),E=hp(d[1],h[9],w),aa=k(n,f,A),q=aa[1],a3=aa[2],a4=k(o(f,_[3]),w,_)[2],ab=o(E,Z[3]),ac=k(ab,E,Z),ad=ac[2],a5=ac[1],a6=h[9],a7=k(o(w,h[9][3]),w,a6)[2],a8=function(b){return a(L[2],b[2][1])},a_=b(l[28],a8,f),ae=h[4],af=t(ab,E,v,u,ae,aV);if(a_)var
ag=[0,DY],a$=0,ba=0,bb=function(k,g,e){if(k===B[2]){var
i=a(l[1],g),j=ad[1],h=function(a,b){return 0===b?0:a?0===a[1][0]?h(a[2],b-1|0)+1|0:h(a[2],b)+1|0:0};ag[1]=[0,h(a(l[9],j),i),i]}if(b(c[44],d[1],e)){var
m=b(c[66],d[1],e)-1|0,n=a(bl,b(l[7],A[1],m)),o=a(K[10][16],n);return b(l[42],o,f)?g:[0,S(d[1],q,e),g]}return[0,S(d[1],q,e),g]},bc=p(D[87],bb,ba,a$,$),bd=cj(m,d,af)[1],be=ag[1],bf=b(N[18],m,d[1]),bg=b(l[17],bf,bc),aj=bd,ai=a(l[9],bg),ah=be;else
var
bo=d[1],bp=function(a){return S(bo,q,a)},bq=b(l[17],bp,$),br=a(l[1],f),ak=b(D[X],br,bq),bs=ak[2],bt=ak[1],bu=S(d[1],q,aX),bv=a(l[1],f),bw=B[2]-bv|0,bx=a(l[1],f),by=[0,B[1]-bx|0,bw],aj=aD(bu,bt),ai=bs,ah=by;var
bi=S(d[1],q,a1),bj=g(N[18],m,d[1],bi),bk=S(d[1],a5,aW),bn=S(d[1],q,aZ);return[4,a3,[0,[0,a2,bj,S(d[1],q,a0)],bn,ah,ae,aj,ai,a4,ad,a7,bk],af]}}function
h(a){return a[5]}var
n=F(0,0,0,e,b(l[17],h,e));return[0,y[1],n]}function
lH(f,d,i,m,e){function
n(a){return a[1]}var
o=b(l[17],n,e),j=lG(f,d[1],o),k=j[2],h=j[1];if(a(a9[2],h))if(!gu(m)){var
q=function(b,c){var
a=b[1],d=b[2],e=$(e4(a));return[0,[0,a[1],a[2],a[3],c[4],a[5]],0,d,[0,a[1][2],h,a[5],e]]};return g(l[23],q,e,k)}function
p(g,o){var
j=g[1],a=j[1],k=a[2],l=a[4],p=g[2],q=a[5],m=$(l),r=b(K[5],k,DZ),n=kt(f,d,D1,0,i,D0,db(f,d,i,[0,a[1],r,a[3],l,q,a[6],a[7],a[8]],m,o[4],0)),e=n[1],s=n[2],t=[0,k,h,e[5],m];return[0,j,[0,[0,e,[0,b(c[75],d[1],e[5])[1],s]]],p,t]}return g(l[23],p,e,k)}function
lI(a,j,i,d){function
f(e,d){var
h=b(c[3],a,d);switch(h[0]){case
1:if(g(c[95],a,j,d))return g(i,e,d,[0]);break;case
9:var
k=h[1],m=h[2];if(g(c[95],a,j,k)){var
n=function(a){return a+1|0},o=p(c[a_],a,n,f,e);return g(i,e,k,b(aY[15],o,m))}break}function
l(a){return a+1|0}return z(c[a_],a,l,f,e,d)}return f(0,d)}function
lJ(a){return[0,[0,a[1],0],a[2],a[3]]}function
lK(k,j,e){var
f=e[3];function
h(f,e){var
g=e[3],h=e[2],d=e[1],j=a(x[1][1][2],f);if(j){var
k=b(c[i][4],d,j[1]),m=1,n=function(a){return cE(m,a)};return[0,[0,k,d],b(l[17],n,h),g]}var
o=a(c[i][4],d),p=[0,b(x[1][1][14],o,f),g],q=1;function
r(a){return cE(q,a)}var
s=[0,D3,b(l[17],r,h)],t=a(c[i][1],1),u=b(l[17],t,d);return[0,[0,a(c[9],1),u],s,p]}var
d=g(l[21],h,f,D2),m=d[1];return[0,an(0,k,[0,j],e,[0,f,d[2],d[3]]),m]}function
fq(a){var
c=eL(a);return b(l[19],cc,c)}function
lL(m,d,k,j,h,f){var
s=f[2],n=f[4],p=f[3];function
o(j,B,n,q,m,A,y){var
k=B,p=A,f=y;for(;;)switch(f[0]){case
0:var
D=f[4],E=f[3],F=f[2],G=f[1],H=function(e,n){var
p=n[1],B=n[2],C=aT(e),g=b(c[i][4],p,C),q=b(c[83],d,g),r=q[1],D=q[2];try{var
l=b(a9[22],e[4],s),L=l[3],M=l[2],x=b_(d,l[1]),y=x[1],N=x[2],A=lt(d,hn(d,aT(e)),N),P=[0,[0,[0,y,A],r],m],Q=[0,[0,[0,y,A],M,L]],u=Q,t=P}catch(a){a=w(a);if(a!==O)throw a;var
u=0,t=m}var
E=z(ad[2],0,0,j,d,r);function
h(p,j,e){var
f=b(c[3],d,p);if(6===f[0])if(e){var
k=e[2],l=e[1],m=f[3],q=f[2],r=f[1];if(b(c[44],d,l)){var
n=h(m,j+1|0,k),s=[0,j,n[2]];return[0,a(c[19],[0,r,q,n[1]]),s]}var
o=h(b(c[i][5],l,m),j+1|0,k);return[0,o[1],o[2]]}if(e)throw[0,I,D4];return[0,g,0]}var
v=h(E,0,D),f=v[1],F=v[2],k=lK(j,d,$(e[1][1][4]))[1],G=o(j,k,f,0,t,D5,e[1][4]),H=e[1][1][5],J=a(a9[2],s)?[0,f,F]:[0,f,D6],K=fq(k);return[0,[0,g,p],[0,[0,J,u,e[5],k[1],H,K,0,G],B]]},t=g(l[21],H,F,D7),r=t[1],K=t[2],u=an(0,j,[0,d],G,k);if(Z[1]){var
L=b(C[15],j,d),P=g(e[39],e[13],L,r),Q=a(e[3],D8),R=b(e[12],Q,P);b(M[10],0,R)}var
T=function(a){return a},U=g1(function(k){var
f=b(c[i][4],r,k),h=g(N[18],j,d,f);function
e(e,i){var
b=e[1],f=e[2],g=b[2],h=b[1];return lI(d,h,function(e,d,b){if(0===e){var
h=[0,f,fp(g,a(aY[11],b))];return a(c[34],h)}return a(c[21],[0,d,b])},i)}return g(l[21],e,m,h)},T,D),V=fq(u),W=b(c[i][4],r,E);return[0,[0,u[1],n,q,V,W,n,[0,2,p[2]],U,[0,K]],0];case
1:var
X=f[4],Y=0,_=function(c,a){if(a){var
d=o(j,k,n,q,m,p,a[1]);return b(J[26],c,d)}return c};return g(aY[17],_,Y,X);case
2:var
aa=f[2];an(0,j,[0,d],k,f[1]);var
f=aa;continue;case
3:var
ab=f[4],ac=an(0,j,[0,d],f[3][1],k),k=ac,p=[0,p[1],0],f=ab;continue;default:var
h=f[2],v=f[1],ae=f[3],af=h[1][2],x=an(0,j,[0,d],v,k),ag=fq(x),ah=fq(an(0,j,[0,d],h[9],x)),ai=[0,hn(d,h[5]).length-1,0],aj=o(j,h[8],h[5],0,m,D9,ae),ak=h[3],al=[0,[0,S(d,h[9],af),ak],0],am=[0,[0,[0,[0,h[5],ai],0,h[4],h[8][1],h[10],ah,al,aj],0]],ao=[0,aD(h[5],h[6])];return[0,[0,v[1],n,q,ag,h[2],n,D_,ao,am],0]}}return o(m,n,p,k,0,j,h[4])}function
D$(b,d){switch(b[0]){case
0:return a(c[10],b[1]);case
1:return a(c[23],[0,b[1],d]);case
2:return a(c[26],[0,b[1],d]);default:return a(c[28],[0,b[1],d])}}function
Ed(n,i,G,d,N,al,t,q,ak,H,aj,ai,s,aB,F,aA,az){var
u=dd(n[1]),r=a(j[1][6],n[1][3]),f=[0,0],O=b(K[5],r,Ee);function
h(q){var
e=q[2],g=e[8][1],k=e[4],h=e[3],m=e[2],r=e[1][1];if(typeof
g==="number")if(0===g)var
i=0;else
var
n=0,i=1;else
var
i=0;if(!i)var
n=1;if(n){var
s=m?m[1][1][1]:r,o=aH(0,k),u=a(c[34],[0,s,o]),v=bI(0,h),w=aV(d,f3(b(K[5],v,Ef))),x=[0,w,b(J[26],o,[0,u,0])],y=a(c[34],x),p=b8(G,d[1],y,k),z=function(a){var
c=a[1],d=bI(0,h);return b(j[1][1],c[1][2],d)},A=b(l[33],z,t)[1],B=typeof
g==="number"?0:1;if(B){var
C=f[1];f[1]=[0,[0,bI(0,h),p,A],C]}return[0,p]}return 0}if(q){var
Q=q[1];if(q[2])var
y=function(f){var
b=f;for(;;){var
c=a(D[fH],b),d=c[2],e=h(c[1]);if(e)return[0,e[1],d];var
b=d;continue}}(q),R=y[2],S=y[1],T=function(g,b){var
e=h(g);if(e){var
i=e[1],f=[0,av(du,d),[0,i,b]];return a(c[21],f)}return b},x=g(l[21],T,R,S);else
var
ay=h(Q),x=a(L[7],ay);var
U=a(l[1],H),V=b(A[9],0,U),B=i?i[1][1][1]:F,am=a(c[34],[0,B,V]),W=function(aM,x,an){var
k=a(v[2],0);g(aJ[22],0,[0,n[1][3],0],[1,[0,[0,0,u,[0,x]],0]]);try{var
h=n[1],p=a(j[1][6],h[3]),y=a(l[1],aj),Q=a(v[2],0),R=b(v[49],Q,s)[1],S=a(v[2],0);d[1]=a(o[17],S);if(dd(h)){d[1]=b(o[fV],d[1],h[2]);d[1]=b(o[fV],d[1],an);var
A=b(P[9],d[1],s),B=A[2],C=A[1],T=z(ad[2],0,0,k,C,B);d[1]=C;var
E=B,D=T}else
var
ah=D$(s,c[2][3]),E=ah,D=a(c[8],R);var
F=lB(k,d,h[7],N,al,ai,y,q,ak,H,am,D),G=F[2],U=F[1],V=function(y,l,x){var
d=a(v[2],0),m=a(o[17],d),n=b(at[32],0,p),f=ba(m,a(ap[9],n)),g=f[2],i=ba(f[1],l),j=i[2],k=iC(i[1]),e=k[1],q=k[2],r=f5(U),s=[0,a(c[8],r),[0,j,0]],t=[0,g,[0,z(ad[2],0,0,d,e,j),s]],u=[0,z(ad[2],0,0,d,e,g),t],w=b(K[6],Ea,p);dq(w,dd(h),e,0,q,u);return 0},W=ln(E,y,a(l[1],t),h,x);try{cV(a(v[2],0),d,G)}catch(f){f=w(f);if(f[1]!==fj[1])throw f;var
X=f[2],Y=[16,b(bD[13],c[8],f[3])],Z=g(bD[2],X,d[1],Y),$=a(e[3],Eb),aa=b(e[12],$,Z);g(_[6],0,0,aa)}var
ab=[0,a(bK[1],V)],ac=[0,h[4]],ae=a(o[bz],d[1]),af=g(c[5],0,d[1],G),ag=b(K[5],p,Ec);bY(aZ[7],ag,0,af,ae,0,0,ac,[0,W],0,ab,0,[0])}catch(f){f=w(f);if(f[1]===fj[1]){var
ao=f[2],aq=[16,b(bD[13],c[8],f[3])],ar=g(bD[2],ao,d[1],aq),as=a(e[3],Eg),au=b(e[12],as,ar);g(_[6],0,0,au)}else{var
aG=b(_[14],0,f),aH=a(e[5],0),aI=a(e[3],Ei),aK=b(e[12],aI,aH),aL=b(e[12],aK,aG);b(M[8],0,aL)}}var
av=a(o[17],k),aw=b(at[32],0,r),I=ba(av,a(ap[9],aw)),m=I[2],J=ba(I[1],x),L=J[2],O=iB(J[1]),f=O[1],ax=O[2],ay=[0,m,[0,z(ad[2],0,0,k,f,L),[0,L,0]]],az=[0,z(ad[2],0,0,k,f,m),ay];dq(b(K[6],Eh,r),u,f,0,ax,az);if(ec[1]){var
aA=d3[3],aB=[0,b(c[75],f,m)[1]];b(v[53],aB,aA);if(i){var
aC=d3[3],aD=[0,b(c[75],f,i[1][1][1])[1]];return b(v[53],aD,aC)}return 0}var
aE=a(ls,b(c[75],f,m)[1]);b(cU[7],0,aE);if(i){var
aF=a(ls,b(c[75],f,i[1][1][1])[1]);return b(cU[7],0,aF)}return 0},X=d[1],Y=a(v[2],0);d[1]=p(ag[2],0,Y,X,x)[1];var
Z=g(c[5],0,d[1],x),$=g(c[5],0,d[1],B);if(u)var
C=d[1];else
var
ax=a(v[2],0),C=a(o[17],ax);var
aa=a(o[bz],C),E=function(b){var
c=[0,a(bK[1],W)],d=[0,a(m[66][24],b)];bY(aZ[7],O,0,Z,aa,0,0,[0,n[1][4]],d,0,c,0,[0]);return 0},ab=ll(N,$,n,r,f[1],t);try{var
aw=E(ab);return aw}catch(f){f=w(f);if(f[1]===fj[1]){var
ac=f[2],ae=[16,b(bD[13],c[8],f[3])],af=g(bD[2],ac,d[1],ae),ah=a(e[3],Ej),an=b(e[12],ah,af);return g(_[6],0,0,an)}var
ao=b(_[14],0,f),aq=a(e[5],0),ar=a(e[3],Ek),as=b(e[12],ar,aq),au=b(e[12],as,ao);b(M[8],0,au);return E(a(k[16],0))}}throw[0,I,El]}function
lM(i,h,d,a){function
c(a){var
e=a[4],f=a[1],c=b(L[25],f,a[2]);return[0,f,e,lL(i,h,d,[0,lo(c[1]),0],c,e)]}var
f=b(l[17],c,a);function
e(a){function
c(a,g){var
c=a[9],d=a[7],q=g[2],r=g[1],h=a[8],i=a[6],j=a[5],k=a[4],l=a[3],m=a[2],n=a[1];if(c)var
o=c[1],p=function(a){var
c=a[7],f=a[6],g=a[5],h=a[4],i=a[3],j=a[2],k=a[1],b=e(a[8]);return[0,[0,[0,k,j,i,h,g,f,c,d],b[1]],b[2]]},f=b(D[78],p,o);else
var
f=0;return[0,[0,[0,n,m,l,k,j,i,d,h],r],b(J[26],f,q)]}return g(l[21],c,a,Em)}function
j(c,h){var
f=c[2],i=c[1],g=e(c[3]),a=i[1],j=g[2],k=g[1],m=[0,lo(a),0],n=b(l[19],cc,f[4][2]),o=[0,[0,[0,f[3],0],d,[0,a[2],0],a[4],a[5],n,0,m],k];return[0,o,b(J[26],j,h)]}return g(l[21],j,f,0)}function
En(f){var
d=a(k[67][5],f),o=a(k[67][2],f),g=b(c[3],d,o);if(9===g[0]){var
h=g[2];if(3===h.length-1){var
e=b(c[3],d,h[2]);if(9===e[0]){var
p=e[2],i=b(c[3],d,e[1]);if(14===i[0]){var
j=i[1][1],l=j[2],m=aa(j[1],l)[l+1],q=aa(p,m)[m+1],n=b(c[3],d,q);return 1===n[0]?gf(n[1]):a(k[16],0)}return a(k[16],0)}return a(k[16],0)}}return a(k[16],0)}var
Eo=a(k[67][9],En);function
hq(I,H,f,F,E,n){if(Z[1]){var
W=M[10],X=function(c){var
d=c[2],h=c[1];if(d)var
i=ci(H,f,0,d[1][4]),j=a(e[3],Ep),g=b(e[12],j,i);else
var
g=a(e[7],0);var
k=a(e[5],0),l=ci(H,f,0,h[4]),m=b(e[12],l,k);return b(e[12],m,g)},Y=g(e[39],e[5],X,n),_=a(e[3],Eq);b(W,0,b(e[12],_,Y))}var
h=a(v[2],0),s=a(l[5],n),O=s[4],P=s[3],Q=s[1],$=j[1][10][1];function
ab(c,a){return b(j[1][10][7],a[3][2][7],c)}var
ae=g(l[20],ab,$,n),t=O[3],R=O[2],i=P[2],af=e4(Q),w=P[1],S=lM(h,f,F,n);function
ai(a){return a[1]}var
x=b(l[17],ai,S),T=a(l[1],x);function
ak(d){var
i=d[2],j=d[1][1];if(i){var
k=i[1][1][1];a3(function(o){var
c=g(C[15],h,f,k),d=a(e[3],Er),i=g(C[15],h,f,j),l=a(e[3],Es),m=b(e[12],l,i),n=b(e[12],m,d);return b(e[12],n,c)});var
l=b(c[83],f,k)[1];return[0,b(c[75],f,l)[1]]}a3(function(l){var
c=a(e[3],Et),d=g(C[15],h,f,j),i=a(e[3],Eu),k=b(e[12],i,d);return b(e[12],k,c)});return 0}var
al=b(D[66],ak,x);function
am(k){var
c=k[3][2],d=0,f=c[6];function
h(i,h){var
d=[1,i];a3(function(g){var
c=a(C[58],d),f=a(e[3],DR);return b(e[12],f,c)});var
f=lF(al,d),g=dd(c);return[0,[0,aJ[4],g,1,0,f],h]}var
i=[0,g(l[21],h,f,d)],j=[0,hg(c),0];return g(aJ[22],0,j,i)}b(l[15],am,n);var
an=1;function
ao(i,a){var
d=a[2],e=a[1],j=a[5],k=a[4],l=e[2],h=b(A[60],f,e[1]),m=b(c[85],f,h)[2],n=b(c[83],f,m)[1],o=d?[0,d[1][1]]:0;return[0,n,[0,h,l],o,T-i|0,k,g(c[5],0,f,j)]}var
U=g(D[73],ao,an,x),d=[0,f],r=dd(i);function
aq(aC,i){var
f=i[1],j=f[2],p=i[2],q=f[8],r=f[7],s=f[6],v=f[5],w=f[4],x=f[3],y=f[1];if(j)var
n=j[1][1],o=[0,n,0,x,w,v,s,r,q],Q=n[2];else
var
o=f,Q=y[2];function
z(f){var
i=f[8],s=f[7],v=f[5],w=f[4],x=f[3],n=f[2],j=f[1],R=s[2],S=s[1];if(x)var
y=x[1],V=y[2],W=y[1][1],X=df[3],Y=function(a){return b(X,0,a)},_=b(at[32],0,V),$=a(ap[9],_),aa=a(m[66][61],$),D=W,z=b(k[17],aa,Y);else{if(g(c[95],d[1],n,t))var
aA=aj(lC(d[1],t)),aB=b(m[66][3],aA,Eo),P=b(m[66][12],u[ea],aB);else
var
P=m[66][2];var
D=n,z=P}var
F=aD(D,w),ab=d[1],ac=b(c[B],j,h),o=b(N[18],ac,ab);if(0===i[0])var
ad=a(o,i[1]),G=b5(h,d,v,a(o,F),ad);else
var
ay=[0,v,a(o,F)],az=[0,iG(d),ay],G=a(c[21],az);var
p=b(c[37],G,j);if(Z[1]){var
af=g(C[15],h,d[1],p),ah=a(e[3],Ev),ai=b(e[12],ah,af);b(M[10],0,ai)}aw(b(ag[2],0,h),d,p);if(0===i[0]){var
ak=i[1],H=a(l[1],j),al=g(N[18],h,d[1],ak),q=hm(d[1],ae,0,E,H,U,al),r=q[2],am=q[3],an=q[1],I=a(c[9],(H+(T-aC|0)|0)+r|0);if(R)var
K=I;else
var
ax=b(A[60],d[1],n),K=aD(I,fp(Q,gi(r,b(c[83],d[1],ax)[2])));var
ao=gi(r,w),aq=aD(K,b(J[26],ao,[0,am,0])),ar=gh(h,d[1],aq,an),L=iV(d[1],ar,j);if(Z[1]){var
as=g(C[15],h,d[1],L),au=a(e[3],Ew),av=b(e[12],au,as);b(M[10],0,av)}var
O=[0,L]}else
var
O=0;return[0,S,z,p,O]}return[0,o,b(l[17],z,p)]}var
V=g(D[73],aq,0,S),ar=0;function
as(b,a){var
c=a[2],d=a[1],e=1;function
f(b,a){return[0,b,a]}return[0,b,d,g(D[73],f,e,c)]}var
q=g(D[73],as,ar,V);function
au(a){return a[2]}var
av=b(l[17],au,V),ax=a(l[13],av),y=[0,a9[1]];function
ay(i,n){var
o=n[3],j=n[2],f=j[5],e=j[4],q=j[3],r=i[3],v=i[2],w=i[1],x=bI(0,q),k=b(K[5],x,Ex);function
A(b){var
c=a(bl,b),d=a(K[10][16],c);return a(G[2],d)}var
B=b(l[19],A,e);y[1]=g(a9[4],q,[0,k,B],y[1]);function
C(a){var
e=a[2][4],f=b(c[5],0,d[1]);return b(L[16],f,e)}var
s=b(D[66],C,o);function
E(c){var
d=c[2],e=d[4],f=d[1],g=c[1];function
h(h){var
c=a(J[22],g),d=1===f?Ey:Ez,e=b(J[17],d,c);return b(K[5],k,e)}return b(L[16],h,e)}var
F=b(D[66],E,o);function
H(f,e){var
g=a(c[8],e),h=b(c[fH],d[1],g);return b(aM[2][7],h,f)}var
I=g(l[20],H,v,s),M=b(c[37],f,e);if(0===z(ad[4],0,0,h,d[1],M))var
m=aM[5];else
var
S=[0,ah([0,0,0,f]),e],T=d[1],t=[0,h,r],u=function(e,d){var
f=d[1],g=d[2],h=a(be,e),i=p(ad[3],0,f,T,h),j=a(ei[14],i),k=b(aM[12],j,g);return[0,b(c[aR],e,f),k]},m=g(l[21],u,S,t)[2];var
N=a(ei[15],m),O=[0,0,f,a(c[13],N)],P=a(c[18],O),Q=b(c[37],P,e),R=[0,k,g(c[5],0,d[1],Q),0,F,s];return[0,[0,[0,R,e,f],w],I,b(aM[12],m,r)]}d[1]=a(o[bN],d[1]);if(!r){var
aA=a(o[fT],d[1]);b(bb[14],0,aA);var
aB=a(v[2],0);d[1]=a(o[17],aB)}function
az(e){var
f=e[3],h=e[2],G=h[2],k=h[3],aA=e[1],H=ck(a(l[1],f),0),p=bI(0,k);function
s(f){var
e=f[2],L=f[1],aB=e[4],k=e[3],s=e[2],x=a(J[22],L),z=b(J[17],EL,x),A=b(K[5],p,z);function
B(aG,M,aF){if(0===aB)g(ac[41],0,1,M);else{var
aE=a(Dh,[0,i[3],M]);b(cU[7],0,aE)}var
N=L-1|0;aa(H,N)[N+1]=1;function
aC(a){return a}var
O=b(a8[34],aC,H);if(O){g(ac[32],[1,w],0,0);if(G){var
aD=[0,b(c[75],d[1],G[1][1][1])[1]];b(v[53],aD,1)}b(v[53],[0,w],1);var
P=I?(aA+1|0)===a(l[1],q)?1:0:I;if(P){var
p=g(l[20],ay,[0,0,aM[2][1],aM[5]],q),T=p[3],V=p[1],W=b(o[hR],d[1],p[2]),x=a(o[bN],W),X=function(e){var
d=e[1],f=e[3],h=e[2],i=d[5],j=d[4],k=d[3],l=a(ei[15],T),m=[0,0,f,a(c[13],l)],n=a(c[18],m),o=b(c[37],n,h),p=g(c[5],0,x,o);return[0,d[1],p,k,j,i]},e=b(l[19],X,V),h=b(o[m3],r,x);g(cu[11],EB,EA,0);var
f=g(kU[2],[0,0,0,0,e,h,0],kT[5],0);g(cu[11],ED,EC,1);var
Y=a(v[28],[0,f,0])[2][8],Z=function(b,a){switch(b){case
0:return 0===a?0:-1;case
1:switch(a){case
0:return 1;case
1:return 0;default:return-1}default:return 2<=a?0:1}},_=b(D[39],Z,Y),k=a(D[aR],_);switch(k){case
0:var
m=EE;break;case
1:var
m=EJ;break;default:var
m=EK}if(e)if(e[2])var
u=0;else{var
C=b(K[5],e[1][1],m),au=0,av=function(a,c){return[0,b(aL[1],0,C),0,[0,f,a],k]},aw=g(D[73],av,au,e);b(lN[5],EI,aw);var
az=b(at[32],0,C),B=b(ee[3],0,az),u=1}else
var
u=0;if(!u){var
$=0,ab=function(c,a){var
d=b(K[5],a[1],EF);return[0,b(aL[1],0,d),0,[0,f,c],k]},z=g(D[73],ab,$,e);b(lN[5],EG,z);var
ad=b(J[17],EH,m),ae=a(j[1][6],i[3]),A=b(K[5],ae,ad),ag=function(b,a){var
c=b[1];return[0,c,lp(a[2][8][1])]},ah=g(l[23],ag,z,q),ai=function(a){var
b=a[1];return a[2]?[0,b]:0},aj=b(D[66],ai,ah);k_(b(aL[1],0,A),aj);var
ak=b(at[32],0,A),B=b(ee[3],0,ak)}switch(h[0]){case
0:var
s=a(c[25],[0,f,0]);break;case
1:var
ao=a(aM[36][4],h[1]),ap=[0,[0,f,0],a(c[2][1],ao)],s=a(c[26],ap);break;default:var
aq=a(aM[38][4],h[1]),ar=a(aM[36][4],aq),as=[0,[0,f,0],a(c[2][1],ar)],s=a(c[26],as)}var
al=function(b,a){var
c=a[5],d=1;function
e(a,c){return[0,aJ[4],r,1,0,[0,[3,[0,[0,f,b],a]]]]}var
h=[0,g(D[73],e,d,c)];return g(aJ[22],0,[0,i[3],0],h)};b(l[16],al,e);var
am=[0,i,y[1],R],an=Q[4];return Ed(am,F,a(v[2],0),d,E,U,n,q,ax,af,e,f,B,k,t,an,s)}var
S=P}else
var
S=O;return S}var
C=[0,u[ea],0];if(a(a9[2],R))var
h=m[66][2];else
var
W=aj(lb(b(J[17],i[3],EM))),h=a(m[66][24],W);var
M=[0,s,[0,iT([1,w]),[0,h,C]]],N=a(m[66][22],[0,u[28],M]);if(!r){var
V=a(v[2],0);d[1]=a(o[17],V)}var
O=[0,a(bK[1],B)],P=[0,i[4]],S=a(o[bz],d[1]),T=g(c[5],0,d[1],k);bY(aZ[7],A,0,T,S,0,0,P,[0,N],0,O,0,[0]);return 0}return b(l[15],s,f)}return b(l[15],az,q)}aB(1279,[0,lq,C9,lt,fp,lu,lK,lF,lx,hm,lI,ly,Dv,lz,lA,lB,DH,DJ,DN,hn,lC,lD,ho,lE,lG,lH,hq,lM,lL,lJ],"Principles");function
lO(a){var
c=a[5];function
d(a){return g(ap[43],0,j[1][10][1],[1,a[1]])}var
e=b(f[17][69],d,c);return b(gK[2][88],1,e)}function
lP(m,x,au,D){var
p=a(v[2],0),h=[0,a(o[17],p)];if(m[1]){var
aq=a(f[17][5],D)[2][2][2];h[1]=b(o[fV],h[1],aq)}var
q=lH(p,h,m,x,D);if(q){var
r=q[1],E=r[2];if(E)if(!q[2]){var
s=r[4],n=r[3],F=E[1],k=F[1],l=r[1],i=F[2][2],G=b(j[1][10][7],n[2][7],i[7]),H=i[6],L=b(f[18],n[2][5],i[5]),d=[0,i[1],i[2],n[2][3],i[4],L,H,G];lO(d);var
y=i[1];if(1===y[0]){var
t=y[1];if(m[1])h[1]=a(o[18],i[2]);var
z=aV(h,i[1]),M=k[4],N=bU(k),A=b(K[5],N,EO),P=function(V,U,T){b(v[53],[0,t],d3[3]);function
i(t,i){var
f=i[2];try{var
r=b(at[32],0,f),s=a(ap[11],r),h=s}catch(c){c=w(c);if(c!==O)throw c;var
k=a(j[1][9],f),l=a(e[3],EP),h=bE(0,b(e[12],l,k))}var
m=a(v[2],0),c=g(dp[15],0,m,[1,h]),n=[0,b(aL[1],0,[0,c,1,0]),0],o=b(J[17],d[3],EQ);b(cJ[1],o,n);var
p=[0,b(aL[1],0,[0,c,0,0]),0],q=b(J[17],d[3],ER);return b(cJ[1],q,p)}b(a9[10],i,s[2]);var
q=a(v[2],0);if(1-m[1])h[1]=a(o[17],q);var
c=k[1],r=k[5],u=k[4],y=k[3],B=k[2],C=c[8],D=c[7],E=c[6],F=c[5],G=c[4],H=c[3],I=bU(l),K=[0,[0,c[1],I,H,G,F,E,D,C],B,y,u,r],L=s[4],M=s[2],N=[0,bU(l),M,z,L],P=d[7],Q=b(f[18],d[6],n[2][6]),R=[0,[0,l,[0,K],[0,t,[0,d[1],d[2],d[3],d[4],d[5],Q,P]],N],0],S=[0,lJ([0,l[5],A,l[4]])];return hq(m[4],p,h[1],S,x,R)};if(1-m[1]){var
Q=a(v[2],0);h[1]=a(o[17],Q)}var
u=e4(k),R=kk(k),S=[0,z,V(0,u)],T=a(c[21],S),U=V(0,u),W=a(c[21],[0,l[5],U]),X=b5(a(v[2],0),h,R,W,T),Y=b(c[37],X,u),Z=h[1],_=a(v[2],0),B=g(ag[9],_,Z,Y),C=B[1],$=B[2],aa=l[4],ab=n[1],ac=s[2],ad=function(a){return lm(d,ac,ab,t,aa,M,a)},ae=[0],af=0,ah=[0,a(bK[1],P)],ai=[0,function(a){return a}],ak=[0,aj(ad)],al=[0,d[4]],am=[0,kj(l)],an=a(o[bz],C),ao=g(c[5],0,C,$);bY(aZ[7],A,0,ao,an,0,am,al,ak,ai,ah,af,ae);return 0}throw[0,I,EN]}}function
ar(a){var
b=a[2],c=a[4],d=a[3],e=a[1],f=b?[0,b[1][1]]:0;return[0,e,f,d,c]}var
as=b(f[17][69],ar,q);return hq(m[4],p,h[1],0,x,as)}function
lQ(m,l,E,g,e){function
n(a){return b(f[17][25],a,E)?0:1}var
u=n(ES);if(u)var
x=u,w=n(ET);else
var
x=0,w=0;var
y=a(v[2],0),d=[0,m,l,x,w],c=[0,a(o[17],y)];function
F(a){return g9(y,c,m,eE(a[1][1][2],[0,g,e]),l,e,a)}var
h=b(f[17][69],F,g),z=fd(0,h);kN(h);var
A=a(v[2],0),i=kO(A,c,0,h),G=i[3],H=i[1],B=b(P[36],c[1],i[2]),J=[0,z,B,d,H,e];function
K(a){return a[2]}var
C=kR(A,c,J,h,b(f[17][69],K,g)),D=s(bB),M=a(v[2],0),N=t===D?bB[1]:q===D?a(r[2],bB):bB,O=a(aF[8],N),Q=a(f[17][5],C)[1][2],R=a(j[1][8],Q),S=a(j[18][6],O),T=a(j[18][12],S);p(aJ[17],0,R,[0,j[60][1],T],1);var
k=ck(a(f[17][1],g),0);return g0(M,c,z,B,d,0,C,function(h,p,g){lO(g);var
i=g[1];if(1===i[0]){var
q=i[1];c[1]=a(o[18],g[2]);aa(k,h)[h+1]=[0,[0,p,[0,q,g]]];var
r=function(b){return 1-a(L[3],b)},j=b(a8[34],r,k);if(j){var
s=a(P[30],c[1]),t=b(f[17][69],s,G),u=function(b){return a(L[7],b)},l=b(f[19][53],u,k),w=function(a){return a[1][1]},x=fd(0,b(f[17][69],w,l)),y=a(v[2],0),z=a(dK[8],y);b(f[17][11],z,e);var
A=d[3],m=A||d[4];if(m)return lP(d,x,t,l);var
n=m}else
var
n=j;return n}throw[0,I,EU]})}function
hr(h,e,d,a,c){function
i(c){var
a=c[1][1],d=b(aL[1],[0,a[1]],a[2]);return g(ey[14],d,0,EV)}b(f[17][11],i,a);return lQ(h,e,d,a,c)}function
lR(A,y,d){var
B=a(E[7],d);function
k(e,h){var
c=a(G[26],e);switch(c[0]){case
6:var
i=c[1];if(i){var
p=c[3],q=i[1],r=a(E[8],d),l=eh(j[1][10][1],q,r),s=a(G[2],l),f=k(b(aA[14],s,p),[0,l]),t=f[3],v=f[2],w=f[1],x=F(u[16]);return[0,b(m[5],x,w),v,t]}break;case
8:var
n=c[1];if(n){var
o=n[1],y=c[4],z=c[2];if(am.caml_string_equal(a(j[1][8],o),EW))return[0,m[1],h,e];var
A=a(E[8],d),B=[0,eh(j[1][10][1],o,A)],g=k(b(aA[14],z,y),B),C=g[3],D=g[2],H=g[1],I=F(u[16]);return[0,b(m[5],I,H),D,C]}break}return[0,m[1],h,e]}var
C=a(E[2],d),l=k(g(c[5],0,C,B),0),p=l[2],D=l[3],H=l[1];if(p)var
I=p[1],q=function(a){return F(b(u[81],a,[1,I]))};else
var
q=function(a){return m[1]};var
J=a(c[8],D),K=a(E[2],d),h=b(c[3],K,J);if(8===h[0]){var
v=h[1];if(v){var
Y=h[4],Z=h[2],_=v[1],$=a(E[2],d),i=b(c[3],$,Y);if(8===i[0]){var
x=i[1];if(x)var
aa=i[4],ab=i[2],ac=x[1],ad=a(E[2],d),ae=eg(g(c[5],0,ad,ab)),af=a(E[2],d),w=[0,_,ac,eg(g(c[5],0,af,Z)),ae,aa],o=1;else
var
o=0}else
var
o=0;if(!o)var
w=bf(EZ);var
e=w,n=1}else
var
n=0}else
var
n=0;if(!n)var
e=bf(EX);var
L=e[5],M=e[4],N=e[3],O=e[2],P=e[1];function
r(g,f){if(0===g)return[0,0,f];var
j=a(E[2],d),e=b(c[3],j,f);if(8===e[0]){var
h=e[1];if(h){var
k=e[3],l=e[2],m=h[1],i=r(g-1|0,e[4]);return[0,[0,[0,m,l,k],i[1]],i[2]]}}return bf(EY)}var
s=r(M,L)[1],t=[0,P,[0,O,b(f[17][69],lq,s)]],Q=F(a(u[75],t)),R=F(a(u[25],t)),S=b(m[5],R,Q),T=F(u[16]),U=b(m[26],N+1|0,T);function
V(a){var
c=a[1],d=a[3],e=a[2],f=q(c),g=b(m[5],f,y),h=F(z(u[bw],0,[0,c],e,[0,d],cA));return b(m[5],h,g)}var
W=b(f[17][69],V,s),X=[0,H,[0,S,[0,U,[0,b(m[11],A,W),0]]]];return b(m[7],X,d)}aB(1280,[0,lQ,lP,hr,lR,function(c,f,e,d){var
a=b(ay[88],c,e),h=g(es,function(a,d){var
e=b9(d),h=g(A[99],c,f,d);return b(j[1][10][3],e,a)?b(j[1][10][7],h,a):a},a,d);return[0,a,b(j[1][10][9],h,a)]}],b1);function
lS(h,e,T,n){var
u=n[1],C=a(v[28],u),E=C[2],al=C[1],am=b(c[2][2],e,n[2]),F=[0,n[1],am],an=b(aA[25],F[2],E[2]),G=c2(e,b(f[17][69],c[bx],an)),ao=a(f[17][1],G),w=al[6],aq=E[6],H=aX(0,ao);b(f[19][55],w,H);a(c[9],1);var
at=[0,a(c[26],n),H],J=a(c[21],at),M=aN(e,dt),av=M[2],N=aN(M[1],b4),O=N[1],as=0,aw=N[2],af=[0,j[1][10][1],0];function
aj(o,i){var
d=i[2],e=i[1],f=a(aC,o),g=f[3],k=f[2],l=f[1];if(l){var
m=b(au[26],l[1],e),q=b(j[1][10][4],m,e);return[0,q,[0,ar([0,m],k,g),d]]}var
r=b(c[B],d,h),n=p(au[10],r,O,g,0),s=b(j[1][10][4],n,e);return[0,s,[0,ar([0,n],k,g),d]]}var
m=g(f[17][16],aj,G,af)[2],ay=a(j[1][6],E4),az=a(j[1][6],E5),aB=[0,ah([0,[0,ay],0,J]),m],k=[0,ah([0,[0,az],0,b(c[i][1],1,J)]),aB],Q=s(aK),aD=t===Q?aK[1]:q===Q?a(r[2],aK):aK,R=p(o[hA],0,0,O,aD),aE=R[1],l=a(c[13],R[2]);b(c[37],l,k);g(x[1][15],c[9],0,m);a(f[17][1],as);var
S=$(k),aF=b(ae[4],h,F),U=aN(aE,ai),aG=U[2],V=aN(U[1],ak),Y=V[2],Z=aN(V[1],bc),d=Z[1],aH=Z[2];function
aI(c){var
d=a(bl,c),e=[0,a(K[10][16],d),1];return b(W[3],0,e)}var
_=b(f[17][14],aI,m);function
aJ(A,z){var
B=a(c[8],z),C=g(c[92],d,w,B)[2],D=b(f[17][X],aq,m)[2];function
E(b){var
d=a(bl,b),e=a(K[10][16],d);return a(c[10],e)}var
F=b(f[17][69],E,D),G=g(c[i][3],F,0,C),l=b(c[91],d,G),n=l[1],H=l[2];function
t(i,a,h){function
e(g){var
a=b(c[3],d,g);switch(a[0]){case
0:return a[1]===i?1:0;case
9:var
h=a[2];return b(c[56],d,a[1])?b(f[19][32],e,h):0;default:return 0}}var
g=b_(d,H)[2];return b(f[19][32],e,g)?[0,1,a]:[0,0,a]}var
v=p(f[17][87],t,1,0,n),y=a(f[17][9],v),I=0;function
J(k,h,s){var
l=k[2],m=k[1],n=a(x[1][1][1],h),o=n?n[1]:a(j[1][6],E8),t=b(K[5],o,E6),e=b(au[26],t,m),q=b(j[1][10][4],e,m),u=b(K[5],o,E7),d=b(au[26],u,q),v=b(j[1][10][4],d,q);if(s)var
w=0,y=0,z=function(f,e,b){var
h=b[3],j=b[2],k=b[1],l=[0,a(c[10],d),0];return[0,[0,k,j,g(c[i][3],l,f,h)],e]},A=p(f[17][87],z,y,w,l),r=a(f[17][9],A);else
var
r=[0,[0,e,d,a(be,h)],l];return[0,[0,v,r],[0,[0,e,1],[0,d,1]]]}var
o=p(f[17][93],J,[0,j[1][10][1],0],n,y),L=o[1][2],q=a(f[17][cR],o[2]),M=q[2],N=q[1];function
r(a){function
c(a){return b(W[3],0,a)}return[1,[0,u,A+1|0],w,b(f[17][14],c,a)]}var
O=r(M),P=[0,b(W[3],0,O),0],Q=r(N),R=[0,b(W[3],0,Q),P],S=b(f[18],_,R),e=a(f[17][9],L);if(e)var
h=e[1],T=e[2],U=h[3],V=h[2],Z=h[1],$=function(f,e){var
d=e[3],j=e[2],g=e[1],k=f[3],l=f[2],h=a(c[19],[0,[0,g],d,f[1]]),m=a(c[10],j),n=b(c[i][5],m,k),o=[0,Y,[0,d,h,a(c[10],j),n]],p=a(c[21],o),q=a(c[10],g),r=b(c[i][5],q,l),s=[0,Y,[0,d,h,a(c[10],g),r]],t=a(c[21],s);return[0,a(c[21],[0,aG,[0,d,h]]),t,p]},aa=a(c[10],V),ab=[0,U,a(c[10],Z),aa],k=g(f[17][15],$,ab,T),s=a(c[21],[0,aH,[0,k[1],k[2],k[3]]]);else
var
s=av;return[0,I,S,[0,[0,[2,s],E9]]]}var
aL=b(f[19][16],aJ,aF),aM=a(f[19][11],aL);function
aa(b){return[0,a(j[1][6],b),1]}var
aO=aa(E_),aP=[0,b(W[3],0,aO),0],aQ=aa(E$),aR=[0,b(W[3],0,aQ),aP],aS=[0,[0,0,b(f[18],_,aR),[0,[0,[2,aw],Fa]]],0],aT=b(f[18],aM,aS),D=a(ap[41],[2,u]),y=[0,d],aU=b(K[6],Fb,D),A=[0,Fc,0,[0,T,0,0,0],a7[1],0],aV=b(c[37],l,k),ab=[0,a(c4[3],Fd),aU,aV,k,l,0,0,0],aW=de(Fe,h,y,ab,A,aT,0,S,0,l);function
aY(aH,aG,aC){b(c[37],l,k);a(c4[3],Ff);var
U=aC[1];if(1===U[0]){var
V=U[1];b(v[53],[0,V],d3[3]);var
aD=a(v[2],0),aE=a(o[17],aD),W=b(P[9],aE,[2,u]),X=b(c[77],W[1],W[2]),aF=a(v[2],0),Y=a(v[28],X[1])[2],Z=b(c[2][2],e,X[2]),_=b(aA[25],Z,Y[2]),$=c2(e,b(f[17][69],c[bx],_)),E=a(f[17][1],$),aa=aX(0,E),ab=b(K[6],E0,D),F=s(bC),ae=b(K[6],E1,D),af=t===F?bC[1]:q===F?a(r[2],bC):bC,n=a(ac[8],af),G=ba(e,[1,V]),H=G[2],J=ba(G[1],n[2]),d=J[1],ah=b(c[77],d,J[2])[2],ai=a(c[21],[0,H,aa]),aj=z(ad[2],0,0,aF,d,H),M=g(c[92],d,E,aj),w=M[1],N=b(c[3],d,M[2]);if(6===N[0]){var
O=ew(d,[0,n,ah],[0,N[2],[0,ai,0]])[1],ak=a(v[2],0),h=b(c[B],w,ak),ao=a(L[7],O),ap=z(ad[2],0,0,h,d,ao),j=d,y=a(L[7],O),x=ap;for(;;){var
A=b(c[3],j,x);if(6===A[0]){var
al=A[3],Q=d6(P[4],0,0,0,0,0,0,0,h,j,A[2]),R=Q[2],am=Q[1],an=b(c[i][5],R,al),j=am,y=a(c[21],[0,y,[0,R]]),x=an;continue}var
S=b(c[38],y,w),aq=b(c[37],x,w),m=p(ag[2],0,h,j,S)[1],ar=function(e,b,d){var
c=p(ac[5],n,dE,1,b);return a(ac[6],c)},as=g(c[5],0,m,aq),at=g(c[5],E3,m,S),C=ax(aZ[5],h,ab,m,0,0,at,as),au=C[4],av=C[3],aw=C[1],ay=[0,a(bK[1],ar)],az=[0,iN(0)],aB=a(o[bz],m);bY(aZ[7],ae,[0,av],au,aB,0,0,[0,[0,2,T,0]],az,0,ay,0,aw);return 0}}throw[0,I,E2]}throw[0,I,Fg]}var
a0=[0,db(h,y,A[3],ab,S,aW,0),0];return g0(h,y,Fh,0,A[3],0,a0,aY)}ca([0,Fi,function(a,b){return b$(lS,a,b)}]);aB(1281,[0,lS],"Noconf_hom");function
lT(i,h,g){function
d(d){var
l=a(k[67][5],d),e=b(c[83],l,g),f=e[1],m=e[2],n=b(E[42][21],d,f),o=a(aY[12],m),p=[0,a(c[9],1),o],q=a(c[21],p),r=[0,[0,a(j[1][6],Fj)],n,q],s=a(c[19],r),t=z(u[bw],0,[0,h],s,0,dR[5]),v=z(u[bw],0,[0,i],f,0,dR[5]);return b(k[18],v,t)}return a(k[67][9],d)}function
lU(c){if(1===c[0]){var
d=a(j[17][9],c[1]),e=a(j[6][5],d);return b(Fm[7],[0,Fl,[0,e,0]],dR[6])}throw[0,I,Fk]}function
lV(o){function
d(h){var
y=a(k[67][4],h),e=a(k[67][5],h),A=a(k[67][2],h),B=ad[2];function
C(a){return g(B,0,0,a)}var
D=g(E[42][1],C,h,o),q=j[60],l=[0,e],f=y,m=A,r=D;for(;;){var
i=b(c[3],e,m),d=b(c[3],e,r);if(6===i[0]){var
w=i[2],Q=i[3],R=i[1];switch(d[0]){case
6:var
S=d[3],x=z(dV[6],f,[0,q],l[1],w,d[2]);if(x){l[1]=x[1];var
T=ah([0,R,0,w]),f=b(c[aR],T,f),m=Q,r=S;continue}return bf(Fp);case
9:var
n=0;break;default:var
n=1}}else
var
n=0;if(!n)if(9===d[0]){var
s=d[1],F=d[2];if(b(c[47],e,s)){var
G=b(c[76],e,s),H=l[1],t=p(ja,f,H,G,a(aY[11],F)),u=t[2],I=t[1],J=u[2],K=function(a){return 0},L=b(a8[53],K,J),v=d5(dV[12],q,f,I,u,L,m),M=v[1];if(v[2]){var
N=function(a){return[0,a,o]},O=b(cF[2],0,N),P=a(k[65][1],M);return b(k[72][2],P,O)}return bf(Fo)}}return bf(Fn)}}return a(k[67][9],d)}aB(1283,[0,lT,lU,lV],mK);a(a1[10],as);var
fr=k[71][1],Fq=Y[8],Fr=0;function
Fs(c,b,a,d){return lT(c,b,a)}var
Fu=[1,[5,a(y[16],Y[11])],Ft,0],Fw=[1,[5,a(y[16],Y[7])],Fv,Fu],Fz=[0,[0,[0,Fy,[1,[5,a(y[16],Y[7])],Fx,Fw]],Fs],Fr];z(T[10][8],as,FA,0,0,Fz);var
FB=0;function
FC(a,b){return lU(a)}var
FF=[0,[0,[0,FE,[1,[5,a(y[16],Y[18])],FD,0]],FC],FB];z(T[10][8],as,FG,0,0,FF);var
FH=0;function
FI(c,a,d){return b(c8[5],c,a)}var
FK=[1,[5,a(y[16],Y[7])],FJ,0],FN=[0,[0,[0,FM,[1,[5,a(y[16],Y[8])],FL,FK]],FI],FH];z(T[10][8],as,FO,0,0,FN);var
FP=0;function
FQ(b,c){return a(c8[4],b)}var
FU=[0,[0,[0,FT,[0,FS,[1,[5,a(y[16],Y[8])],FR,0]]],FQ],FP];z(T[10][8],as,FV,0,0,FU);var
FW=0,FY=[0,[0,FX,function(a){return c8[2]}],FW];function
FZ(b,c){return a(c8[1],b)}var
F2=[0,[0,[0,F1,[1,[5,a(y[16],Y[8])],F0,0]],FZ],FY];z(T[10][8],as,F3,0,0,F2);var
F4=0;function
F5(a,b){return jV(a)}var
F8=[0,[0,[0,F7,[1,[5,a(y[16],Y[7])],F6,0]],F5],F4];z(T[10][8],as,F9,0,0,F8);var
F_=0;function
F$(d,c,b,a,e){return p(c8[3],d,c,b,a)}var
Gb=[1,[5,a(y[16],Y[7])],Ga,0],Gd=[1,[5,a(y[16],Y[7])],Gc,Gb],Gf=[1,[5,a(y[16],Y[11])],Ge,Gd],Gi=[0,[0,[0,Gh,[1,[5,a(y[16],Y[11])],Gg,Gf]],F$],F_];z(T[10][8],as,Gj,0,0,Gi);var
Gk=0;function
Gl(a,e){var
c=0;function
d(b){return hd(c,a,b)}return b(k[71][1],0,d)}var
Gp=[0,[0,[0,Go,[0,Gn,[1,[5,a(y[16],Y[11])],Gm,0]]],Gl],Gk];z(T[10][8],as,Gq,0,0,Gp);var
Gr=0;function
Gs(a,d){function
c(b){return hd(Gt,a,b)}return b(k[71][1],0,c)}var
Gy=[0,[0,[0,Gx,[0,Gw,[0,Gv,[1,[5,a(y[16],Y[11])],Gu,0]]]],Gs],Gr];z(T[10][8],as,Gz,0,0,Gy);var
GA=0;function
GB(a,e){var
c=0;function
d(b){return k9(c,a,b)}return b(k[71][1],0,d)}var
GE=[0,[0,[0,GD,[1,[5,a(y[16],Y[11])],GC,0]],GB],GA];z(T[10][8],as,GF,0,0,GE);var
GG=0;function
GH(c,f){function
d(b){if(k5(b,c))return a(bP[9],b);var
d=a(e[3],GI);return g(bP[41],0,d,b)}return b(k[71][1],0,d)}var
GL=[0,[0,[0,GK,[1,[5,a(y[16],Y[8])],GJ,0]],GH],GG];z(T[10][8],as,GM,0,0,GL);var
GN=0;function
GO(d,c,a){var
e=F(b(T[13][24],a,c)),f=F(b(T[13][24],a,d));return b(fr,0,function(a){return lR(f,e,a)})}var
GQ=[1,[5,a(y[16],T[2][8])],GP,0],GT=[0,[0,[0,GS,[1,[5,a(y[16],T[2][8])],GR,GQ]],GO],GN];z(T[10][8],as,GU,0,0,GT);var
GV=0;function
GW(e,d,f){return b(fr,0,hh(d,a(iJ,b(l[17],c[aE][1],e))))}var
GY=[1,[5,a(y[16],Y[20])],GX,0],G1=[0,[0,[0,G0,[1,[2,[5,a(y[16],Y[11])]],GZ,GY]],GW],GV];function
G2(c,a,d){return b(fr,0,hh(a,c))}var
G4=[1,[5,a(y[16],Y[20])],G3,0],G7=[0,[0,[0,G6,[1,[0,[5,a(y[16],Y[17])]],G5,G4]],G2],G1];z(T[10][8],as,G8,0,0,G7);var
bV=a(y[2],G9);function
G_(b,a){return[0,b,a]}b(bk[9],bV,G_);function
G$(b,a){return a}b(bk[10],bV,G$);function
Ha(g,c){var
d=a(y[6],bV),e=a(ab[3],d),f=b(ab[1][8],e,c);return a(bW[1],f)}b(ab[7],bV,Ha);b(ab[4],bV,0);var
Hb=a(y[4],bV),fs=g(h[16],h[13],Hc,Hb),Hd=0,He=0;function
Hf(b,a){return Hg}var
Hi=[0,[0,[0,0,[0,a(cK[10],Hh)]],Hf],He];function
Hj(b,a){return Hk}var
Hm=[0,[0,[0,0,[0,a(cK[10],Hl)]],Hj],Hi];function
Hn(b,a){return Ho}var
Hq=[0,[0,[0,0,[0,a(cK[10],Hp)]],Hn],Hm];function
Hr(b,a){return Hs}var
Hu=[0,0,[0,[0,0,0,[0,[0,[0,0,[0,a(cK[10],Ht)]],Hr],Hq]],Hd]];g(h[21],fs,0,Hu);p(T[5][1],bV,eB,eB,eB);var
Hv=[0,fs,0];function
Hw(c){var
d=c[2],e=a(y[4],bV);return[0,b(y[7],e,d)]}g(T[10][5],Hx,Hw,Hv);var
bv=a(y[2],Hy);function
Hz(b,a){return[0,b,a]}b(bk[9],bv,Hz);function
HA(b,a){return a}b(bk[10],bv,HA);function
HB(g,c){var
d=a(y[6],bv),e=a(ab[3],d),f=b(ab[1][8],e,c);return a(bW[1],f)}b(ab[7],bv,HB);b(ab[4],bv,0);var
HC=a(y[4],bv),hs=g(h[16],h[13],HD,HC),HE=0,HF=0;function
HG(d,a,c,b){return a}var
HI=[0,a(cK[10],HH)],HK=[0,[0,[0,[0,[0,0,[0,a(cK[10],HJ)]],[1,[6,fs]]],HI],HG],HF],HL=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],HK]],HE]];g(h[21],hs,0,HL);p(T[5][1],bv,eC,eC,eC);var
HM=[0,hs,0];function
HN(c){var
d=c[2],e=a(y[4],bv);return[0,b(y[7],e,d)]}g(T[10][5],HO,HN,HM);function
ft(e,d,c,b){return a(j[1][9],b[2])}var
bX=a(y[2],HP);function
HQ(b,a){return[0,b,a]}b(bk[9],bX,HQ);function
HR(b,a){return a}b(bk[10],bX,HR);function
HS(g,c){var
d=a(y[6],bX),e=a(ab[3],d),f=b(ab[1][8],e,c);return a(bW[1],f)}b(ab[7],bX,HS);b(ab[4],bX,0);var
HT=a(y[4],bX),fu=g(h[16],h[13],HU,HT),HV=0,HW=0;function
HX(b,a){return[0,a,b]}g(h[21],fu,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,h[17][2]]],HX],HW]],HV]]);p(T[5][1],bX,ft,ft,ft);var
HY=[0,fu,0];function
HZ(c){var
d=c[2],e=a(y[4],bX);return[0,b(y[7],e,d)]}g(T[10][5],H0,HZ,HY);var
H1=0;function
lW(f,d,c,b){return a(e[7],0)}function
lX(f,d,c,b){return a(e[7],0)}function
lY(f,d,c,b){return a(e[7],0)}var
fv=a(y[3],H2),H3=a(y[4],fv),ht=g(h[16],h[12],H4,H3),H5=b(ab[4],fv,0);p(T[5][1],fv,lW,lX,lY);var
fw=a(y[3],H6),H7=b(ab[4],fw,0);function
lZ(f,d,c,b){return a(e[7],0)}function
l0(f,d,c,b){return a(e[7],0)}function
l1(f,d,c,b){return a(e[7],0)}var
H8=a(y[4],fw),fx=g(h[16],l2[1],H9,H8);p(T[5][1],fw,lZ,l0,l1);var
fy=a(y[3],H_),H$=b(ab[4],fy,0);function
l3(f,d,c,b){return a(e[7],0)}function
l4(f,d,c,b){return a(e[7],0)}function
l5(f,d,c,b){return a(e[7],0)}var
Ia=a(y[4],fy),d4=g(h[16],h[13],Ib,Ia);p(T[5][1],fy,l3,l4,l5);var
dh=a(y[3],Ic),Id=b(ab[4],dh,0);function
l6(f,d,c,b){return a(e[7],0)}function
l7(f,d,c,b){return a(e[7],0)}function
l8(f,d,c,b){return a(e[7],0)}var
Ie=a(y[4],dh),l9=g(h[16],l2[1],If,Ie);p(T[5][1],dh,l6,l7,l8);function
Ig(b,a){return a}function
l_(e,d){var
c=a(y[2],d);b(ab[4],c,e);return c}var
Ii=a(y[6],Y[4]),fz=l_([0,a(ab[3],Ii)],Ih);function
l$(b,a){return[0,b,a]}function
ma(b,a){return a}function
mb(c,b){return a(bW[1],b)}function
mc(c,d){function
e(f,e){function
g(d){var
e=a(y[6],c),f=a(ab[3],e),g=b(ab[1][8],f,d);return a(bW[1],g)}var
h=b(d,f,e);return b(bW[2],h,g)}return b(ab[7],c,e)}function
md(a){b(bk[9],a,l$);b(bk[10],a,ma);return mc(a,mb)}md(fz);var
Ij=a(y[4],fz),me=g(h[16],h[13],Ik,Ij);a(cK[1],Il);var
a2=h[1][5][1],hu=a(a2,Im),hv=a(a2,In),mf=a(a2,Io),mg=a(a2,Ip),mh=a(a2,Iq),cL=a(a2,Ir),mi=a(a2,Is),mj=a(a2,It),mk=a(a2,Iu),ml=a(a2,Iv),mm=a(a2,Iw),mn=a(a2,Ix),hw=a(a2,Iy),hx=a(a2,Iz),IA=0,IB=0;function
IC(a,b){return a}var
IE=a(h[1][17],ID),IF=[0,b(h[1][21],h[1][20],IE),IC],IG=[0,[0,0,0,[0,a(h[1][23],IF),IB]],IA];g(h[1][26],me,0,IG);var
IH=0,II=0;function
IJ(a,b){return a}var
IK=a(h[1][7],h[18][16]),IL=[0,b(h[1][21],h[1][20],IK),IJ],IM=[0,[0,0,0,[0,a(h[1][23],IL),II]],IH];g(h[1][26],ht,0,IM);var
IN=0,IO=0;function
IP(a,b){return a}var
IR=a(h[1][17],IQ),IS=a(h[1][7],hv),IT=g(h[1][10],IS,IR,0),IU=[0,b(h[1][21],h[1][20],IT),IP],IV=[0,[0,0,0,[0,a(h[1][23],IU),IO]],IN];g(h[1][26],fx,0,IV);var
IW=0,IX=0;function
IY(d,a,c,b){return a}var
I0=a(h[1][17],IZ),I2=a(h[1][17],I1),I3=a(h[1][7],h[18][3]),I4=g(h[1][10],I3,I2,0),I6=a(h[1][17],I5),I7=b(h[1][21],h[1][20],I6),I8=b(h[1][21],I7,I4),I9=[0,b(h[1][21],I8,I0),IY],I_=[0,[0,0,0,[0,a(h[1][23],I9),IX]],IW];g(h[1][26],d4,0,I_);var
I$=0,Ja=0;function
Jb(i,c,h,g,f){var
d=a(y[4],eA),e=[12,0,0,[0,b(y[7],d,c)]];return b(aL[1],0,e)}var
Jd=a(h[1][17],Jc),Jf=a(h[1][17],Je),Jg=a(h[1][7],hv),Jh=g(h[1][12],Jg,Jf,0),Jj=a(h[1][17],Ji),Jl=a(h[1][17],Jk),Jm=b(h[1][21],h[1][20],Jl),Jn=b(h[1][21],Jm,Jj),Jo=b(h[1][21],Jn,Jh),Jp=[0,b(h[1][21],Jo,Jd),Jb],Jq=[0,[0,0,0,[0,a(h[1][23],Jp),Ja]],I$];g(h[1][26],h[18][5],0,Jq);var
Jr=0,Js=0;function
Jt(c,b){return[0,a(h[31],b),c]}var
Ju=a(h[1][7],h[18][6]),Jv=[0,b(h[1][21],h[1][20],Ju),Jt],Jw=[0,[0,0,0,[0,a(h[1][23],Jv),Js]],Jr];g(h[1][26],hu,0,Jw);var
Jx=0,Jy=0;function
Jz(b,a,d,c){return[0,[1,a],b]}var
JA=a(h[1][7],hw),JC=a(h[1][17],JB),JD=a(h[1][7],h[18][3]),JE=g(h[1][12],JD,JC,0),JG=a(h[1][17],JF),JH=b(h[1][21],h[1][20],JG),JI=b(h[1][21],JH,JE),JJ=[0,b(h[1][21],JI,JA),Jz],JK=[0,a(h[1][23],JJ),Jy];function
JL(b,a,c){return[0,[0,a],b]}var
JM=a(h[1][7],hw),JN=a(h[1][7],mf),JO=b(h[1][21],h[1][20],JN),JP=[0,b(h[1][21],JO,JM),JL],JQ=[0,[0,0,0,[0,a(h[1][23],JP),JK]],Jx];g(h[1][26],hv,0,JQ);var
JR=0,JS=0;function
JT(a,b){return a}var
JU=a(h[1][7],h[18][3]),JV=[0,b(h[1][21],h[1][20],JU),JT],JW=[0,[0,0,0,[0,a(h[1][23],JV),JS]],JR];g(h[1][26],mf,0,JW);var
JX=0,JY=0;function
JZ(a,b){return a}var
J1=a(h[1][17],J0),J2=a(h[1][7],h[18][3]),J3=g(h[1][12],J2,J1,0),J4=[0,b(h[1][21],h[1][20],J3),JZ],J5=[0,[0,0,0,[0,a(h[1][23],J4),JY]],JX];g(h[1][26],mg,0,J5);var
J6=0,J7=0;function
J8(b,a,e,d,c){return[0,[1,[0,a,b]]]}var
J9=a(h[1][7],h[18][1]),J_=a(h[1][13],J9),J$=a(h[1][7],h[18][1]),Kb=a(h[1][17],Ka),Kd=a(h[1][17],Kc),Ke=b(h[1][21],h[1][20],Kd),Kf=b(h[1][21],Ke,Kb),Kg=b(h[1][21],Kf,J$),Kh=[0,b(h[1][21],Kg,J_),J8],Ki=[0,a(h[1][23],Kh),J7];function
Kj(a,d,c,b){return[0,[0,a]]}var
Kk=a(h[1][7],hu),Kl=a(h[1][13],Kk),Kn=a(h[1][17],Km),Kp=a(h[1][17],Ko),Kq=b(h[1][21],h[1][20],Kp),Kr=b(h[1][21],Kq,Kn),Ks=[0,b(h[1][21],Kr,Kl),Kj],Kt=[0,a(h[1][23],Ks),Ki];function
Ku(a){return 0}var
Kv=[0,[0,0,0,[0,a(h[1][23],[0,h[1][20],Ku]),Kt]],J6];g(h[1][26],mh,0,Kv);var
Kw=0,Kx=0;function
Ky(f,i,e,d,h,c,b,g,a){return[0,[0,b,a,c,[0,d],e],f]}var
Kz=a(h[1][7],hx),KB=a(h[1][17],KA),KC=a(h[1][7],mh),KD=a(h[1][7],h[18][3]),KF=a(h[1][17],KE),KG=a(h[1][7],ht),KH=a(h[1][7],fu),KI=b(h[1][21],h[1][20],KH),KJ=b(h[1][21],KI,KG),KK=b(h[1][21],KJ,KF),KL=b(h[1][21],KK,KD),KM=b(h[1][21],KL,KC),KN=b(h[1][21],KM,KB),KO=[0,b(h[1][21],KN,Kz),Ky],KP=[0,[0,0,0,[0,a(h[1][23],KO),Kx]],Kw];g(h[1][26],cL,0,KP);var
KQ=0,KR=0;function
KS(c,b,e,a,d){return[1,[0,a,b,c]]}var
KT=0;function
KU(a,c,b){return a}var
KW=a(h[1][17],KV),KY=a(h[1][17],KX),KZ=b(h[1][21],h[1][20],KY),K0=[0,b(h[1][21],KZ,KW),KU],K1=[0,a(h[1][23],K0),KT],K2=a(h[1][18],K1),K3=a(h[1][13],K2),K4=a(h[1][7],h[18][1]),K6=a(h[1][17],K5),K7=a(h[1][7],h[17][22]),K8=b(h[1][21],h[1][20],K7),K9=b(h[1][21],K8,K6),K_=b(h[1][21],K9,K4),K$=[0,b(h[1][21],K_,K3),KS],La=[0,a(h[1][23],K$),KR];function
Lb(b,c){return[0,a(b,Lc)]}var
Ld=a(h[1][7],cL),Le=[0,b(h[1][21],h[1][20],Ld),Lb],Lf=[0,[0,0,0,[0,a(h[1][23],Le),La]],KQ];g(h[1][26],mi,0,Lf);var
Lg=0,Lh=0;function
Li(a,c,b){return a}var
Lj=a(h[1][7],mi),Ll=a(h[1][17],Lk),Lm=b(h[1][21],h[1][20],Ll),Ln=[0,b(h[1][21],Lm,Lj),Li],Lo=[0,a(h[1][23],Ln),Lh];function
Lp(b,d,c){return[0,a(b,Lq)]}var
Lr=a(h[1][7],cL),Lt=a(h[1][17],Ls),Lu=b(h[1][21],h[1][20],Lt),Lv=[0,b(h[1][21],Lu,Lr),Lp],Lw=[0,a(h[1][23],Lv),Lo];function
Lx(b,c){return[0,a(b,0)]}var
Ly=a(h[1][7],cL),Lz=[0,b(h[1][21],h[1][20],Ly),Lx],LA=[0,[0,0,0,[0,a(h[1][23],Lz),Lw]],Lg];g(h[1][26],mj,0,LA);var
LB=0,LC=0;function
LD(a,c){function
b(a){if(a){var
c=a[1];if(0===c[0]){var
f=c[1],d=b(a[2]);return[0,[0,f,d[1]],d[2]]}var
g=c[1],e=b(a[2]);return[0,e[1],[0,g,e[2]]]}return LE}return b(a)}var
LF=a(h[1][7],mj),LG=a(h[1][9],LF),LH=[0,b(h[1][21],h[1][20],LG),LD],LI=[0,[0,0,0,[0,a(h[1][23],LH),LC]],LB];g(h[1][26],mk,0,LI);var
LJ=0,LK=0;function
LL(c,b,e,a,d){return[1,[0,a,b,c]]}var
LM=0;function
LN(a,c,b){return a}var
LP=a(h[1][17],LO),LR=a(h[1][17],LQ),LS=b(h[1][21],h[1][20],LR),LT=[0,b(h[1][21],LS,LP),LN],LU=[0,a(h[1][23],LT),LM],LV=a(h[1][18],LU),LW=a(h[1][13],LV),LX=a(h[1][7],h[18][1]),LZ=a(h[1][17],LY),L0=a(h[1][7],h[17][22]),L1=b(h[1][21],h[1][20],L0),L2=b(h[1][21],L1,LZ),L3=b(h[1][21],L2,LX),L4=[0,b(h[1][21],L3,LW),LL],L5=[0,a(h[1][23],L4),LK];function
L6(b,c){return[0,a(b,L7)]}var
L8=a(h[1][7],cL),L9=[0,b(h[1][21],h[1][20],L8),L6],L_=[0,[0,0,0,[0,a(h[1][23],L9),L5]],LJ];g(h[1][26],ml,0,L_);var
L$=0,Ma=0;function
Mb(a,c,b){return a}var
Mc=a(h[1][7],ml),Me=a(h[1][17],Md),Mf=b(h[1][21],h[1][20],Me),Mg=[0,b(h[1][21],Mf,Mc),Mb],Mh=[0,a(h[1][23],Mg),Ma];function
Mi(b,d,c){return[0,a(b,Mj)]}var
Mk=a(h[1][7],cL),Mm=a(h[1][17],Ml),Mn=b(h[1][21],h[1][20],Mm),Mo=[0,b(h[1][21],Mn,Mk),Mi],Mp=[0,[0,0,0,[0,a(h[1][23],Mo),Mh]],L$];g(h[1][26],mm,0,Mp);var
Mq=0,Mr=0;function
Ms(a,c){function
b(a){if(a){var
c=a[1];if(0===c[0]){var
f=c[1],d=b(a[2]);return[0,[0,f,d[1]],d[2]]}var
g=c[1],e=b(a[2]);return[0,e[1],[0,g,e[2]]]}return Mt}return b(a)}var
Mu=a(h[1][7],mm),Mv=a(h[1][9],Mu),Mw=[0,b(h[1][21],h[1][20],Mv),Ms],Mx=[0,[0,0,0,[0,a(h[1][23],Mw),Mr]],Mq];g(h[1][26],mn,0,Mx);var
My=0,Mz=0;function
MA(a,c,b){return[0,[1,a]]}var
MB=a(h[1][7],hu),MD=a(h[1][17],MC),ME=b(h[1][21],h[1][20],MD),MF=[0,b(h[1][21],ME,MB),MA],MG=[0,a(h[1][23],MF),Mz];function
MH(b,a,d,c){return[0,[0,[0,a],b]]}var
MI=a(h[1][7],mn),MJ=a(h[1][7],h[18][3]),MK=0;function
ML(b,a){return 0}var
MN=a(h[1][17],MM),MO=[0,b(h[1][21],h[1][20],MN),ML],MP=[0,a(h[1][23],MO),MK];function
MQ(b,a){return 0}var
MS=a(h[1][17],MR),MT=[0,b(h[1][21],h[1][20],MS),MQ],MU=[0,a(h[1][23],MT),MP],MV=a(h[1][18],MU),MW=b(h[1][21],h[1][20],MV),MX=b(h[1][21],MW,MJ),MY=[0,b(h[1][21],MX,MI),MH],MZ=[0,a(h[1][23],MY),MG];function
M0(b,e,a,d,c){return[0,[2,a,b]]}var
M1=a(h[1][7],hx),M2=0;function
M3(b,a){return M4}var
M6=a(h[1][17],M5),M7=[0,b(h[1][21],h[1][20],M6),M3],M8=[0,a(h[1][23],M7),M2];function
M9(b,a){return M_}var
Na=a(h[1][17],M$),Nb=[0,b(h[1][21],h[1][20],Na),M9],Nc=[0,a(h[1][23],Nb),M8],Nd=a(h[1][18],Nc),Ne=a(h[1][7],mg),Nf=0;function
Ng(b,a){return Nh}var
Nj=a(h[1][17],Ni),Nk=[0,b(h[1][21],h[1][20],Nj),Ng],Nl=[0,a(h[1][23],Nk),Nf],Nm=a(h[1][18],Nl),Nn=b(h[1][21],h[1][20],Nm),No=b(h[1][21],Nn,Ne),Np=b(h[1][21],No,Nd),Nq=[0,b(h[1][21],Np,M1),M0],Nr=[0,a(h[1][23],Nq),MZ];function
Ns(a){return 0}var
Nt=[0,[0,0,0,[0,a(h[1][23],[0,h[1][20],Ns]),Nr]],My];g(h[1][26],hw,0,Nt);var
Nu=0,Nv=0;function
Nw(d,a,c,b){return a}var
Ny=a(h[1][17],Nx),Nz=a(h[1][7],fx),NB=a(h[1][17],NA),NC=b(h[1][21],h[1][20],NB),ND=b(h[1][21],NC,Nz),NE=[0,b(h[1][21],ND,Ny),Nw],NF=[0,a(h[1][23],NE),Nv];function
NG(a,b){return a}var
NH=a(h[1][7],fx),NI=[0,b(h[1][21],h[1][20],NH),NG],NJ=[0,[0,0,0,[0,a(h[1][23],NI),NF]],Nu];g(h[1][26],hx,0,NJ);var
NK=0,NL=0;function
NM(b,c,f){var
d=b[2],e=b[1];return[0,[0,a(c,0),e],d]}var
NN=a(h[1][7],mk),NO=a(h[1][7],cL),NP=b(h[1][21],h[1][20],NO),NQ=[0,b(h[1][21],NP,NN),NM],NR=[0,[0,0,0,[0,a(h[1][23],NQ),NL]],NK];g(h[1][26],l9,0,NR);function
mo(a){return NS}var
NT=0,NU=0;function
NV(d,b,g,c){var
e=b[2],f=b[1];hr(a(ef[29],0),0,d,f,e);return c}var
NX=[1,NW,[5,a(y[16],dh)],0],N0=[0,[0,0,[0,NZ,[1,NY,[5,a(y[16],bv)],NX]],NV,NU],NT],N1=0,N2=[0,function(a){return hy[4]}];p(fA[10],N3,N2,N1,N0);var
N4=0,N5=0;function
N6(d,b,g,c){var
e=b[2],f=b[1];hr(a(ef[29],0),1,d,f,e);return c}var
N8=[1,N7,[5,a(y[16],dh)],0],N$=[0,[0,0,[0,N_,[1,N9,[5,a(y[16],bv)],N8]],N6,N5],N4];p(fA[10],Oa,[0,mo],0,N$);function
mp(c,b,a){return dJ(c,b,0,a[1])}function
mq(g,c,e){var
d=a(bP[2],c),h=b(Ob[3][1],d,c[1]),i=[0,a(j[1][11][28],g[1])];function
k(a){return mp(h,i,a)}return[0,d,b(f[17][69],k,e)]}function
mr(d,c){var
e=a(T[9][7],d);return b(f[17][69],e,c)}function
ms(b,a){return a}function
mt(e,d,c,b){return bF(a(v[2],0),b)}function
mu(c,h,f,b){function
d(b){return a(e[3],Oc)}return g(e[39],d,c,b)}function
mv(c,i,h,b){function
d(b){return a(c,b)}function
f(b){return a(e[3],Od)}return g(e[39],f,d,b)}var
bL=a(y[2],Oe);function
Of(a,b){return[0,a,mr(a,b)]}b(bk[9],bL,Of);b(bk[10],bL,ms);function
Og(e,d){function
c(f){function
g(a){return mq(e,a,d)}var
c=b(E[42][3],g,f),h=c[2],i=c[1],j=a(y[6],bL),l=a(ab[3],j),m=b(ab[1][8],l,h),n=a(bW[1],m),o=a(k[65][1],i);return b(k[18],o,n)}return a(bW[6],c)}b(ab[7],bL,Og);b(ab[4],bL,0);b(h[14],bL,d4);p(T[5][1],bL,mu,mv,mt);var
Oh=[0,d4,0];function
Oi(c){var
d=c[2],e=a(y[4],bL);return[0,b(y[7],e,d)]}g(T[10][5],Oj,Oi,Oh);var
Ok=0;function
Ol(c,b,d){return d1([0,b],[0,a(c4[3],Om),c])}var
Op=[0,Oo,[1,[5,a(y[16],bL)],On,0]],Ot=[0,[0,[0,Os,[0,Or,[1,[5,a(y[16],Y[7])],Oq,Op]]],Ol],Ok];function
Ou(b,c){return d1(0,[0,a(c4[3],Ov),b])}var
Oz=[0,[0,[0,Oy,[0,Ox,[1,[5,a(y[16],Y[7])],Ow,0]]],Ou],Ot];z(T[10][8],as,OA,0,0,Oz);var
OB=0;function
OC(f,g){function
d(g){var
h=a(k[67][5],g),d=b(c[3],h,f);if(1===d[0])if(a(A[a_],d[1]))return a(k[16],0);var
i=a(e[3],OD);return b(m[66][4],0,i)}return a(k[67][9],d)}var
OG=[0,[0,[0,OF,[1,[5,a(y[16],Y[11])],OE,0]],OC],OB];z(T[10][8],as,OH,0,0,OG);var
OI=0;function
OJ(a,b){return lV(a)}var
OM=[0,[0,[0,OL,[1,[5,a(y[16],Y[13])],OK,0]],OJ],OI];z(T[10][8],as,ON,0,0,OM);var
OO=0;function
OP(a,e){var
c=1;function
d(b){return he(c,a,b)}return b(k[71][1],0,d)}var
OS=[0,[0,[0,OR,[1,[5,a(y[16],Y[7])],OQ,0]],OP],OO];function
OT(a,e){var
c=0;function
d(b){return he(c,a,b)}return b(k[71][1],0,d)}var
OW=[0,[0,[0,OV,[1,[5,a(y[16],Y[7])],OU,0]],OT],OS];z(T[10][8],as,OX,0,0,OW);var
OY=0;function
OZ(b,a,c){return i0(b,a)}var
O1=[1,[5,a(y[16],Y[11])],O0,0],O4=[0,[0,[0,O3,[1,[5,a(y[16],Y[7])],O2,O1]],OZ],OY];z(T[10][8],as,O5,0,0,O4);var
O6=0,O7=0;function
O8(e,d,k,c){var
g=a(ef[29],0);function
h(a){var
c=b(ee[3],0,a);return[0,a[2],c]}var
i=b(f[17][69],h,d);je(g,b(f[17][69],j[1][8],e),i);return c}var
O$=[0,O_,[1,O9,[2,[5,a(y[16],Y[19])]],0]],Pc=[0,[0,0,[0,Pb,[1,Pa,[0,[5,a(y[16],Y[7])]],O$]],O8,O7],O6],Pd=0,Pe=[0,function(a){return hy[4]}];p(fA[10],Pf,Pe,Pd,Pc);var
fB=a(y[3],Pg),Ph=b(ab[4],fB,0);function
mw(c,b,a){return dX}function
mx(c,b,a){return dX}function
my(c,b,a){return dX}var
Pi=a(y[4],fB),di=g(h[16],h[13],Pj,Pi);p(T[5][1],fB,mw,mx,my);var
fC=h[1][5][1],mz=a(fC,Pk),mA=a(fC,Pl),mB=a(fC,Pm),mC=a(fC,Pn),Po=0,Pp=0;function
Pq(a,b){return a}var
Pr=a(h[1][7],mz),Ps=a(h[1][11],Pr),Pt=[0,b(h[1][21],h[1][20],Ps),Pq],Pu=[0,[0,0,0,[0,a(h[1][23],Pt),Pp]],Po];g(h[1][26],di,0,Pu);var
Pv=0,Pw=0;function
Px(c,b){return[0,[0,a(h[31],b)],c]}var
Py=a(h[1][7],mA),Pz=[0,b(h[1][21],h[1][20],Py),Px],PA=[0,[0,0,0,[0,a(h[1][23],Pz),Pw]],Pv];g(h[1][26],mz,0,PA);var
PB=0,PC=0;function
PD(a,b){return[0,a]}var
PE=a(h[1][7],mB),PF=[0,b(h[1][21],h[1][20],PE),PD],PG=[0,a(h[1][23],PF),PC];function
PH(b,a){return 0}var
PJ=a(h[1][17],PI),PK=[0,b(h[1][21],h[1][20],PJ),PH],PL=[0,a(h[1][23],PK),PG];function
PM(b,a){return 1}var
PO=a(h[1][17],PN),PP=[0,b(h[1][21],h[1][20],PO),PM],PQ=[0,a(h[1][23],PP),PL];function
PR(b,a){return 2}var
PT=a(h[1][17],PS),PU=[0,b(h[1][21],h[1][20],PT),PR],PV=[0,[0,0,0,[0,a(h[1][23],PU),PQ]],PB];g(h[1][26],mA,0,PV);var
PW=0,PX=0;function
PY(b,a){return PZ}var
P1=a(h[1][17],P0),P2=[0,b(h[1][21],h[1][20],P1),PY],P3=[0,a(h[1][23],P2),PX];function
P4(b,a){return P5}var
P7=a(h[1][17],P6),P8=[0,b(h[1][21],h[1][20],P7),P4],P9=[0,a(h[1][23],P8),P3];function
P_(b,a){return 1}var
Qa=a(h[1][17],P$),Qb=[0,b(h[1][21],h[1][20],Qa),P_],Qc=[0,a(h[1][23],Qb),P9];function
Qd(b,a){return Qe}var
Qg=a(h[1][17],Qf),Qh=[0,b(h[1][21],h[1][20],Qg),Qd],Qi=[0,a(h[1][23],Qh),Qc];function
Qj(e,a,d,c,b){return[2,a]}var
Ql=a(h[1][17],Qk),Qm=a(h[1][7],di),Qo=a(h[1][17],Qn),Qq=a(h[1][17],Qp),Qr=b(h[1][21],h[1][20],Qq),Qs=b(h[1][21],Qr,Qo),Qt=b(h[1][21],Qs,Qm),Qu=[0,b(h[1][21],Qt,Ql),Qj],Qv=[0,a(h[1][23],Qu),Qi];function
Qw(a,b){return[1,a]}var
Qx=a(h[1][7],mC),Qy=[0,b(h[1][21],h[1][20],Qx),Qw],Qz=[0,[0,0,0,[0,a(h[1][23],Qy),Qv]],PW];g(h[1][26],mB,0,Qz);var
QA=0,QB=0;function
QC(b,a){return 0}var
QE=a(h[1][17],QD),QF=[0,b(h[1][21],h[1][20],QE),QC],QG=[0,a(h[1][23],QF),QB];function
QH(b,a){return 1}var
QJ=a(h[1][17],QI),QK=[0,b(h[1][21],h[1][20],QJ),QH],QL=[0,[0,0,0,[0,a(h[1][23],QK),QG]],QA];g(h[1][26],mC,0,QL);function
fD(c,b,a){return dX}var
bM=a(y[2],QM);function
QN(b,a){return[0,b,a]}b(bk[9],bM,QN);function
QO(b,a){return a}b(bk[10],bM,QO);function
QP(g,c){var
d=a(y[6],bM),e=a(ab[3],d),f=b(ab[1][8],e,c);return a(bW[1],f)}b(ab[7],bM,QP);b(ab[4],bM,0);b(h[14],bM,di);p(T[5][1],bM,fD,fD,fD);var
QQ=[0,di,0];function
QR(c){var
d=c[2],e=a(y[4],bM);return[0,b(y[7],e,d)]}g(T[10][5],QS,QR,QQ);var
QT=0,QV=[0,[0,QU,function(a){return gR(0)}],QT];function
QW(a,b){return gR(a)}var
QZ=[0,[0,[0,QY,[1,[5,a(y[16],bM)],QX,0]],QW],QV];z(T[10][8],as,Q0,0,0,QZ);var
Q1=0;function
Q2(b,a,c){return fi(b,a)}var
Q4=[1,[2,[5,a(y[16],Y[3])]],Q3,0],Q7=[0,[0,[0,Q6,[1,[2,[5,a(y[16],fz)]],Q5,Q4]],Q2],Q1];z(T[10][8],as,Q8,0,0,Q7);var
Q9=0,Q_=0;function
Q$(c,b,d,a){h8(b,c);return a}var
Rc=[0,Rb,[1,Ra,[5,a(y[16],Y[19])],0]],Rf=[0,[0,0,[0,Re,[1,Rd,[5,a(y[16],Y[19])],Rc]],Q$,Q_],Q9],Rg=0,Rh=[0,function(a){return hy[4]}];p(fA[10],Ri,Rh,Rg,Rf);aB(1293,[0,as,fr,Fq,bV,fs,bv,hs,ft,bX,fu,H1,lW,lX,lY,fv,ht,H5,fw,H7,lZ,l0,l1,fx,fy,H$,l3,l4,l5,d4,dh,Id,l6,l7,l8,l9,Ig,l_,fz,l$,ma,mb,mc,md,me,mo,mp,mq,mr,ms,mt,mu,mv,bL,d4,fB,Ph,mw,mx,my,di,fD,bM,di],"G_equations");a(a1[10],Rj);a(a1[10],Rk);a(a1[10],Rl);a(a1[10],Rm);a(a1[10],Rn);a(a1[10],Ro);a(a1[10],Rp);a(a1[10],Rq);a(a1[10],Rr);a(a1[10],Rs);a(a1[10],Rt);a(a1[10],Ru);a(a1[10],Rv);aB(1294,[0],"Equations_plugin_mod");return}
