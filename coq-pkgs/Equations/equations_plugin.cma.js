function(Kf){"use strict";var
ba=123,dK="$l",ln="valid",bt=108,lG="Subgoal: ",lm="my_preident",c2="(",ll="Depelim",fa="src/splitting.ml",Y="Init",g2="pattern",lk="_equation_",lF="<>",dQ=115,gE="|",l5="True",mu="mfix",lj="elimination",l4="This is not an equality between constructors.",fb="refine",br="Datatypes",gQ=120,bZ=144,l3="Program",li="with",l2=117,dP="=>",mt="Covering",ms="Equations.Simplify",l0="binders2",l1="mkHEq",g1=128,lh="decompose_app",b3="$id",lf="uncurry_call",lg="succeeded",lE=136,e6=248,mr="pair",gD="Subterm",le="_unfold",mq="target",lZ=167,gC="P",h=107,g0="simplify",lY="?(",mp="simp",aM="Coq",e$="product",gZ="by",mn=141,mo="Applying ",lD="sigmaI",aY=112,lX="simplification_rules",c1="Below",dO="x",lW="Schemes",ld="->",mm="deppat_equations",gB="Equations_Logic",lC=105,lV="prod",ml="$i",lc=" for ",b4="src/principles_proofs.ml",c0=110,e5="y",lb="_where_rev",c5=152,lU="_rec",gY="curry",ck="equations",la="elim_patterns",lB="Equations_common",e4="dependent",mk="is_secvar",gK="Could not generate a permutation",lT=157,mj="<->",mi="False",gP="<-",lA="-",gJ=139,lz="-!",gI="def",gO="Define_equations",c4="covering",gA=" := ",ly="rec",mh="funelim",gH=" and ",a3="Logic",H=109,gN="split",lS="NoConfusionPackage",gX="lident",mg="Syntax",cm=101,mf="prog",k$="struct",gM="equation_user_option",lR=1080,V=125,lQ="*",cl="}",k_="end_of_section",u=250,bJ="$c",k=246,k8=165,k9="move_after_deps",b2=102,c3="Extension: cannot occur",me=" Fail error ",k7="Extra_tactics",e_="JMeq",md=113,e9=122,cZ="EqDec",cj="{",I="",lP=149,mc="Sigma_types",k6=134,gW="FunctionalInduction",k5="get_signature_pack",mb="eqns_specialize_eqs",gz="equation_options",lx="solve_equations",lw="refine_ho",k4="<=",lv=169,ma="IDENT",gV="sigma",lu="pattern_call",e8="_ind",dN="Derive",lt="Eqdec",gG="src/sigma_types.ml",bs=" : ",l$=":=!",gU="src/simplify.ml",dJ=" on ",gT="$",ls="needs_generalization",gF="Type error while building context map: ",l_=153,lr="autounfold_ref",l9=171,gy="src/equations.ml",lq=127,lO="?",b1=111,lN="deppat_elim",gL=" in context ",dL="src/covering.ml",bY=" ",k3="deppat",l8="index",b0=")",lp="interp_pats",k2="interp_pat",lL="Noconf",lM=":",lJ="where ",aL="DepElim",lK="Failed with: ",N="Equations",gx="subterm_relation",l7="_where",k1="comp",dM="_",gS="Splitting",bX="src/principles.ml",dI="Signature",gR="wildcard",bq=147,ci=":=",gw="Unnexpected goal",e7="where",lo="Relations",l6="equations_plugin",lH="g_simplification_rules",lI="Elimination",ak=Kf.jsoo_runtime,S=ak.caml_check_bound,e3=ak.caml_fresh_oo_id,a2=ak.caml_make_vect,d=ak.caml_new_string,t=ak.caml_obj_tag,aG=ak.caml_register_global,E=ak.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):ak.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):ak.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):ak.caml_call_gen(a,[b,c,d])}function
n(a,b,c,d,e){return a.length==4?a(b,c,d,e):ak.caml_call_gen(a,[b,c,d,e])}function
M(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):ak.caml_call_gen(a,[b,c,d,e,f])}function
cY(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):ak.caml_call_gen(a,[b,c,d,e,f,g])}function
aF(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):ak.caml_call_gen(a,[b,c,d,e,f,g,h])}function
k0(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):ak.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
ch(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):ak.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
Ke(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):ak.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
bI(a,b,c,d,e,f,g,h,i,j,k,l,m){return a.length==12?a(b,c,d,e,f,g,h,i,j,k,l,m):ak.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l,m])}var
p=ak.caml_get_global_data(),ff=d(N),d0=[0,d("Lists"),[0,d("List"),0]],fo=d(gx),cu=[0,[0,0],1],aj=d(l6),s=p.CamlinternalLazy,aO=p.Universes,at=p.Constr,q=p.Termops,c=p.EConstr,Q=p.Assert_failure,i=p.Names,aP=p.Univ,Z=p.Inductiveops,e=p.Util,hS=p.Reduction,h9=p.Term,ac=p.Typeclasses,ar=p.Globnames,aH=p.Hints,A=p.Evarutil,w=p.Evd,af=p.CErrors,f=p.Pp,aq=p.Feedback,D=p.Context,G=p.Option,bi=p.Printer,j=p.Proofview,cx=p.Refiner,hW=p.Pretype_errors,hX=p.Himsg,z=p.Tacmach,r=p.Tactics,aN=p.Libnames,L=p.Ltac_plugin,az=p.Environ,U=p.Reductionops,y=p.Global,aA=p.Coqlib,R=p.CAst,m=p.Genarg,B=p.Stdarg,X=p.Geninterp,am=p.Nametab,W=p.Not_found,l=p.Tacticals,dY=p.Evarsolve,aU=p.Namegen,bf=p.Declare,C=p.Pervasives,g$=p.Flags,ao=p.Typing,g_=p.Univops,cp=p.Smartlocate,g4=p.Printexc,g5=p.Printf,co=p.Goptions,hb=p.Lazy,ax=p.Retyping,dk=p.Dumpglob,o=p.List,fB=p.CString,bN=p.Ppconstr,ig=p.DAst,a6=p.Constrintern,J=p.Loc,cD=p.Constrexpr_ops,ca=p.Impargs,O=p.Int,eh=p.Failure,i3=p.Goal,a7=p.Invalid_argument,K=p.Nameops,a9=p.Tacred,F=p.CList,bF=p.CArray,aJ=p.Evar,bd=p.CClosure,cc=p.Refine,cM=p.Inductive,ds=p.Locusops,cL=p.Extraction_plugin,jd=p.ComInductive,aW=p.Obligations,bQ=p.Lemmas,dt=p.Vars,aK=p.Array,eG=p.Evarconv,cT=p.Equality,ga=p.Class_tactics,cf=p.Autorewrite,j7=p.Indschemes,j6=p.Sorts,gj=p.Conv_oracle,kb=p.Find_subterm,kn=p.Metasyntax,km=p.Pretyping,bW=p.Ftactic,v=p.Pcoq,gq=p.Egramml,cV=p.Vernac_classifier,gp=p.Vernacinterp,a_=p.Genintern,aX=p.Mltop,bn=p.CLexer,eY=p.Gramext,o1=[0,d("src/equations_common.ml"),527,9],pZ=[0,0,0],pY=[0,0,0],pX=[0,0,0,0,0],pR=d(lG),pQ=d(lg),pL=d(dJ),pM=d(lc),pN=d(me),pP=d(" Pretype error: "),pO=d(lK),pJ=d(dJ),pK=d(mo),pG=d(" depends"),pH=d("Found no hypothesis on which "),pI=[0,d("move_before_deps")],pz=[0,0],pt=d("Equations.DepElim.depind"),pr=d("Equations.DepElim.simpl_dep_elim"),pq=d("Equations.DepElim.depelim_nosimpl"),pp=d("Equations.DepElim.do_empty"),pn=d("Equations.DepElim.depelim"),pm=d("Equations.DepElim.impossible_call"),pe=d(dO),pd=d("Equations.DepElim.simpl_equations"),pc=d("Equations.EqDecInstances.eqdec_proof"),pb=d("Equations.NoConfusion.solve_noconf"),pa=d("Equations.Subterm.pi"),o$=d("Equations.DepElim.find_empty"),o_=d("Equations.Equations.solve_rec"),o9=d("Equations.DepElim.set_eos"),o8=d("Equations.DepElim.equations"),o7=d("Equations.Subterm.unfold_recursor"),o6=d("Equations.Subterm.rec_wf_eqns_rel"),o5=d("Equations.Below.rec"),oY=d("Unknown database "),oZ=[0,d("autounfold")],o0=d("Nothing to unfold"),oX=[0,0,0],oU=d("pr2"),oV=[0,d(N),[0,d(Y),0]],oS=d("pr1"),oT=[0,d(N),[0,d(Y),0]],oQ=d(lD),oR=[0,d(N),[0,d(Y),0]],oO=d(gV),oP=[0,d(N),[0,d(Y),0]],oH=d(k_),oI=[0,d(N),[0,d(aL),0]],oA=d("add_pattern"),oB=[0,d(N),[0,d(aL),0]],oy=d("hidebody"),oz=[0,d(N),[0,d(Y),0]],ow=d("hide_pattern"),ox=[0,d(N),[0,d(aL),0]],ot=d("block"),ou=[0,d(N),[0,d(aL),0]],or=d("inaccessible_pattern"),os=[0,d(N),[0,d(aL),0]],op=d(lS),oq=[0,d(N),[0,d(aL),0]],n7=d("DependentEliminationPackage"),n8=[0,d(N),[0,d(aL),0]],n5=d("FunctionalElimination"),n6=[0,d(N),[0,d(gW),0]],n3=d(gW),n4=[0,d(N),[0,d(gW),0]],n1=[0,1],nX=d(lV),nY=[0,d(Y),[0,d(br),0]],nZ=d(e$),nT=d(mr),nU=[0,d(Y),[0,d(br),0]],nV=d(e$),nQ=d("and"),nR=[0,d(Y),[0,d(a3),0]],nS=d(e$),nM=d("conj"),nN=[0,d(Y),[0,d(a3),0]],nO=d(e$),nJ=d("Id_rect_dep_r"),nK=[0,d(N),[0,d(aL),0]],nG=d("Id_rect_r"),nH=[0,d(N),[0,d(aL),0]],nD=d("id_refl"),nE=[0,d(N),[0,d(Y),0]],nA=d("Id"),nB=[0,d(N),[0,d(Y),0]],nx=d("Empty"),ny=[0,d(N),[0,d(Y),0]],nv=d("fixproto"),nw=[0,d(N),[0,d(Y),0]],ns=d("JMeq_refl"),nt=[0,d(a3),[0,d(e_),0]],nu=d(l1),np=d(e_),nq=[0,d(a3),[0,d(e_),0]],nr=d(l1),nm=d("eq_rect_dep_r"),nn=[0,d(N),[0,d(aL),0]],ni=d("eq_rect_r"),nj=[0,d(Y),[0,d(a3),0]],nk=d("coq_eq_case"),ng=d("nat"),nh=[0,d(Y),[0,d(br),0]],ne=d("S"),nf=[0,d(Y),[0,d(br),0]],nc=d("O"),nd=[0,d(Y),[0,d(br),0]],m7=d(mi),m8=[0,d(aM),[0,d(Y),[0,d(a3),0]]],m4=d("I"),m5=[0,d(aM),[0,d(Y),[0,d(a3),0]]],m1=d(l5),m2=[0,d(aM),[0,d(Y),[0,d(a3),0]]],mZ=d("tt"),m0=[0,d(aM),[0,d(Y),[0,d(br),0]]],mX=d("unit"),mY=[0,d(aM),[0,d(Y),[0,d(br),0]]],mW=[1,10],mU=[0,0],mV=d(" is defined"),mS=d(ck),mO=[0,[11,d("Exception while typechecking context "),[2,0,[11,d(bs),[2,0,[12,10,0]]]]],d("Exception while typechecking context %s : %s\n")],mz=[0,d(N),[0,d("OCaml"),[0,d(gS),0]]],mA=d("splitting variables in OCaml"),mD=[0,d(N),[0,d("WithK"),0]],mE=d("using K during simplification"),mH=[0,d(N),[0,d("Transparent"),0]],mI=d("leave definitions transparent"),mL=[0,d(N),[0,d("Debug"),0]],mM=d("Equations debug output"),mT=[0,0],m_=d(lV),m$=[0,d(aM),[0,d(Y),[0,d(br),0]]],na=d(mr),nb=[0,d(aM),[0,d(Y),[0,d(br),0]]],n2=[0,d(N),[0,d(N),0]],n9=[0,d(N),[0,d(c1),0]],n_=d("WellFounded"),n$=[0,d(N),[0,d("Classes"),0]],oa=d("well_founded"),ob=[0,d(aM),[0,d(Y),[0,d("Wf"),0]]],oc=d("relation"),od=[0,d(aM),[0,d(lo),[0,d("Relation_Definitions"),0]]],oe=d("clos_trans"),of=[0,d(aM),[0,d(lo),[0,d("Relation_Operators"),0]]],og=d("id"),oh=[0,d(aM),[0,d(Y),[0,d(br),0]]],oj=d("list"),ol=d("nil"),on=d("cons"),oC=d("eos"),oD=d("the_end_of_the_section"),oE=[0,d(N),[0,d(aL),0]],oF=d(k_),oG=[0,d(N),[0,d(aL),0]],oJ=d("notT"),oK=[0,d(aM),[0,d(Y),[0,d("Logic_Type"),0]]],oM=d("ImpossibleCall"),oN=[0,d(N),[0,d(aL),0]],o2=[0,d(c1),[0,d(N),0]],pf=[0,d(aL),[0,d(N),0]],pi=d("solve_equation"),p6=d("No derive declared for "),p4=d("Expected an inductive type"),p7=d("#"),p8=d(b0),p9=d(c2),p_=d(b0),p$=d(lY),qb=d(ci),qc=d(l$),qd=d(cl),qe=d(cj),qf=d("rec "),qg=d(dP),qh=d(cl),qi=d(cj),qj=d(dP),qk=d(k4),ql=d(cl),qm=d(cj),qn=d(gZ),qo=d(cl),qp=d(cj),qq=d(gZ),qr=d(bs),qu=d(gR),qv=d("Aliases not supported by Equations"),qw=[0,d(lp)],qz=d(gR),qA=d(" cannot be applied"),qB=d("Pattern variable "),qC=[0,d(k2)],qD=d("Or patterns not supported by Equations"),qE=d(k2),qF=d("Expecting a pattern for "),qG=d(lp),qH=d(gI),qI=d(lF),qJ=d(gI),qK=d(lF),qy=[0,0,0],qt=[0,0,0],qN=d(" appears twice"),qO=d("Non-linear pattern: variable "),qP=d(gR),rc=[0,d(dL),310,13],rA=[0,0,0,0],rB=[0,0,0],rC=[0,0,0],sj=d("Non-exhaustive pattern-matching, no clause found for:"),sk=d(k3),sE=d("covering_aux: unexpected output of tactic call"),sF=d(" refining "),sG=d("Unable to find a covering for the result of a by clause:"),sH=[0,d(c4)],sz=d("And the user patterns are: "),sA=d("Problem is "),sB=d("Non-matching clause in with subprogram:"),sC=d(c4),sD=[0,d(dL),1468,52],sy=d("unknown"),st=d("This pattern must be innaccessible and equal to "),su=[0,d(c4)],sn=d("is not inaccessible, but should refine pattern "),so=d("Pattern "),sp=[0,d(c4)],sq=d("should be convertible to "),sr=d("Incompatible innaccessible pattern "),ss=[0,d(c4)],sl=d("Unbound variable "),sm=d(ck),sv=d(fb),sw=[0,1],sx=[0,0],sJ=[0,0],sI=d("interp_wheres"),sK=d("Unable to build a covering for:"),sL=d(k3),sh=d("Unused clause "),si=d(c4),sf=d("In context: "),se=d(bY),sd=d("named_of_rel_context"),r$=[0,d(dL),lR,19],r_=[0,d(dL),1069,21],rN=d("*impossible case*"),rG=d(gA),rH=d(bs),rI=d(lJ),rJ=d(gL),rK=d(bs),rL=d(gA),rM=d(" :=! "),rO=d(gL),rP=d(bs),rQ=d(" split: "),rR=d(gL),rS=d("Valid "),rT=d("Mapping "),rU=d("RecValid "),rV=d("New problem to problem substitution is: "),rW=d("Revctx is: "),rX=d(" eliminating "),rY=d(" for type "),rZ=d("New problem: "),r0=d(" in "),r1=d(bY),r2=d(bY),r3=d(bs),r4=d(bY),r5=d(" refine "),rF=d(lM),rD=[0,0,0],rx=d("Occurs check singleton subst"),rw=[0,d(dL),638,11],rv=[0,0],ru=[0,1],rr=d(gH),rs=d("Contexts do not agree for composition: "),rt=d("check_eq_context_nolet"),rq=[0,[11,d("Exception while comparing contexts "),[2,0,[11,d(gH),[2,0,[11,d(bs),[2,0,[12,10,0]]]]]]],d("Exception while comparing contexts %s and %s : %s\n")],rp=[0,0],rn=d("split_tele"),rm=d("split_context"),ri=d(gK),rg=[0,0],rh=[0,0],rf=d(gK),re=d(gK),rd=[0,0,0],rb=[0,0,0],q7=d(gF),q8=d(ck),q9=d("Invalid_argument: "),q_=d(gF),q$=d(ck),q4=d("Anomaly: "),q5=d(gF),q6=d(ck),q3=[0,0],qY=d(bs),qZ=d(bY),q0=d("|-"),q1=d(bY),qX=d(bY),qV=d("; "),qS=d(ci),qU=d(dM),qT=d(bs),qQ=d(b0),qR=d(c2),qL=[0,0],qM=[0,1],ry=d("Equations.Covering.Conflict"),rz=d("Equations.Covering.Stuck"),tp=d("Goal cannot be curried"),tn=d("No currying to do in "),te=[0,d(gG),474,14],tf=[0,d(gG),512,17],tl=[0,1],tj=d("Could not revert a cut, please report."),tg=[0,1],th=[0,1],ti=[0,1],tk=[0,1],s$=d(".] to avoid this."),ta=d(". Use [Derive Signature for "),tb=d("Automatically inlined signature for type "),s3=d("_sig"),s4=[1,0],s5=d("_sig_pack"),s6=d("_var"),s7=[1,0],s8=d("_Signature"),s1=d("No signature to derive for non-dependent inductive types"),s2=d("Derive Signature"),sY=d("signature_pack"),sZ=[0,d(N),[0,d(dI),0]],sV=d("signature"),sW=[0,d(N),[0,d(dI),0]],sT=d(dI),sU=[0,d(N),[0,d(dI),0]],sR=d(l8),sS=d(l8),sQ=[0,d(gG),85,10],sO=d("constrs_of_coq_sigma"),s_=d(dI),tM=[0,0,1],tH=d("c"),tI=d(gC),tJ=d("step"),tK=d(ly),tL=d("below"),tN=d("Below_"),tO=[1,0],tP=d("below_"),tQ=[1,0],tD=[0,d("src/subterm.ml"),229,55],tA=d("_subterm"),tB=d("well_founded_"),tE=d(e5),tF=d(dO),tC=[1,0],ty=d(dM),tz=d(dM),tx=d("_direct_subterm"),tu=d(dO),tv=d(e5),tw=d("z"),tt=d("Equations.Subterm.solve_subterm"),tG=d(gD),tR=d(c1),t3=d("_eqdec"),t1=[1,10],t2=d("_EqDec"),tX=d(dO),tY=d(e5),tV=d("dec_eq"),tW=[0,d(N),[0,d(cZ),0]],tT=d(cZ),tU=[0,d(N),[0,d(cZ),0]],tS=d("param"),t4=d(cZ),t5=d(dO),t6=d(e5),t7=d("NoConfusion_"),t8=d("noConfusion_"),t9=d("NoConfusionPackage_"),t_=[1,0],t$=[0,d("src/noconf.ml"),gJ,11],ua=d("NoConfusion"),vI=d(ms),vV=d(lO),vW=d(mj),vX=d(lQ),vL=d("NoConfusionOut"),vM=d("NoCycle"),vN=d("ElimTrue"),vO=d("ElimFalse"),vQ=d(lz),vP=d(lA),vS=d(gP),vR=d(ld),vT=d(gT),vJ=[0,d(gU),903,19],vK=[0,1],vz=[1,0],vA=[1,1],vB=d("Neither side of the equality is a variable."),vC=[1,0],vD=[1,1],vE=[0,0],vF=d("Could not infer a simplification step."),vy=d("[elim_true] The first hypothesis is not the empty type."),vw=d("[elim_true] The first hypothesis is not the unit type."),vx=[0,0,0],vv=d("[noCycle] is not implemented!"),vs=d("Expected a full application of [opaque_ind_pack_eq_inv]. Maybeyou did not solve completely a NoConfusion step?"),vt=d("[opaque_ind_pack_eq_inv] should be applied to [eq_refl]."),vu=[0,0,0],vq=d("[noConfusion] Cannot find an instance of NoConfusion for type "),vr=[0,0,0],vl=d(l4),vm=d(l4),vn=d("[noConfusion] Cannot simplify without K on type "),vo=[0,0,0],vj=d("[solution] The variable appears on both sides of the equality."),vf=[0,1],vi=[0,0,0],vg=[0,0,0],vh=d("[deletion] Cannot simplify without K on type "),vb=[0,0,0],va=[0,0,0],vc=[0,0,0],vd=d("If you see this, please report."),u6=d("The first hypothesis in the goal is not an equality."),u7=d("The first hypothesis in the goal is not an equality between identical terms."),u8=d("The left-hand side of the first hypothesis in the goal is not a variable."),u9=d("The right-hand side of the first hypothesis in the goal is not a variable."),u5=d("The goal is not a product."),u2=[0,d(gU),277,12],u0=d("Unexpected mismatch."),uY=[0,d(gU),218,30],uc=[0,d(N),[0,d(aL),0]],ub=d(ms),ue=d(cZ),uf=[0,d(N),[0,d(cZ),0]],uh=d(mi),ui=[0,d(aM),[0,d(Y),[0,d(a3),0]]],uj=d(l5),uk=[0,d(aM),[0,d(Y),[0,d(a3),0]]],ul=d("True_rect_dep"),um=d("False_rect"),un=[0,d(aM),[0,d(Y),[0,d(a3),0]]],up=d("False_rect_dep"),ur=d(lS),us=[0,d(N),[0,d(aL),0]],uu=d("apply_noConfusion"),uv=d("simplify_ind_pack"),uw=d("simplify_ind_pack_inv"),ux=d("opaque_ind_pack_eq_inv"),uy=d("eq_simplification_sigma1"),uz=d("eq_simplification_sigma1_dep"),uA=d("eq_simplification_sigma1_dep_dep"),uB=d("simplification_K"),uC=d("simplification_K_dec"),uD=d("solution_left"),uF=d("solution_left_dep"),uH=d("solution_right"),uJ=d("solution_right_dep"),uL=d(gV),uM=[0,d(N),[0,d(Y),0]],uN=d(lD),uO=[0,d(N),[0,d(Y),0]],uX=d("Equations.Simplify.CannotSimplify"),wl=[0,d(fa),457,13],wk=[0,d(fa),458,24],wi=[0,[0,1,0]],wj=d("_functional"),v_=d("m_"),v5=d("Simplifying term:"),v6=d("... in context:"),v7=[0,1],v8=[0,1],v9=d("Should not fail here, please report."),vY=[0,d(fa),c0,22],vZ=d(mf),v0=[0,0],v3=[0,d(fa),119,7],v1=d(gN),v2=[3,[0,0],0],v4=[0,[0,0,2],0],v$=[0,0,0],wa=d("branches"),wb=d(mq),wc=[0,0],wd=d(mf),we=[0,0],wX=d("Not enough products in "),wY=d("Cannot do a fixpoint on a non inductive type."),xw=d("Unexpected refinement goal in functional induction proof"),xx=d("convert concl"),xy=d("letin"),xt=d("Unexpected goal in functional induction proof"),xi=[0,d(b4),361,35],xq=d(l7),xj=d("one where"),xk=d("Couldn't find associated args of where"),xl=d(" context map "),xm=d(" instance: "),xn=d("term: "),xo=d(" where: "),xp=d("Found path "),xr=d(lb),xs=d("whererev"),xd=d("solving premises of compute rule"),xe=d("applying compute rule"),xf=d("wheretac"),xg=d("compute "),xh=d("compute empty"),xu=d(gN),xv=d(ln),xz=d(fb),yb=[0,d(b4),801,14],ya=d(mh),yc=d("exception"),x8=d("Unexpected unfolding lemma goal"),x4=d("Unexpected unfolding goal"),xX=[1,[0,1,0]],xY=[1,[0,1,0]],xW=[0,d(b4),691,16],xZ=d(e7),xV=[0,d(b4),673,29],x2=[0,d(b4),715,9],x0=d("compute rhs"),x1=d("compute"),x5=d(gN),x6=d(ln),x7=d("recvalid"),x9=d("refined"),x3=[0,d(b4),728,14],xT=[1,[0,1,0]],xU=[1,[0,1,0]],xS=d(le),x_=[1,[0,1,0]],x$=[1,[0,1,0]],xJ=[0,d(b4),541,25],xK=d("after mut -> nested and mut provable"),xL=d("mutfix"),xM=d("spliting nested"),xN=d("assert mut -> nest first subgoal "),xO=d(lU),xP=[0,0,1],xI=d(mh),xQ=[0,0,0],xH=d(lG),xG=d(lg),xC=d(dJ),xD=d(lc),xE=d(me),xF=d(lK),xA=d(dJ),xB=d(mo),xc=[0,1],xb=[0,10],w6=d("Fixpoints should be on the same mutual inductive declaration."),w7=d(" already used in the environment"),w8=d("Name "),w9=[0,d("Logic.prim_refiner")],w_=[0,d(b4),197,29],xa=d("fix_"),wZ=d(" subgoal"),w0=d(gH),w1=d(" index"),w$=d(" indices"),w2=d(bY),w3=d(" name"),w4=d("Cannot apply mutual fixpoint, invalid arguments: "),w5=[0,d(mu)],wU=d(dJ),wV=d("Trying "),wT=d("failed"),wW=d("Couldn't rewrite"),wQ=d("_wf_obligations"),wP=[0,d(gx),[0,d(c1),[0,d("rec_decision"),0]]],wK=d(c1),wJ=d("Helper not found while proving induction lemma."),yT=[0,0,0],y8=d(lk),y9=d("_helper_"),y3=d("_mut"),yY=[0,d(lI),[0,d(lW),0]],yZ=[0,1],y0=[0,d(lI),[0,d(lW),0]],y1=[0,1],y2=d(e8),y6=d(lU),y7=d("_rect"),y5=d("_ind_comb"),y4=d(e8),yW=d(lk),yV=d("_refinement_"),yU=d(e8),yX=d("_ind_"),yQ=d("FunctionalInduction_"),yN=d(I),yP=d(dM),yO=d(e8),yM=d("_ind_fun"),yS=[0,d(bX),881,12],yR=d("Induction principle could not be proved automatically: "),yI=d("FunctionalElimination_"),yK=d("_ind_ind"),yJ=d("_elim"),yF=[0,0,0],yG=[0,0,1],yH=[0,1,1],yD=d("_unfold_eq"),yE=[0,-1],yA=d(gA),yB=d(bs),yC=d(lJ),yy=[0,1,0],yw=[0,d(bX),433,16],yt=[0,0],ys=d("Heq"),yq=d("refine_eq"),yr=d(fb),yn=d("abs"),yo=[0,d(bX),352,8],yp=[0,0,0],yu=[0,d(bX),391,30],yv=[0,d(bX),385,9],yx=[0,d(bX),292,18],ym=[0,d(bX),247,16],yj=[0,0,0,0],yk=d("recres"),yl=d("Hind"),yg=[0,d(bX),72,12],yd=[0,1],ye=[0,0],zy=d("Generated clauses: "),zz=d("dummy"),zv=[0,d("src/depelim.ml"),523,19],zw=[0,1],zx=[12,0,0,0],zA=d("Could not eliminate variable "),zB=d("No such hypothesis: "),zu=d("Specialization not allowed on dependent hypotheses"),zt=d("Nothing to do in hypothesis "),zr=d("destPolyRef"),zq=[0,0],zn=d("DependentElimination_"),zj=d(gC),zk=d(gC),zl=d("_dep_elim"),zm=[1,7],zi=[0,0],zh=[0,1],zf=[0,d(aM),[0,d(a3),[0,d(e_),0]]],zg=[0,1],zc=[0,1],zb=d("gen_x"),zd=[0,1],za=[0,1],y$=[0,1],y_=d("Equations.Depelim.Seen"),zp=d("DependentElimination"),Ai=d(gw),Ag=d(mq),Aj=d(gw),Ah=d(gw),Af=d(gI),Ae=[0,d(gy),561,63],Ac=[1,0],z0=d(" found"),z1=d("No argument named "),z2=d("struct_index"),Ab=[0,[1,0]],z3=d("_comp"),z4=[1,0],z5=d(c1),z6=d("program"),z7=d(gx),z8=d("_comp_proj"),z9=d(k1),z_=[1,0],z$=[0,1,0,1],Aa=[0,[0,0]],zX=[0,0],zY=[3,0],zZ=[2,1],Ad=[0,0],zW=[1,5],zV=[1,5],zS=d(l7),zT=d(lb),zQ=[0,d(gy),162,67],zR=d("_eq"),zP=d(le),zU=[0,0],zI=[0,0],zJ=[0,1],zK=[0,0],zL=[0,1],zM=[0,0],zN=[0,1],zO=[0,0],zE=d("fix_proto"),zF=[0,d(l3),[0,d("Tactics"),0]],zG=[0,d(gy),48,9],zC=d(l3),Aq=d("Products do not match"),Ap=d("Second-order matching failed"),Ao=d("Couldn't find a second-order pattern to match"),Al=[0,d("src/extra_tactics.ml"),19,11],Am=d("core"),Ak=d("f"),Jm=[2,0],Jj=[0,1],Jg=[0,0],IS=d(dN),IG=d(dN),ID=d(c3),IB=d(dN),Iy=d(c3),H0=d("Not a section variable or hypothesis"),HQ=[0,0,0],HD=[0,0,0],Ht=d(gO),Hk=d(gO),Hh=d(c3),Hf=d(gO),Hc=d(c3),F1=[0,d("src/g_equations.ml4"),368,20],D0=[3,0],DW=[3,1],DS=[2,0],DO=[2,1],DK=[1,0],DC=[0,1],Dy=[0,0],CM=d("No generalization needed"),CJ=d(gB),Cb=d(gB),B_=d(c3),B8=d(gB),B5=d(c3),BN=[0,0],At=d(bJ),Ax=d("$h'"),AB=d("$h"),AE=d(lh),AG=d(lh),AJ=d("$myref"),AM=d(lr),AO=d(lr),AR=d("$id'"),AV=d(b3),AY=d(k5),A0=d(k5),A3=d(b3),A6=d(gV),A7=d(g2),A9=d("pattern_sigma"),A$=[0,d(gY),0],Bc=d(b3),Bf=d(gY),Bh=d(gY),Bk=d(b3),Bn=d("uncurry_hyps"),Bp=d("curry_hyps"),Bs=d(b3),Bw=d(bJ),Bz=d(lf),BB=d(lf),BE=d(bJ),BH=d(g2),BI=d(e4),BK=d("dependent_pattern"),BO=d(bJ),BR=d("from"),BS=d(g2),BT=d(e4),BV=d("dependent_pattern_from"),BY=d(bJ),B1=d(lu),B3=d(lu),CF=[0,d(a3)],CG=[0,d(N)],CN=d(b3),CQ=d(ls),CS=d(ls),CV=d("$tac"),CZ=d("$destruct"),C2=d(lx),C4=d(lx),C7=d(bJ),C$=d(dK),Dc=d("simpc"),Df=d(bJ),Dj=d(dK),Dm=d(mp),Do=d(mp),Dp=d(gM),Du=d(gM),Dz=d("noind"),DD=d("ind"),DH=d(k$),DL=d("nostruct"),DP=d(k1),DT=d("nocomp"),DX=d("eqns"),D1=d("noeqns"),D5=d(gM),D6=d(gz),D$=d(gz),Ed=d(b0),Ef=d(c2),Ek=d(gz),El=d(gX),Eq=d(gX),Ew=d(gX),Ez=d(l0),EB=d(l0),ED=d(mm),EG=d(mm),EH=d(lN),EK=d(lN),EL=d(ck),EO=d(ck),EQ=d(lm),ET=d(lm),EU=d("identloc"),EV=d("equation"),EW=d("ipatt"),EX=d("patt"),EY=d("pat_head"),EZ=d("lpatt"),E0=d(fb),E1=d("struct_annot"),E2=d("rec_annot"),E3=d("where_clause"),E4=d(e7),E5=d("rhs"),E6=d("sub_equations"),E9=[0,[10,[0,d(ma),d(I)]],0],Fe=[10,[0,d(I),d(";")]],Fi=[0,[10,[0,d(I),d("]")]],0],Fj=[10,[0,d(I),d(gE)]],Fk=[10,[0,d(I),d("[")]],Fs=[10,[0,d(I),d(gE)]],Ft=[10,[0,d(I),d(gE)]],Fx=[0,[10,[0,d(I),d(cl)]],0],Fy=[10,[0,d(I),d(ci)]],Fz=[10,[0,d(I),d(cj)]],FG=[0,[10,[0,d(I),d(dM)]],0],FI=[0,[10,[0,d(I),d(b0)]],0],FJ=[10,[0,d(I),d(c2)]],FM=[0,[10,[0,d(I),d(b0)]],0],FN=[10,[0,d(I),d(lY)]],FQ=d("0"),F2=[10,[0,d(I),d(",")]],F5=[0,[10,[0,d(I),d(b0)]],0],F6=[10,[0,d(I),d(k$)]],F7=[10,[0,d(I),d(c2)]],Ga=[10,[0,d(I),d(e7)]],Gc=[10,[0,d(I),d(li)]],Gi=[10,[0,d(I),d(ci)]],Gj=[10,[0,d(I),d(lM)]],Gm=[10,[0,d(I),d(e7)]],Gr=[10,[0,d(I),d(l$)]],Gw=[0,[10,[0,d(I),d(ci)]],0],Gy=[0,[10,[0,d(I),d(dP)]],0],GE=[0,[10,[0,d(I),d(ci)]],0],GG=[0,[10,[0,d(I),d(dP)]],0],GK=[0,[10,[0,d(I),d(li)]],0],GM=[0,[10,[0,d(I),d(k4)]],0],GQ=[10,[0,d(I),d(b0)]],GR=[10,[0,d(I),d(c2)]],GS=[10,[0,d(I),d(gP)]],GX=[0,[10,[0,d(I),d(ci)]],0],GZ=[0,[10,[0,d(I),d(dP)]],0],G2=[10,[0,d(ma),d(ly)]],G3=[10,[0,d(I),d(gZ)]],G6=[0,[10,[0,d(I),d(cl)]],0],G7=[10,[0,d(I),d(cj)]],Hq=[0,d(N)],Hu=d(la),HA=d(la),HE=d(dK),HH=d("as"),HJ=d(b3),HM=d(lj),HN=d(e4),HR=d(b3),HU=d(lj),HV=d(e4),HX=d("dependent_elimination"),H1=d("$x"),H4=d(mk),H6=d(mk),H9=d(bJ),Ia=d(lw),Ic=d(lw),If=d(ml),Ii=d(mb),Ik=d(mb),In=d(bJ),Ir=d(ml),Iu=d(k9),Iw=d(k9),IK=[0,d("for")],IP=[0,d(dN)],IT=d(lH),IW=d(lH),IX=d("simplification_rule_located"),IY=d("simplification_rule"),IZ=d("simplification_step"),I0=d("direction"),I_=[0,[10,[0,d(I),d(lO)]],0],Ja=[0,[10,[0,d(I),d(mj)]],0],Jc=[0,[10,[0,d(I),d(lQ)]],0],Jh=[0,[10,[0,d(I),d(lA)]],0],Jk=[0,[10,[0,d(I),d(lz)]],0],Jn=[0,[10,[0,d(I),d(gT)]],0],Jp=[0,[10,[0,d(I),d(cl)]],0],Jq=[10,[0,d(I),d(cj)]],Jr=[10,[0,d(I),d(gT)]],Jw=[0,[10,[0,d(I),d(ld)]],0],Jy=[0,[10,[0,d(I),d(gP)]],0],JA=d(lX),JG=d(lX),JI=[0,d(g0),0],JL=d(dK),JO=d(g0),JQ=d(g0),JT=d(dK),JX=d("$li"),J0=d(mu),J2=d("mutual_fix"),J3=d(lB),J4=d(mc),J5=d(lt),J6=d(gD),J7=d(ll),J8=d(mg),J9=d(mt),J_=d(gS),J$=d(lL),Ka=d(k7),Kb=d(N),Kc=d("equations_plugin_mod"),Kd=d(l6),p3=p.Evardefine,qx=p.Lib,r6=p.Topfmt,tc=p.Patternops,tZ=p.Safe_typing,t0=p.Future,uZ=p.Typeops,An=p.Eauto;function
mv(d,c,b){return a(d,a(c,b))}function
mw(d,c,b){var
e=b[1],f=a(c,b[2]);return[0,a(d,e),f]}function
mx(a){return a}function
c6(b){var
d=b[1];return[0,d,a(c[2][1],b[2])]}function
bK(d,a){var
e=a[1];return[0,e,b(c[2][2],d,a[2])]}var
cn=[0,1],dR=[0,1],dS=[0,0];function
my(a){cn[1]=a;return 0}var
mB=[0,0,mA,mz,function(a){return cn[1]},my];b(co[4],0,mB);function
mC(a){dR[1]=a;return 0}var
mF=[0,0,mE,mD,function(a){return dR[1]},mC];b(co[4],0,mF);function
mG(a){dS[1]=a;return 0}var
mJ=[0,0,mI,mH,function(a){return dS[1]},mG];b(co[4],0,mJ);var
be=[0,0];function
mK(a){be[1]=a;return 0}var
mN=[0,0,mM,mL,function(a){return be[1]},mK];b(co[4],0,mN);function
fc(d,c,b,a){return n(ao[5],d,[0,c],b,a)}function
g3(c,b,a){g(ao[4],c,[0,b],a);return 0}function
fd(j,i,h){try{var
d=function(d,e){g3(e,i,a(D[1][1][3],d));var
f=a(D[1][1][2],d);function
g(b){return fc(e,i,b,a(D[1][1][3],d))}b(G[13],g,f);return b(c[bt],d,e)};g(e[17][19],d,h,j);var
o=0;return o}catch(d){d=E(d);var
k=a(g4[1],d),l=b(c[H],h,j),m=a(q[g1],l),n=a(f[49],m);g(g5[3],mO,n,k);throw d}}function
fe(d){var
b=a(c[8],at[6]);return ch(A[5],az[28],w[16],0,0,0,0,0,0,b)[2]}function
x(b){return a(j[70][8],b)}function
au(a){return b(j[70][1],0,a)}function
g6(c){var
d=[0,c,0];function
f(g,c){var
d=c[1],f=b(e[18],c[2],[0,d,0]);return[0,a(e[17][6],d),f]}return g(e[17][19],f,c,d)[2]}function
g7(e){return function(g,f){var
c=g,a=f;for(;;){if(a){var
h=a[2],d=b(e,c,a[1]);if(d)return d;var
c=c+1|0,a=h;continue}return 0}}}function
mP(a){return g(e[19][7],a,0,a.length-1-1|0)}function
mQ(a){return b(e[19][51],a.length-1-1|0,a)}function
g8(e,d){return function(f){var
a=f;for(;;){if(a){var
c=a[1],g=a[2],h=c[1];if(b(e,d,c[2]))return h;var
a=g;continue}throw W}}}function
mR(c,b){var
d=0;function
f(d,b){var
e=a(c,d);function
f(a){return[0,a,b]}return g(G[24],f,b,e)}var
h=g(e[19][18],f,b,d);return a(e[19][12],h)}function
a4(c,a){return b(A[13],c,a)}function
al(a,c){var
b=a4(a[1],c),d=b[2];a[1]=b[1];return d}function
ap(c,b,a){return al(a,g(aA[1],ff,c,b))}function
ab(b,a){return g(aA[1],ff,b,a)}function
dT(b,a){return g(aA[2],mS,b,a)}function
fg(b){var
c=a(aN[34],b),d=a(am[10],c);return a(cp[2],d)}function
g9(b){var
d=a(aN[34],b),e=a(am[9],d),f=a(aO[50],e);return a(c[8],f)}var
fh=a(ao[3],mT);function
dU(z,j,d,f,i){var
o=j?j[1]:0,e=a(y[2],0);n(ao[3],0,e,d,i);var
p=f?(n(ao[3],0,e,d,f[1]),d[1]):d[1],h=a(A[47],p)[1],k=b(c[5],h,i),q=a(c[5],h),l=b(G[16],q,f),r=b(g_[1],e,k),s=aP[2][1],t=a(g_[1],e),u=g(G[24],t,s,l),v=b(aP[2][7],r,u),m=b(w[124],h,v);d[1]=m;var
x=[0,b(w[c5],o,m)];return aF(bf[2],0,0,0,l,x,0,k)}function
bu(c,j,h,g,e,d){var
k=[0,[0,dU(mU,[0,g],[0,e],h,j)],d],l=M(bf[3],0,0,c,0,k),m=a(i[1][8],c),n=b(C[16],m,mV),o=a(f[3],n),p=aq[6];function
q(a){return b(p,0,a)}b(g$[25],q,o);return l}function
c7(k,j,i,e,d,h){var
f=b(ac[16],d,h),l=f[2],m=a(G[7],f[1]),o=b(c[38],m,e),g=bu(k,o,[0,b(c[37],l,e)],j,i,mW),p=n(ac[5],d[1],aH[4],1,[1,g]);a(ac[6],p);return a(c[22],g)}var
aB=[k,function(a){return ab(mY,mX)}],bg=[k,function(a){return ab(m0,mZ)}],m3=[k,function(a){return ab(m2,m1)}],m6=[k,function(a){return ab(m5,m4)}],m9=[k,function(a){return ab(m8,m7)}];function
dV(a){return ap(m$,m_,a)}function
fi(a){return ap(nb,na,a)}var
c8=[k,function(a){return dT(nd,nc)}],c9=[k,function(a){return dT(nf,ne)}],bv=[k,function(a){return dT(nh,ng)}];function
b5(b){if(0===b){var
c=t(c8),e=u===c?c8[1]:k===c?a(s[2],c8):c8;return a(aO[50],e)}var
d=t(c9),f=[0,b5(b-1|0)],g=u===d?c9[1]:k===d?a(s[2],c9):c9,h=[0,a(aO[50],g),f];return a(at[13],h)}function
dW(d){var
b=a(at[26],d);if(9===b[0]){var
c=b[2];if(1===c.length-1)return dW(c[1])+1|0}return 0}function
dX(e,d,c){var
f=a(az[9],c),g=a(q[77],f),h=a(i[1][10][35],g),j=b(i[1][10][7],e,h);return b(aU[26],d,j)}function
ha(d,c,b){return dX(d,c,a(z[8],b))}var
cq=a(hb[3],aA[39]),cr=[k,function(b){return a(aA[36],0)[3]}],nl=[k,function(a){return g(aA[2],nk,nj,ni)}],no=[k,function(a){return ab(nn,nm)}],bw=[k,function(a){return g(aA[2],nr,nq,np)}],bx=[k,function(a){return g(aA[2],nu,nt,ns)}],aE=[k,function(a){return ab(nw,nv)}],nz=[k,function(a){return ab(ny,nx)}],nC=[k,function(a){return ab(nB,nA)}],nF=[k,function(a){return ab(nE,nD)}],nI=[k,function(a){return ab(nH,nG)}],nL=[k,function(a){return ab(nK,nJ)}],nP=[k,function(a){return g(aA[2],nO,nN,nM)}],fj=[0,cq,cr,nl,no,0,m9,m3,m6,[k,function(a){return g(aA[2],nS,nR,nQ)}],nP],nW=[k,function(a){return g(aA[2],nV,nU,nT)}],by=[0,fj],n0=[0,nC,nF,nI,nL,2,nz,aB,bg,[k,function(a){return g(aA[2],nZ,nY,nX)}],nW];function
hc(a){by[1]=a;return 0}function
cs(a){return by[1][5]}function
hd(d){var
b=by[1][1],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
he(d){var
b=by[1][2],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
hf(d){var
b=by[1][3],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
hg(d){var
b=by[1][4],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
fk(d){var
b=by[1][7],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
hh(d){var
b=by[1][8],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
hi(d){var
b=by[1][6],c=t(b);return u===c?b[1]:k===c?a(s[2],b):b}function
hj(b){var
e=cs(0),f=b[1],g=a(y[2],0),d=M(w[lZ],0,0,g,f,e),h=d[2];b[1]=d[1];return a(c[13],h)}function
b6(g,b,f,e){var
d=cY(c[e9],0,0,0,g,b[1],f),h=d[2];b[1]=d[1];return a(c[21],[0,h,e])}function
ct(d,a,c){var
b=aF(dY[5],0,0,0,n1,d,a[1],c),e=b[2];a[1]=b[1];return e}function
bh(b,a,e,d,c){var
f=[0,ct(b,a,e),d,c];return b6(b,a,hd(0),f)}function
dZ(b,a,d,c){var
e=[0,ct(b,a,d),c];return b6(b,a,he(0),e)}function
fl(c,b,h,g,f,e){var
i=ct(c,b,f),d=t(bw),j=[0,ct(c,b,h),g,i,e],l=u===d?bw[1]:k===d?a(s[2],bw):bw;return b6(c,b,l,j)}function
fm(c,b,f,e){var
d=t(bx),g=[0,ct(c,b,f),e],h=u===d?bx[1]:k===d?a(s[2],bx):bx;return b6(c,b,h,g)}var
av=0;function
aI(d,c){var
e=[0,a(aN[31],d)],f=[29,[0,av,[3,[0,av,[0,b(R[1],0,e),c]]]]];return a(L[13][26],f)}function
c_(d,c){var
e=b(ac[11],d,c);return a(G[7],e)[2][1]}function
hk(b){var
a=[0,b],c=ap(n4,n3,a),d=c_(a[1],c);return[0,a[1],d]}function
hl(b){var
a=[0,b],c=ap(n6,n5,a),d=c_(a[1],c);return[0,a[1],d]}function
hm(a){var
b=ap(n8,n7,a);return c_(a[1],b)}function
hn(a){return ap(n$,n_,a)}function
ho(a){return ap(ob,oa,a)}function
hp(a){return ap(od,oc,a)}function
hq(a){return ap(of,oe,a)}function
oi(a){return ap(oh,og,a)}function
ok(a){return ap(d0,oj,a)}function
om(a){return ap(d0,ol,a)}function
oo(a){return ap(d0,on,a)}var
c$=[k,function(a){return ab(oq,op)}],bz=[k,function(a){return ab(os,or)}],ov=[k,function(a){return ab(ou,ot)}],bA=[k,function(a){return ab(ox,ow)}],da=[k,function(a){return ab(oz,oy)}],bB=[k,function(a){return ab(oB,oA)}],fn=a(i[1][6],oC);function
hr(a){return ap(oE,oD,a)}function
hs(a){return ap(oG,oF,a)}var
bC=[k,function(a){return ab(oI,oH)}];function
oL(a){return ap(oK,oJ,a)}function
ht(a){return ap(oN,oM,a)}var
db=[k,function(f){var
b=t(bB),c=0,d=u===b?bB[1]:k===b?a(s[2],bB):bB,e=[0,[0,0,[1,a(ar[8],d)]],c];return a(r[68],e)}],ad=[k,function(a){return ab(oP,oO)}],aw=[k,function(a){return ab(oR,oQ)}];function
hu(d,c){var
e=ab(d,c),f=a(ar[8],e);return b(i[H][1],f,0)}var
aC=[k,function(a){return hu(oT,oS)}],ag=[k,function(a){return hu(oV,oU)}];function
oW(e,g){var
a=g;for(;;){var
h=b(q[57],e,a),f=b(q[60],e,h),d=b(c[3],e,f);switch(d[0]){case
6:var
a=d[3];continue;case
8:var
a=d[4];continue;case
9:var
a=d[1];continue;default:return f}}}function
_(a){var
b=a[3],c=a[2],d=a[1];return c?[1,d,c[1],b]:[0,d,b]}function
dc(e,i,d){function
j(e,d){if(d){var
k=d[2],f=a(D[1][1][17],d[1]),l=f[3],m=f[2],n=f[1],o=j(e-1|0,k),p=g(c[h][2],i,e,l),q=b(c[h][2],i,e);return[0,_([0,n,b(G[16],q,m),p]),o]}return 0}return j(a(D[1][4],d)+e|0,d)}function
aV(b,a){return dc(0,b,a)}function
hv(d){var
f=a(c[h][1],1);return b(e[17][15],f,d)}function
fp(q,k,d){var
s=[0,i[1][10][1],i[19][1]];function
x(c,d){var
j=c[2],k=c[1];try{var
q=a(aH[15],d),e=q}catch(c){c=E(c);if(c!==W)throw c;var
l=a(f[3],d),m=a(f[3],oY),n=b(f[12],m,l),e=g(af[6],0,oZ,n)}var
h=a(aH[14][18],e),o=h[1],p=b(i[19][7],h[2],j);return[0,b(i[1][10][7],o,k),p]}var
o=g(e[17][18],x,s,q),y=k?b(z[19],d,k[1][1]):a(z[7],d),m=a(z[2],d),t=a(z[8],d),u=o[2],v=o[1];function
h(f){var
d=b(c[3],m,f);switch(d[0]){case
1:var
k=d[1];if(b(i[1][10][3],k,v)){var
l=b(az[38],k,t);return l?[0,1,a(c[8],l[1])]:[0,0,f]}break;case
9:var
n=d[2],o=d[1],p=h(o);if(0===p[1]){var
z=function(f,c,a){var
b=c[2],d=c[1];if(d)return[0,d,[0,a,b]];var
e=h(a);return 0===e[1]?[0,0,[0,a,b]]:[0,1,[0,e[2],b]]},q=g(e[19][43],z,oX,n),A=q[2];if(q[1]){var
B=a(e[17][9],A),C=[0,o,a(e[19][12],B)];return[0,1,a(c[21],C)]}return[0,0,f]}var
D=a(c[21],[0,p[2],n]);return[0,1,b(U[25],w[16],D)];case
10:var
r=d[1],s=r[1],E=r[2];if(b(i[19][3],s,u)){var
F=[0,s,b(c[2][2],m,E)],G=b(az[55],t,F);return[0,1,a(c[8],G)]}break}var
j=[0,0];function
x(a){if(j[1])return a;var
b=h(a),c=b[2];j[1]=b[1];return c}var
y=g(c[100],m,x,f);return[0,j[1],y]}var
n=h(y),p=n[2];if(n[1]){if(k){var
A=k[1],B=a(r[47],p),C=g(r[54],0,B,A);return b(j[70][8],C,d)}var
D=b(r[5],p,2);return b(j[70][8],D,d)}var
F=a(f[3],o0);return g(l[24],0,F,d)}function
hw(d){var
c=d;for(;;){var
b=a(at[26],c);switch(b[0]){case
9:var
c=b[1];continue;case
10:var
e=a(i[17][9],b[1][1]);return a(i[6][5],e);default:throw[0,Q,o1]}}}var
hx=a(e[17][15],hw),o3=b(e[17][15],i[1][6],o2),hy=a(i[5][4],o3);function
o4(b){var
c=a(i[6][4],b),d=a(i[5][4],0);return g(i[13][1],[0,hy],d,c)}function
hz(c){var
d=b(R[1],0,[1,[0,c]]),e=a(m[4],B[7]);return[0,b(m[7],e,d)]}function
hA(d,c){var
e=[0,hz(c),[0,[1,[0,d]],0]],f=[0,a(aN[31],o5)];return[29,[0,av,[3,[0,av,[0,b(R[1],0,f),e]]]]]}function
hB(e,d,c){var
f=[0,hz(d),[0,[1,[0,e]],[0,[1,[0,c]],0]]],g=[0,a(aN[31],o6)];return[29,[0,av,[3,[0,av,[0,b(R[1],0,g),f]]]]]}function
hC(a){return aI(o7,0)}function
hD(a){return aI(o8,0)}function
dd(a){return aI(o9,0)}function
hE(a){return aI(o_,0)}function
hF(a){return aI(o$,0)}function
hG(a){return aI(pa,0)}function
hH(a){return aI(pb,0)}function
hI(a){return aI(pc,0)}function
hJ(a){return aI(pd,0)}function
d1(a){return[2,b(R[1],0,[1,a])]}var
pg=b(e[17][15],i[1][6],pf),ph=[0,a(i[5][4],pg)],pj=a(i[6][4],pi),pk=g(i[13][1],ph,i[5][6],pj);function
hK(p){var
d=a(i[1][6],pe),e=a(m[6],B[13]),f=a(X[3],e),h=a(aO[35],[0,p,aP[29][1]]),j=a(c[8],h),k=b(X[1][8],f,j),l=X[5][1],n=[0,g(i[1][11][4],d,k,i[1][11][1]),l],o=[29,[0,av,[3,[0,av,[0,[0,[0,av,pk]],[0,[2,[1,b(R[1],0,d)]],0]]]]]];return b(L[13][23],n,o)}function
pl(d){var
c=[0,b(am[43],i[1][10][1],d)],e=[0,[2,b(R[1],0,c)],0],f=[0,a(aN[31],pm)],g=[29,[0,av,[3,[0,av,[0,b(R[1],0,f),e]]]]],h=a(L[9][3],g),j=a(m[5],L[2][1]);return b(m[7],j,h)}function
hL(a){return aI(pn,[0,d1(a),0])}function
po(a){return aI(pp,[0,d1(a),0])}function
hM(a){return aI(pq,[0,d1(a),0])}function
fq(a){return aI(pr,0)}function
ps(a){return aI(pt,[0,d1(a),0])}function
pu(d,c,b){return b6(d,c,a(aA[51],0),[0,b])}function
pv(d,e){var
f=a(D[1][1][2],d);if(f)return b(c[h][5],f[1],e);var
g=a(D[1][1][3],d),i=[0,a(D[1][1][1],d),g,e];return a(c[18],i)}function
hN(f,e,d){var
i=a(c[9],1);return g(q[37],f,i,d)?b(c[35],e,d):b(c[h][5],c[14],d)}function
hO(c,b,a){function
d(b,a){return hN(c,a,b)}return g(e[17][18],d,b,a)}function
pw(d,e){var
f=a(D[1][1][2],d);if(f)return b(c[h][5],f[1],e);var
g=a(D[1][1][3],d),i=[0,a(D[1][1][1],d),g,e];return a(c[19],i)}function
hP(j,i,d){var
e=a(D[1][1][17],i),f=e[2],k=e[3],l=e[1];if(f)return b(c[h][5],f[1],d);var
m=a(c[9],1);return g(q[37],j,m,d)?a(c[19],[0,l,k,d]):b(c[h][5],c[14],d)}function
hQ(j,i,d){var
e=a(D[1][1][17],i),f=e[2],k=e[3],l=e[1];if(f)return b(c[h][5],f[1],d);var
m=a(c[9],1);return g(q[37],j,m,d)?a(c[18],[0,l,k,d]):b(c[h][5],c[14],d)}function
b7(h,a,f,d){function
i(e,d){var
f=b(c[35],d,e);return b(U[29],a,f)}var
j=g(e[17][18],i,f,d);return g(U[17],h,a,j)}function
fr(j,d,i,f){function
k(f,e){var
g=0===a(D[1][1][1],e)?b(c[h][5],c[14],f):b(c[35],e,f);return b(U[29],d,g)}var
l=g(e[17][18],k,i,f);return g(U[17],j,d,l)}function
d2(d,a){function
f(d,a){return b(c[36],a,d)}var
h=g(e[17][18],f,d,a);return b(U[29],w[16],h)}function
px(c,b,a){function
d(b,a){return hP(c,a,b)}return g(e[17][18],d,b,a)}function
py(c,b,a){function
d(b,a){return hQ(c,a,b)}return g(e[17][18],d,b,a)}function
fs(f,d){var
g=a(c[h][1],f);return b(e[17][15],g,d)}function
cv(d,g,j,h){var
m=g?g[1]:0;function
f(g,j){var
h=b(c[3],d,j);switch(h[0]){case
1:return b(i[1][10][4],h[1],g);case
9:var
o=h[2],k=b(c[3],d,h[1]);switch(k[0]){case
11:var
l=k[1][1];break;case
12:var
l=k[1][1][1];break;default:return n(c[lC],d,f,g,j)}var
p=a(y[27],l)[1],q=m?0:p[6];return n(e[19][48],q,f,g,o);default:return n(c[lC],d,f,g,j)}}return f(j,h)}function
d3(j,d,g){var
f=b(c[3],j,d);switch(f[0]){case
11:var
h=f[1][1];break;case
12:var
h=f[1][1][1];break;default:return[0,d,g]}var
k=a(y[27],h)[1][7],i=b(e[19][51],k,g),l=i[2];return[0,a(c[21],[0,d,i[1]]),l]}function
hR(f,a,e,d){try{var
b=aF(U[83],0,pz,0,f,a[1],e,d),c=b[2],g=b[1],h=c?(a[1]=g,1):c;return h}catch(a){a=E(a);if(a===hS[6])return 0;throw a}}function
pA(h,f,d){var
e=i[1][10][1];function
j(t,k,j){var
e=a(D[2][1][17],k),l=e[3],m=e[2],o=e[1],p=0;function
r(b){var
e=a(c[8],b);return n(q[33],d,h,f,e)}if(!g(G[24],r,p,m)){var
s=a(c[8],l);if(!n(q[33],d,h,f,s))return j}return b(i[1][10][4],o,j)}return g(az[39],j,d,e)}var
pB=i[1][10][1];function
pC(c,a){return b(i[1][10][4],a,c)}var
pD=b(e[17][18],pC,pB);function
hT(a){return b(L[5][10],aN[41],a)}function
hU(c){var
b=c[1];return 0===b[0]?a(aN[40],b[1]):b[1][1]}function
d4(b){var
c=hU(b);return a(i[1][6],c)}var
pE=ax[2];function
pF(a){return g(pE,0,0,a)}var
cw=a(z[24],pF);function
hV(c,m){function
d(d){var
h=a(j[66][6],d),n=a(j[66][4],d),o=b(q[42],h,m),p=b(z[42][16],c,d),s=b(q[42],h,p),t=b(i[1][10][9],o,s);function
u(c){var
d=a(D[2][1][1],c);return b(i[1][10][3],d,t)}var
v=a(e[17][9],n),k=b(e[17][c0],u,v)[2];if(k)var
l=a(D[2][1][1],k[1]);else
var
w=a(f[3],pG),x=a(i[1][9],c),y=a(f[3],pH),A=b(f[12],y,x),B=b(f[12],A,w),l=g(af[6],0,pI,B);return b(r[81],c,[0,l])}return a(j[66][10],d)}function
as(e,c){return be[1]?function(h){var
d=a(bi[84],h),i=a(f[3],pJ),l=a(f[3],e),m=a(f[3],pK),n=b(f[12],m,l),o=b(f[12],n,i),p=b(f[12],o,d);b(aq[10],0,p);function
q(i){var
c=i[1];if(c[1]===cx[29])var
d=c[3],o=c[2],p=a(bi[84],h),l=t(d),q=a(f[3],pL),r=u===l?d[1]:k===l?a(s[2],d):d,v=a(f[13],0),w=a(f[3],e),x=a(f[3],pM),y=a(f[16],o),z=a(f[3],pN),A=b(f[12],z,y),B=b(f[12],A,x),C=b(f[12],B,w),D=b(f[12],C,v),E=b(f[12],D,r),F=b(f[12],E,q),m=b(f[12],F,p);else{if(c[1]===hW[1])var
I=g(hX[2],c[2],c[3],c[4]),J=a(f[3],pP),n=b(f[12],J,I);else
var
n=a(af[17],i);var
m=n}var
G=a(f[3],pO),H=b(f[12],G,m);b(aq[10],0,H);return a(j[16],0)}function
r(c){if(0===c){var
d=a(f[3],pQ),g=a(f[3],e),h=b(f[12],g,d);b(aq[10],0,h);return a(j[16],0)}return au(function(c){var
d=a(bi[84],c),e=a(f[3],pR),g=b(f[12],e,d);b(aq[10],0,g);return[0,[0,c[1],0],c[2]]})}var
v=b(j[71][1],j[53],r),w=au(c),y=b(j[18],w,v);return a(x(b(j[23],y,q)),h)}:c}function
T(b,a){return g(D[1][14],c[9],b,a)}function
aQ(b,a){return g(D[1][13],c[9],b,a)}var
ah=D[1][1][17],bD=D[2][1][17],pS=D[2][1][18];function
hY(a){return b(e[17][15],_,a)}function
pT(a){return[0,a]}function
pU(a){return[1,a]}var
bj=D[1][1][3],ft=D[1][1][2],b8=D[1][1][1],fu=D[2][1][3],fv=D[2][1][2];function
pV(b,a){return[0,b,a]}function
an(c,b,a){return b?[1,c,b[1],a]:[0,c,a]}function
fw(c,b,a){return b?[1,c,b[1],a]:[0,c,a]}var
pW=D[1][7];function
cy(f,m,i){var
n=f?f[1]:0;function
j(o,d){var
i=d[2],j=d[1],p=d[4],q=d[3],r=a(c[h][4],j),e=b(D[1][1][14],r,o),k=a(b8,e),f=k?k[1]:a(m,0),s=a(bj,e),t=[0,f,a(ft,e),s],u=a(D[2][1][18],t);if(n)var
g=0;else
if(a(D[1][1][6],e))var
g=0;else
var
l=i,g=1;if(!g)var
l=[0,a(c[10],f),i];return[0,[0,a(c[10],f),j],l,[0,f,q],[0,u,p]]}var
d=g(e[17][19],j,i,pX),k=d[4],l=d[1];return[0,l,a(e[17][9],d[2]),k]}function
cz(d){function
f(i,f){var
d=f[2],j=f[1],e=a(bD,i),g=e[1],k=e[2],l=b(c[h][11],d,e[3]),m=a(c[h][11],d);return[0,[0,an([0,g],b(G[16],m,k),l),j],[0,g,d]]}return g(e[17][19],f,d,pY)}var
d5=aH[4];function
hZ(c,b){if(0===b[0]){var
d=b[1];return[0,d,a(c,b[2])]}var
e=b[2],f=b[1],g=a(c,b[3]);return[1,f,a(c,e),g]}function
de(d,f,a){var
i=[0,d,0];function
j(e,a){var
d=a[1],g=a[2];return[0,d+1|0,[0,hZ(b(c[h][3],f,d),e),g]]}return g(e[17][19],j,a,i)[2]}function
df(f,d){function
i(a,e){var
d=a[1],g=a[2];return[0,d+1|0,[0,hZ(b(c[h][3],[0,f,0],d),e),g]]}var
j=g(e[17][18],i,pZ,d)[2];return a(e[17][9],j)}function
d6(l,k,j){var
f=1,g=0,d=j;for(;;){if(d){var
i=d[2],m=d[1];if(f===l){var
n=a(e[17][9],g),o=de(0,[0,b(c[h][1],-f|0,k),0],n);return b(e[18],o,i)}var
f=f+1|0,g=[0,m,g],d=i;continue}return 0}}function
h0(m,l,k){var
f=1,g=0,d=k;for(;;){if(d){var
j=d[2],i=d[1];if(f===m){var
n=a(D[1][1][3],i),o=b(c[h][1],-f|0,l),p=[0,[1,a(D[1][1][1],i),o,n],j],q=a(e[17][9],g);return b(e[18],q,p)}var
f=f+1|0,g=[0,i,g],d=j;continue}return 0}}function
aZ(b){return a(D[2][1][1],b)}var
d7=D[2][9],d8=D[1][8],bb=D[1][1][14],h1=D[2][1][14],h2=D[2][7],h3=D[2][5];function
h4(g,m,l){var
f=0,d=l;for(;;){if(d){var
j=d[2],k=d[1],n=aZ(k);if(b(i[1][1],n,g)){var
o=a(e[17][9],f);return b(e[18],o,j)}var
f=[0,b(h1,a(c[h][9],[0,[0,g,m],0]),k),f],d=j;continue}return 0}}function
dg(a){return b(aq[6],0,a)}function
bL(a){return g(af[6],a[1],[0,a[2]],a[3])}function
a5(b){var
c=a(f[3],b);return g(af[6],0,0,c)}function
bM(b,a){return g(af[6],0,[0,b],a)}var
h5=af[4];function
p0(a){return b(af[16],0,a)}var
d9=U[20];function
fx(b,a){return g(af[3],0,b,a)}function
h6(j,i,h,e,f){var
k=a(c[lq][1],h),d=b(w[4],j,k),l=e?[0,d[1],d[2],d[3],d[4],e[1],d[6],d[7]]:d;return g(w[22],f,i,l)}function
cA(d,c,b,a){return ch(A[4],d,c,b,0,0,0,0,0,a)}function
h7(d,c,b,a){return aF(A[8],d,c,b,0,0,0,a)}function
p1(a){return a}function
p2(a){return a}var
h8=p3[7];function
d_(c,b,a){return g(aH[22],0,[0,a,0],[4,[0,[1,c],0],b])}function
fy(e,d,a){var
f=b(c[5],e,a);return b(ar[11],d,f)}function
dh(d,b){var
e=bK(d,b),f=a(aO[35],e);return a(c[8],f)}function
d$(f,d){var
g=a(c[126],f),h=b(e[17][15],g,d),i=a(q[88],h);return b(e[17][15],c[V],i)}function
a0(d,a){var
f=b(q[8],d,a);return b(e[19][15],c[8],f)}function
aD(d,b){return a(c[34],[0,d,b])}function
fz(d,c,a){return b(ac[16],c,a)}function
cB(e,d){var
a=b(c[3],e,d);return 9===a[0]?[0,a[1],a[2]]:[0,d,[0]]}function
di(f){var
d=a(Z[6],f),g=d[1],h=b(e[17][15],c[8],d[2]);return[0,c6(g),h]}function
fA(d,g,f){var
h=a(c[5],d),i=b(e[19][15],h,f),j=b(c[5],d,g),k=b(h9[74],j,i);return a(c[8],k)}function
h_(d,g,f){var
h=a(c[5],d),i=b(e[19][15],h,f),j=b(c[5],d,g),k=b(hS[25],j,i);return a(c[8],k)}function
ea(d,c,b){var
a=g(Z[71],d,c,b);return[0,a[1],a[2]]}function
dj(d){var
c=a(aP[37][4],d),e=[0,c,b(aP[37][7],c,d)],f=a(aP[36][1],e);return[0,c,a(aP[40][15],f)]}aG(1028,[0,cn,dR,dS,be,x,au,mv,mw,mx,mP,mQ,g8,mR,g6,g7,oW,cu,av,dX,ha,aI,dc,aV,hv,fs,fe,fc,g3,fd,hR,fh,pu,pv,hN,hO,pw,hP,hQ,b7,fr,d2,px,py,cv,pA,pD,d3,ct,a4,al,ff,ap,ab,dT,fg,g9,c_,dU,bu,c7,hc,fj,n0,cs,hd,he,hf,hg,fk,hh,hi,aB,bg,dV,fi,ad,aw,aC,ag,c8,c9,bv,b5,dW,cq,cr,bw,bx,aE,hj,b6,bh,dZ,fl,fm,n2,n9,d0,fo,hk,hl,hm,hn,ho,hp,hq,oi,ok,om,oo,c$,bz,ov,bA,da,bB,fn,hr,hs,bC,oL,ht,db,as,hy,o4,hC,hD,dd,hE,hF,hG,hH,hI,hJ,hK,pl,hL,po,hM,fq,ps,hA,hB,fp,hw,hx,hT,hU,d4,cw,hV,T,aQ,ah,bD,_,pS,bj,b8,ft,pV,an,fw,hY,pT,pU,cy,cz,de,aZ,fu,fv,pW,d7,d8,bb,h1,h2,h3,p1,p2,dg,bL,a5,bM,h5,p0,fx,d9,df,d6,h0,h4,h6,cA,h7,d5,h8,d_,c6,bK,fy,dh,d$,a0,aD,fz,cB,di,fA,h_,ea,dj],lB);function
h$(g,f,e){var
c=a(y[2],0),h=a(w[17],c),d=b(A[13],h,e);return n(g,c,d[1],f,d[2])}function
b9(i,e,d){return h$(function(l,d,k,j){var
e=b(c[3],d,j);if(11===e[0]){var
h=e[1];return n(i,l,d,k,[0,h[1],h[2]])}var
m=a(f[3],p4);return g(af[6],0,0,m)},e,d)}var
fC=[0,fB[49][1]];function
b_(a){fC[1]=g(fB[49][4],a[1],a[2],fC[1]);return 0}function
p5(d){try{var
c=b(fB[49][22],d,fC[1]);return c}catch(c){c=E(c);if(c===W){var
e=a(f[3],d),h=a(f[3],p6),i=b(f[12],h,e);return g(af[6],0,0,i)}throw c}}function
ia(d,c,a){function
e(a){var
c=a[2];b(dk[12],a[1],c);return c}var
f=b(o[17],e,a);function
g(e){var
a=p5(e);function
c(c){return b(a,d,c)}return b(o[15],c,f)}return b(o[15],g,c)}aG(1032,[0,h$,b9,b_,ia],dN);function
ib(d,j){var
c=j[2];switch(c[0]){case
0:var
k=c[1],l=c[2]?a(f[3],p7):a(f[7],0),m=a(i[1][9],k);return b(f[12],m,l);case
1:var
g=c[3],h=b(bi[62],d,c[1]);if(a(e[17][53],g))return h;var
n=a(f[3],p8),o=cC(d,g),p=a(f[13],0),q=a(f[3],p9),r=b(f[12],q,h),s=b(f[12],r,p),t=b(f[12],s,o);return b(f[12],t,n);default:var
u=c[1],v=a(f[3],p_),w=a(bN[20],u),x=a(f[3],p$),y=b(f[12],x,w);return b(f[12],y,v)}}function
cC(b,a){function
c(a){return ib(b,a)}return g(f[39],f[13],c,a)}function
qa(b){return dg(cC(a(y[2],0),b))}function
ic(d,c){switch(c[0]){case
0:var
h=c[2],j=c[1],aY=function(e){var
c=e[1],g=e[2],h=a(b$(d),g),j=c[3],k=c[1][2],l=a(bN[20],c[4]),m=a(f[3],qr),n=a(bN[17],j),o=a(i[1][9],k),p=b(f[12],o,n),q=b(f[12],p,m),r=b(f[12],q,l);return b(f[12],r,h)},aZ=g(f[39],f[5],aY,h),k=a(bN[20],j),l=a(f[13],0),m=a(f[3],qb),n=a(f[13],0),o=b(f[12],n,m),p=b(f[12],o,l),q=b(f[12],p,k);return b(f[12],q,aZ);case
1:var
r=a(i[1][9],c[1][2]),s=a(f[13],0),t=a(f[3],qc),u=a(f[13],0),v=b(f[12],u,t),w=b(f[12],v,s);return b(f[12],w,r);case
2:var
x=c[4],y=c[3],z=c[1],A=a(f[3],qd),B=a(b$(d),x),C=a(f[3],qe),D=b(f[12],C,B),E=b(f[12],D,A),F=b(f[26],1,E),G=a(f[13],0),H=function(b){return a(i[1][9],b[2])},I=b(f[34],H,y),J=a(f[13],0),K=a(bN[20],z),M=a(f[3],qf),N=a(f[13],0),O=a(f[3],qg),P=a(f[13],0),Q=b(f[12],P,O),R=b(f[12],Q,N),S=b(f[12],R,M),T=b(f[12],S,K),U=b(f[12],T,J),V=b(f[12],U,I),W=b(f[12],V,G);return b(f[12],W,F);case
3:var
X=c[2],Y=c[1],Z=a(f[3],qh),_=a(b$(d),X),$=a(f[3],qi),aa=b(f[12],$,_),ab=b(f[12],aa,Z),ac=b(f[26],1,ab),ad=a(f[13],0),ae=a(f[3],qj),af=a(f[13],0),ag=a(bN[20],Y),ah=a(f[13],0),ai=a(f[3],qk),aj=a(f[13],0),ak=b(f[12],aj,ai),al=b(f[12],ak,ah),am=b(f[12],al,ag),an=b(f[12],am,af),ao=b(f[12],an,ae),ap=b(f[12],ao,ad);return b(f[12],ap,ac);default:var
e=c[1];if(0===e[0]){var
aq=c[2],ar=e[1],as=a(f[3],ql),at=a(b$(d),aq),au=a(f[3],qm),av=b(f[12],au,at),aw=b(f[12],av,as),ax=b(f[26],1,aw),ay=a(f[13],0),az=a(L[5][23],ar),aA=a(f[13],0),aB=a(f[3],qn),aC=a(f[13],0),aD=b(f[12],aC,aB),aE=b(f[12],aD,aA),aF=b(f[12],aE,az),aG=b(f[12],aF,ay);return b(f[12],aG,ax)}var
aH=c[2],aI=e[1],aJ=a(f[3],qo),aK=a(b$(d),aH),aL=a(f[3],qp),aM=b(f[12],aL,aK),aN=b(f[12],aM,aJ),aO=b(f[26],1,aN),aP=a(f[13],0),aQ=b(L[5][25],d,aI),aR=a(f[13],0),aS=a(f[3],qq),aT=a(f[13],0),aU=b(f[12],aT,aS),aV=b(f[12],aU,aR),aW=b(f[12],aV,aQ),aX=b(f[12],aW,aP);return b(f[12],aX,aO)}}function
dl(c,a){var
d=a[2],e=ic(c,a[3]),g=cC(c,d);return b(f[12],g,e)}function
b$(a){function
c(b){return dl(a,b)}return b(f[39],f[5],c)}function
qs(b){return dg(dl(a(y[2],0),b))}function
eb(d,a){var
c=b(aU[25],d,a[1]);a[1]=b(i[1][10][4],c,a[1]);return c}function
ec(e,d,c,b){return a(f[7],0)}function
ed(e,d,c,b){return a(f[7],0)}function
id(a){if(a)if(0===a[1][0])return 1;return 0}function
ee(g,d,f){if(0===d[0]){var
h=d[1][2],e=b(c[3],g,f);switch(e[0]){case
1:return b(i[1][1],h,e[1]);case
10:var
j=a(i[17][9],e[1][1]),k=a(i[6][7],j);return b(i[1][1],h,k);default:return 0}}return fy(g,[1,d[1][3]],f)}var
ie=a(J[3],qt);function
fD(m,h,d,c){if(0===c[0]){var
j=c[1];if(j)return[0,d,[0,j[1],0]];var
k=eb(a(i[1][6],qu),h);h[1]=b(i[1][10][4],k,h[1]);return[0,d,[0,k,1]]}var
l=c[1],n=c[2],o=l[1];if(c[3]){var
p=a(f[3],qv);return g(af[6],d,qw,p)}function
q(a,b){return fD(m,h,a,b)}var
r=a(ig[9],q),s=b(e[17][15],r,n);return[0,d,[1,l,a(Z[35],o),s]]}function
fE(a){var
c=i[1][10][1];function
d(a,f){var
c=f[2];if(typeof
c==="number")return a;else
switch(c[0]){case
0:var
g=c[1][2],d=fE(c[2]);try{var
h=d4(g),j=b(i[1][10][4],h,d),e=j}catch(a){var
e=d}return b(i[1][10][7],e,a);case
1:return a;default:return a}}return g(e[17][18],d,c,a)}function
ef(n,s,r){var
c=r[2],o=r[1],h=s?s[1]:[0,i[1][10][1]];if(typeof
c==="number"){var
t=eb(a(i[1][6],qz),h);h[1]=b(i[1][10][4],t,h[1]);return[0,[0,o],[0,t,1]]}else
switch(c[0]){case
0:var
j=c[2],u=c[1],k=u[2],d=u[1];try{var
J=[0,b(cp[5],0,k)],l=J}catch(a){var
l=[1,[0,d4(k),0]]}if(0===l[0]){var
v=l[1];if(3===v[0]){var
m=v[1],F=a(Z[35],m[1]),w=a(Z[47],m),x=a(e[17][1],j);if(x<w)var
G=b(e[17][64],w-x|0,[0,d,0]),y=b(e[18],G,j);else
var
y=j;b(dk[12],[0,d],[3,m]);var
H=[0,h],I=function(a){return ef(n,H,a)};return[0,[0,d],[1,m,F,b(e[17][15],I,y)]]}if(0===j)return[0,[0,d],[0,d4(k),0]];var
A=a(f[3],qA),B=hT(k),C=a(f[3],qB),D=b(f[12],C,B),E=b(f[12],D,A);return g(af[6],[0,d],qC,E)}return[0,[0,d],l[1]];case
1:return[0,[0,o],[2,c[1]]];default:var
p=b(a6[8],n,c[1])[2];if(p)if(p[2])var
q=0;else
var
K=p[1][2],L=function(a,b){return fD(n,h,a,b)},z=b(ig[9],L,K),q=1;else
var
q=0;if(!q)var
z=bL([0,[0,o],qE,a(f[3],qD)]);return z}}function
ih(u,m,l,z,c){var
d=[0,i[1][10][1]],o=[0,d];function
A(a){return ef(l,o,a)}function
h(m,g,h,B,l){var
o=l[3],p=l[2],q=l[1];if(0===p[0])var
c=p[1];else
var
O=p[1],P=function(a){return[0,0,a]},Q=b(e[17][15],P,O),c=b(e[18],B,Q);function
C(a){return a[2]}var
D=fE(b(e[17][15],C,c));d[1]=b(i[1][10][7],d[1],D);function
F(c){var
d=c[2],e=c[1];if(1-b(i[1][1],d,g)){var
h=a(i[1][9],g),j=a(f[3],qF);bL([0,[0,e],qG,b(f[12],j,h)])}var
k=a(i[1][8],d);return n(dk[17],[0,e],qI,k,qH)}b(G[13],F,q);function
j(f,c){if(f){var
g=f[2],h=f[1];if(a(ca[14],h)){var
k=a(ca[16],h),l=function(c){var
a=c[1];return a?b(i[1][1],a[1][2],k):0};try{var
p=function(a){return l(a)?[0,a[2]]:0},q=b(e[17][b2],p,c),r=[0,q,j(g,b(e[17][97],l,c))];return r}catch(a){a=E(a);if(a===W){var
m=eb(k,d),n=[2,[0,b(R[1],0,[1,m])]],o=[2,b(R[1],0,n)];d[1]=b(i[1][10][4],m,d[1]);return[0,[0,ie,o],j(g,c)]}throw a}}if(c){var
s=c[1],t=j(g,c[2]);return[0,s[2],t]}var
u=function(a){return a[2]};return b(e[17][15],u,c)}function
v(a){return a[2]}return b(e[17][15],v,c)}var
y=j(z,c);if(q)var
v=q[1][1];else
var
w=a(e[17][5],c),x=w[1],N=x?x[1][1]:w[2][1],v=N;var
H=a(e[17][md],c)[2][1],r=b(J[4],v,H),s=b(e[17][15],A,y);if(h){var
t=h[1];if(0===t[0]){var
I=t[1],K=function(a){var
c=a[2],d=a[1];if(1===c[0])if(!c[1])if(b(i[1][1],d,u))return 0;return[0,[0,0,[0,d,0]]]},L=b(e[17][70],K,I),M=k(m,g,h,c,o);return[0,r,b(e[18],L,s),M]}return[0,r,s,k([0,[0,g,t[1]],m],g,h,c,o)]}return[0,r,s,k(m,g,h,c,o)]}function
k(f,g,l,k,c){switch(c[0]){case
0:var
o=c[2],p=c[1],I=function(c){var
d=c[1],g=d[1],j=g[2],k=c[2],l=g[1],m=a(i[1][8],j);n(dk[17],[0,l],qK,m,qJ);var
o=0,p=0;function
q(a){return h(f,j,p,o,a)}return[0,d,b(e[17][15],q,k)]},J=b(e[17][15],I,o),q=d[1],r=function(a,b){return j(f,q,a,b)};return[0,b(R[6],r,p),J];case
1:return[1,c[1]];case
2:var
m=c[1],s=c[4],t=c[3],u=c[2],v=a(cD[6],m),w=[0,[0,g,[0,[0,a(G[7],v),g]]],f],x=function(a){return h(w,g,l,k,a)};return[2,m,u,t,b(e[17][15],x,s)];case
3:var
y=c[2],z=c[1],A=function(a){return h(f,g,l,k,a)},B=b(e[17][15],A,y),C=d[1],D=function(a,b){return j(f,C,a,b)};return[3,b(R[6],D,z),B];default:var
E=c[2],F=c[1],H=function(a){return h(f,g,l,k,a)};return[4,F,b(e[17][15],H,E)]}}function
j(f,p,o,d){var
c=o?o[1]:ie;if(7===d[0]){var
q=d[1];if(!q[1]){var
k=q[2][1];if(0===k[0]){var
r=k[1],s=r[1];if(0!==s[0]){var
l=r[2],h=s[1],A=d[2],B=k[2];if(g(e[17][135],i[1][1],h,f)){var
t=g(e[17][133],i[1][1],h,f),C=function(a){var
c=a[2],d=a[1];function
e(a,b){return j(f,p,a,b)}return[0,b(R[6],e,d),c]},u=b(e[17][15],C,A),D=[0,b(R[1],l,[1,h]),B],E=[7,[0,0,b(R[1],[0,c],D)],u],m=a(qx[18],h),x=[12,[0,[0,[1,b(i[17][1],m,m)],qy,0]],0,0],F=[0,[0,b(R[1],[0,c],x),0],0],H=[7,[0,0,b(R[1],[0,c],E)],F],v=b(R[1],[0,c],H);if(0===t[0])return v;var
w=t[1],I=a(G[3],w[1])?[0,[0,v,0],0]:0,J=b(am[43],i[1][10][1],[1,w[3]]),K=b(e[18],u,I),L=[0,b(R[1],l,[0,J]),0],M=[7,[0,0,b(R[1],l,L)],K];return b(R[1],[0,c],M)}}}}}var
y=b(R[1],[0,c],d);function
z(b){function
c(a,c){return j(f,b,a,c)}return a(R[6],c)}return n(cD[30],i[1][10][4],z,p,y)}return h(0,u,m,0,c)}aG(1040,[0,ib,cC,cC,qa,ic,dl,b$,qs,id,ee,eb,ec,ed,fD,fE,ef,ih],mg);function
ii(b,a,d){var
e=M(ax[2],0,0,b,a[1],d),c=aF(dY[5],[0,w[aY]],qM,0,qL,b,a[1],e),f=c[2];a[1]=c[1];return f}function
ij(f,d,b){var
e=t(bz),g=[0,ii(f,d,b),b],h=u===e?bz[1]:k===e?a(s[2],bz):bz,i=[0,al(d,h),g];return a(c[21],i)}function
ik(f,d,b){var
e=t(bA),g=[0,ii(f,d,b),b],h=u===e?bA[1]:k===e?a(s[2],bA):bA,i=[0,al(d,h),g];return a(c[21],i)}function
bc(d){switch(d[0]){case
0:return a(c[9],d[1]);case
1:var
f=d[2],g=a(c[28],d[1]),h=b(e[17][15],bc,f),i=[0,g,a(e[19][12],h)];return a(c[21],i);case
2:return d[1];default:return a(c[9],d[1])}}function
il(g,f,d,b){switch(b[0]){case
0:return a(c[9],b[1]);case
1:var
j=b[2],k=a(c[28],b[1]),l=im(g,f,d,j),m=[0,k,a(e[19][12],l)];return a(c[21],m);case
2:var
h=b[1];if(g)try{var
n=ij(f,d,h);return n}catch(a){return h}return h;default:var
i=b[1];return g?ik(f,d,a(c[9],i)):a(c[9],i)}}function
im(f,d,c,a){function
g(a){return il(f,d,c,a)}return b(e[17][15],g,a)}function
dm(a,e,d,c){var
f=a?a[1]:1,b=[0,d],g=il(f,e,b,c);return[0,b[1],g]}function
io(a,e,d,c){var
f=a?a[1]:1,b=[0,d],g=im(f,e,b,c);return[0,b[1],g]}function
iq(c){var
d=O[2][1];function
f(c,f){var
d=ip(f),e=b(O[2][8],d,c);if(a(O[2][2],e))return b(O[2][7],d,c);var
g=a(O[2][26],e),h=a(C[21],g),i=b(C[16],h,qN);return a5(b(C[16],qO,i))}return g(e[17][18],f,d,c)}function
ip(b){switch(b[0]){case
0:return a(O[2][5],b[1]);case
1:return iq(b[2]);case
2:return O[2][1];default:return O[2][1]}}function
dn(a){function
c(a){return[2,a]}return b(e[17][15],c,a)}function
ir(c,a){function
d(a){return cb(c,a)}return b(e[17][15],d,a)}function
cb(d,j){var
f=b(c[3],d,j);switch(f[0]){case
0:return[0,f[1]];case
9:var
i=f[2],h=f[1];if(2===i.length-1){var
l=i[2],m=t(bz),q=u===m?bz[1]:k===m?a(s[2],bz):bz;if(g(c[ba],d,q,h))return[2,l];var
n=t(bA),r=u===n?bA[1]:k===n?a(s[2],bA):bA;if(g(c[ba],d,r,h))return[3,b(c[65],d,l)]}if(b(c[56],d,h)){var
o=b(c[77],d,h),v=a(Z[35],o[1][1]),p=b(e[19][51],v,i),w=p[1],x=ir(d,a(e[19][11],p[2])),y=dn(a(e[19][11],w));return[1,o,b(e[18],y,x)]}break;case
12:return[1,f[1],0]}return[2,j]}function
is(c,d,m,f){var
n=c?c[1]:[0,i[1][10][1]],t=[0,n];function
g(f){var
c=t?n:[0,i[1][10][1]];switch(f[0]){case
0:var
o=b(e[17][7],m,f[1]-1|0),p=a(D[1][1][1],o),g=b(aU[28],p,c[1]);c[1]=b(i[1][10][4],g,c[1]);return[0,[0,d,[0,g,0]]];case
1:var
j=f[1][1],q=f[2],k=a(Z[35],j[1]);return[0,[0,d,[1,j,k,is([0,c],d,m,b(e[17][h],k,q)[2])]]];case
2:var
r=c[1],s=a(i[1][6],qP),l=b(aU[25],s,r);c[1]=b(i[1][10][4],l,c[1]);return[0,[0,d,[0,l,1]]];default:return 0}}return b(e[17][70],g,f)}function
it(c,d,b){var
f=b[2],g=b[1],h=c?c[1]:i[1][10][1],j=is([0,[0,h]],d,g,f);return a(e[17][9],j)}function
iu(i,e,d){var
h=g(q[V],i,e,d);if(9===b(c[3],e,d)[0]){var
j=a(f[3],qQ),k=a(f[3],qR),l=b(f[12],k,h);return b(f[12],l,j)}return h}function
iv(a,d,c){var
b=dm(0,a,d,c);return iu(a,b[1],b[2])}function
cE(j,h,n){function
k(d,l){var
c=a(ah,l),e=c[2],j=c[1],m=c[3];if(e)var
n=g(q[V],d,h,e[1]),o=a(f[13],0),p=a(f[3],qS),r=b(f[12],p,o),k=b(f[12],r,n);else
var
k=a(f[7],0);var
s=j?a(i[1][9],j[1]):a(f[3],qU),t=g(q[V],d,h,m),u=a(f[3],qT),v=b(f[12],s,k),w=b(f[12],v,u);return b(f[12],w,t)}var
d=a(e[17][9],n);if(d)var
l=d[1],o=d[2],p=k(j,l),r=[0,b(c[bt],l,j),p],s=function(e,d){var
g=e[1],h=e[2],i=k(g,d),j=a(f[3],qV),l=b(f[12],h,j),m=b(f[12],l,i);return[0,b(c[bt],d,g),m]},m=g(e[17][18],s,r,o)[2];else
var
m=a(f[7],0);return m}function
qW(c,b,a){return dg(cE(c,b,a))}function
fF(d,c,b){var
h=a(e[17][9],b);function
i(a){return iv(d,c,a)}function
j(b){return a(f[3],qX)}return g(f[39],j,i,h)}function
ay(h,g,d){var
i=d[1],l=d[3],m=d[2],n=b(c[H],i,h),j=cE(h,g,i),o=cE(h,g,l),p=a(f[3],qY),q=fF(n,g,m),r=a(f[3],qZ),s=a(f[3],q0);if(a(e[17][53],i))var
k=j;else
var
x=a(f[3],q1),k=b(f[12],j,x);var
t=b(f[12],k,s),u=b(f[12],t,r),v=b(f[12],u,q),w=b(f[12],v,p);return b(f[12],w,o)}function
q2(c,b,a){return dg(ay(c,b,a))}function
iw(g,f,d){var
i=d[3],j=d[1],l=d[2];fd(g,f,j);fd(g,f,i);var
k=b(c[H],j,g),m=[0,f,0];function
o(l,j,d){var
e=d[2],m=d[1],n=a(ah,l)[3],f=dm(q3,k,m,j),g=f[2],i=f[1];fc(k,i,g,b(c[h][4],e,n));return[0,i,[0,g,e]]}n(e[17][24],o,i,l,m);return 0}function
dp(c,h,e,d){var
k=c?c[1]:0;if(k)return d;try{iw(h,e,d);return d}catch(c){c=E(c);if(c[1]===hW[1]){var
i=c[4];if(16===i[0]){var
j=c[2],t=g(hX[1],j,e,i[1]),u=a(f[13],0),v=ay(j,e,d),w=a(f[3],q7),x=b(f[12],w,v),y=b(f[12],x,u);return bM(q8,b(f[12],y,t))}}else
if(c[1]===a7){var
z=a(f[3],c[2]),A=a(f[3],q9),B=a(f[13],0),C=ay(h,e,d),D=a(f[3],q_),F=b(f[12],D,C),G=b(f[12],F,B),H=b(f[12],G,A);return bM(q$,b(f[12],H,z))}if(a(h5,c)){var
l=b(af[16],0,c),m=a(f[3],q4),n=a(f[13],0),o=ay(h,e,d),p=a(f[3],q5),q=b(f[12],p,o),r=b(f[12],q,n),s=b(f[12],r,m);return bM(q6,b(f[12],s,l))}throw c}}function
aR(a,f,e,d,c,b){var
g=a?a[1]:0;return dp([0,g],f,e,[0,d,c,b])}function
ix(f,d){function
g(d){switch(d[0]){case
1:var
e=d[2],g=a(f,a(c[28],d[1])),h=b(c[77],w[16],g);return[1,h,ix(f,e)];case
2:return[2,a(f,d[1])];default:return d}}return b(e[17][15],g,d)}function
a8(c,a){var
d=a[2],e=a[1],f=b(d8,c,a[3]),g=ix(c,d);return[0,b(d8,c,e),g,f]}function
eg(f,d,k,a){function
g(d,a){var
i=b(c[3],f,a);if(0===i[0]){var
j=i[1]-d|0;if(0<j)try{var
l=bc(b(e[17][7],k,j-1|0)),m=b(c[h][1],d,l);return m}catch(b){b=E(b);if(b[1]===eh)return a;throw b}return a}function
n(a){return a+1|0}return M(c[cm],f,n,g,d,a)}return g(d,a)}function
ra(f,d,a){function
c(e,a){var
c=a[1],g=a[2];return[0,c+1|0,[0,b(bb,function(a){return eg(f,c,d,a)},e),g]]}return g(e[17][19],c,a,rb)[2]}function
ei(g,d,c){switch(c[0]){case
0:var
h=c[1];try{var
i=b(e[17][7],d,h-1|0);return i}catch(a){a=E(a);if(a[1]===eh)return c;throw a}case
1:var
j=c[2],k=c[1];return[1,k,a(ej(g,d),j)];case
2:return[2,bk(g,d,c[1])];default:var
f=b(e[17][7],d,c[1]-1|0);switch(f[0]){case
0:return[3,f[1]];case
1:throw[0,Q,rc];case
2:return[2,f[1]];default:return[3,f[1]]}}}function
bk(c,b,a){return eg(c,0,b,a)}function
ej(c,b){function
d(a){return ei(c,b,a)}return a(e[17][15],d)}function
cF(f,d,a){function
c(e,a){var
c=a[1],g=a[2];return[0,c+1|0,[0,b(bb,function(a){return eg(f,c,d,a)},e),g]]}return g(e[17][19],c,a,rd)[2]}function
P(d,c,b){return bk(d,a(e[8],c),b)}function
fG(j,e,f,d){switch(d[0]){case
0:var
i=d[1];return i===e?[2,f]:e<i?[0,i-1|0]:d;case
1:var
k=d[2],l=d[1];return[1,l,a(iy(j,e,f),k)];case
2:return[2,g(c[h][3],[0,f,0],e-1|0,d[1])];default:var
m=a(c[9],d[1]),n=g(c[h][3],[0,f,0],e-1|0,m);return[3,b(c[65],j,n)]}}function
iy(d,c,b){function
f(a){return fG(d,c,b,a)}return a(e[17][15],f)}function
fH(f,e,d){switch(d[0]){case
0:var
i=d[1];return e<=i?[0,i+f|0]:d;case
1:var
j=d[2],k=d[1];return[1,k,a(fI(f,e),j)];case
2:return[2,g(c[h][2],f,e+1|0,d[1])];default:var
l=a(c[9],d[1]),m=g(c[h][2],f,e+1|0,l);return[3,b(c[65],w[16],m)]}}function
fI(c,b){function
d(a){return fH(c,b,a)}return a(e[17][15],d)}function
cG(b,a){return fH(b,0,a)}function
bE(c,b){return a(fI(c,0),b)}function
iz(m,d,l,k){var
o=k[1],f=l[1],u=k[2],v=l[2],h=m?m[1]:a(y[2],0),p=a(e[17][1],f),i=a2(p,0);function
w(c,b){var
d=b-1|0,e=S(i,d)[d+1];if(e)return c===e[1]?0:a(C[2],re);var
f=b-1|0;return S(i,f)[f+1]=[0,c]}function
j(a,f,e){if(b(c[44],d,e)){var
g=b(c[65],d,e);if(a<g)if(g<=(p+a|0))return[0,g-a|0,f];return f}function
h(a){return a+1|0}return cY(q[23],d,h,j,a,f,e)}function
x(c,b){var
d=j(0,0,c),f=j(0,0,b);try{var
h=g(e[17][20],w,d,f);return h}catch(b){b=E(b);if(b[1]===a7)return a(C[2],rf);throw b}}var
r=b(c[H],f,h),s=b(c[H],o,h);function
t(d,c,a){var
e=b(bd[8][12],bd[9],bd[8][2]);return n(U[15],e,d,c,a)}function
z(f,e){var
a=dm(rg,r,d,f),g=a[2],b=dm(rh,s,a[1],e),c=b[1],h=b[2],i=t(r,c,g);return x(i,t(s,c,h))}g(e[17][20],z,v,u);function
A(b){return b?[0,b[1]]:a(C[2],ri)}var
B=b(e[19][15],A,i);return aR(0,h,d,f,a(e[19][11],B),o)}function
iA(b){var
a=b;for(;;)switch(a[0]){case
0:return a[1];case
1:return a[1];case
2:return a[1];case
3:return a[1];case
4:var
a=a[2];continue;default:return a[1]}}function
rj(d,c,b){return bk(d,a(e[8],c),b)}function
dq(c){var
d=a(e[17][1],c);return b(q[9],0,d)}function
fJ(d){var
c=a(e[17][1],d);function
f(a){return[0,c-a|0]}return b(F[54],c,f)}function
iB(a){function
c(a){return[0,a+1|0]}return b(F[54],a,c)}var
rk=O[2][1];function
rl(c,a){return b(O[2][4],a,c)}var
iC=b(e[17][18],rl,rk);function
ek(f,d){var
c=b(e[17][h],f,d),a=c[2],g=c[1];if(a)return[0,g,a[1],a[2]];throw[0,a7,rm]}function
fK(g,f){var
d=0,c=g,b=f;for(;;){if(0===c){if(b){var
h=b[2],i=b[1];return[0,h,i,a(e[17][9],d)]}}else
if(b){var
d=[0,b[1],d],c=c-1|0,b=b[2];continue}throw[0,a7,rn]}}function
fL(d,c){var
f=a(e[17][1],d);function
g(a){return c+(a+1|0)|0}return a(iC,b(F[54],f-c|0,g))}function
iD(d,h){var
e=b(c[3],d,h);if(8===e[0]){var
f=t(aE),i=e[2],j=u===f?aE[1]:k===f?a(s[2],aE):aE;return g(c[ba],d,j,i)}return 0}function
iE(d,c){var
f=O[2][1],g=1;function
h(f,c,e){return iD(d,a(bj,e))?b(O[2][4],f,c):c}return n(e[17][90],h,g,f,c)}function
iF(j,i,f,d,g){var
k=a(ah,b(e[17][7],f,d-1|0)),m=k[3],n=k[2],o=a(c[h][1],d),l=b(G[16],o,n),p=b(c[h][1],d,m),q=l?bO(j,i,f,l[1],g):O[2][1],r=bO(j,i,f,p,g),s=b(O[2][7],q,r),t=a(O[2][5],d);return b(O[2][7],t,s)}function
bO(f,c,j,e,d){var
h=b(q[36],c,e);if(b(O[2][3],d,h))var
k=g(d9,f,c,e),i=b(q[36],c,k);else
var
i=h;var
l=O[2][1];function
m(b){var
e=iF(f,c,j,b,d);return a(O[2][7],e)}return g(O[2][15],m,i,l)}function
ro(i,a,f){var
d=O[2][1],j=1;function
k(d,a,e){var
j=e[3],k=b(c[h][1],-d|0,f);return g(q[37],i,k,j)?a:b(O[2][4],d,a)}return n(e[17][90],k,j,d,a)}function
iG(i,f,d){var
j=[0,f,1,0];function
k(j,d){var
f=d[2],g=d[1],k=d[3],e=a(ah,j),l=e[3],m=e[2],o=e[1],p=a(c[9],f),r=[0,an(o,m,n(q[51],i,g,p,l)),k];return[0,b(c[h][1],1,g),f+1|0,r]}return g(e[17][19],k,d,j)[3]}function
fM(y,x,r,d,i,f,p){var
G=y?y[1]:1,H=x?x[1]:0;bO(r,d,i,p,f);var
I=iE(d,i),J=bO(r,d,i,p,f),K=b(O[2][7],J,I),L=G?fL(i,f):a(O[2][5],f),s=b(O[2][7],L,K),M=1;function
N(a,c){if(b(O[2][3],a,s))if(a<f){var
e=f-a|0;return b(bb,function(a){var
c=b(q[36],d,a);return b(O[2][3],e,c)?g(d9,r,d,a):a},c)}return c}var
t=g(e[17][75],N,M,i),P=a(e[17][1],t),z=a(O[2][20],s),n=1,u=1,m=0,o=1,l=0,k=0,j=t,Q=P-z|0;for(;;){if(j){var
A=j[2],B=j[1];if(b(O[2][3],n,s)){var
n=n+1|0,R=[0,[0,u],k],u=u+1|0,m=[0,B,m],l=df(a(c[9],(z+Q|0)-(o-1|0)|0),l),k=R,j=A;continue}var
n=n+1|0,m=df(c[14],m),S=[0,[1,o],k],o=o+1|0,l=[0,B,l],k=S,j=A;continue}var
v=a(e[17][9],l),C=a(e[17][9],m),w=a(e[17][1],v),D=a(e[17][9],k),T=1,U=function(b,a){return 0===a[0]?[0,a[1]+w|0,b]:[0,a[1],b]},V=g(e[17][75],U,T,D),W=function(a){return 0===a[0]?[0,a[1]+w|0]:[0,a[1]]},E=b(e[17][15],W,D);if(H)var
X=bk(d,E,p),Y=iG(d,b(c[h][1],-w|0,X),v),F=b(e[18],Y,C);else
var
F=b(e[18],v,C);return[0,[0,F,E,t],V]}}function
fN(m,f,o,l,u,E){var
G=u?u[1]:fL(o,l),H=bO(m,f,o,E,l),p=b(O[2][7],G,H),I=1;function
J(a,c){if(b(O[2][3],a,p))if(a<l){var
d=l-a|0;return b(bb,function(a){var
c=b(q[36],f,a);return b(O[2][3],d,c)?g(d9,m,f,a):a},c)}return c}var
n=g(F[75],J,I,o),v=a(D[1][4],n),w=v-a(O[2][20],p)|0,r=a2(v,rp),d=1,t=0,s=0,i=1,k=0,j=n;for(;;){if(j){var
x=j[2],K=j[1],y=b(bb,a(c[h][1],d),K);if(b(O[2][3],d,p)){var
z=(d+w|0)-i|0;S(r,z)[z+1]=[0,d];var
L=[0,[0,((w+d|0)-i|0)+1|0],k],d=d+1|0,t=[0,y,t],k=L,j=x;continue}var
A=i-1|0;S(r,A)[A+1]=[0,d];var
d=d+1|0,s=[0,y,s],M=[0,[0,i],k],i=i+1|0,k=M,j=x;continue}var
B=a(F[9],k),N=b(e[18],t,s),P=a(F[9],N),Q=a(e[19][11],r),R=1,T=function(d,a){return b(bb,function(e){var
a=bk(f,B,e);return b(c[h][1],-d|0,a)},a)},C=g(F[75],T,R,P),U=aR(0,m,f,C,B,n);return[0,U,aR(0,m,f,n,Q,C)]}}function
iH(b){var
c=fJ(b);return a(e[17][9],c)}function
$(a){return[0,a,iH(a),a]}function
iI(h,k,j,i){try{var
d=[0,h,1],u=function(f,l,e){var
h=e[2],d=e[1];if(h){var
i=a(bj,f),j=a(bj,l),m=g(c[94],k,i,j),n=m||M(U[79],0,d,k,i,j);return[0,b(c[bt],f,d),n]}return[0,d,h]},v=n(e[17][24],u,j,i,d)[2];return v}catch(d){d=E(d);if(d[1]===a7)return 0;var
l=a(g4[1],d),m=b(c[H],i,h),o=a(q[g1],m),p=a(f[49],o),r=b(c[H],j,h),s=a(q[g1],r),t=a(f[49],s);n(g5[3],rq,t,p,l);throw d}}function
iJ(d,c,g,e){if(iI(d,c,g[3],e[1]))return 0;var
h=ay(d,c,e),i=a(f[3],rr),j=ay(d,c,g),k=a(f[3],rs),l=b(f[12],k,j),m=b(f[12],l,i);return bM(rt,b(f[12],m,h))}function
ai(g,f,e,c,b){var
i=b[3],j=b[2],k=c[2],l=c[1],h=g?g[1]:0,d=e?e[1]:w[16];if(1-h)iJ(f,d,c,b);return aR([0,h],f,d,l,a(ej(d,k),j),i)}function
iK(f,c,a){var
d=a[2],g=a[3],h=a[1],i=b(bb,function(a){return bk(f,d,a)},c),j=[0,c,g],k=1;function
l(a){return cG(k,a)}return[0,[0,i,h],[0,ru,b(e[17][15],l,d)],j]}function
iL(d,a,c,b){function
f(c,b){return iK(a,c,b)}return dp(0,d,a,g(e[17][19],f,b,c))}function
cH(p,k,f,d,o,i){var
q=p?p[1]:0,j=bc(o),u=a(c[9],d);if(g(c[94],f,j,u))return $(i);if(n(c[h][14],f,1,d,j)){var
v=d6(d,j,i),w=function(b){var
a=b+1|0;return a===d?cG(-1,o):d<a?[0,a-1|0]:[0,a]},x=a(e[17][1],i);return aR([0,q],k,f,v,b(F[54],x,w),i)}var
l=fM(0,0,k,f,i,d,j)[1],m=l[2],y=l[3],z=l[1],r=b(e[17][7],m,d-1|0),s=0===r[0]?r[1]:a5(rx),t=bk(f,m,j),A=d6(s,t,z),B=1;function
C(d,a){return fG(f,s,b(c[h][1],-1,t),a)}return aR([0,q],k,f,A,g(e[17][75],C,B,m),y)}var
bl=[e6,ry,e3(0)],aS=[e6,rz,e3(0)];function
el(f,c,i,h,d,b){if(d){if(b){var
k=b[2],l=d[2],g=iM(f,c,i,h,d[1],b[1]),m=g[2],n=g[1],o=function(a){return bk(c,m,a)},j=a(e[17][15],o),p=a(j,l);return ai(0,f,[0,c],el(f,c,i,n,p,a(j,k)),g)}}else
if(!b)return $(h);throw bl}function
iM(i,a,h,f,e,d){if(g(c[94],a,e,d))return $(f);var
j=b(c[3],a,e);if(0===j[0]){var
k=j[1];if(b(O[2][3],k,h))return cH(0,i,a,k,[2,d],f);throw aS}var
l=b(c[3],a,d);if(0===l[0]){var
m=l[1];if(b(O[2][3],m,h))return cH(0,i,a,m,[2,e],f);throw aS}var
n=b(c[82],a,e),o=n[1],r=n[2],p=b(c[82],a,d),q=p[1],s=p[2];if(b(c[56],a,o))if(b(c[56],a,q)){if(g(c[94],a,o,q))return el(i,a,h,f,r,s);throw bl}throw aS}function
iN(c,a){var
d=[0,1,O[2][1]];function
f(c,e,f){var
d=c[2],a=c[1];return 2===e[0]?[0,a+1|0,b(O[2][4],a,d)]:[0,a+1|0,d]}return n(e[17][23],f,d,c,a)[2]}function
iP(a){var
c=O[2][1];function
d(c,a){var
d=iO(a);return b(O[2][7],c,d)}return g(e[17][18],d,c,a)}function
iO(b){switch(b[0]){case
0:return a(O[2][5],b[1]);case
1:return iP(b[2]);default:return O[2][1]}}function
em(a){return 3===a[0]?1:0}function
en(c,a){if(c){if(a){var
i=a[2],j=a[1],k=c[2],l=c[1];try{var
t=[0,iQ(l,j)],d=t}catch(a){a=E(a);if(a!==aS)throw a;var
d=0}try{var
s=[0,en(k,i)],f=s}catch(a){a=E(a);if(a!==aS)throw a;var
f=0}if(d)if(f){var
g=f[1],h=d[1],m=g[2],n=g[1],o=h[2],p=h[1],q=b(e[18],h[3],g[3]),r=b(e[18],o,m);return[0,b(e[18],p,n),r,q]}throw aS}}else
if(!a)return rA;throw bl}function
iQ(d,a){var
c=d[2],f=d[1];switch(c[0]){case
0:var
g=c[1];if(2!==a[0])return[0,[0,[0,g,a],0],0,0];break;case
1:var
j=c[3],k=c[2],l=c[1];switch(a[0]){case
1:var
m=a[2];if(b(i[46],l,a[1][1]))return en(j,b(e[17][h],k,m)[2]);throw bl;case
2:break;default:throw aS}break;default:return[0,0,[0,[0,c[1],a],0],0]}return[0,0,0,[0,[0,[0,f,c],a[1]],0]]}function
iR(d,c){var
f=c[2];try{var
g=a(e[17][9],f),h=function(a){return 1-em(a)},i=[0,en(d,b(e[17][33],h,g))];return i}catch(a){a=E(a);if(a===bl)return 0;if(a===aS)return 1;throw a}}function
eo(c,a){if(c){if(a){var
i=a[2],j=a[1],k=c[2],l=c[1];try{var
q=[0,iS(l,j)],d=q}catch(a){a=E(a);if(a!==aS)throw a;var
d=0}try{var
p=[0,eo(k,i)],f=p}catch(a){a=E(a);if(a!==aS)throw a;var
f=0}if(d)if(f){var
g=f[1],h=d[1],m=g[1],n=h[1],o=b(e[18],h[2],g[2]);return[0,b(e[18],n,m),o]}throw aS}}else
if(!a)return rC;throw bl}function
iS(c,d){var
a=d[2];switch(c[0]){case
0:return[0,[0,[0,c[1],a],0],0];case
1:var
f=c[2],g=c[1][1];switch(a[0]){case
0:return[0,0,[0,[0,a[1],c],0]];case
1:var
j=a[3],k=a[2];if(b(i[46],a[1],g))return eo(b(e[17][h],k,f)[2],j);throw bl}break;case
2:return rB}throw aS}function
iT(d,c){var
f=d[2];try{var
g=a(e[17][9],f),h=function(a){return 1-em(a)},i=[0,eo(b(e[17][33],h,g),c)];return i}catch(a){a=E(a);if(a===bl)return 0;if(a===aS)return 1;throw a}}function
fO(k,f,p,j){var
q=b(c[H],f,k),l=[0,0,0,0,0,i[1][10][1]];function
m(a,j){var
f=j[2],d=j[1],k=a[5],e=a[4],l=a[3],m=a[2],o=a[1],g=bc(f);if(0===f[0]){var
r=f[1];return[0,o,m,[0,[0,r,d],l],e,b(i[1][10][4],d,k)]}var
s=n(ao[3],0,q,p,g),t=b(i[1][10][4],d,k),u=b(c[h][1],e,s);return[0,[0,an([0,d],[0,b(c[h][1],e,g)],u),o],[0,g,m],l,e+1|0,t]}var
d=g(e[17][18],m,l,j),o=d[5],r=d[3],s=d[2],t=d[1],u=[0,o,a(e[17][1],f),0];function
v(m,c){var
h=c[3],d=c[2],f=c[1],g=a(ah,m),j=g[3],k=g[2],n=g[1];try{var
p=[0,f,d-1|0,[0,an([0,b(e[17][36],d,r)],k,j),h]];return p}catch(a){a=E(a);if(a===W){var
l=b(aU[28],n,f),o=[0,an([0,l],k,j),h];return[0,b(i[1][10][4],l,f),d-1|0,o]}throw a}}return[0,s,t,g(e[17][19],v,f,u)[3]]}function
ep(o,k,j,n,i){var
p=b(c[H],k,j);function
q(f,d){var
e=d[2],g=d[1],i=a(ah,f)[2],j=a(G[7],i);return[0,[0,b(c[h][1],-e|0,j),g],e+1|0]}var
l=g(e[17][19],q,i,rD),d=l[2],r=l[1];function
s(a){var
b=a[1];return[0,b,cG(d,a[2])]}var
t=b(e[17][15],s,n),f=fO(j,b(e[18],i,k),o,t),m=f[2],u=f[3],v=f[1],w=a(e[17][1],m),x=b(e[18],m,u),y=a(c[h][1],-d|0),z=b(e[17][15],y,v);return[0,x,p,w+d|0,b(e[18],z,r)]}function
iU(e,a,k,d,j,i){var
f=d[4],l=d[1],p=d[3],q=d[2];if(i){var
r=b(c[h][1],p,i[1]),m=b(A[35],a[1],r),s=function(a){var
d=b(c[H],l,e);return M(a6[18],d,a,[0,k],j,m)},t=b(A[64],s,a)[1];a[1]=aF(ac[29],0,[0,ac[17]],0,0,0,e,a[1]);var
u=g(c[h][3],f,0,t),v=b(A[35],a[1],u),w=g(c[h][3],f,0,m);return[0,v,b(A[35],a[1],w)]}function
x(a){var
d=b(c[H],l,e);return n(a6[17],d,a,[0,k],j)}var
y=b(A[64],x,a)[1],z=g(c[h][3],f,0,y);a[1]=aF(ac[29],0,[0,ac[17]],0,0,0,e,a[1]);var
o=b(A[35],a[1],z);return[0,o,M(ax[2],0,0,q,a[1],o)]}function
fP(b,h,a,g,f,e,d,c){var
i=g[3];return iU(b,a,i,ep(a,h,b,e,d),c,f)}function
iV(j,d,f,N,u,t){try{var
v=a(q[76],t),w=a(q[76],f),k=[0,b(e[18],w,v)],x=function(c){var
d=a(a6[27],c);return d?d:b(e[17][29],c,k[1])},l=function(c){var
a=b(aU[24],c,x);k[1]=[0,a,k[1]];return a},i=b(c[H],f,j),m=ea(i,d[1],u),y=m[2],o=di(m[1]),p=o[1],z=o[2],A=b(a9[10],i,d[1]),r=b(e[17][15],A,y),B=b(a9[10],i,d[1]),s=b(e[17][15],B,z),C=b(e[18],s,r),D=[0,a(c[26],p),C],F=a(c[34],D),G=bK(d[1],p),I=b(Z[3],i,G),J=function(k,u){var
v=a(c[8],u),w=g(q[58],d[1],v,s),m=b(c[90],d[1],w),x=m[2],y=m[1],z=0;function
A(k,e){var
f=a(ah,k),g=f[3],h=f[2],j=f[1];if(j)return[0,an([0,l(j[1])],h,g),e];var
m=d[1],o=b(c[H],e,i);return[0,an([0,l(n(aU[9],o,m,g,0))],h,g),e]}var
f=g(e[17][19],A,y,z),o=b(c[H],f,j),p=ea(o,d[1],x),B=p[2],r=di(p[1]),t=r[2],h=r[1],C=dq(f),D=b(e[18],t,C),E=[0,a(c[29],[0,h,k+1|0]),D],F=a(c[34],E),G=dn(dq(f)),I=fJ(f),J=dn(t),K=b(e[18],J,I);return[0,o,f,F,[1,[0,[0,h[1],k+1|0],h[2]],K],G,B]},K=b(e[19][16],J,I),L=function(g){var
k=g[2],m=g[6],n=g[5],o=g[4],p=g[3];a(e[17][1],f);var
i=a(e[17][1],k),l=b(e[18],k,f);try{var
q=a(c[h][1],i),s=b(e[17][15],q,r),t=bE(i,dn(dq(f))),u=iN(b(e[18],t,n),l),v=[0,[0,el(j,d[1],u,l,s,m),i,p,o]];return v}catch(a){a=E(a);if(a===bl)return 0;if(a===aS)return 1;throw a}},M=[0,[0,F,b(e[19][15],L,K)]];return M}catch(a){a=E(a);if(a===W)return 0;throw a}}function
iW(d,c){var
f=c[2];function
g(j,o){var
a=o;for(;;){if(j){if(a){var
c=a[1],p=j[2],q=j[1];if(3===c[0]){var
a=a[2];continue}var
r=g(p,a[2]),f=q[2];switch(f[0]){case
0:var
d=0;break;case
1:var
k=f[3],l=f[2],m=f[1];switch(c[0]){case
0:var
d=[0,c[1],0];break;case
1:var
n=c[2],d=b(i[46],m,c[1][1])?g(k,b(e[17][h],l,n)[2]):0;break;default:var
d=0}break;default:var
d=0}return b(e[18],d,r)}}else
if(!a)return 0;return 0}}return g(d,a(e[17][9],f))}function
eq(e,d){var
f=a(b8,b(c[118],d,e));return a(K[10][8],f)}function
rE(c){var
d=a(q[114],c);function
e(b){return a(f[3],rF)}return b(f[39],e,d)}function
iX(f,e){var
c=f,a=e;for(;;){if(c){if(a){var
g=a[2],h=c[2],d=b(aJ[3],c[1],a[1]);if(d){var
c=h,a=g;continue}return d}}else
if(!a)return 1;return 0}}function
fQ(a){function
d(a){var
f=a[8],g=a[7],i=a[1],d=b(e[17][15],aZ,a[4]),j=b(c[h][11],d,f);return an([0,i],[0,b(c[h][11],d,g)],j)}return b(e[17][15],d,a)}function
fR(j,d,h,l){var
o=h?h[1]:0;function
m(b){return o?b:a(f[7],0)}function
n(d,c,b){return fF(d,c,a(e[8],b))}function
k(h){switch(h[0]){case
0:var
s=h[4],z=h[2],o=h[1],D=h[3],E=a(e[7],o),p=b(c[H],E,j),F=function(c){var
e=k(c[9]),h=a(f[5],0),j=a(f[3],rG),l=g(q[V],p,d,c[8]),m=a(f[3],rH),n=a(i[1][9],c[1]),o=a(f[3],rI),r=b(f[12],o,n),s=b(f[12],r,m),t=b(f[12],s,l),u=b(f[12],t,j),v=b(f[12],u,h),w=b(f[12],v,e);return b(f[26],2,w)},G=g(f[39],f[5],F,z),I=fQ(z),t=b(c[H],I,p),J=ay(j,d,o),K=a(f[3],rJ),L=m(b(f[12],K,J)),M=a(f[5],0);if(0===s[0])var
N=s[1],O=g(q[V],t,d,D),Q=a(f[3],rK),R=m(b(f[12],Q,O)),S=g(q[V],t,d,N),T=a(f[3],rL),U=n(p,d,o),W=b(f[12],U,T),X=b(f[12],W,S),A=b(f[12],X,R);else
var
_=eq(t,s[1]),$=a(f[3],rM),aa=n(p,d,o),ab=b(f[12],aa,$),A=b(f[12],ab,_);var
Y=b(f[12],A,M),Z=b(f[12],Y,G);return b(f[12],Z,L);case
1:var
u=h[1],ac=h[4],ad=h[3],ae=h[2],af=a(e[7],u),v=b(c[H],af,j),ag=a(f[7],0),ah=function(e,c){if(c)var
d=k(c[1]);else
var
h=a(f[5],0),i=a(f[3],rN),d=b(f[12],i,h);var
g=b(f[23],2,d);return b(f[12],e,g)},ai=g(e[19][17],ah,ag,ac),aj=a(f[13],0),ak=ay(j,d,u),al=a(f[3],rO),am=g(q[V],v,d,ad),an=a(f[3],rP),ao=b(f[12],an,am),ap=b(f[12],ao,al),aq=b(f[12],ap,ak),ar=m(b(f[12],aq,aj)),as=a(f[5],0),at=eq(v,ae),au=a(f[3],rQ),av=n(v,d,u),aw=b(f[12],av,au),ax=b(f[12],aw,at),az=b(f[12],ax,as),aA=b(f[12],az,ar);return b(f[12],aA,ai);case
2:var
B=h[1],aB=h[6],aC=a(e[7],B);b(c[H],aC,j);var
aD=a(f[7],0),aE=function(c,a){var
d=k(a[5]);return b(f[12],c,d)},aF=g(e[17][18],aE,aD,aB),aG=ay(j,d,B),aH=a(f[3],rR),aI=a(f[3],rS),aJ=b(f[12],aI,aH),aK=b(f[12],aJ,aG),aL=b(f[12],aK,aF);return b(f[26],2,aL);case
3:var
aM=h[1],aN=k(h[2]),aO=a(f[5],0),aP=ay(j,d,aM),aQ=a(f[3],rT),aR=b(f[12],aQ,aP),aS=b(f[12],aR,aO),aT=b(f[12],aS,aN);return b(f[26],2,aT);case
4:var
aU=h[1],aV=k(h[2]),aW=a(f[5],0),aX=a(i[1][9],aU),aY=a(f[3],rU),aZ=b(f[12],aY,aX),a0=b(f[12],aZ,aW),a1=b(f[12],a0,aV);return b(f[26],2,a1);default:var
l=h[2],w=h[1],x=l[8],C=l[7],y=l[1],a2=h[3],a3=l[10],a4=l[3],a5=l[2],a6=y[3],a7=y[2],a8=y[1],a9=a(e[7],w),r=b(c[H],a9,j),a_=k(a2),a$=a(f[14],0),ba=ay(j,d,l[9]),bb=a(f[3],rV),bc=a(f[13],0),bd=ay(j,d,C),be=a(f[3],rW),bf=a(f[13],0),bg=a(e[7],x),bh=eq(b(c[H],bg,j),a4),bi=a(f[3],rX),bj=a(f[13],0),bk=a(f[13],0),bl=a(e[7],x),bm=b(c[H],bl,j),bn=g(q[V],bm,d,a3),bo=a(f[3],rY),bp=ay(j,d,x),bq=a(f[3],rZ),br=a(f[13],0),bs=ay(j,d,w),bt=a(f[3],r0),bu=a(f[3],r1),bv=g(q[V],r,d,a5),bw=a(f[3],r2),bx=g(q[V],r,d,a6),by=a(f[3],r3),bz=b(f[12],by,bx),bA=b(f[12],bz,bw),bB=b(f[12],bA,bv),bC=b(f[12],bB,bu),bD=b(f[12],bC,bt),bE=b(f[12],bD,bs),bF=b(f[12],bE,br),bG=b(f[12],bF,bq),bH=b(f[12],bG,bp),bI=b(f[12],bH,bo),bJ=b(f[12],bI,bn),bK=b(f[12],bJ,bk),bL=b(f[12],bK,bj),bM=b(f[12],bL,bi),bN=b(f[12],bM,bh),bO=b(f[12],bN,bf),bP=b(f[12],bO,be),bQ=b(f[12],bP,bd),bR=b(f[12],bQ,bc),bS=b(f[12],bR,bb),bT=b(f[12],bS,ba),bU=m(b(f[12],bT,a$)),bV=P(d,C,a7),bW=g(q[V],r,d,bV),bX=a(f[3],r4),bY=a(i[1][9],a8),bZ=a(f[3],r5),b0=n(r,d,w),b1=b(f[12],b0,bZ),b2=b(f[12],b1,bY),b3=b(f[12],b2,bX),b4=b(f[12],b3,bW),b5=b(f[12],b4,bU),b6=b(f[12],b5,a_);return b(f[26],2,b6)}}return k(l)}function
r7(d){var
c=a(y[2],0),e=fR(c,a(w[17],c),0,d);return b(f[48],r6[9][1],e)}function
r8(f,d,k,a){function
g(d,a){var
i=b(c[3],f,a);if(0===i[0]){var
j=i[1]-d|0;if(0<=j)try{var
l=b(e[17][36],j,k),m=b(c[h][1],d,l);return m}catch(b){b=E(b);if(b===W)return a;throw b}return a}function
n(a){return a+1|0}return M(c[cm],f,n,g,d,a)}return g(d,a)}function
r9(a){var
c=a[2];function
d(a){switch(a[0]){case
0:return 1;case
1:return 0;default:return 1}}return b(e[17][25],d,c)}function
fS(c){var
d=[0,i[1][10][1],0];function
f(h,d){var
e=d[1],j=d[2],c=a(ah,h),f=c[1],k=c[3],l=c[2];if(f){var
g=b(aU[25],f[1],e),m=[0,an([0,g],l,k),j];return[0,b(i[1][10][4],g,e),m]}throw[0,Q,r_]}return g(e[17][19],f,c,d)[2]}function
er(l,k,r){var
c=l[2],d=l[1],f=fK(k-1|0,r),g=f[3],m=f[2],i=f[1],j=a(ah,m),n=j[1],s=j[2],o=iV(d,c,i,n,j[3],g);function
t(a){if(typeof
a==="number"){if(0===a)return 0;throw[0,Q,r$]}var
f=a[1],j=f[1],k=j[2],l=f[2],n=j[1],o=ei(c[1],k,f[4]),p=fS(n),q=[0,p,[0,o,b(e[17][h],l,k)[2]],[0,m,i]],r=dp(0,d,c[1],q);return[0,iL(d,c[1],r,g)]}if(o){var
p=o[1],q=p[2],u=p[1],v=function(a){return 1===a?1:0};if(b(e[19][29],v,q))return 0;var
w=[0,an(n,s,u),i],x=b(e[18],g,w),y=b(e[19][15],t,q);return[0,[0,k,fS(x),y]]}return 0}function
iY(f,c){function
g(a){return a+1|0}var
h=a(e[17][1],c),i=b(F[54],h,g);function
j(d){var
a=er(f,d,c);if(a){var
e=a[1][3],g=function(a){return 0===a?1:0};return b(bF[31],g,e)}return 0}var
d=b(e[17][33],j,i);return d?[0,d[1]]:0}function
iZ(c){function
f(d,c){function
h(d,c){switch(d[0]){case
0:return[0,[0,d[1],0],c];case
1:var
g=f(0,a(e[17][9],d[2]));return b(e[18],g,c);case
2:return c;default:return[0,[0,d[1],1],c]}}return g(e[17][19],h,c,d)}var
d=f(0,c);function
h(b,a){return b[1]-a[1]|0}return b(e[17][46],h,d)}function
sa(a){var
b=a[1];return a[2]?[3,b]:[0,b]}var
sb=a(e[17][15],sa);function
sc(e,d){return b(bb,a(c[h][1],e),d)}function
es(e){var
h=1;return function(j){var
c=h,a=j;for(;;){if(a){var
d=a[1],f=a[2],g=aZ(d);if(b(i[1][1],e,g))return[0,c,d];var
c=c+1|0,a=f;continue}throw W}}}function
i0(w,l,k,d){var
h=cy(0,function(a){throw[0,a7,sd]},k),i=h[3],m=h[1],n=a(c[67],l),j=b(e[17][15],n,m),o=0;function
p(d,c){return[0,a(es(b(e[17][7],j,d[1]-1|0)),i)[2],c]}var
f=g(e[17][19],p,d,o),q=1;function
r(g,e){var
c=a(es(e),f)[1];function
h(a){var
b=a[2];if(a[1]===g){var
d=b?[3,c]:[0,c];return[0,d]}return 0}return b(F[b2],h,d)}var
s=g(e[17][75],r,q,j),t=1;function
u(c,e){var
f=a(es(a(bD,e)[1]),i)[1];function
g(a){var
b=a[2];if(a[1]===f){var
d=b?[3,c]:[0,c];return[0,d]}return 0}return b(F[b2],g,d)}var
v=g(e[17][75],u,t,f);return[0,cz(f)[1],s,v]}function
i1(f,d,e){if(0===a(c[dQ],d))return b(c[H],f,d);var
g=hs(e),h=fw(fn,[0,hr(e)],g),i=b(c[b1],h,d);return b(c[H],f,i)}function
fT(f,d){function
g(e){var
g=a(fu,e),d=t(bC),h=b(c[5],f,g),i=u===d?bC[1]:k===d?a(s[2],bC):bC;return b(ar[11],i,h)}return b(e[17][c0],g,d)}function
fU(m,j,h,g){var
d=g[1],n=g[2],o=m[1],p=b(c[H],d,j),k=cE(j,h,d);if(a(e[17][53],d))var
l=k;else
var
v=a(f[5],0),w=a(f[3],sf),x=a(f[5],0),y=b(f[12],x,w),z=b(f[12],y,v),l=b(f[12],z,k);var
q=fF(p,h,n),r=a(f[3],se),s=a(i[1][9],o),t=b(f[12],s,r),u=b(f[12],t,q);return b(f[12],u,l)}function
sg(d,c){var
f=b(e[17][7],d,c-1|0),g=a(e[7],f);return a(K[10][16],g)}var
i2=a(e[17][19],c[b1]);function
fV(h,g){function
i(a){return 1-a[2]}var
c=b(e[17][33],i,g);if(c){var
d=c[1][1],j=d[1],k=dl(h,d),l=a(f[3],sh);return bL([0,[0,j],si,b(f[12],l,k)])}return 0}function
cI(l,d,r,bh,bg,J,x,w,p){var
M=bh,z=bg;for(;;){var
ap=x[3],aq=x[2],bi=x[1];if(z){var
ar=z[2],as=z[1],_=as[1],m=_[3],N=_[2],aa=_[1],ab=iR(N,x);if(typeof
ab==="number"){if(0===ab){var
M=[0,as,M],z=ar;continue}var
bj=iW(N,x),bl=0,bm=function(f,k){if(f)return f;var
h=er([0,l,d],k,a(e[7],x));if(h){var
c=h[1],i=[0,c[2],aq,ap],m=c[3],n=c[1];try{var
o=function(g,f){if(f){var
b=f[1],j=a(e[8],b),k=bk(d[1],j,p),m=a(e[8],b),n=cF(d[1],m,w),c=cI(l,d,r,0,g,J,ai(0,l,[0,d[1]],b,i),n,k);if(c){var
h=c[1];return[0,h[1],[0,h[2]]]}throw W}return[0,g,0]},q=a(e[17][9],M),s=b(e[18],q,z),j=g(e[19][63],o,s,m),t=[0,[0,j[1],[1,i,n,p,j[2]]]];return t}catch(a){a=E(a);if(a===W)return 0;throw a}}return 0};return g(e[17][18],bm,bl,bj)}var
ac=ab[1],v=[0,bi,aq,ap],B=ac[1],au=v[3],o=v[1],bv=ac[3],bw=ac[2],bx=v[2],F=i1(o,l,d),bA=function(i){var
c=i[2],e=i[1],h=fP(l,o,d,r,0,B,w,e)[1];if(2===c[0]){var
j=c[1],k=aF(U[83],0,0,0,F,d[1],h,j),C=k[1];if(k[2]){d[1]=C;return 0}var
D=g(q[V],F,d[1],j),E=a(f[3],sq),G=a(f[13],0),H=g(q[V],F,d[1],h),I=a(f[3],sr),J=b(f[12],I,H),K=b(f[12],J,G),L=b(f[12],K,E),M=b(f[12],L,D),N=a(cD[6],e);return g(af[6],N,ss,M)}var
m=bc(c),n=g(q[V],F,d[1],m),p=a(f[3],sn),s=a(f[13],0),t=g(q[V],F,d[1],h),u=a(f[3],so),v=b(f[12],u,t),x=b(f[12],v,s),y=b(f[12],x,p),z=b(f[12],y,n),A=a(cD[6],e);return g(af[6],A,sp,z)},bC=function(e){var
i=e[1],j=i[2],k=i[1],n=e[2];if(a(G[3],k))return 0;if(0===j[0])if(0!==j[2])return 0;var
m=ep(d,o,l,B,w),p=m[1],r=g(c[h][3],m[4],0,n),s=d[1],t=b(c[H],p,l),u=g(q[V],t,s,r),v=a(f[3],st),x=b(f[12],v,u);return g(af[6],k,su,x)};b(e[17][14],bA,bw);b(e[17][14],bC,bv);switch(m[0]){case
0:var
ax=m[2],bD=m[1],Z=ep(d,o,l,B,w),am=Z[4],ba=Z[2],ao=Z[1],cM=Z[3],bb=cy(0,function(a){throw[0,a7,sI]},ao),bd=bb[3],be=bb[1],cN=b(i2,bd,l),cO=function(i,r){var
m=r[1],s=m[1],f=s[2],t=i[5],j=i[4],o=i[2],C=r[2],D=m[4],E=m[3],F=s[1],G=i[3],H=i[1],I=n(a6[25],0,0,0,j),u=g(A[65],I,d,E)[2],v=u[1],K=u[2],L=v[2],M=a(a6[16],v[1]);function
N(a){return b(M,a,0)}var
O=g(A[65],N,d,D),w=de(o,am,L),P=a(e[17][1],w)+o|0,Q=g(c[h][3],am,P,O),x=b(A[41],d[1],w),p=b(A[35],d[1],Q),k=b(c[37],p,x),R=[0,f,0,aF(a6[3],j,d[1],0,0,[0,f,0],[0,k,0],[0,K,0])],y=$(x),S=a(c[67],d[1]),T=b(e[17][15],S,be),z=b(c[h][11],T,k),B=cA(t,d[1],[0,[0,[0,F],[3,sJ,[0,f]]]],z),l=B[2];d[1]=B[1];var
q=[0,b(c[75],d[1],l)[1],J],U=et(j,d,R,C,q,y,p),V=an([0,f],[0,l],z),W=fw(f,[0,b(c[h][4],be,l)],k);return[0,[0,V,H],o+1|0,[0,[0,f,q,q,bd,y,p,l,k,U],G],b(c[b1],W,j),t]},bf=g(e[17][18],cO,[0,ao,0,0,cN,ba],ax),cP=bf[3],cQ=bf[1];b(c[H],ao,l);var
bF=a(e[17][1],ax),bG=[0,b(c[h][1],bF,p)],aA=iU(l,d,a(e[9],r),[0,cQ,ba,cM,am],bD,bG),y=[0,[0,v,cP,aA[2],[0,aA[1]]]];break;case
1:var
aB=m[1],aC=aB[2],bH=aB[1],aw=b(e[17][36],aC,B);if(0===aw[0])var
aD=aw[1];else
var
by=a(i[1][9],aC),bz=a(f[3],sl),aD=bL([0,[0,bH],sm,b(f[12],bz,by)]);var
y=[0,[0,v,0,p,[1,aD]]];break;case
2:var
aE=m[3],aG=m[2],aH=m[1],bI=m[4],aI=aE?aE[1][2]:a(e[7],r),bJ=aG?hB(aH,aI,aG[1]):hA(aH,aI),aJ=cI(l,d,r,0,[0,[0,[0,aa,N,[4,[0,bJ],bI]],0],0],J,v,w,p);if(aJ)var
bK=aJ[1][2],y=[0,[4,a(e[7],r),bK]];else
var
y=0;break;case
3:var
bN=m[2],aK=fP(l,o,d,r,0,B,w,m[1]),aL=aK[2],ad=aK[1],ae=iZ(bx),aM=i0(l,d[1],o,ae),aN=aM[2],R=aM[1],O=dp(0,l,d[1],[0,R,aN,o]),bO=a(q[76],R),bP=a(i[1][10][35],bO),bQ=a(i[1][6],sv),aO=b(aU[25],bQ,bP),aP=[0,[0,[0,aO],P(d[1],O,aL)],R],bR=P(d[1],O,ad),ag=b(c[h][1],1,bR),aQ=fM(sx,sw,l,d[1],aP,1,ag),T=aQ[2],I=aQ[1],bS=function(a){return 1===a[2]?1:0},bT=b(e[17][31],bS,T)[1],bU=a(e[7],I),bV=h0(bT,P(d[1],I,ag),bU),bW=bE(1,aN),bX=a(e[8],I),bY=[0,bV,a(ej(d[1],bX),bW),R],aS=ai(0,l,[0,d[1]],bY,O),ah=a(e[7],I),bZ=dq(ah),b0=function(g){var
f=b(c[65],d[1],g),h=a(e[8],aS);function
i(a){return 3===a[0]?f===a[1]?1:0:0}return b(e[17][26],i,h)?[3,f]:[0,f]},aT=[0,ah,b(e[17][17],b0,bZ),ah],b2=b(c[H],aP,l),b3=P(d[1],O,p),b4=b(c[h][1],1,b3),b5=g(a9[9],b2,d[1],b4),b6=g(q[50],d[1],ag,b5),aW=P(d[1],I,b6),b7=function(b,a){return a[1]-b[1]|0},b8=b(e[17][46],b7,T),b9=function(a){return a[2]},b_=b(e[17][17],b9,b8),X=function(g,j){var
c=[0,-1],k=a(i[1][6],sy);function
q(e){c[1]++;var
d=a(C[21],c[1]);return b(K[5],k,d)}function
m(j){var
c=j[3],m=j[2],r=j[1],s=a(e[17][1],m)-g|0,n=b(e[17][h],s,m),k=n[2],o=n[1];if(k){var
t=k[2],u=k[1],p=iT(v,o);if(typeof
p==="number"){var
w=cC(l,o),x=a(f[5],0),y=a(f[3],sz),z=ay(l,d[1],v),A=a(f[13],0),B=a(f[3],sA),C=a(f[5],0),D=a(f[3],sB),F=b(f[12],D,C),G=b(f[12],F,B),H=b(f[12],G,A),I=b(f[12],H,z),J=b(f[12],I,y),K=b(f[12],J,x);return bM(sC,b(f[12],K,w))}var
L=p[1][1],M=function(a){if(1===a)return[0,u];function
c(b){var
c=b[1]===(a-1|0)?1:0,d=b[2],e=c?d:c;return e}if(b(e[17][26],c,ae))return 0;try{var
d=[0,[0,av,b(e[17][36],a-1|0,L)]];return d}catch(a){a=E(a);if(a===W)return[0,[0,av,[0,q(0),1]]];throw a}},N=b(e[17][70],M,b_);switch(c[0]){case
2:var
P=c[3],R=c[2],S=c[1],i=[2,S,R,P,X(g,c[4])];break;case
3:var
T=c[1],i=[3,T,X(g+1|0,c[2])];break;case
4:var
U=c[1],i=[4,U,X(g,c[2])];break;default:var
i=c}var
O=a(e[17][9],N);return[0,[0,r,b(e[18],O,t),i]]}throw[0,Q,sD]}return b(e[17][70],m,j)},b$=X(1,bN),ca=function(b,a){return a[1]-b[1]|0},aX=[0,0],cc=b(e[17][46],ca,T),cd=function(d){var
f=d[2],g=d[1];if(1===f){aX[1]=a(e[17][1],T)-g|0;return ad}var
h=b(e[17][7],ae,(f-1|0)-1|0)[1];return a(c[9],h)},ce=b(e[17][15],cd,cc),cf=aX[1],cg=a(az[9],l),ch=function(b){var
d=a(D[2][1][1],b);return a(c[10],d)},ci=b(e[17][15],ch,cg),cj=a(e[19][12],ci),aj=fe(0),aY=[0,aj,J],ck=a(e[17][1],w),cl=fO(l,o,d,B)[2],cm=aV(1,w),cn=aV(ck+1|0,cl),co=b(e[18],cn,cm),cp=a(e[8],I),cq=cF(d[1],cp,co),cr=function(a){return[0,a,0]},a0=cI(l,d,r,0,b(e[17][15],cr,b$),aY,aT,cq,aW);if(a0){var
a1=a0[1],cs=a1[2];fV(l,a1[1]);var
y=[0,[5,v,[0,[0,aO,ad,aL],p,cf,aY,aj,[0,a(c[12],[0,aj,cj]),ce],O,aT,aS,aW],cs]]}else
var
y=0;break;default:var
ak=m[1],ct=m[2],a3=g(A[62],F,d[1],p),cu=a3[2],al=a(c[l2],a3[1]),cv=fT(d[1],al)[1],cw=b(e[17][15],aZ,al),cx=a(i[1][10][35],cw),a4=0===ak[0]?n(L[13][25],i[1][11][1],cx,0,ak[1]):a(L[13][22],ak[1]),cB=a(c[116],al),a5=b(az[42],cB,l),a8=b(j[3],d[1],[0,[0,a5,cu],0]),a_=a8[1],Y=g(j[15],a5,a4,a8[2])[2],a$=a(j[1],Y),cE=a$[1];d[1]=a$[2];if(a(j[5],Y))var
cH=b(j[7],a_,Y),y=[0,[0,v,0,p,[0,a(e[17][5],cH)]]];else
var
cJ=function(n){var
O=b(i3[3][2],d[1],n),P=a(c[l2],O),R=b(i3[3][4],d[1],n),z=fT(d[1],P)[1],A=cz(z),g=A[1],p=b(c[h][11],A[2],R),q=b(c[3],d[1],p);if(9===q[0]){var
v=q[2],B=t(bB),T=q[1],U=u===B?bB[1]:k===B?a(s[2],bB):bB;if(fy(d[1],U,T)){var
x=S(v,1)[2],V=S(v,2)[3],y=cb(d[1],V);if(a(e[8],r)){var
W=b(c[82],d[1],x)[2],X=d[1],Y=function(a){return cb(X,a)},C=b(e[17][17],Y,W),Z=-1,_=function(a){return cG(Z,a)},ab=b(e[17][15],_,C),D=a(e[17][6],g),ac=d[1],j=a2(a(e[17][1],D),rv),M=function(b,a){switch(a[0]){case
0:var
c=a[1]-1|0;return S(j,c)[c+1]=[0,b+1|0];case
3:var
d=a[1]-1|0;return S(j,d)[d+1]=[3,b+1|0];default:throw[0,Q,rw]}};b(e[17][87],M,ab);var
E=aR(0,l,ac,o,a(e[19][11],j),D)[2],ad=[0,a(e[17][5],g),0],ae=cF(d[1],E,ad),F=b(e[18],ae,o),af=aR(0,l,d[1],g,[0,y,C],F),ag=3===y[0]?x:p,ah=iH(o),ai=1,aj=function(a){return cG(ai,a)},ak=b(e[17][15],aj,ah),al=aR(0,l,d[1],F,ak,o),am=a(e[17][6],g),G=[0,ag,af,al,[0,aR(0,l,d[1],o,E,am)]]}else
var
aG=iB(a(e[17][1],g)),L=[0,y,a(e[17][6],aG)],aH=a(e[17][6],g),G=[0,x,[0,g,L,g],[0,g,a(e[17][6],L),aH],0];var
i=G}else
var
aI=a(e[19][11],v),aJ=d[1],aK=function(a){return cb(aJ,a)},aL=[0,g,b(e[17][17],aK,aI),au],i=[0,p,aL,$(au),0];var
H=i[2],an=i[4],ao=i[3],ap=i[1],aq=function(a){return[0,a,0]},I=cI(l,d,r,0,b(e[17][15],aq,ct),J,H,w,ap);if(I){var
K=I[1],ar=K[2],as=K[1],at=function(b){if(0===a(fv,b)){var
d=aZ(b);return[0,a(c[10],d)]}return 0},av=b(e[17][70],at,z),aw=a(e[17][9],av);fV(l,as);return[0,n,aw,ao,an,ar]}var
ax=ay(l,d[1],H),az=a(f[3],sF),aA=dl(l,[0,aa,N,m]),aB=a(f[5],0),aC=a(f[3],sG),aD=b(f[12],aC,aB),aE=b(f[12],aD,aA),aF=b(f[12],aE,az);return fx(sH,b(f[12],aF,ax))}throw[0,a7,sE]},cK=b(e[17][15],cJ,cE),cL=a(j[70][8],a4),y=[0,[2,v,p,b(e[17][15],aZ,cv),cL,[0,a_,Y],cK]]}if(y){var
bn=y[1],bo=a(e[17][9],M);return[0,[0,b(e[18],bo,[0,[0,[0,aa,N,m],1],ar]),bn]]}return 0}var
at=iY([0,l,d],a(e[7],x));if(at){var
bp=[0,x,0,p,[1,at[1]]],bq=a(e[17][9],M);return[0,[0,b(e[18],bq,z),bp]]}var
br=fU(r,l,d[1],x),bs=a(f[5],0),bt=a(f[3],sj),bu=b(f[12],bt,bs);return bM(sk,b(f[12],bu,br))}}function
et(c,h,g,m,l,d,k){function
n(a){return[0,a,0]}var
i=cI(c,h,g,0,b(e[17][15],n,m),l,d,0,k);if(i){var
j=i[1],o=j[2];fV(c,j[1]);return o}var
p=fU(g,c,h[1],d),q=a(f[5],0),r=a(f[3],sK),s=b(f[12],r,q);return bM(sL,b(f[12],s,p))}aG(1052,[0,ij,ik,bc,dm,io,ip,iq,dn,ir,cb,it,iu,iv,cE,qW,ay,q2,iw,dp,aR,a8,eg,ra,ei,bk,ej,cF,P,fG,iy,fH,fI,cG,bE,iz,rE,iX,fR,r7,iA,rj,dq,fJ,iB,iC,ek,fK,fL,iD,iE,iF,bO,ro,iG,fM,fN,$,iI,iJ,ai,iK,iL,cH,bl,aS,iM,el,iN,iO,iP,em,iQ,en,iR,iS,eo,iT,fO,fP,iV,iW,eq,r8,r9,fS,er,iY,iZ,sb,sc,es,i0,i1,fT,fU,sg,i2,fQ,ep,cI,et],mt);function
i4(b,e){var
d=t(b),f=u===d?b[1]:k===d?a(s[2],b):b,g=[0,a(ar[10],f),e];return a(c[28],g)}function
i5(b,e){var
d=t(b),f=u===d?b[1]:k===d?a(s[2],b):b,g=[0,a(ar[8],f),e];return a(c[23],g)}function
cJ(e,d,b){var
f=[0,al(e,d),b];return a(c[21],f)}function
sM(d,c,b){return cJ(d,c,a(e[19][12],b))}function
sN(f,b){var
d=b[2],e=t(ad),g=[0,d,a(c[19],[0,b[1],d,b[3]])],h=u===e?ad[1]:k===e?a(s[2],ad):ad;return cJ(f,h,g)}function
eu(f,d,e,h){function
j(m,h,l,r){var
i=b(c[3],d[1],l);if(9===i[0]){var
e=i[2],n=t(aw),v=i[1],w=u===n?aw[1]:k===n?a(s[2],aw):aw;if(g(c[ba],d[1],w,v))if(4===e.length-1){var
x=S(e,1)[2],y=M(ax[2],0,0,m,d[1],x),f=b(c[3],d[1],y);if(6===f[0]){var
o=f[1],p=t(aC),z=f[3],A=f[2],B=u===p?aC[1]:k===p?a(s[2],aC):aC,q=t(ag),C=a(c[24],[0,B,h]),D=u===q?ag[1]:k===q?a(s[2],ag):ag,E=a(c[24],[0,D,h]),F=S(e,3)[4],G=_([0,o,0,A]),H=j(b(c[bt],G,m),E,F,z),I=S(e,0)[1];return[0,[0,o,S(e,2)[3],C,I],H]}throw[0,a7,sO]}}return[0,[0,0,l,h,r],0]}return j(f,h,e,M(ax[2],0,0,f,d[1],e))}function
ev(d,j){var
h=t(ad),l=u===h?ad[1]:k===h?a(s[2],ad):ad,e=b(c[3],d,j);if(9===e[0]){var
f=e[2],i=e[1];if(g(c[ba],d,l,i))if(2===f.length-1){var
m=b(c[76],d,i)[2],n=S(f,1)[2];return[0,[0,m,S(f,0)[1],n]]}}return 0}function
sP(j,d,g){var
f=b(c[3],j,d);switch(f[0]){case
11:var
h=f[1][1];break;case
12:var
h=f[1][1][1];break;default:return[0,d,g]}var
k=a(y[27],h)[1][7],i=b(e[19][51],k,g),l=i[2];return[0,a(c[21],[0,d,i[1]]),l]}function
cK(d,i){if(i){var
f=i[2],j=i[1];if(f){var
l=a(ah,j),m=l[3],w=l[1],x=a(e[17][1],f)+1|0,y=[0,m,0],z=function(e,l){var
m=e[2],n=e[1],f=a(ah,l),g=f[3],h=a(c[19],[0,f[1],g,n]),i=t(ad),o=[0,g,h],p=u===i?ad[1]:k===i?a(s[2],ad):ad,j=cJ(d,p,o),q=b(c[73],d[1],j)[1];return[0,j,[0,[0,b(c[76],d[1],q)[2],h],m]]},o=g(e[17][18],z,y,f),p=o[2],A=o[1],B=[0,a(c[9],1),2],C=function(g,f){var
e=f[2],l=f[1],m=g[1],i=b(c[h][1],e,g[2]),n=b(c[71],d[1],i)[2],j=t(aw),o=[0,n,i,a(c[9],e),l],p=u===j?aw[1]:k===j?a(s[2],aw):aw,q=[0,dh(d[1],[0,p,m]),o];return[0,a(c[21],q),e+1|0]},D=g(e[17][19],C,p,B)[1],E=[0,a(c[9],1),1,0],F=a(e[17][9],p),G=function(y,m,d){var
e=d[2],f=d[1],n=d[3],i=a(ah,m),j=t(aC),o=i[3],p=i[1],q=u===j?aC[1]:k===j?a(s[2],aC):aC,l=t(ag),r=a(c[24],[0,q,f]),v=u===l?ag[1]:k===l?a(s[2],ag):ag,w=a(c[24],[0,v,f]),x=[0,_([0,p,[0,r],g(c[h][2],1,e,o)]),n];return[0,b(c[h][1],1,w),e+1|0,x]},q=n(e[17][24],G,F,f,E),H=q[3],I=q[1];return[0,A,[0,_([0,w,[0,I],g(c[h][2],1,x,m)]),H],D]}var
r=a(ah,j),v=r[3],J=r[1],K=a(c[9],1),L=b(c[h][1],1,v);return[0,v,[0,_([0,J,[0,a(c[9],1)],L]),0],K]}throw[0,Q,sQ]}function
fW(ak,r,d,o,m){var
v=b(c[H],o,r),D=M(ax[2],0,0,v,d[1],m),E=g(U[68],v,d[1],D)[1],p=cK(d,d$(d[1],E)),j=p[2],l=p[1],F=p[3],f=a(e[17][1],j),I=a0(0,f),J=[0,b(c[h][1],f+1|0,m),I],K=a(c[21],J),L=b(c[37],K,j),N=[0,[0,a(i[1][6],sR)],l,L],w=a(c[19],N),x=t(ad),O=[0,l,w],P=u===x?ad[1]:k===x?a(s[2],ad):ad,y=t(aC),Q=cJ(d,P,O),R=u===y?aC[1]:k===y?a(s[2],aC):aC,z=t(ag),S=u===z?ag[1]:k===z?a(s[2],ag):ag;function
T(b){var
c=a(ah,b),d=a(e[8],c);return a(G[7],d)}var
V=g6(b(e[17][15],T,j));function
W(d){var
f=a(e[17][5],d),g=a(e[17][6],d);return b(c[h][4],g,f)}var
X=b(e[17][17],W,V),q=b(c[h][1],f+1|0,l),Y=dc(1,f+1|0,j),Z=a0(0,f),_=[0,b(c[h][1],2*(f+1|0)|0,m),Z],$=a(c[21],_),aa=b(c[37],$,Y),ab=[0,[0,a(i[1][6],sS)],q,aa],ac=a(c[19],ab);b(c[h][1],f,q);var
ae=a(c[9],1),B=t(aw),af=[0,q,ac,b(c[h][1],1,F),ae],ai=u===B?aw[1]:k===B?a(s[2],aw):aw,aj=cJ(d,ai,af),C=b(c[38],w,o);n(ao[3],0,r,d,C);a(A[48],d);return[0,l,C,o,X,R,S,aj,Q]}function
i6(b){return a(am[41],[2,b])}var
bP=[k,function(a){return ab(sU,sT)}],sX=[k,function(a){return ab(sW,sV)}],s0=[k,function(a){return ab(sZ,sY)}];function
dr(n,j,d){var
o=a(y[27],d[1])[1],p=bK(j,d),q=a(Z[41],p),g=d$(j,b(e[17][15],c[V],q)),r=o[7],k=a(e[17][1],g)-r|0;if(0===k)bL([0,0,s2,a(f[3],s1)]);var
l=b(e[17][h],k,g)[2],s=T(0,l),t=[0,a(c[26],d),s],u=a(c[21],t),v=T(0,g),w=[0,a(c[26],d),v],m=[0,j],x=a(c[21],w),i=fW(0,n,m,l,u);return[0,m[1],i[2],i[3],x,i[7],g,k,i[1]]}function
i7(e,d){var
f=b(c[5],e,d);return a(c[8],f)}function
fX(o,I,f,n){var
p=n[1],d=dr(o,I,[0,p,n[2]]),q=d[7],i=d[6],r=d[1],J=d[8],L=d[5],M=d[4],N=d[3],O=d[2],g=i6(p),P=b(a9[9],o,r),e=a(w[k8],r),v=i7(e,M),Q=i7(e,J),S=bu(b(K[5],g,s3),O,0,f,e,s4),x=b(K[5],g,s5),U=[0,_([0,[0,b(K[5],g,s6)],0,v]),i],V=bu(x,a(P,b(c[38],L,U)),0,f,e,s7);if(f)var
z=e;else
var
ai=a(y[2],0),z=a(w[17],ai);var
j=t(bP),E=u===j?bP[1]:k===j?a(s[2],bP):bP,l=a4(z,E),m=l[1],F=b(ac[11],m,l[2]),H=a(G[7],F)[2][1],A=a(y[2],0),B=cY(w[l9],0,0,0,A,m,[1,S]),W=B[2],C=cY(w[l9],0,0,0,A,B[1],[1,V]),X=C[2],Y=C[1],D=b(K[5],g,s8),Z=T(0,i),$=[0,a(c[8],X),Z],aa=[0,a(c[21],$),0],ab=T(q,N),ad=[0,a(c[8],W),ab],ae=[0,a(c[21],ad),aa],af=c7(D,f,Y,i,H,[0,v,[0,b(c[h][1],q,Q),ae]]),ag=[0,b(R[1],0,[1,x]),0];b(cL[2][88],1,ag);var
ah=[0,b(R[1],0,[1,D]),0];b(cL[2][88],1,ah);return af}function
s9(d,c,b,a){fX(d,c,b,a);return 0}b_([0,s_,function(a,b){return b9(s9,a,b)}]);function
i8(g,d,h){try{var
r=h7(g,d,[0,[0,av,0]],w[aY]),i=r[2][1],$=r[1];b(c[75],d,i);var
v=t(bP),aa=u===v?bP[1]:k===v?a(s[2],bP):bP,x=a4($,aa),ab=x[1],y=a(c[21],[0,x[2],[0,h,i]]),z=n(ac[30],0,g,ab,y),B=z[2],o=z[1],ad=b(c[73],d,y)[1],C=b(c[76],d,ad)[2],ae=[0,i5(sX,C),[0,h,i,B]],af=a(c[21],ae),ag=[0,i5(s0,C),[0,h,i,B]],ah=a(c[21],ag),ai=b(A[35],o,ah),aj=[0,o,b(A[35],o,af),ai];return aj}catch(i){i=E(i);if(i===W){var
D=b(c[5],d,h),p=b(cM[1],g,D),m=p[1],F=p[2],j=dr(g,d,c6(m)),l=j[1],G=j[6],H=j[5],I=j[2],J=a(f[3],s$),K=b(bi[66],g,m),L=a(f[3],ta),M=b(bi[66],g,m),N=a(f[3],tb),O=b(f[12],N,M),P=b(f[12],O,L),Q=b(f[12],P,K),R=b(f[12],Q,J);b(aq[8],0,R);var
S=[0,_([0,0,0,h]),G],T=b(c[38],H,S),q=b(e[17][15],c[8],F),V=b(U[55],l,[0,T,q]),X=b(A[35],l,V),Y=[0,I,a(e[19][12],q)],Z=a(c[21],Y);return[0,l,b(A[35],l,Z),X]}throw i}}function
i9(k,u,t,i,s){var
f=[0,s],h=eu(i,f,u,a(c[10],t));if(k)var
d=h;else{if(h)if(h[2])var
q=h[1],F=eu(i,f,q[2],q[3]),d=b(e[18],F,h),n=1;else
var
n=0;else
var
n=0;if(!n)var
d=h}if(k)var
m=d;else{if(d)if(d[2])var
p=d[1],E=eu(i,f,p[2],p[3]),m=b(e[18],d,E),o=1;else
var
o=0;else
var
o=0;if(!o)var
m=d}function
v(d){var
h=d[2],j=a(r[47],d[3]),e=b(c[5],f[1],h);return[0,g(tc[8],i,f[1],e),j]}var
w=b(e[17][15],v,m);function
y(a){return x(g(r[71],[0,a[1]],a[2],ds[6]))}var
z=k?e[17][17]:e[17][15],A=b(z,y,w),B=a(l[7],A),C=a(cx[11],f[1]),D=b(l[5],C,B);return b(j[70][1],0,D)}function
td(s,d,p,j){function
k(o,B,n,m,l,f,k){var
e=b(c[71],d,f),p=e[3],q=[0,_([0,e[1],0,e[2]]),0],i=[0,_([0,n,0,p]),q],r=a(c[9],1),s=a(c[9],2),t=b(c[h][1],2,f),u=[0,b(c[h][1],2,l),t,s,r],v=[0,i4(aw,m),u],j=a(c[21],v),w=[0,b(c[h][1],2,o),[0,j]],x=a(c[21],w),y=b(c[38],x,i),z=g(c[h][2],2,2,k),A=b(c[h][5],j,z);return[0,y,b(c[37],A,i)]}function
l(i,f){var
g=b(c[3],d,f);if(6===g[0]){var
t=g[3],u=g[1],m=ev(d,g[2]);if(m){var
j=m[1],n=k(i,f,u,j[1],j[2],j[3],t),o=n[2],p=n[1],h=b(c[3],d,o);if(6===h[0]){var
q=h[2],r=h[1],v=h[3],w=b(c[71],d,p),s=l(a(e[9],w),v),x=s[1],y=a(c[18],[0,r,q,s[2]]);return[0,a(c[19],[0,r,q,x]),y]}return[0,p,o]}return[0,i,f]}return[0,i,f]}var
f=b(c[3],d,j);if(6===f[0]){var
q=f[3],r=f[1],m=ev(d,f[2]);if(m){var
i=m[1],n=k(p,j,r,i[1],i[2],i[3],q),o=l(n[1],n[2]);return[0,[0,o[1],o[2]]]}return 0}return 0}function
fY(d,i,f){function
n(z,f){var
o=ev(d,f);if(o){var
j=o[1],i=j[3],p=j[2],A=j[1];if(b(c[52],d,i))var
q=b(c[71],d,i),l=q[1],r=q[3];else
var
I=[0,i,[0,a(c[9],1)]],l=0,r=a(c[21],I);var
v=n(l,r),w=v[1],B=v[2],m=a(e[17][1],w),C=a(c[9],m+1|0),D=b(c[h][1],m+1|0,i),E=[0,b(c[h][1],m+1|0,p),D,C,B],F=[0,i4(aw,A),E],G=a(c[21],F),H=[0,_([0,l,0,p]),0];return[0,b(e[18],w,H),G]}var
x=t(aB),J=u===x?aB[1]:k===x?a(s[2],aB):aB;if(g(c[ba],d,J,f)){var
y=t(bg),K=b(c[76],d,f)[2],L=u===y?bg[1]:k===y?a(s[2],bg):bg;return[0,0,dh(d,[0,L,K])]}var
M=a(c[9],1);return[0,[0,_([0,z,0,f]),0],M]}return n(i,f)}function
i_(v){function
d(d){var
w=a(j[66][4],d),x=a(j[66][5],d),h=a(j[66][6],d);function
y(b){var
d=t(bC),f=a(fu,b),i=u===d?bC[1]:k===d?a(s[2],bC):bC,e=g(c[ba],h,i,f);if(e)return e;var
j=aZ(b);return a(q[b2],j)}var
z=b(e[17][c0],y,w)[1],i=t(bg);function
A(d,v){var
w=d[3],x=d[2],y=d[1],h=a(bD,v),i=h[3],j=h[1],l=t(aw),z=u===l?aw[1]:k===l?a(s[2],aw):aw,m=a4(y,z),n=m[2],o=m[1],A=b(c[77],o,n)[2],p=[0,i,g(c[39],j,i,w)],B=[0,a(c[10],j),x],C=[0,n,b(e[19][5],p,B)],f=t(ad),D=a(c[21],C),q=u===f?ad[1]:k===f?a(s[2],ad):ad,r=[0,a(ar[9],q),A],E=[0,a(c[26],r),p];return[0,o,D,a(c[21],E)]}var
B=u===i?bg[1]:k===i?a(s[2],bg):bg,l=a4(h,B),m=t(aB),C=l[2],D=l[1],E=u===m?aB[1]:k===m?a(s[2],aB):aB,o=a4(D,E),f=g(d7,A,[0,o[1],C,o[2]],z),p=f[2],F=f[3],G=n(ao[2],0,x,f[1],p)[1],H=M(r[bZ],0,[0,v],p,[0,F],cu),I=a(j[64][1],G);return b(j[71][2],I,H)}return a(j[66][10],d)}function
i$(m,d,l){var
f=b(c[82],d,l),i=f[2],n=M(ax[2],0,0,m,d,f[1]),o=a(e[17][1],i),j=[0,d],k=cK(j,g(c[91],d,o,n)[1]),p=k[3],q=k[1],r=a(e[17][9],i),s=b(c[h][4],r,p);return[0,j[1],s,q]}function
ja(f,d,m,L,Y){var
aI=ek(L-1|0,m)[2],aJ=a(D[1][1][3],aI),aK=b(c[h][1],L,aJ),aL=a(at[1],L),aM=b(c[5],d[1],aK),_=b(cM[2],f,aM),x=_[1],aN=_[2],aa=a(y[28],x),i=aa[2],ab=aa[1],ac=b(e[17][h],ab[6],aN),ad=ac[1],aO=b(e[18],ac[2],[0,aL,0]),aP=a(Z[5],[0,x,ad]),r=n(Z[65],f,d[1],1,aP),aQ=a(e[17][9],r),ae=b(e[17][15],c[8],ad),af=b(e[17][15],c[8],aO),u=0,B=af,t=aQ,A=0,N=0,z=0;for(;;){if(B){if(t){var
v=B[1],aS=t[2],aT=t[1],aU=B[2];if(b(c[44],d[1],v)){var
aV=b(q[37],d[1],v);if(b(e[17][26],aV,ae))var
w=0,H=0;else{var
aX=b(q[37],d[1],v);if(b(e[17][26],aX,u))var
w=0,H=0;else
var
ag=b(c[65],d[1],v),aZ=a(D[1][1][3],aT),a1=b(q[36],d[1],aZ),a2=1,a3=function(i){return function(f,c){if(c)try{var
g=b(e[17][7],i,f-1|0),h=a(G[2],g);return h}catch(a){a=E(a);if(a[1]!==eh)if(a[1]!==a7)throw a;var
d=1}else
var
d=c;return d}}(A),a4=[0,ag],a5=g(O[2][15],a3,a1,a2)?[0,ag]:0,w=a5,H=a4}}else
var
w=0,H=0;var
aW=a(G[2],w)?z:z+1|0,u=[0,v,u],B=aU,t=aS,A=[0,w,A],N=[0,H,N],z=aW;continue}}else
if(!t){var
l=0,o=A,k=N,j=u,R=z;for(;;){if(o){var
ah=o[1];if(ah){if(k)if(j){var
l=[0,[0,ah[1]],l],o=o[2],k=k[2],j=j[2];continue}}else
if(k){var
aj=k[1];if(aj){if(j){var
ak=j[2],al=k[2],S=aj[1],am=o[2],a6=[0,[0,0,Y],b(F[aY],S-1|0,m)],a8=0,a9=function(l,m){return function(f,i){var
j=a(D[1][1][3],i),k=a(c[9],m-f|0),h=1-g(q[37],d[1],k,j);return h?h:b(e[17][29],[0,f],l)}}(l,S);if(g(F[94],a9,a8,a6)){var
l=[0,[0,S],l],o=am,k=al,j=ak,R=R-1|0;continue}var
l=[0,0,l],o=am,k=al,j=ak;continue}}else
if(j){var
l=[0,0,l],o=o[2],k=k[2],j=j[2];continue}}}else
if(!k)if(!j){var
T=a(e[17][9],l),a_=$(b(e[18],r,m)),a$=[0,a_,$(b(e[18],r,m))],ba=function(h,g){var
j=h[2],e=h[1],l=e[1];if(g){var
m=g[1],n=a(c[9],1),o=P(d[1],e,n),p=a(c[9],(m+i[6]|0)+1|0),q=P(d[1],e,p),r=b(c[65],d[1],q),k=fN(f,d[1],l,r,0,o),s=k[2],t=ai(0,f,[0,d[1]],k[1],e);return[0,t,ai(0,f,[0,d[1]],j,s)]}return[0,e,j]},an=g(e[17][18],ba,a$,T),ao=an[2],ap=an[1],bb=a(c[9],1),bc=P(d[1],ap,bb),aq=b(c[65],d[1],bc)-1|0,bd=a(e[9],ao),I=b(e[17][dQ],(aq+i[6]|0)+1|0,bd),be=a(e[8],ao),bf=b(e[17][dQ],(aq+i[6]|0)+1|0,be),bg=bE(-(i[6]+1|0)|0,bf),ar=aR(0,f,d[1],m,bg,I),bi=0,bj=function(h,g,e){var
j=e[2],k=e[1];if(g){var
l=g[1],m=ei(d[1],j,[0,(i[6]+1|0)-h|0]),n=a(c[9],(l+i[6]|0)+1|0),o=P(d[1],e,n),p=b(c[65],d[1],o),q=cH(tg,f,d[1],p,m,k);return ai(th,f,[0,d[1]],q,e)}return e},s=n(F[89],bj,bi,T,ap),bk=a(c[9],1),bl=P(d[1],s,bk),p=b(c[65],d[1],bl)-1|0,bm=$(m),bn=a(e[8],bm),bo=bE(i[6]+1|0,bn),bp=b(e[18],r,m),bq=aR(0,f,d[1],bp,bo,m),J=ai(ti,f,[0,d[1]],s,bq),as=P(d[1],J,Y),br=a(e[7],s),au=b(e[17][h],p,br),K=au[1],av=b(e[17][aY],i[6]+1|0,au[2]),bs=function(g){var
h=a(e[8],s),d=-i[6]|0,b=h;for(;;){if(b){var
f=b[1];if(0===f[0])if(g===f[1])return a(c[9],d);var
d=d+1|0,b=b[2];continue}return a(C[2],tj)}},bt=function(a){return a+1|0},bu=b(F[54],p,bt),bv=b(F[15],bs,bu),aw=a(e[17][9],bv);if(0===R)var
az=as,ay=0,ax=0;else
var
bX=a(e[8],J),bY=cF(d[1],bX,r),bZ=d[1],b0=function(a){return P(bZ,J,a)},b1=b(e[17][15],b0,u),b2=[0,p+1|0,0,0,0],b3=function(b,k,j,h){var
e=b[4],f=b[3],g=b[2],d=b[1];return h?[0,d+1|0,df(a(c[9],(p+i[6]|0)+1|0),g),f,e]:[0,d+1|0,[0,k,g],[0,j,f],[0,a(c[9],d),e]]},X=M(F[92],b3,b2,bY,b1,T),b4=X[4],b5=X[3],aD=cK(d,a(e[17][9],X[2])),aE=aD[3],aF=aD[1],b6=a(e[17][9],b5),aG=b(c[h][4],b6,aE),b7=a(e[17][9],b4),b8=bh(f,d,aF,b(c[h][4],b7,aE),aG),b9=[0,0,b8,b(c[h][1],1,as)],b_=a(c[18],b9),aH=function(a){var
f=b(c[38],a,K),g=b(c[38],f,av),h=P(d[1],ar,g),i=[0,h,b(e[18],af,aw)];return b(U[55],d[1],i)},b$=aH(aG),az=b_,ay=[0,dZ(f,d,aH(aF),b$),0],ax=1;var
bw=d[1],bx=function(a){return P(bw,J,a)},by=b(e[17][15],bx,ae),bz=a(c[h][1],-((p+i[6]|0)+1|0)|0),bA=b(e[17][15],bz,by),bB=b(q[14],az,K),aA=b(c[38],bB,av),bC=a(c[5],d[1]),aB=b(e[17][15],bC,bA),bD=b(c[5],d[1],aA),bF=n(cM[22],x,[0,ab,i],aB,bD),bG=a(Z[5],[0,x,aB]),aC=b(Z[60],f,bG),W=$(m),bH=W[3],bI=W[1],bJ=bE(i[6]+1|0,W[2]),bK=b(e[18],r,bI),bL=aR(0,f,d[1],bK,bJ,bH),bM=ai(tk,f,[0,d[1]],s,bL),bN=$(I),bO=a(e[8],bN),bP=$(K),bQ=a(e[8],bP),bR=function(g){var
j=g[5],k=a(e[19][12],g[2]),l=b(e[19][15],c[8],k),m=b(e[19][15],c[8],j),n=c6(g[1]),o=[0,a(c[28],n),l],q=a(c[21],o),r=b(c[h][1],g[3],q),t=[0,r,a0(0,g[3])],u=[0,a(c[21],t),0],v=a(e[19][11],m),w=b(e[18],v,u),x=b(e[17][15],c[V],g[4]),y=a(e[17][9],w),z=d[1];function
A(a){return cb(z,a)}var
B=b(e[17][15],A,y),C=bE(g[3],bO),i=b(e[18],B,C),D=cF(d[1],i,K),E=bE(p,i),F=b(e[18],bQ,E),G=a(e[7],s),H=b(e[18],x,I),J=b(e[18],D,H),L=aR(0,f,d[1],J,F,G);return ai(tl,f,[0,d[1]],L,bM)},bS=b(e[19][15],bR,aC),bT=function(a){return a[3]},bU=b(e[19][15],bT,aC),bV=function(e,d,b){return[0,a(c[8],e),d,b]},bW=n(e[19][56],bV,bF,bU,bS);return[0,I,aA,bW,p,ar,b(e[18],aw,ay),ax]}throw[0,Q,tf]}}throw[0,Q,te]}}function
tm(d){function
e(e){var
s=a(bD,b(z[18],e,d)),t=s[2],F=s[3],n=a(z[2],e),G=a(z[8],e),k=b(c[3],n,F);if(6===k[0])var
y=k[3],o=fY(n,k[1],k[2]),p=o[2],q=o[1],A=[0,a(c[10],d),[0,p]],B=a(c[21],A),C=b(c[h][5],p,y),D=g(U[18],G,n,C),E=b(c[37],D,q),m=[0,[0,b(c[38],B,q),E]];else
var
m=0;if(m){var
u=m[1],v=u[2],w=u[1];if(t){var
H=b(c[h][9],[0,[0,d,t[1]],0],w),I=x(M(r[bZ],0,[0,d],H,[0,v],cu)),J=x(a(r[75],[0,d,0]));return g(l[5],J,I,e)}var
K=a(z[38],w),L=b(r[k6],d,v),N=a(j[70][8],L);return a(b(l[9],N,K),e)}var
O=a(i[1][9],d),P=a(f[3],tn),Q=b(f[12],P,O);return g(l[24],0,Q,e)}return b(j[70][1],0,e)}function
to(m){var
q=a(j[66][5],m),n=a(j[66][6],m),o=a(j[66][3],m),d=b(c[3],n,o);if(6===d[0]){var
r=d[2],v=d[1],P=d[3],w=function(j){var
m=fY(j,v,r),d=m[1],A=b(c[h][5],m[2],P),B=b(c[38],A,d),C=[0,B,T(0,d)],D=a(c[21],C),E=b(c[37],D,d);function
l(b){var
c=t(b),d=u===c?b[1]:k===c?a(s[2],b):b,e=a(i[H][3],d);return a(bd[8][8],e)}var
w=[0,bd[8][1],[0,bd[8][4],0]],x=[0,l(ag),w],y=[0,l(aC),x],z=a(bd[8][15],y),F=g(a(U[15],z),q,j,E);function
n(h,m,d){var
b=d[2],e=d[1];if(h)return[0,[0,b,e],b];var
f=t(aC),i=u===f?aC[1]:k===f?a(s[2],aC):aC,g=t(ag),j=a(c[24],[0,i,b]),l=u===g?ag[1]:k===g?a(s[2],ag):ag;return[0,[0,j,e],a(c[24],[0,l,b])]}if(d){var
o=d[2],G=d[1];if(o)var
I=[0,0,a(c[9],1)],J=0,K=function(a,b){return n(J,a,b)},f=n(1,G,g(e[17][19],K,o,I))[1];else{a(c[9],1);var
f=[0,a(c[9],1),0]}}else{a(c[9],1);var
f=[0,a(c[9],1),0]}var
p=cA(q,j,0,F),L=p[2],M=p[1],N=[0,L,a(bF[67],f)],O=[0,v,r,a(c[21],N)];return[0,M,a(c[19],O)]};return b(cc[2],1,w)}var
p=a(f[3],tp);return b(l[66][4],0,p)}var
tq=a(j[66][9],to);function
tr(h,g){function
c(d){var
e=a(j[66][5],d),c=i$(e,a(j[66][6],d),h),f=c[2],i=c[3],k=n(ao[2],0,e,c[1],f)[1],l=M(r[bZ],0,[0,g],f,[0,i],cu),m=a(j[64][1],k);return b(j[71][2],m,l)}return a(j[66][10],c)}function
ts(e,g){function
d(d){var
h=a(j[66][5],d),i=a(j[66][6],d),f=i8(h,i,b(z[42][16],e,d)),k=f[3],l=f[1],m=[0,k,[0,a(c[10],e)]],n=a(c[21],m),o=M(r[bZ],0,[0,g],n,0,cu),p=a(j[64][1],l);return b(j[71][2],p,o)}return a(j[66][10],d)}var
cN=[0,tm,tq,tr,function(d){function
c(c){var
e=a(j[66][5],c),f=a(j[66][6],c),g=a(fv,b(z[42][15],d,c));return i9(1,a(G[7],g),d,e,f)}return a(j[66][10],c)},ts];aG(1058,[0,cJ,sM,sN,eu,ev,sP,cK,fW,i6,fX,i8,i9,td,dr,i_,fY,i$,ja,cN],mc);function
jb(a){return aI(tt,0)}function
jc(cS,f,q,m){var
ar=bK(f,m),x=a(y[28],ar),M=x[2][2],as=x[1],at=a(e[17][1],M),o=as[6],l=at-o|0,au=b(e[17][15],c[V],M),N=b(e[17][h],l,au),j=N[2],d=N[1],av=T(0,j),aw=[0,a(c[26],m),av],z=a(c[21],aw),aq=1;function
O(a){var
d=b(c[82],f,a)[2];return b(e[17][h],o,d)[2]}var
ax=bK(f,m),ay=b(cM[18],ax,x);function
az(k,r){var
s=a(c[8],r),l=b(c[90],f,s),d=l[1],t=l[2],n=a(e[17][1],d),p=n-o|0,q=b(e[17][h],p,d)[1];function
u(d,k){var
l=a(ah,k)[3],e=b(c[90],f,l),g=e[2],n=e[1],o=b(c[82],f,g)[1],j=b(c[3],f,o);if(11===j[0])if(b(i[37],j[1][1],m[1])){var
p=O(b(c[h][1],d+1|0,g));return[0,[0,n,d,a(c[9],d+1|0),p]]}return 0}var
v=b(F[71],u,q),w=T(0,d),x=[0,a(c[29],[0,m,k+1|0]),w],y=a(c[21],x),z=O(t),A=1;function
B(i,f){var
g=f[1],l=f[4],m=f[3],o=f[2],d=a(e[17][1],g),r=[0,b(c[h][1],d,y),0],s=T(0,g),t=[0,b(c[h][1],d,m),s],u=[0,a(c[21],t),r],v=a(c[h][1],d),w=b(e[17][15],v,z),x=b(e[18],w,u),A=b(e[18],l,x),B=aQ(p+d|0,j),C=b(e[18],B,A),D=a(e[19][12],C),E=[0,a(c[9],(n+1|0)+d|0),D],F=a(c[21],E),G=aV(o+1|0,g),H=b(c[37],F,G);return[0,k,i,b(c[37],H,q)]}return g(e[17][75],B,A,v)}var
aA=b(e[19][16],az,ay),aB=0;function
aC(c,a){return b(e[18],c,a)}var
P=g(e[19][18],aC,aA,aB),R=aV(l,d),aE=aV(l,R);function
B(e){var
f=T(e+ak.caml_mul(2-e|0,l)|0,d),g=[0,b(c[h][1],(3*l|0)+e|0,z),f];return a(c[21],g)}var
aG=B(0),aI=[0,[0,[0,a(i[1][6],tu)],0,aG],0],aJ=B(1),aK=[0,[0,[0,a(i[1][6],tv)],0,aJ],aI],aL=B(2),aM=[0,[0,[0,a(i[1][6],tw)],0,aL],aK],aN=b(e[18],R,d),aP=b(e[18],aE,aN),aR=hY(aM),p=3*(l+1|0)|0,aS=b(e[18],aR,aP),aT=[0,a(c[9],2),0],aU=[0,a(c[9],3),aT],aX=aQ(l+3|0,d),aY=b(e[18],aX,aU),aZ=aQ((2*l|0)+3|0,d),a0=b(e[18],aZ,aY),a1=aQ(p,j),a2=b(e[18],a1,a0),a3=a(e[19][12],a2),a4=[0,a(c[9],(p+1|0)+o|0),a3],a5=a(c[21],a4),a6=[0,a(c[9],1),0],a7=[0,a(c[9],2),a6],a8=aQ(3,d),a_=b(e[18],a8,a7),a$=aQ(l+3|0,d),ba=b(e[18],a$,a_),bb=aQ(p,j),bc=b(e[18],bb,ba),bd=a(e[19][12],bc),be=[0,a(c[9],(p+1|0)+o|0),bd],bf=a(c[21],be),bg=[0,a(c[9],1),0],bh=[0,a(c[9],3),bg],bi=aQ(3,d),bj=b(e[18],bi,bh),bk=aQ((2*l|0)+3|0,d),bl=b(e[18],bk,bj),bm=aQ(p,j),bn=b(e[18],bm,bl),bo=a(e[19][12],bn),bp=[0,a(c[9],(p+1|0)+o|0),bo],br=a(c[21],bp),bs=b(c[h][1],2,br),bt=[0,0,b(c[h][1],1,bf),bs],bv=[0,0,a5,a(c[18],bt)],bw=a(c[18],bv);b(c[37],bw,aS);var
bS=b(w[l_],q,f),bx=a(am[41],[2,m[1]]),S=b(K[5],bx,tx),bT=0;function
by(a){return b(c[5],f,a[3])}var
bz=b(e[17][15],by,P);function
bA(c){var
d=c[1],e=a(C[21],c[2]),f=b(C[16],ty,e),g=a(C[21],d),h=b(C[16],g,f),i=b(C[16],tz,h);return b(K[5],S,i)}var
bB=b(e[17][15],bA,P),D=a(e[17][1],d),bC=aV(D,d),bD=b(e[18],bC,d),bE=T(2*D|0,j),bF=[0,a(c[26],m),bE],U=a(c[21],bF),bG=c[14],bH=[0,U,T(0,d)],bJ=a(c[21],bH),bL=[0,0,b(c[h][1],1,bJ),bG],bM=a(c[18],bL),bN=[0,U,T(D,d)],bO=[0,0,a(c[21],bN),bM],bP=a(c[18],bO),bR=b(c[37],bP,bD),W=[0,[0,S,b(c[5],f,bR),0,bB,bz],bT],bU=0;function
bV(h){var
d=a(ah,h),e=d[2],g=d[1],i=d[3];if(e){var
j=[0,b(c[5],f,e[1])];return[0,a(K[10][16],g),j]}var
k=[1,b(c[5],f,i)];return[0,a(K[10][16],g),k]}var
bW=[0,0,0,b(e[17][15],bV,j),W,bS,bU],E=g(jd[2],bW,aO[8],0),I=a(y[2],0),bX=a(w[17],I),X=n(w[lv],0,I,bX,[0,E,0]),bY=X[1];fX(I,bY,q,c6(X[2]));var
Y=a(c[25],[0,E,0]),bZ=0;function
b0(b,a){var
c=a[5],d=1;function
f(a,c){return[0,d5,q,1,0,[0,[3,[0,[0,E,b],a]]]]}return g(e[17][75],f,d,c)}var
b1=g(e[17][75],b0,bZ,W),b2=[0,a(e[17][12],b1)];g(aH[22],0,[0,fo,0],b2);var
b3=a(am[41],[2,m[1]]),Z=b(K[5],b3,tA),_=b(K[6],tB,Z),k=[0,f],u=a(y[2],0),$=c_(f,hn(k));if(a(e[17][53],d))var
b4=[0,Y,T(0,j)],s=j,r=z,aa=a(c[21],b4);else
var
t=fW(0,u,k,j,z),L=t[8],ai=t[6],aj=t[5],al=t[3],ct=t[4],an=b(c[H],al,u),cu=b(c[h][2],2,2),ap=b(e[17][15],cu,ct),cv=[0,aj,a(c[9],1)],cw=a(c[24],cv),cx=a(c[h][5],cw),cy=b(e[17][15],cx,ap),cz=[0,aj,a(c[9],2)],cA=a(c[24],cz),cB=a(c[h][5],cA),cC=b(e[17][15],cB,ap),cD=[0,ai,a(c[9],1)],cE=[0,a(c[24],cD),0],cF=[0,ai,a(c[9],2)],cG=[0,a(c[24],cF),cE],cH=b(e[18],cy,cG),cI=b(e[18],cC,cH),cJ=aQ(2,j),cK=aD(Y,b(e[18],cJ,cI)),cL=b(c[h][1],1,L),cN=[0,[0,a(i[1][6],tE)],cL,cK],cO=a(c[19],cN),cP=[0,[0,a(i[1][6],tF)],L,cO],cQ=a(c[19],cP),cR=g(a9[9],an,k[1],L),s=al,r=cR,aa=g(a9[9],an,k[1],cQ);var
b5=[0,hq(k),[0,r,aa]],b6=a(c[21],b5),b7=b(c[38],b6,s),b8=[0,hp(k),[0,r]],b9=a(c[21],b8),b_=[0,b(c[37],b9,j)],ab=bu(Z,b7,b_,q,k[1],tC);g(aH[22],0,[0,fo,0],[3,[0,[1,ab],0]]);var
b$=T(0,j),ca=[0,a(c[22],ab),b$],ad=a(c[21],ca),cb=b(c[H],s,u),cc=[0,ho(k),[0,r,ad]],cd=a(c[21],cc),ce=[0,r,[0,ad,[0,ch(A[7],cb,k,0,0,0,0,0,0,cd),0]]],ae=fz(k[1],$,ce),cf=ae[1],af=b(c[37],ae[2],s),cg=a(G[7],cf),ag=b(c[38],cg,s);function
ci(e,b,d){if(1===b[0]){var
c=n(ac[5],$[1],d5,aq,[1,b[1]]);return a(ac[6],c)}throw[0,Q,tD]}var
cj=a(y[2],0);n(ao[3],0,cj,k,ag);var
ck=a(y[2],0);n(ao[3],0,ck,k,af);var
v=a(A[47],k[1])[1],cl=b(c[5],v,af),cm=b(c[5],v,ag),J=aF(aW[5],u,_,v,0,0,cm,cl),cn=J[4],co=J[3],cp=J[1],cq=a(w[bq],v),cr=[0,a(bQ[1],ci)],cs=[0,jb(0)];bI(aW[7],_,[0,co],cn,cq,0,0,[0,[0,2,q,10]],cs,0,cr,0,cp);return 0}b_([0,tG,function(a,b){return b9(jc,a,b)}]);function
je(u,S,t,s){var
j=s[1],d=[0,S],U=s[2],z=a(y[27],j),p=z[2],v=z[1],o=v[6],B=p[6],k=p[7],C=b(e[17][15],c[V],p[2]),W=T(0,C),X=[0,a(c[26],s),W],Y=a(c[21],X),D=[0,_([0,[0,a(i[1][6],tH)],0,Y]),C],E=b(e[17][h],k+1|0,D),H=E[2],l=E[1],I=g(A[11],[0,w[b1]],u,d),J=b(c[37],I,l),$=b(c[38],I,l),aa=b(c[h][1],k+1|0,$),ab=a0(k+1|0,o),L=T(0,b(F[aY],k+1|0,D)),f=a(i[1][6],tI),M=[0,[0,f],J],ac=b(c[h][1],1,J),x=a(i[1][6],tJ),m=a(i[1][6],tK),r=a(i[1][6],tL),ad=[0,a(c[10],f)],ae=b(e[19][5],ab,ad),N=b(e[19][5],ae,L),af=[0,a(c[10],r),N],O=a(c[21],af),ag=b(c[37],O,l),ah=b(c[h][1],2,ag),ai=b(c[38],O,l),aj=b(c[h][1],k+1|0,ai),ak=p[9];function
an(B,A){var
i=a(c[8],A),l=a(Z[47],[0,j,B+1|0]),p=a(c[10],m),C=b(c[2][2],d[1],U),D=g(cM[5],j[1],v,C),E=b(e[17][15],c[8],D),F=b(c[h][4],E,i),s=b(c[90],d[1],F)[1],H=a(e[17][1],s)-o|0,t=b(e[17][h],H,s)[1],I=v[4]-j[2]|0,J=a0(-o|0,o),K=[0,a(c[9],I),J],L=a(c[21],K),M=n(q[51],d[1],L,p,i),u=b(c[90],d[1],M)[1],N=a(e[17][1],u)-o|0,O=b(e[17][h],N,u)[1];function
P(e,f){var
d=e[2],g=e[1],i=a(bj,f),j=b(c[h][1],d,i);return[0,[0,[0,a(c[9],d),j],g],d+1|0]}var
w=g(e[17][18],P,tM,O)[1];function
y(f,h){var
i=0;function
j(e,h){if(e){var
i=e[1],j=i[2],k=i[1],l=b(f,function(b){var
e=b[2],f=b[1],g=[0,dV(d),[0,e,j]],h=a(c[21],g),i=[0,fi(d),[0,e,j,f,k]];return[0,a(c[21],i),h]},h),m=function(a){return[0,a]};return g(G[24],m,e,l)}return b(f,function(a){return a},h)}var
k=g(e[17][18],j,i,h),l=al(d,fk(0)),m=[0,al(d,hh(0)),l];function
n(a){return a}return g(G[24],n,m,k)}function
z(o,n,j){var
q=j[1],k=b(c[90],d[1],j[2]),f=k[1],l=b(c[82],d[1],k[2]),r=l[2];if(g(c[94],d[1],l[1],p)){var
i=a(e[17][1],f),s=a0(0,i),t=[0,b(c[h][1],i,q),s],u=[0,a(c[21],t),0],v=b(e[18],r,u),m=b(o,a(e[19][12],v),i),w=m[2],x=b(c[38],m[1],f);return[0,a(n,[0,x,b(c[37],w,f)])]}return 0}function
Q(b,l){var
e=[0,a(c[10],m),b],g=a(c[21],e),h=[0,a(c[10],f),b],i=[0,a(c[21],h),g],j=[0,dV(d),i],k=a(c[21],j);return[0,a(c[9],0),k]}var
R=y(function(a,b){return z(Q,a,b)},w)[2];function
S(g,j){var
k=[0,a(c[10],m),g],h=a(c[21],k),n=[0,a(c[10],f)],p=b(e[19][5],n,g),q=a0(l+j|0,o),i=b(e[19][5],q,p),s=[0,a(c[10],x),g],t=[0,a(c[21],s),[0,h]],u=a(c[21],t),v=[0,a(c[10],r),i],w=a(c[21],v),y=[0,a(c[10],f),g],z=[0,a(c[21],y),w,u,h],A=[0,fi(d),z],B=a(c[21],A),C=[0,a(c[10],r),i],D=a(c[21],C),E=[0,a(c[10],f),g],F=[0,a(c[21],E),D],G=[0,dV(d),F];return[0,B,a(c[21],G)]}var
T=y(function(a,b){return z(S,a,b)},w)[1],V=b(c[38],R,t),W=b(c[h][1],k+1|0,V),X=b(c[38],T,t);return[0,l,W,b(c[h][1],k+1|0,X)]}var
P=b(e[19][16],an,ak),ao=b(e[19][15],e[8],P),ap=a(c[9],1),aq=[0,g(Z[76],u,j,4),aa,ap,ao],ar=a(c[30],aq),as=b(e[19][15],e[9],P),at=a(c[9],1),au=[0,g(Z[76],u,j,4),aj,at,as],av=a(c[30],au),aw=b(c[38],av,l),ax=b(c[h][1],3,aw),ay=b(c[38],ar,l),az=b(c[h][1],2,ay),aA=[0,[0,[0,B],0],[0,[0,[0,m]],[0,ac],[0,b(c[h][11],[0,m,[0,f,0]],az)]]],aB=a(c[31],aA),aC=b(c[38],aB,[0,M,H]),aD=a(am[41],[2,j]),aE=b(K[6],tN,aD),Q=bu(aE,aC,0,t,d[1],tO),aF=[0,[0,[0,B],0],[0,[0,[0,m]],[0,ah],[0,b(c[h][11],[0,m,[0,x,0]],ax)]]],aG=a(c[31],aF),aH=a(c[h][1],1),aI=b(e[19][15],aH,L),aJ=[0,a(c[10],f),aI],aK=a(c[21],aJ),aL=[0,a(c[22],Q),N],aM=[0,0,a(c[21],aL),aK],aN=a(c[18],aM),aO=b(c[37],aN,l),aP=[0,[0,x],b(c[h][1],1,aO)],aQ=b(c[36],aP,aG),aR=b(c[h][11],[0,f,0],aQ),aS=b(c[38],aR,[0,M,H]),aT=[0,[0,r,a(c[22],Q)],0],aU=b(c[h][9],aT,aS),aV=a(am[41],[2,j]),aW=b(K[6],tP,aV);if(t)var
R=d[1];else
var
aX=a(y[2],0),R=a(w[17],aX);bu(aW,aU,0,t,R,tQ);return 0}b_([0,tR,function(a,b){return b9(je,a,b)}]);aG(1062,[0,jb,jc,je],gD);var
jf=a(e[17][15],c[V]);function
jg(d,l){var
m=l[2],n=l[1],f=n[1],o=a(y[27],n)[1],q=o[8],r=b(c[2][2],d,m),s=a(jf,b(dt[25],r,q)),j=cy(0,function(b){return a(i[1][6],tS)},s),k=j[3],p=j[1],t=j[2],u=a(e[17][1],k),v=a(y[2],0),w=g(e[17][19],c[b1],k,v);function
x(i,j){var
k=[0,[0,f,i],m],l=de(0,p,a(jf,b(e[17][h],j[6],j[2])[1])),n=a(am[41],[2,[0,f,i]]),o=[0,a(c[26],k),t],q=a(c[34],o),r=bK(d,k),s=b(Z[4],w,r);function
v(e){var
f=a(c[8],e),i=g(c[91],d,u,f)[2],j=b(c[h][4],p,i);return b(c[90],d,j)}var
x=b(e[19][15],v,s);return[0,n,q,l,x,function(e,d,b){var
h=a(y[2],0),j=[0,g(Z[76],h,[0,f,i],4),d,e,b];return a(c[30],j)}]}return[0,k,b(e[19][16],x,o[1])]}function
jh(c){var
d=ap(tU,tT,c),e=b(ac[11],c[1],d);return a(G[7],e)}function
ji(a){return ap(tW,tV,a)}function
jj(d){function
f(b){var
d=aZ(b);return a(c[10],d)}var
g=b(e[17][15],f,d);return a(e[19][12],g)}function
jk(u,f,o,m){var
j=jg(f,m),g=j[1],d=[0,f],k=jh(d)[2][1],p=a(e[19][11],j[2]);function
r(f){var
l=T(0,f[3]),j=a(c[21],[0,f[2],l]),m=[0,ji(d),[0,j]],n=a(c[21],m),p=[0,a(i[1][6],tX)],r=[0,a(i[1][6],tY)],s=a(c[9],1),t=[0,a(c[9],2),s],u=[0,b(c[h][1],2,n),t],v=a(c[21],u),x=[0,r,b(c[h][1],1,j),v],z=[0,p,j,a(c[18],x)],A=a(c[18],z),B=b(c[37],A,f[3]),C=b(q[17],B,g);return[0,f,[0,C,function(i){var
m=jj(g),n=b(e[19][5],m,l),p=[0,j,[0,b6(a(y[2],0),d,i,n),0]],h=b(ac[16],k,p),r=h[2],s=f[3],t=a(G[7],h[1]),u=b(c[38],t,s),v=b(q[19],u,g),x=b(w[c5],o,d[1]),z=b(c[37],r,f[3]),A=b(q[17],z,g),B=[0,b(c[5],d[1],A)],C=tZ[6],D=aP[40][1],E=[0,[0,b(c[5],d[1],v),D],C];return[0,b(t0[6],0,E),0,0,B,x,0,0]}]]}var
l=b(e[17][15],r,p);function
s(g,d,f){function
c(c){var
e=c[1],f=[0,[0,a(c[2][2],d)],t1],g=b(K[5],e[1],t2),h=[1,M(bf[3],0,0,g,0,f)],i=n(ac[5],k[1],aH[4],1,h);return a(ac[6],i)}return b(e[17][14],c,l)}function
t(e){var
f=e[2][1],g=b(K[5],e[1][1],t3),h=[0,a(bQ[1],s)],i=[0,hI(0)],j=a(w[bq],d[1]),k=b(c[5],d[1],f);bI(aW[7],g,0,k,j,0,0,0,i,0,h,0,[0]);return 0}return b(e[17][14],t,l)}b_([0,t4,function(a,b){return b9(jk,a,b)}]);aG(1066,[0,jg,jh,ji,jj,jk],lt);function
fZ(i,f,k,q,p){var
l=di(ea(i,f,M(ax[2],0,0,i,f,k))[1]),d=l[1],r=l[2],s=d[1][1],m=a(y[27],d[1]),j=m[2],n=m[1],t=n[1];function
u(b,e){return a(c[26],[0,[0,s,b],d[2]])}var
v=b(e[19][16],u,t),w=a(e[19][11],v),x=a(e[17][9],w);a(e[17][1],j[2]);var
o=n[6],z=g(Z[76],i,d[1],4),A=j[9],B=j[4];function
C(n,m,l){var
q=a(c[8],l),s=b(c[h][4],x,q),g=b(c[90],f,s),i=g[1],t=g[2],u=a(e[17][1],i)-o|0,v=b(e[17][h],u,i)[1],w=b(c[37],t,v),y=a(e[17][9],r),z=b(c[h][4],y,w),j=b(c[90],f,z),k=j[1],A=cY(p,d,n,m,o,k,j[2]);return b(c[38],A,k)}var
D=[0,z,q,k,g(e[19][55],C,B,A)];return a(c[30],D)}function
jl(o,as,ar,m){var
N=m[1],d=[0,as],at=m[2],O=a(y[27],N),P=O[2],au=O[1],av=P[2],aw=b(c[2][2],d[1],at),ay=b(dt[25],aw,av),az=b(e[17][15],c[V],ay),R=d$(d[1],az),aA=a(e[17][1],R),aB=au[6],aC=P[6],S=a0(0,aA),T=b(e[19][51],aB,S)[2];if(0===T.length-1)var
aD=a(c[9],1),aE=[0,a(c[26],m),S],q=a(c[21],aE),v=aD,j=R,p=0;else{var
l=dr(o,d[1],m),an=l[3],bo=l[8],bp=l[7],br=l[6],bs=l[2];d[1]=l[1];var
ap=t(ad),bt=u===ap?ad[1]:k===ap?a(s[2],ad):ad,bu=b(A[14],d,bt),bv=b(c[5],d[1],bs),bw=a(e[17][1],an),bx=b(h9[81],bw,bv)[2],by=[0,bu,[0,bo,a(c[8],bx)]],bz=a(c[21],by),bA=b(e[17][aY],bp,br),aq=t(ag),bB=a(c[9],1),bC=u===aq?ag[1]:k===aq?a(s[2],ag):ag,bD=a(c[24],[0,bC,bB]),q=g(U[19],o,d[1],bz),v=bD,j=an,p=bA}var
aG=al(d,fk(0)),aH=al(d,hi(0)),W=a(i[1][6],t5),x=a(i[1][6],t6),z=[0,_([0,[0,W],0,q]),j],aI=[0,_([0,[0,x],0,b(c[h][1],1,q)]),z],aJ=cs(0),aK=g(w[lZ],0,0,o),aL=g(A[65],aK,d,aJ),B=a(c[13],aL),aM=b(c[37],B,aI),X=b(c[H],z,o),Y=g(D[1][14],c[9],0,j);function
Z(a){return b(c[h][1],a,q)}function
$(d){var
f=a(c[h][1],d),g=b(e[19][15],f,Y),i=b(e[19][5],g,T),j=[0,a(c[26],m),i];return a(c[21],j)}var
aa=a(e[17][1],p),aN=Z(aC+2|0),aO=aV(1,p),aP=[0,_([0,[0,W],0,$(aa+1|0)]),aO],aQ=_([0,0,0,aN]),aR=b(c[35],aQ,B),aS=b(c[38],aR,aP);function
aT(w,o,u,t,f,s){var
g=[0,[0,x],0,Z(a(e[17][1],f)+1|0)],j=[0,_(g),f],i=b(c[H],j,X),k=[0,[0,x],0,$((a(e[17][1],f)+aa|0)+2|0)];function
l(t,p,s,r,j,q){if(o===p){if(0===a(e[17][1],f))return aG;var
k=b(c[H],j,i),l=cK(d,f)[3],m=cK(d,j)[3],n=a(e[17][1],f)+1|0,g=b(c[h][1],n,l);return bh(k,d,M(ax[2],0,0,k,d[1],g),g,m)}return aH}var
m=[0,_(k),p],n=b(c[38],B,m),q=fZ(i,d[1],v,n,l),r=_(g);return b(c[36],r,q)}var
aU=fZ(X,d[1],v,aS,aT),aX=dU(0,[0,ar],d,[0,aM],b(c[38],aU,z)),C=a(am[41],[2,N]),aZ=b(K[6],t7,C),a1=b(K[6],t8,C),a2=b(K[6],t9,C),a3=M(bf[3],0,0,aZ,0,[0,[0,aX],t_]),ab=a(y[2],0),f=[0,a(w[17],ab)],ae=t(c$),a4=u===ae?c$[1]:k===ae?a(s[2],c$):c$,E=a(ac[8],a4),a5=b(A[14],f,[1,a3]),a6=b(A[14],f,E[2]),a7=b(c[76],f[1],a6)[2],af=a(c[21],[0,a5,Y]),a8=M(ax[2],0,0,ab,f[1],af),ah=b(c[3],f[1],a8);if(6===ah[0]){var
ai=fz(f[1],[0,E,a7],[0,ah[2],[0,af,0]])[1],a9=a(y[2],0),r=b(c[H],j,a9),ba=a(G[7],ai),bb=M(ax[2],0,0,r,f[1],ba),I=a(G[7],ai),F=bb;for(;;){var
J=b(c[3],f[1],F);if(6===J[0]){var
a_=J[3],aj=ch(A[7],r,f,0,0,0,0,0,0,J[2]),a$=b(c[h][5],aj,a_),I=a(c[21],[0,I,[0,aj]]),F=a$;continue}var
ak=b(c[38],I,j),bc=b(c[37],F,j);n(ao[3],0,r,f,ak);var
bd=function(e,b,d){var
c=n(ac[5],E,d5,1,b);return a(ac[6],c)},be=b(c[5],f[1],bc),bg=b(c[5],f[1],ak),L=aF(aW[5],r,a1,f[1],0,0,bg,be),bi=L[4],bj=L[3],bk=L[1],bl=[0,a(bQ[1],bd)],bm=[0,hH(0)],bn=a(w[bq],f[1]);bI(aW[7],a2,[0,bj],bi,bn,0,0,0,bm,0,bl,0,bk);return 0}}throw[0,Q,t$]}b_([0,ua,function(a,b){return b9(jl,a,b)}]);aG(1067,[0,fZ,jl],lL);var
f0=a(aA[1],ub);function
ew(d,c){return[k,function(f){var
e=b(f0,d,c);return a(ar[9],e)}]}function
f1(d,c){return[k,function(f){var
e=b(f0,d,c);return a(ar[8],e)}]}function
aT(a){return f1(uc,a)}var
dv=[k,function(c){var
b=a(aA[39],0);return a(ar[9],b)}],ud=[k,function(c){var
b=a(aA[40],0);return a(ar[10],b)}],ug=f1(uf,ue),bR=ew(ui,uh),bS=ew(uk,uj),ex=aT(ul),uo=f1(un,um),uq=aT(up),ut=ew(us,ur),ey=aT(uu),ez=aT(uv),eA=aT(uw),bT=aT(ux),eB=aT(uy),eC=aT(uz),eD=aT(uA),eE=aT(uB),eF=aT(uC),uE=aT(uD),uG=aT(uF),uI=aT(uH),uK=aT(uJ),dw=ew(uM,uL),du=[k,function(d){var
c=b(f0,uO,uN);return a(ar[10],c)}];function
f2(c,e){var
d=t(c),f=u===d?c[1]:k===d?a(s[2],c):c;return b(A[14],e,[2,f])}function
cd(c,e){var
d=t(c),f=u===d?c[1]:k===d?a(s[2],c):c;return b(A[14],e,[1,f])}function
jm(c,e){var
d=t(c),f=u===d?c[1]:k===d?a(s[2],c):c;return b(A[14],e,[3,f])}function
uP(a){return f2(dw,a)}function
uQ(a){return jm(du,a)}function
uR(a){return f2(dv,a)}function
jn(a){return jm(ud,a)}function
jo(a){return cd(ug,a)}function
uS(a){return f2(ut,a)}function
uT(a){return cd(uE,a)}function
uU(a){return cd(uG,a)}function
uV(a){return cd(uI,a)}function
uW(a){return cd(uK,a)}var
aa=[e6,uX,e3(0)];function
dx(f,d,l,e,k){var
g=e[2],h=e[1],m=l[1],o=b(c[H],h,f),i=ch(A[7],o,d,0,0,0,0,0,0,g),j=a(k,i),p=b(c[H],m,f);n(ao[3],0,p,d,j);return[0,[0,[0,[0,h,g],b(c[75],d[1],i)]],j]}function
ce(d,e,v,u,i,n){var
x=v[1];switch(i[0]){case
0:throw[0,Q,uY];case
1:var
y=i[1],z=b(w[168],0,d),o=g(A[65],z,e,y),B=a(at[16],o),j=[0,B,b(uZ[30],d,o)];break;case
2:var
L=i[1],M=b(w[lv],0,d),s=g(A[65],M,e,L),N=a(at[19],s),j=[0,N,b(Z[1],d,s)];break;default:var
O=i[1],P=b(w[170],0,d),t=g(A[65],P,e,O),R=a(at[21],t),j=[0,R,b(Z[2],d,t)]}var
D=j[2],J=a(c[8],j[1]),q=a(c[8],D),l=q,k=n;for(;;){var
f=b(c[3],e[1],l);switch(f[0]){case
5:var
l=f[1];continue;case
6:if(k){var
p=k[1],E=f[3],H=f[2];if(p){var
I=k[2],l=b(c[h][5],p[1],E),k=I;continue}var
r=H,m=1}else
var
m=0;break;case
8:var
l=b(c[h][5],f[2],f[4]);continue;default:var
m=0}if(!m)var
r=a(C[2],u0);var
K=b(U[24],e[1],r);return dx(d,e,[0,x,q],[0,u,K],function(d){var
e=a(G[25],d),f=b(F[15],e,n),g=[0,J,a(aK[12],f)];return a(c[21],g)})}}var
u1=a(eG[13],i[61]);function
f3(g,f,e,d,a){var
h=b(c[H],e,g);return aF(U[83],0,0,0,h,f,d,a)[2]}function
jp(B,d,i,f){var
j=i[1],m=f[2],p=f[1],q=i[2];if(j){var
k=j[1],e=k[2][1],r=k[1][1],s=b(w[23],d[1],e),t=a(w[6],s),u=a(o[1],r),v=b(F[aY],u,t),x=function(b){var
d=a(D[2][1][1],b);return a(c[10],d)},y=b(o[17],x,v),l=b(c[h][4],y,m),z=d[1],A=b(c[5],d[1],l);d[1]=g(w[31],e,A,z);d[1]=n(dY[16],d[1],e,l,u1);return[0,p,q]}throw[0,Q,u2]}function
jq(d,c,b,a){var
e=a[2],f=b[2],g=jp(d,c,b[1],a[1]);return[0,g,ai(0,d,[0,c[1]],e,f)]}function
u3(j,e,d,a){var
f=a[2],h=a[1],i=g(j,e,d,[0,h,f]),k=i[1][2],l=b(c[H],h,e);n(ao[5],l,d,k,f);return i}function
cO(h,f,b,a,e){var
c=g(f,b,a,e),d=c[1][1];return d?jq(b,a,c,g(h,b,a,d[1][1])):c}function
cP(c,b,a){var
d=$(a[1]);return[0,dx(c,b,a,a,function(a){return a}),d]}function
u4(h,d,c,b){function
e(c,b,i){var
a=g(h,c,b,i),f=a[1][1];if(f){var
j=f[1][1];try{var
d=[0,b[1]],k=jq(c,d,a,e(c,d,j));b[1]=d[1];return k}catch(b){b=E(b);if(b[1]===aa)return a;throw b}}return a}try{var
a=e(d,c,b);return a}catch(a){a=E(a);if(a[1]===aa)return cP(d,c,b);throw a}}function
cQ(d,a){return b(c[ba],d,[2,a])}function
jr(d,a){return b(c[ba],d,[1,a])}function
bm(g,e){try{var
d=b(c[70],g,e)}catch(b){b=E(b);if(b===at[54])throw[0,aa,a(f[3],u5)];throw b}return[0,d[1],d[2],d[3]]}function
bU(w,d,v,l,j,i,r){var
m=l?l[1]:0,n=j?j[1]:0,o=i?i[1]:0,p=cB(d,r),e=p[2],q=t(dv),x=p[1],y=u===q?dv[1]:k===q?a(s[2],dv):dv;if(1-a(cQ(d,y),x))throw[0,aa,a(f[3],u6)];var
z=S(e,0)[1],g=S(e,2)[3],h=S(e,1)[2],A=m?1-f3(w,d,v,h,g):m;if(A)throw[0,aa,a(f[3],u7)];var
B=n?1-b(c[44],d,h):n;if(B)throw[0,aa,a(f[3],u8)];var
C=o?1-b(c[44],d,g):o;if(C)throw[0,aa,a(f[3],u9)];return[0,z,h,g]}function
f4(e,h){var
f=cB(e,h),d=f[2],g=t(du),i=f[1],j=u===g?du[1]:k===g?a(s[2],du):du;if(a(b(c[ba],e,[3,j]),i)){var
l=S(d,3)[4],m=S(d,2)[3],n=S(d,1)[2];return[0,[0,S(d,0)[1],n,m,l]]}return 0}function
u_(m,e,d,h){var
n=h[2],f=h[1];try{var
s=[0,d[1]],G=g(m,e,s,[0,f,n]);d[1]=s[1];return G}catch(h){h=E(h);if(h[1]===aa){var
i=function(a){var
h=b(c[H],f,e);return g(a9[11],h,d[1],a)},o=i(n);try{var
j=b(c[70],d[1],o),t=j[3],u=j[1],v=[0,u,i(j[2]),t],q=a(c[18],v);try{var
k=bm(d[1],q),w=k[3],x=k[1],l=bU(e,d[1],f,0,0,0,k[2]),y=l[3],z=l[1],A=i(l[2]),B=[0,z,A,i(y)],C=[0,uR(d),B],D=[0,x,a(c[21],C),w],F=a(c[18],D),r=F}catch(a){a=E(a);if(a[1]!==aa)throw a;var
r=q}var
p=r}catch(a){a=E(a);if(a!==at[54])throw a;var
p=o}return g(m,e,d,[0,f,p])}throw h}}function
u$(z,d,y){var
A=y[2],e=y[1],l=bm(d[1],A),i=l[3],B=l[2],L=l[1],C=bU(z,d[1],e,0,0,0,B),M=C[3],D=f4(d[1],C[2]),F=f4(d[1],M);if(D)if(F){var
G=F[1],m=G[4],n=G[3],j=D[1],o=j[4],p=j[3],r=j[2],v=j[1];if(g(c[h][13],d[1],1,i))try{var
I=b(c[71],d[1],r)[3];if(!g(c[h][13],d[1],1,I))throw at[54];var
J=t(eB),P=u===J?eB[1]:k===J?a(s[2],eB):eB,R=a(q[47],I),Q=[1,P],S=[0,[0,v],[0,[0,R],[0,[0,a(q[47],i)],[0,[0,p],[0,[0,n],[0,[0,o],[0,[0,m],vb]]]]]]],x=Q,w=S}catch(b){b=E(b);if(b!==at[54])throw b;var
H=t(eC),N=u===H?eC[1]:k===H?a(s[2],eC):eC,x=[1,N],w=[0,[0,v],[0,[0,r],[0,[0,a(q[47],i)],[0,[0,p],[0,[0,n],[0,[0,o],[0,[0,m],va]]]]]]]}else
var
K=t(eD),T=u===K?eD[1]:k===K?a(s[2],eD):eD,x=[1,T],w=[0,[0,v],[0,[0,r],[0,[0,p],[0,[0,n],[0,[0,o],[0,[0,m],[0,[0,a(c[19],[0,L,B,i])],vc]]]]]]];var
O=$(e);return[0,ce(z,d,[0,e,A],e,x,w),O]}throw[0,aa,a(f[3],vd)]}function
ve(a,b,c){return u4(u$,a,b,c)}function
js(C,i,e,r){var
j=r[2],d=r[1],m=bm(e[1],j),n=m[3],o=m[2],v=m[1],w=bU(i,e[1],d,vf,0,0,o),x=w[2],l=w[1],p=$(d);if(g(c[h][13],e[1],1,n)){var
D=function(d){var
e=[0,v,o,b(c[h][1],1,d)];return a(c[19],e)};return[0,dx(i,e,[0,d,j],[0,d,a(q[47],n)],D),p]}var
y=a(c[19],[0,v,o,n]);try{var
B=t(eF),K=u===B?eF[1]:k===B?a(s[2],eF):eF,L=[0,jo(e),[0,l]],M=a(c[21],L),N=b(c[H],d,i),O=b(ac[30],0,N),P=[0,ce(i,e,[0,d,j],d,[1,K],[0,[0,l],[0,[0,g(A[65],O,e,M)],[0,[0,x],[0,[0,y],vi]]]]),p];return P}catch(h){h=E(h);if(h===W){if(!C)if(!dR[1]){var
G=b(c[H],d,i),I=g(q[V],G,e[1],l),J=a(f[3],vh);throw[0,aa,b(f[12],J,I)]}var
z=t(eE),F=u===z?eE[1]:k===z?a(s[2],eE):eE;return[0,ce(i,e,[0,d,j],d,[1,F],[0,[0,l],[0,[0,x],[0,[0,y],vg]]]),p]}throw h}}function
jt(W,k,d,A){var
r=A[2],n=A[1],o=0===W?1:0,s=bm(d[1],r),B=s[2],C=s[1],X=s[3],t=bU(k,d[1],n,0,[0,o],[0,1-o],B),E=t[3],G=t[2],Y=t[1];if(o)var
u=G,p=E;else
var
u=E,p=G;var
v=b(c[65],d[1],u),Z=bO(k,d[1],n,p,v);if(b(O[2][3],v,Z))throw[0,aa,a(f[3],vj)];var
H=fN(k,d[1],n,v,0,p),i=H[1],w=i[1],_=H[2],x=P(d[1],i,u),e=b(c[65],d[1],x),j=P(d[1],i,p),l=P(d[1],i,Y),I=P(d[1],i,B),J=ek(e-1|0,w),y=J[1],K=a(D[1][1][1],J[2]),m=g(c[h][13],d[1],1,X),$=0===o?0===m?uW:uV:0===m?uU:uT,L=$(d),ab=P(d[1],i,r),M=b(c[70],d[1],ab)[3];if(m)var
N=a(q[47],M),ac=b(c[37],N,y),ad=[0,K,b(c[h][1],-e|0,l),ac],Q=a(c[19],ad),z=N;else
var
ap=g(c[h][2],1,e+1|0,M),aq=a(c[9],e),V=b(c[h][5],aq,ap),ar=aV(1,y),as=b(c[37],V,ar),at=[0,C,b(c[h][1],1-e|0,I),as],au=a(c[19],at),av=[0,K,b(c[h][1],-e|0,l),au],Q=a(c[19],av),z=V;var
R=b(c[h][1],e,Q),ae=T(1,y),S=d6(e,j,w),af=b(F[h],e-1|0,S)[1];if(m)var
ag=[0,b(c[h][1],-e|0,j),0],U=g(c[h][3],ag,e-1|0,z);else
var
al=[0,jn(d),[0,l,j]],am=a(c[21],al),an=[0,b(c[h][1],-e|0,j),0],ao=[0,b(c[h][1],-e|0,am),an],U=g(c[h][3],ao,e-1|0,z);var
ah=cb(d[1],j),aj=cH(0,k,d[1],e,ah,w),ak=ai(0,k,[0,d[1]],aj,i);return[0,dx(k,d,[0,n,r],[0,S,U],function(g){var
i=b(c[38],g,af),f=b(c[h][1],e,i),k=m?a(c[21],[0,L,[0,l,R,j,f,x]]):a(c[21],[0,L,[0,l,j,R,f,x]]),n=b(c[h][1],1,k),o=[0,n,[0,a(c[9],1)]],p=[0,a(c[21],o),ae],q=[0,C,I,a(c[21],p)],r=a(c[19],q);return P(d[1],_,r)}),ak]}function
vk(e,d,r){var
l=r[2],i=r[1],m=bm(d[1],l),v=m[2],K=m[3],L=m[1],n=bU(e,d[1],i,0,0,0,v),w=n[3],x=n[2],M=n[1],N=b(c[82],d[1],x)[1],O=b(c[82],d[1],w)[1],y=b(c[56],d[1],N),P=y?b(c[56],d[1],O):y;if(1-P)throw[0,aa,a(f[3],vl)];try{var
Q=g(Z[71],e,d[1],M)}catch(b){b=E(b);if(b===W)throw[0,aa,a(f[3],vm)];throw b}var
z=a(Z[12],Q),B=z[2],R=z[1];if(a(F[53],B))return cP(e,d,[0,i,l]);var
C=di(R),D=C[2],j=dr(e,d[1],C[1]),S=j[8],T=j[5],X=j[2];d[1]=j[1];var
Y=a(o[9],D),p=b(c[h][4],Y,S),_=[0,jo(d),[0,p]],ab=a(c[21],_),I=b(c[H],i,e);try{var
af=b(ac[30],0,I),ag=g(A[65],af,d,ab)}catch(c){c=E(c);if(c===W){var
ad=g(q[V],I,d[1],p),ae=a(f[3],vn);throw[0,aa,b(f[12],ae,ad)]}throw c}var
ah=f4(d[1],T),ai=a(G[7],ah)[3],aj=a(q[47],ai),ak=a(F[9],B),J=t(ez),al=b(c[h][4],ak,aj),am=u===J?ez[1]:k===J?a(s[2],ez):ez,an=b(U[55],d[1],[0,X,D]),ao=b(c[H],i,e),ap=g(a9[9],ao,d[1],an),aq=[0,[0,p],[0,[0,ag],[0,[0,ap],[0,[0,al],[0,[0,x],[0,[0,w],[0,[0,a(c[19],[0,L,v,K])],vo]]]]]]],ar=$(i);return[0,ce(e,d,[0,i,l],i,[1,am],aq),ar]}function
vp(h,d,m){var
n=m[2],e=m[1],i=bm(d[1],n),o=i[2],v=i[3],w=i[1],j=bU(h,d[1],e,0,0,0,o),l=j[1],x=j[3],y=j[2],z=[0,uS(d),[0,l]],B=a(c[21],z),p=b(c[H],e,h);try{var
F=b(ac[30],0,p),G=g(A[65],F,d,B)}catch(c){c=E(c);if(c===W){var
C=g(q[V],p,d[1],l),D=a(f[3],vq);throw[0,aa,b(f[12],D,C)]}throw c}var
r=t(ey),I=u===r?ey[1]:k===r?a(s[2],ey):ey,J=[0,[0,l],[0,[0,G],[0,[0,y],[0,[0,x],[0,[0,a(c[19],[0,w,o,v])],vr]]]]],K=$(e);return[0,ce(h,d,[0,e,n],e,[1,I],J),K]}function
ju(i,d,n){var
j=n[2],h=n[1];try{var
x=function(a){var
e=b(c[H],h,i);return g(a9[11],e,d[1],a)},o=function(h){var
b=cB(d[1],h),c=b[2],e=b[1],g=t(bT),i=u===g?bT[1]:k===g?a(s[2],bT):bT,j=1-a(jr(d[1],i),e),l=j||1-(8===c.length-1?1:0);if(l)throw[0,aa,a(f[3],vs)];return[0,e,c]};try{var
M=o(j)[2],e=M,q=j}catch(a){a=E(a);if(a[1]!==aa)throw a;var
p=x(j),e=o(p)[2],q=p}var
l=S(e,0)[1],y=S(e,1)[2],m=S(e,2)[3],r=S(e,3)[4],v=S(e,4)[5],z=S(e,6)[7],A=S(e,7)[8],B=[0,uP(d),[0,l,m]],C=a(c[21],B),D=[0,uQ(d),[0,l,m,r,v]],F=[0,C,a(c[21],D)],G=[0,jn(d),F],I=a(c[21],G);if(1-f3(i,d[1],h,A,I))throw[0,aa,a(f[3],vt)];var
w=t(eA),J=u===w?eA[1]:k===w?a(s[2],eA):eA,K=$(h),L=[0,ce(i,d,[0,h,q],h,[1,J],[0,[0,l],[0,[0,y],[0,[0,m],[0,[0,r],[0,[0,v],[0,[0,z],vu]]]]]]),K];return L}catch(a){a=E(a);if(a[1]===aa)return cP(i,d,[0,h,j]);throw a}}function
jv(a,b,c){return cO(vp,vk,a,b,c)}function
jw(d,c,b){return a(C[2],vv)}function
jx(o,d,n){var
i=n[2],e=n[1],j=bm(d[1],i),l=j[3],m=j[2],p=j[1],r=t(bS),x=u===r?bS[1]:k===r?a(s[2],bS):bS;if(1-a(cQ(d[1],x),m))throw[0,aa,a(f[3],vw)];var
v=$(e);if(g(c[h][13],d[1],1,l)){var
y=function(d){var
e=[0,p,m,b(c[h][1],1,d)];return a(c[19],e)};return[0,dx(o,d,[0,e,i],[0,e,a(q[47],l)],y),v]}var
w=t(ex),z=a(c[19],[0,p,m,l]),A=u===w?ex[1]:k===w?a(s[2],ex):ex;return[0,ce(o,d,[0,e,i],e,[1,A],[0,[0,z],vx]),v]}function
jy(w,d,j){var
l=j[1],e=bm(d[1],j[2]),i=e[3],m=e[2],o=t(bR),x=e[1],y=u===o?bR[1]:k===o?a(s[2],bR):bR;if(1-a(cQ(d[1],y),m))throw[0,aa,a(f[3],vy)];var
z=$(l);if(g(c[h][13],d[1],1,i))var
A=a(q[47],i),r=A,p=cd(uo,d);else
var
C=a(c[19],[0,x,m,i]),r=C,p=cd(uq,d);var
v=a(c[21],[0,p,[0,r]]),B=b(c[H],l,w);n(ao[3],0,B,d,v);return[0,[0,0,v],z]}function
f5(A,z,i,d,o){var
p=o[2],j=o[1],r=t(bT),B=cB(d[1],p)[1],C=u===r?bT[1]:k===r?a(s[2],bT):bT;if(a(jr(d[1],C),B))return 0;var
l=bm(d[1],p)[2],v=t(bR),D=u===v?bR[1]:k===v?a(s[2],bR):bR;if(a(cQ(d[1],D),l))return 3;var
w=t(bS),G=u===w?bS[1]:k===w?a(s[2],bS):bS;if(a(cQ(d[1],G),l))return 2;var
m=bU(i,d[1],j,0,0,0,l),e=m[3],h=m[2],I=m[1];if(z){if(b(c[44],d[1],h))if(b(c[44],d[1],e)){b(c[65],d[1],e);b(c[65],d[1],h);return[1,0]}if(b(c[44],d[1],h))return vz;if(b(c[44],d[1],e))return vA;throw[0,aa,a(f[3],vB)]}function
n(f,e){var
a=b(c[65],d[1],f),g=bO(i,d[1],j,e,a);return 1-b(O[2][3],a,g)}if(b(c[44],d[1],h))if(b(c[44],d[1],e))if(n(h,e)){b(c[65],d[1],e);b(c[65],d[1],h);return[1,0]}if(b(c[44],d[1],h))if(n(h,e))return vC;if(b(c[44],d[1],e))if(n(e,h))return vD;function
J(a){var
e=b(c[82],d[1],a)[1];try{var
f=b(c[5],d[1],e);b(cM[1],i,f);var
g=1;return g}catch(a){a=E(a);if(a===W)return 0;throw a}}function
x(a){var
e=b(c[82],d[1],a)[1],f=b(c[H],j,i),h=g(a9[11],f,d[1],e);return b(c[56],d[1],h)}if(f3(i,d[1],j,h,e))return vE;if(J(I))if(x(h))if(x(e))return[2,[0,[0,A,2],0]];function
y(e,i){function
a(e){if(g(c[94],d[1],e,i))throw q[28];var
f=b(c[82],d[1],e),j=f[2],h=b(c[56],d[1],f[1]);return h?b(F[14],a,j):h}try{a(e);var
f=0;return f}catch(a){a=E(a);if(a===q[28])return 1;throw a}}if(!y(h,e))if(!y(e,h))throw[0,aa,a(f[3],vF)];return 1}function
vG(f,e,d,h){var
i=h[1],j=g(U[27],e,d[1],h[2]),l=bm(d[1],j)[2];try{var
m=g(U[27],e,d[1],l),n=bU(e,d[1],i,0,0,0,m)[1],o=function(l,j){var
g=l,e=j;for(;;){var
m=b(U[26],d[1],g),h=cB(d[1],m),i=t(dw),n=h[2],o=h[1],p=u===i?dw[1]:k===i?a(s[2],dw):dw;if(a(cQ(d[1],p),o)){var
q=[0,a(c[9],1),0],r=[0,S(n,1)[2],q],g=b(U[55],d[1],r),e=[0,f,e];continue}return e}}(n,[0,f,0]);return o}catch(a){a=E(a);if(a[1]===aa)return[0,f,0];throw a}}function
jz(a){if(typeof
a==="number")switch(a){case
0:return ju;case
1:return jw;case
2:return jx;default:return jy}else
switch(a[0]){case
0:var
b=a[1];return function(a,c,d){return js(b,a,c,d)};case
1:var
c=a[1];return function(a,b,d){return jt(c,a,b,d)};default:var
d=dy(a[1]),e=function(a,b,c){return cO(d,jv,a,b,c)};return function(a,b,c){return cO(ju,e,a,b,c)}}}function
vH(d){var
a=d[2],b=d[1];function
e(e,d,c,a){try{var
f=g(e,d,c,a);return f}catch(a){a=E(a);if(a[1]===aa)return bL([0,b,vI,a[2]]);throw a}}function
c(d){function
a(c,b,a){return g(jz(g(d,c,b,a)),c,b,a)}function
b(b,c,d){return cO(a,ve,b,c,d)}function
c(a,c,d){return u_(b,a,c,d)}return function(a,b,d){return e(c,a,b,d)}}if(typeof
a==="number")switch(a){case
0:var
f=0;return c(function(a,c,d){return f5(b,f,a,c,d)});case
1:var
h=1;return c(function(a,c,d){return f5(b,h,a,c,d)});default:return function(d,c,a){var
f=[0,b,0];return g(dy(e(function(a,b,c){return vG(f,a,b,c)},d,c,a)),d,c,a)}}var
i=a[1];return c(function(c,b,a){return i})}function
dy(c){var
a=b(o[19],vH,c);return a?g(o[20],cO,a[1],a[2]):cP}function
f6(k){function
d(l){var
d=a(j[66][1],l),m=a(j[66][5],d),n=a(az[41],m),p=a(j[66][4],d);function
r(b){var
c=a(D[2][1][1],b);return a(q[b2],c)}var
e=b(F[c0],r,p),s=e[1],t=b(c[aY],e[2],n),f=cz(s),i=f[1],u=f[2],v=cy(vK,function(a){throw[0,Q,vJ]},i)[2],w=a(j[66][3],d),x=b(c[h][11],u,w);function
y(e){var
d=[0,e],f=g(dy(k),t,d,[0,i,x])[1][2],j=a(o[9],v),l=b(c[h][4],j,f);return[0,d[1],l]}return b(cc[2],1,y)}return a(j[66][10],d)}function
vU(d){var
c=d[2];if(typeof
c==="number")switch(c){case
0:return a(f[3],vV);case
1:return a(f[3],vW);default:return a(f[3],vX)}var
b=c[1];if(typeof
b==="number")switch(b){case
0:return a(f[3],vL);case
1:return a(f[3],vM);case
2:return a(f[3],vN);default:return a(f[3],vO)}else
switch(b[0]){case
0:return 0===b[1]?a(f[3],vP):a(f[3],vQ);case
1:return 0===b[1]?a(f[3],vR):a(f[3],vS);default:return a(f[3],vT)}}var
dz=b(f[39],f[13],vU);aG(1071,[0,aa,jp,u3,cO,js,jt,jv,jw,jx,jy,cP,jz,f5,dy,f6,dz],"Simplify");function
dA(c,d){function
g(d){switch(d[0]){case
0:var
h=d[4],j=d[3],k=d[2],l=d[1];if(0===h[0]){var
n=h[1],o=function(d){var
e=a(c,d[8]),f=a(c,d[7]),h=a(c,d[6]),i=a8(c,d[5]),j=b(h2,c,d[4]),k=d[3],l=d[2],m=d[1];return[0,m,l,k,j,i,h,f,e,g(d[9])]},p=b(e[17][15],o,k),q=a8(c,l),r=[0,a(c,n)];return[0,q,p,a(c,j),r]}var
s=a8(c,l);return[0,s,k,a(c,j),h];case
1:var
t=d[4],u=d[3],v=d[2],w=a8(c,d[1]),x=a(G[16],g),y=b(e[19][15],x,t);return[1,w,v,a(c,u),y];case
2:var
z=d[6],A=d[5],B=d[4],C=d[3],D=d[2],E=a8(c,d[1]),F=function(a){var
d=a[4],f=a[3],h=a[2],i=a[1],j=g(a[5]),k=a8(c,f);return[0,i,b(e[17][15],c,h),k,d,j]},H=b(e[17][15],F,z);return[2,E,a(c,D),C,B,A,H];case
3:var
I=d[2],J=a8(c,d[1]);return[3,J,g(I)];case
4:var
K=d[1];return[4,K,g(d[2])];default:var
f=d[2],L=d[3],M=a8(c,d[1]),i=f[1],m=f[6],N=i[3],O=i[2],P=i[1],Q=m[2],R=m[1],S=g(L),T=a(c,f[10]),U=a8(c,f[9]),V=a8(c,f[8]),W=a8(c,f[7]),X=b(e[17][15],c,Q),Y=[0,a(c,R),X],Z=f[5],_=f[4],$=f[3],aa=a(c,f[2]),ab=a(c,N);return[5,M,[0,[0,P,a(c,O),ab],aa,$,_,Z,Y,W,V,U,T],S]}}return g(d)}function
f7(f,d,j,i,h){var
b=g(A[62],j,f,i),k=b[3],l=h6(b[1],d,b[2],[0,h],f),m=[0,d,a(e[19][12],k)];return[0,l,a(c[12],m)]}function
f8(aR,r,X,l){var
o=[0,aJ[8][1]],E=[0,0];function
v(d,p,aS){var
l=aS;for(;;)switch(l[0]){case
0:var
K=l[4],Y=l[3],_=l[2],L=l[1][1];if(0===K[0]){var
aT=K[1],aW=[0,p,L],aX=function(d,l){var
f=d[4],m=d[1],x=l[2],y=l[1],z=d[9],A=d[7],n=b(c[aY],f,X),j=v(n,y,z),k=j[3],p=j[1],B=j[2],q=b(e[17][15],aZ,f),C=b(c[h][11],q,k),s=b(c[3],p,A);if(3===s[0]){var
D=s[1][1],F=b(c[h][1],1,k),G=[0,[0,a(i[1][6],vZ)],B,k,F],t=f7(p,D,n,a(c[20],G),[0,av,[3,v0,[0,m]]]),u=t[2],H=t[1],w=b(c[75],r[1],u)[1],I=o[1],J=a(e[17][1],f);o[1]=g(aJ[8][4],w,J,I);E[1]=[0,[0,w,0],E[1]];return[0,H,[0,an([0,m],[0,b(c[h][11],q,u)],C),x]]}throw[0,Q,vY]},$=g(e[17][19],aX,_,aW),aa=$[2],ab=$[1],a0=b(c[38],aT,aa);return[0,ab,a0,b7(d,ab,Y,aa)]}var
a1=K[1];if(a(e[17][53],_)){var
ac=t(bv),a2=u===ac?bv[1]:k===ac?a(s[2],bv):bv,ad=a4(p,a2),a3=ad[2],a5=ad[1],a6=b5((a(e[17][1],L)-a1|0)+1|0),a7=[0,a(c[8],a6)],a8=an([0,a(i[1][6],v1)],a7,a3),ae=b(c[37],Y,L),a_=b(c[h][1],1,ae),af=cA(d,a5,[0,[0,av,v2]],b(c[36],a8,a_)),ag=af[2],ah=af[1],a$=b(c[75],ah,ag)[1];o[1]=g(aJ[8][4],a$,0,o[1]);return[0,ah,ag,ae]}throw[0,Q,v3];case
1:var
aj=l[4],I=l[3],B=l[2],ak=l[1],x=ak[1];if(cn[1]){var
m=[0,p],z=ja(d,m,x,B,I),am=z[7],ap=z[5],ar=z[4],M=z[1],ba=z[6],bc=z[3],bd=z[2],bf=am?b(dy(v4),d,m):function(a){return cP(d,m,a)},bg=function(o,l){var
U=o[3],u=g(c[91],r[1],o[2],o[1]),x=u[2],j=g(aU[19],d,r[1],u[1]);if(am)var
t=0;else
if(0<ar)var
t=0;else
var
y=x,t=1;if(!t)var
W=b(e[18],j,M),X=b(c[H],W,d),y=g(a9[11],X,m[1],x);var
z=g(c[91],r[1],ar,y),A=z[2],p=z[1];if(be[1]){var
Y=b(e[18],j,M),B=b(e[18],p,Y),Z=a(f[3],v5);b(aq[6],0,Z);var
_=b(c[H],B,d),$=g(q[V],_,m[1],A);b(aq[6],0,$);var
aa=a(f[3],v6);b(aq[6],0,aa);var
ab=cE(d,m[1],B);b(aq[6],0,ab)}var
ac=b(e[18],j,M),E=a(bf,[0,b(e[18],p,ac),A]),F=E[1],G=F[2],I=F[1],ad=E[2],ae=ai(v7,d,[0,m[1]],U,ak),af=ai(v8,d,[0,m[1]],ad,ae);if(I)if(l){var
J=l[1],K=I[1],L=K[2],N=K[1][1],O=v(d,m[1],J),i=O[1],ag=O[2],ah=T(0,N),aj=h_(r[1],ag,ah),al=P(i,iz([0,d],i,af,iA(J)),aj),Q=b(w[24],i,L[1]),an=a(w[6],Q),ap=function(b){var
d=a(D[2][1][1],b);return a(c[10],d)},as=b(e[17][15],ap,an),at=a(e[17][1],N),R=b(e[17][h],at,as),au=R[2],av=R[1],aw=a(az[9],d),ax=function(c,b){return[0,a(D[2][1][1],c),b]},ay=g(e[17][21],ax,aw,au),aA=b(c[h][9],ay,al),S=b(c[h][4],av,aA),aB=a(w[12],Q);n(ao[2],0,aB,i,S);var
aC=b(c[5],i,S);m[1]=g(w[31],L[1],aC,i);var
s=G,k=1}else
var
k=0;else
if(l)var
k=0;else
var
s=G,k=1;if(!k)var
s=a(C[2],v9);var
aD=b(e[18],p,j);return b(c[38],s,aD)},bh=g(e[19][54],bg,bc,aj),bi=P(m[1],ap,bd),bj=m[1],bk=function(a){return P(bj,ap,a)},bl=b(e[19][15],bk,bh),bm=ek(B-1|0,x)[2],bn=a(D[1][1][3],bm),bo=b(c[h][1],B,bn),bp=a(c[9],B),as=g(Z[72],d,m[1],bo),at=as[1],bq=as[2],br=g(Z[76],d,at[1],4),bs=[0,bK(m[1],at),bq],bt=a(Z[5],bs),bu=aF(Z[77],d,m[1],bt,br,bi,bp,bl),bw=[0,bu,a(e[19][12],ba)],bx=a(c[21],bw),by=b(c[38],bx,x),au=b7(d,p,I,x),aw=b(A[35],m[1],by);n(ao[5],d,m,aw,au);return[0,m[1],aw,au]}var
J=[0,p],ax=t(bv),bz=fK(B-1|0,x)[1],bA=u===ax?bv[1]:k===ax?a(s[2],bv):bv,N=al(J,bA),bB=function(e){if(e){var
b=v(d,J[1],e[1]),f=b[3],g=b[2];J[1]=b[1];return[0,g,f]}var
h=b5(B);return[0,a(c[8],h),N]},ay=b(e[19][15],bB,aj),bC=J[1],bD=function(d,c){var
e=c[2],f=[0,c[1]],g=a(C[21],d),h=b(C[16],v_,g);return an([0,a(i[1][6],h)],f,e)},bE=b(e[19][16],bD,ay),bF=function(d,f){var
e=d[1],g=d[2];return[0,e+1|0,[0,b(bb,a(c[h][1],e),f),g]]},aA=g(e[19][17],bF,v$,bE)[2],bG=aV(ay.length-1,x),bH=b(c[37],I,bG),bI=b(c[38],bH,aA),bJ=b5(a(e[17][1],aA)),bL=[0,a(c[8],bJ)],bM=an([0,a(i[1][6],wa)],bL,N),bN=b5(a(e[17][1],bz)),bO=[0,a(c[8],bN)],bP=[0,bM,[0,an([0,a(i[1][6],wb)],bO,N),0]],bQ=b(c[h][1],2,bI),aB=cA(d,bC,[0,[0,av,[3,aR,0]]],b(c[38],bQ,bP)),aC=aB[2],O=aB[1],bR=b(c[75],O,aC)[1];o[1]=g(aJ[8][4],bR,0,o[1]);var
aE=b7(d,O,I,x);return[0,O,a(c[17],[0,aC,2,aE]),aE];case
2:var
aG=l[5],aH=l[1][1],bS=l[6],bT=aG[2],bU=aG[1],bV=l[3],bW=l[2],bX=function(a){var
c=a[5],e=a[2];function
f(b){var
a=v(d,b,c),f=a[1];return[0,f,aD(a[2],e)]}return b(cc[2],0,f)},bY=b(e[17][15],bX,bS),bZ=a(j[37],bY),b0=a(j[64][1],p),b1=b(j[18],b0,bZ),aI=g(j[15],d,b1,bT)[2],b2=b(j[7],bU,aI),b3=a(e[17][5],b2),b4=b(c[37],bW,aH),b6=b(c[h][11],bV,b3),b8=b(c[38],b6,aH);return[0,a(j[6],aI),b8,b4];case
3:var
aK=l[1],aL=aK[1],b9=aK[2],R=v(d,p,l[2]),F=R[1],b_=R[3],b$=R[2],ca=io(wc,d,F,b9)[2],aM=a(e[19][67],ca),cb=a(c[21],[0,b$,aM]),cd=b(U[24],F,cb),ce=b(c[38],cd,aL);return[0,F,ce,b7(d,F,fA(F,b_,aM),aL)];case
4:var
l=l[2];continue;default:var
G=l[2],aN=l[1][1],aO=G[6],S=G[5],cf=aO[2],cg=aO[1],ch=G[3],ci=G[2],cj=G[1][1],W=v(d,p,l[3]),aP=W[3],ck=W[2],cl=W[1],cm=b(c[h][1],1,aP),co=[0,[0,a(i[1][6],wd)],ck,aP,cm],cp=a(c[20],co),aQ=f7(cl,S,a(y[2],0),cp,[0,av,[3,we,[0,cj]]])[1];o[1]=g(aJ[8][4],S,0,o[1]);E[1]=[0,[0,S,ch],E[1]];var
cq=a(c[34],[0,cg,cf]),cr=b(c[38],cq,aN);return[0,aQ,cr,b7(d,aQ,ci,aN)]}}var
d=v(X,r[1],l),m=d[3],p=d[2];r[1]=d[1];return[0,E[1],o[1],p,m]}function
jA(g,d,b){if(d){var
e=d[1];if(typeof
b!=="number"&&0===b[0]){var
f=b[1];if(1===f[0]){var
h=b[2][1],i=f[1];return 0===e[0]?ee(g,e,a(c[22],i)):0===h?1:0}}return 0}return 0}var
wf=b(bd[8][11],bd[8][10],bd[8][7]),wg=[0,a(a9[15],wf),2],wh=a(r[50],wg);function
cR(b){return a(e[8],b[3])}function
f9(H,O,F,o,E,d,D,m,C,B,N){var
f=m[1],P=m[3],Q=m[2];d[1]=a(A[45],d[1]);var
h=f8(E,d,D,B),I=h[2],S=h[4],T=h[3],V=h[1];a(A[48],d);var
W=dA(a(A[35],d[1]),B),X=b(U[29],d[1],S),Y=b(c[5],d[1],X),Z=b(c[5],d[1],T),p=aF(aW[5],D,f,d[1],0,[0,E],Z,Y),q=p[3],J=p[2],L=J[1],s=[0,i[1][10][1]],_=J[2],$=p[1];function
aa(c){var
h=c[3],e=c[1],n=c[5],o=c[4],p=c[2],j=a(g8(i[1][1],e),L);if(b(aJ[8][3],j,I))var
q=b(aJ[8][22],j,I),t=hD(0),u=b(l[66][29],q,r[16]),f=[0,b(l[66][3],u,t)];else
if(jA(d[1],C,h[2])){s[1]=b(i[1][10][4],e,s[1]);var
k=a(G[7],C);if(0===k[0])var
m=l[1];else
var
A=k[1][1],B=l[1],D=function(b){return x(a(r[68],[0,[0,wi,[1,b]],0]))},m=g(G[24],D,B,A);var
v=[0,m,[0,x(hE(0)),0]],w=[0,x(r[28]),v],y=[0,x(wh),w],z=a(l[7],y),f=[0,au(a(l[21],z))]}else
var
f=[0,aW[6][1]];return[0,e,p,h,o,n,f]}var
j=b(e[19][15],aa,$);function
ab(a){var
c=a[1],d=a[2];return[0,c,d,b(e[17][36],c,L)]}var
ac=b(e[17][15],ab,V);function
ad(d,c){function
h(a){return b(R[1],0,[1,a[1]])}var
k=b(e[19][49],h,j);b(cL[2][88],1,k);var
l=a(i[1][8],f);return g(N,W,_,[0,c,l,[0,d,F,0],ac,s[1]])}var
t=a(bQ[1],ad);function
u(e){var
f=bd[14],g=a(c[8],e),h=d[1],i=a(y[2],0),j=n(U[15],f,i,h,g);return b(c[5],d[1],j)}var
v=[0,2,F,0],k=b(c[37],P,Q);if(H){var
M=H[1];if(0===M[0]){var
z=M[1];if(z)if(!z[2]){var
ai=z[1],aj=b(c[37],k,[0,[0,0,k],0]),ak=b(c[5],d[1],aj),al=0,am=0,an=a(e[9],ai),ao=function(a){return b(R[1],[0,a[1]],a[2])},ap=[0,[0,[0,b(G[16],ao,an),am],al]],aq=a(w[bq],d[1]);Ke(aW[8],[0,[0,f,q,ak,o,j],0],aq,0,0,[0,v],[0,u],[0,t],0,0,ap);return 0}var
ae=b(c[37],k,O),af=b(c[5],d[1],ae),ag=a(w[bq],d[1]),ah=b(K[5],f,wj);bI(aW[7],ah,[0,q],af,ag,0,[0,o],[0,v],0,[0,u],[0,t],0,j);return 0}}var
ar=b(c[5],d[1],k),as=a(w[bq],d[1]);bI(aW[7],f,[0,q],ar,as,0,[0,o],[0,v],0,[0,u],[0,t],0,j);return 0}function
jB(g,d,c){if(0===c[0])return[0,P(g,d,c[1])];var
h=c[1];try{var
i=a(e[8],d),f=b(e[17][7],i,h-1|0);if(0===f[0]){var
j=[1,f[1]];return j}throw[0,Q,wl]}catch(a){a=E(a);if(a===W)throw[0,Q,wk];throw a}}function
f_(d,c,b){return 0===b[0]?[0,a(d,b[1])]:[1,a(c,b[1])]}function
jC(f,e,d){var
g=b(c[5],f,d);return b(e,function(d){var
e=[0,a(aN[34],d)],f=b(R[1],0,e),g=a(am[18],f),h=a(y[2],0),c=b(y[47],h,g),i=c[1],j=dj(c[2])[1],k=[0,a(ar[16],i),j];return a(aO[35],k)},g)}function
f$(b,a){function
c(c){return jC(b,a,c)}return function(a){return dA(c,a)}}aG(1072,[0,f7,f8,jA,cR,f9,jB,f_,jC,dA,f$],gS);var
jD=[0,function(f,e){var
c=f,a=e;for(;;){if(c){var
g=c[2],h=c[1];if(a){var
i=a[2],d=b(aJ[4],h,a[1]);if(0===d){var
c=g,a=i;continue}return d}return-1}return a?1:0}}],ae=a(e[21][1],jD),jE=ae[22],wm=ae[1],wn=ae[2],wo=ae[3],wp=ae[4],wq=ae[5],wr=ae[6],ws=ae[7],wt=ae[8],wu=ae[9],wv=ae[10],ww=ae[11],wx=ae[12],wy=ae[13],wz=ae[14],wA=ae[15],wB=ae[16],wC=ae[17],wD=ae[18],wE=ae[19],wF=ae[20],wG=ae[21],wH=ae[23],wI=ae[24];function
jF(d,c){try{var
g=d[4],h=function(d){var
e=d[3],f=a(ar[16],c),g=a(aN[34],e),h=a(am[9],g);return b(ar[5],h,f)},i=b(e[17][31],h,g);return i}catch(b){b=E(b);if(b===W)return fx(0,a(f[3],wJ));throw b}}function
jG(c){var
b=a(aH[15],wK);return a(aH[14][14],b)}function
wL(b){return x(a(r[62],[0,b,0]))}var
wM=a(l[53],wL),wN=x(r[61]),wO=b(l[5],wN,wM);function
dB(c,a){var
d=b(e[18],a,wP),f=[0,jG(0)];return x(M(ga[7],0,f,0,c,d))}function
gb(a){return b(C[16],a[2],wQ)}function
wR(b){var
c=dB(0,b),d=[0,a(l[21],c),0],e=g(cf[2],0,l[66][2],b),f=[0,a(j[70][8],e),d],h=a(l[7],f);return a(l[17],h)}function
gc(c,b){var
d=dB(0,b),e=[0,a(l[21],d),0],f=g(cf[6],0,b,c),h=[0,a(j[70][8],f),e],i=a(l[7],h);return a(l[17],i)}function
wS(b){var
c=g(cf[2],0,l[66][2],[0,b,0]),d=a(j[70][8],c);return a(l[17],d)}function
cS(c){var
e=a(cf[4],c);function
d(c){if(c){var
e=c[1],m=c[2],h=a(ar[16],e[1]),n=e[5]?cT[3]:cT[4],o=function(a){return b(n,0,a)},p=a(l[66][59],h),i=b(j[17],p,o),r=function(e){if(be[1]){var
c=a(f[3],wT);b(aq[10],0,c)}return d(m)};if(be[1])var
s=function(c){var
d=a(j[66][3],c),e=a(j[66][6],c),k=a(j[66][5],c),l=g(q[V],k,e,d),m=a(f[3],wU),n=a(bi[58],h),o=a(f[3],wV),p=b(f[12],o,n),r=b(f[12],p,m),s=b(f[12],r,l);b(aq[10],0,s);return i},k=a(j[66][9],s);else
var
k=i;return b(j[22],k,r)}var
t=a(f[3],wW);return b(l[66][4],0,t)}var
h=d(e);return a(j[70][8],h)}function
jH(c,b,a){if(a){var
f=a[2],d=ch(A[4],c,b,0,0,0,0,0,0,a[1]),g=d[2],e=jH(c,d[1],f);return[0,e[1],[0,g,e[2]]]}return[0,b,0]}function
jI(o,h,n,m){var
e=o,j=n,i=m;for(;;){var
p=b(q[60],h,i),d=b(c[3],h,p);switch(d[0]){case
6:var
k=d[2],u=d[3],v=d[1];if(1===j)try{var
l=g(Z[72],e,h,k)[1],w=[0,l[1][1],l[2]];return w}catch(a){a=E(a);if(a===W)return a5(wY);throw a}var
e=b(c[bt],[0,v,k],e),j=j-1|0,i=u;continue;case
8:var
x=d[4],e=b(c[bt],[1,d[1],d[2],d[3]],e),i=x;continue;default:var
r=g(q[V],e,h,i),s=a(f[3],wX),t=b(f[12],s,r);return g(af[6],0,0,t)}}}function
gd(u,p){function
d(G){function
d(d){function
k(av){var
q=b(e[17][15],j[10],av);function
H(e){var
f=b(w[23],d,e),g=a(w[5],f);return a(c[8],g)}var
l=b(e[17][15],H,q);function
I(c){var
e=b(w[23],d,c);return a(w[6],e)}var
J=b(e[17][15],I,q),v=a(e[17][cm],J),x=v[1],K=v[2];function
L(a){return g(D[2][6],at[74],x,a)}var
m=b(e[17][25],L,K)?b(az[32],x,G):G;if(u)var
k=b(e[17][15],i[1][6],u);else
var
au=function(f,e){var
c=b(w[50],e,d);if(c)return c[1];var
g=a(C[21],f),h=b(C[16],xa,g);return a(i[1][6],h)},k=b(e[17][16],au,q);var
o=a(e[17][1],k),r=a(e[17][1],p),s=a(e[17][1],l),y=o===r?1:0,M=y?o===s?1:0:y;if(1-M){var
N=b(e[15][43],s,wZ),O=a(f[3],N),P=a(f[16],s),R=a(f[3],w0),S=1===r?w1:w$,T=a(f[3],S),U=a(f[16],r),V=a(f[3],w2),X=b(e[15][43],o,w3),Y=a(f[3],X),Z=a(f[16],o),_=a(f[3],w4),$=b(f[12],_,Z),aa=b(f[12],$,Y),ab=b(f[12],aa,V),ac=b(f[12],ab,U),ad=b(f[12],ac,T),ae=b(f[12],ad,R),ag=b(f[12],ae,P),ah=b(f[12],ag,O);g(af[6],0,w5,ah)}function
ai(c,b,a){return[0,c,b,a]}var
z=n(F[77],ai,k,p,l),A=a(e[17][5],z),aj=jI(m,d,A[2],A[3])[1];function
ak(p,o){var
h=p,e=o;for(;;){if(e){var
j=e[1],l=j[3],k=j[1],q=e[2],r=jI(m,d,j[2],l)[1];if(1-b(i[23][13],aj,r))a5(w6);try{b(D[2][5],k,h);var
x=1,n=x}catch(a){a=E(a);if(a!==W)throw a;var
n=0}if(n){var
s=a(f[3],w7),t=a(bN[9],k),u=a(f[3],w8),v=b(f[12],u,t),w=b(f[12],v,s);g(af[6],0,w9,w)}var
h=[0,[0,k,b(c[5],d,l)],h],e=q;continue}return h}}var
al=ak(a(az[9],m),z);function
am(a){return a-1|0}var
B=b(e[19][50],am,p);function
an(a){return[0,a]}var
ao=b(e[19][50],an,k),t=[0,function(a){throw[0,Q,w_]}];function
ap(f){var
g=a(az[27],al),d=jH(b(az[42],g,m),f,l),i=d[2],j=d[1],n=a(e[17][9],k),o=a(c[h][11],n),p=b(e[19][50],o,i),q=[0,ao,a(e[19][12],l),p];t[1]=function(b){return a(c[31],[0,[0,B,b],q])};return[0,j,a(t[1],0)]}var
aq=b(cc[2],0,ap);function
ar(c){function
d(b){return[0,b,a(t[1],c+1|0)]}return b(cc[2],0,d)}var
as=[0,aq,b(e[17][54],B.length-1-1|0,ar)];return a(j[37],as)}return b(j[71][1],j[64][6],k)}return b(j[71][1],j[54],d)}return b(j[71][1],j[55],d)}function
eH(e,d,c){var
b=jF(e,d),a=b[2],f=b[1];return[0,f,a,S(c,a)[a+1]]}function
jJ(d,i,h,g){function
f(g,a){var
i=0;function
j(g,a,j){if(g)return g;var
e=b(c[82],d,j);switch(a[0]){case
0:var
i=e[1];if(a[1]===h)return[0,b(c[67],d,i)];break;case
1:return f(a[2],e[2])}return 0}return n(e[17][23],j,i,g,a)}var
j=f(a(e[17][9],i),g);return a(G[7],j)}function
dC(d){var
e=a(z[7],d),f=a(z[2],d);switch(b(c[3],f,e)[0]){case
6:var
h=x(r[16]);return g(l[5],h,dC,d);case
8:var
i=x(r[58]);return g(l[5],i,dC,d);default:return a(l[1],d)}}function
eI(d){var
c=[0,x(b(r[H],0,0)),0];return a(l[7],c)}function
jK(a){return 1===a[0]?[0,a[4]]:0}function
jL(a){return 5===a[0]?[0,a[3]]:0}function
jM(a){return 0===a[0]?[0,a[2]]:0}function
eJ(c,b){return b?a(c,b[1]):0}function
ge(c){function
d(a){var
d=[0,gb(c),0],e=au(dB(xb,[0,c[2],d])),f=b(r[mn],0,a);return b(j[18],f,e)}var
e=a(l[66][59],c[1]);return b(j[17],e,d)}function
jN(h,g,b,f){var
d=a(e[19][8],g);S(d,b)[b+1]=f;return a(c[21],[0,h,d])}function
bV(d,o,v,p,L){var
m=L;for(;;)switch(m[0]){case
0:var
A=m[2],N=m[4],F=eJ(jM,v);if(F)var
O=F[1],P=function(a){return[0,a]},H=b(e[17][15],P,O);else
var
ah=function(a){return 0},H=b(e[17][15],ah,A);if(a(e[17][53],A))var
B=l[1];else{var
aa=function(I,j,k){if(k){var
m=k[1];try{var
J=d[3],K=a(e[17][5],m[2]),u=b(aJ[8][22],K,J)}catch(a){a=E(a);if(a===W)throw[0,Q,xi];throw a}var
L=u[1],M=m[4],N=[0,u[2],p],O=a(e[17][1],m[4]),B=L,A=o[1]+O|0,z=N,v=M}else
var
aU=j[4],aV=a(e[17][1],j[4]),B=j[7],A=o[1]+aV|0,z=p,v=aU;var
P=[0,A,o[2]],R=0,S=j[9];function
T(a){return a[9]}var
U=[0,bV(d,P,b(G[16],T,k),z,S),R],X=a(G[3],k)?l[1]:cS(b(C[16],d[1][2],xq)),Y=[0,x(r[28]),[0,X,U]],Z=x(b(r[81],fn,1)),_=[0,a(l[21],Z),Y],$=as(xj,a(l[7],_));try{var
aT=b(jE,j[2],d[2]),n=aT}catch(a){a=E(a);if(a!==W)throw a;var
n=a5(xk)}var
F=n[1],aa=n[2];if(be[1]){var
s=a(y[2],0),ab=ay(s,w[16],j[5]),ac=a(f[3],xl),ad=function(b){var
d=a(c[8],b);return g(q[V],s,w[16],d)},ae=g(f[39],f[13],ad,aa),af=a(f[3],xm),ag=g(q[V],s,w[16],j[7]),ah=a(f[3],xn),ai=a(bN[9],j[1]),aj=a(f[3],xo),ak=a(i[1][8],F),al=a(f[3],ak),an=a(f[3],xp),ao=b(f[12],an,al),ap=b(f[12],ao,aj),ar=b(f[12],ap,ai),at=b(f[12],ar,ah),av=b(f[12],at,ag),aw=b(f[12],av,af),ax=b(f[12],aw,ae),az=b(f[12],ax,ac),aA=b(f[12],az,ab);b(aq[10],0,aA)}var
aB=a(aN[34],F),aC=a(am[9],aB),t=a(e[7],j[5]);function
aE(b){var
d=a(D[2][1][1],b);return a(c[10],d)}var
H=b(e[17][15],aE,v),aF=aQ(0,t),aG=aD(b(c[h][4],H,B),aF),aH=aQ(0,t),aI=b(e[17][10],H,aH),aK=b(e[17][10],aI,[0,aG,0]),aL=a(aO[50],aC),aM=aD(a(c[8],aL),aK),aP=b(c[37],aM,t),aR=au($),aS=x(g(r[gJ],[0,j[1]],aP,aR));return b(l[5],I,aS)},ab=n(e[17][23],aa,l[1],A,H),ac=[0,dB(0,0),0];if(a(G[3],v))var
I=l[1];else
var
ag=cS(b(C[16],d[1][2],xr)),I=as(xs,a(l[21],ag));var
ad=[0,eI(d[1]),[0,I,ac]],ae=cS(d[1][2]),af=[0,ab,[0,a(l[21],ae),ad]],B=a(l[7],af)}if(0===N[0]){var
R=[0,as(xd,x(ge(d[1]))),0],T=function(c){var
d=cT[3];function
e(a){return b(d,0,a)}var
f=fg(c),g=a(l[66][59],f),h=x(b(j[17],g,e));return a(l[21],h)},U=[0,b(l[30],T,p),R],X=[0,as(xe,eI(d[1])),U],Y=[0,as(xf,B),X],Z=cS(d[1][2]),_=[0,dC,[0,a(l[21],Z),Y]];return as(xg,a(l[7],_))}var
$=[0,dC,[0,B,[0,x(hF(0)),0]]];return as(xh,a(l[7],$));case
1:var
ai=m[4],aj=m[2],ak=m[1][2],J=eJ(jK,v);if(J)var
al=J[1],K=function(a){return S(al,a)[a+1]};else
var
K=function(a){return 0};var
an=function(a){var
c=a-1|0,e=S(ai,c)[c+1];if(e){var
f=e[1],g=bV(d,o,K(a-1|0),p,f),h=x(fq(0));return b(l[5],h,g)}return x(fq(0))},ao=function(d){var
k=a(z[7],d),m=a(z[2],d),i=b(c[3],m,k);if(9===i[0]){var
p=a(e[19][11],i[2]),j=a(e[17][gQ],p),q=0<=o[1]?b(e[17][h],o[1],j)[2]:j,r=function(a){return 1-em(a)},s=b(e[17][33],r,ak);return a(x(hM(jJ(a(z[2],d),s,aj,q))),d)}var
n=a(f[3],xt);return g(l[24],0,n,d)};return as(xu,b(l[8],ao,an));case
2:var
ap=m[6],ar=m[4],at=function(f){var
c=t(db),g=bV(d,o,v,p,b(e[17][7],ap,f-1|0)[5]),h=u===c?db[1]:k===c?a(s[2],db):db,i=x(h);return b(l[5],i,g)},av=b(l[8],ar,at),aw=x(r[28]);return as(xv,b(l[5],aw,av));case
3:var
m=m[2];continue;case
4:var
m=m[2];continue;default:var
ax=m[3],az=m[2],aA=eJ(jL,v),aB=a(e[7],az[1]),aC=function(h){var
F=a(z[7],h),G=a(z[2],h),m=b(c[3],G,F);if(9===m[0]){var
s=m[2],I=m[1],t=b(bF[51],s.length-1-1|0,s),n=t[1],J=S(t[2],0)[1],K=a(z[2],h),u=b(c[73],K,J),v=u[2],w=u[1],L=a(z[2],h),N=b(c[5],L,w),y=eH(d[1],N,v),q=y[2],O=y[3],k=b(z[20],aB,h),A=q-o[2]|0,P=a(z[2],h),B=g(e[19][7],n,A+1|0,n.length-1-(A+1|0)|0),C=i[1][10][1],D=function(b,a){return cv(P,xc,a,b)},E=g(e[19][18],D,B,C),Q=a(i[1][10][21],E),R=function(a){return[0,[0,0,a],0]},T=[0,[0,b(e[17][15],R,Q)],1],U=jN(w,v,q,a(c[10],k)),V=a(c[10],k),W=[0,jN(I,n,q-o[2]|0,V),[0,U]],X=a(c[21],W),Y=[0,bV(d,o,aA,p,ax),0],Z=a(r[76],[0,k,0]),_=[0,a(j[70][8],Z),Y],$=[0,as(xx,x(b(r[5],X,2))),_],aa=[0,a(z[2],h),O],ab=[0,as(xy,x(M(r[145],1,0,[0,k],aa,T))),$];return b(l[7],ab,h)}var
H=a(f[3],xw);return g(l[24],0,H,h)},aE=[0,x(ge(d[1])),0],aF=a(l[20],[0,aC,0]),aG=eI(d[1]),aH=cS(d[1][2]),aI=a(l[21],aH),aK=b(l[5],aI,aG),aL=[0,b(l[10],aK,aF),aE],aM=[0,x(r[28]),aL];return as(xz,a(l[7],aM))}}function
eK(d,c){if(be[1]){var
g=function(h){function
g(g){function
h(i){var
h=b(e[17][15],j[10],i),l=k0(bi[85],0,0,g,0,0,0,0,h),m=a(f[3],xA),n=a(f[3],d),o=a(f[3],xB),p=b(f[12],o,n),q=b(f[12],p,m),r=b(f[12],q,l);b(aq[10],0,r);function
v(i){var
e=i[1];if(e[1]===cx[29])var
c=e[3],n=e[2],o=k0(bi[85],0,0,g,0,0,0,0,h),l=t(c),p=a(f[3],xC),q=u===l?c[1]:k===l?a(s[2],c):c,r=a(f[13],0),v=a(f[3],d),w=a(f[3],xD),x=a(f[16],n),y=a(f[3],xE),z=b(f[12],y,x),A=b(f[12],z,w),B=b(f[12],A,v),C=b(f[12],B,r),D=b(f[12],C,q),E=b(f[12],D,p),m=b(f[12],E,o);else
var
m=a(af[17],i);var
F=a(f[3],xF),G=b(f[12],F,m);b(aq[10],0,G);return a(j[16],0)}function
w(c){if(0===c){var
e=a(f[3],xG),g=a(f[3],d),h=b(f[12],g,e);b(aq[10],0,h);return a(j[16],0)}return au(function(c){var
d=a(bi[84],c),e=a(f[3],xH),g=b(f[12],e,d);b(aq[10],0,g);return[0,[0,c[1],0],c[2]]})}var
x=b(j[71][1],j[53],w),y=b(j[18],c,x);return b(j[23],y,v)}return b(j[71][1],j[64][6],h)}return b(j[71][1],j[54],g)};return b(j[71][1],j[55],g)}return c}function
jO(o,D,d,C,n,B,A){if(o){var
p=o[1];if(0===p[0]){var
f=p[1];if(f)if(!f[2]){var
Z=a(at[66],D),_=a(y[2],0),$=b(az[55],_,Z),aa=S(a(at[72],$)[1][1],0)[1],ab=b(K[5],C,xO),ac=[0,bV(d,xP,0,0,n),0],ad=[0,x(r[28]),ac],ae=function(f,e){var
d=a(bD,f),h=d[3],i=d[1];function
l(n,e){var
b=t(aB),f=u===b?aB[1]:k===b?a(s[2],aB):aB,g=a(aO[50],f),d=t(aE),i=a(c[8],g),j=u===d?aE[1]:k===d?a(s[2],aE):aE,l=a(aO[50],j),m=[0,0,a(c[8],l),i,h];return[0,e,a(c[20],m)]}var
m=g(r[54],0,l,[0,i,0]);return b(j[70][8],m,e)},af=[0,a(l[40],ae),ad],ag=[0,x(b(r[8],[0,ab],aa+1|0)),af],ah=[0,x(dd(0)),ag];return au(a(l[7],ah))}var
E=function(a){return 0===a[2][0]?1:0},q=b(e[17][35],E,f),h=q[1],F=q[2],G=function(b){var
a=b[2];return 0===a[0]?a[1]+1|0:-1},H=b(e[17][15],G,h),I=function(b){var
a=b[1][5];if(a)if(0===a[1][0])return 1;return 0},v=b(e[17][35],I,A),w=v[2],i=v[1],J=ga[7],L=[0,xI,[0,d[1][2],0]],m=function(c){if(c){var
d=c[2];if(d){var
e=[0,m(d),0],f=[0,a(j[16],0),e],g=a(j[37],f),h=a(r[md],0);return b(j[71][2],h,g)}}return a(j[16],0)},z=function(c){function
f(b){var
c=b[3][5];return au(bV(d,[0,0,a(e[17][1],h)],0,0,c))}var
g=b(e[17][15],f,c),i=a(j[37],g);return b(j[71][2],r[28],i)},N=z(w),O=function(e){var
c=e[2];if(1===c[0]){var
d=c[1];if(d)return b(r[8],0,d[1]+1|0)}return a(j[16],0)},P=b(e[17][15],O,F),R=a(j[37],P),T=b(j[71][2],R,N),U=z(i),V=gd(0,H),W=b(j[71][2],V,U),X=a(e[17][1],i),Y=function(o){function
k(g,d){var
m=a(j[66][6],o),e=b(c[3],m,g);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
h=f[1],i=f[2],n=e[1];if(1===d)return[0,h,[0,i]];var
l=k(i,d-1|0),p=l[2];return[0,a(c[21],[0,n,[0,h,l[1]]]),p]}}if(1===d)return[0,g,0];throw[0,Q,xJ]}var
d=k(a(j[66][3],o),X),f=d[2],l=d[1],p=eK(xK,M(J,0,0,0,0,L)),q=[0,a(j[16],0),0],s=a(e[17][1],h),t=g(j[32],1,s,W),u=m(i),v=[0,eK(xL,b(j[71][2],u,t)),q],x=a(j[37],v),y=b(r[lE],0,l);if(f)var
z=f[1],A=[0,a(j[16],0),0],B=eK(xM,m(w)),C=b(j[71][2],r[16],B),D=[0,eK(xN,b(j[71][2],C,T)),A],E=a(j[37],D),F=a(c[18],[0,0,l,z]),G=b(r[lE],0,F),n=b(j[71][2],G,E);else
var
n=a(j[16],0);var
H=dd(0),I=b(j[71][2],H,n),K=b(j[71][2],I,y),N=b(j[71][2],K,x);return b(j[71][2],N,p)};return a(j[66][10],Y)}}var
ai=[0,bV(d,xQ,B,0,n),0],aj=[0,x(r[28]),ai],ak=[0,x(dd(0)),aj],al=a(l[7],ak);return au(a(l[22],al))}function
xR(a){function
c(d){function
c(a){return b(y[52],[0,a],1)}return b(e[17][14],c,a)}return[0,c,function(d){function
c(a){return b(y[52],[0,a],0)}return b(e[17][14],c,a)}]}function
jP(o,J,i,d,h,I,H,G){function
w(a){return hL(a)}var
L=o[4];function
N(b){var
c=a(a6[29],b[3]);return a(ar[8],c)}var
A=t(da),O=b(e[17][15],N,L),P=u===A?da[1]:k===A?a(s[2],da):da,B=xR([0,a(ar[8],P),O]),R=B[2],S=B[1];function
D(c,b){a(S,0);var
d=a(c,b);a(R,0);return d}function
p(a){return D(x(hJ(0)),a)}var
U=x(r[61]);function
V(a){return D(U,a)}var
X=0,Y=[0,b(C[16],o[2],xS),0];function
Z(a){return fp(Y,X,a)}var
_=0,$=[0,o[2],0];function
aa(a){return fp($,_,a)}var
q=b(l[5],aa,Z);function
ab(e){var
k=a(z[7],e),m=a(z[2],e),j=b(c[3],m,k);if(9===j[0]){var
h=j[2];if(3===h.length-1){var
n=h[2],o=h[3],f=a(z[2],e),q=b(c[82],f,n)[1],s=b(c[82],f,o)[1],t=a(c[22],d);if(g(c[94],f,t,q))if(ee(f,i,s)){var
u=0===i[0]?[0,i[1][2]]:[1,i[1][3]],v=a(r[68],[0,[0,xU,[1,d]],[0,[0,xT,u],0]]),w=[0,p,[0,x(hG(0)),0]],y=[0,x(v),w];return b(l[7],y,e)}return a(x(r[e9]),e)}}return a(x(r[e9]),e)}var
ac=x(r[e9]),F=b(l[4],ac,ab);function
v(a){return g(r[c5],0,0,a)}function
m(i,B){var
d=B;for(;;){switch(i[0]){case
0:var
s=i[2];if(0===i[4][0])switch(d[0]){case
0:var
t=d[2];if(0===d[4][0]){var
C=function(p,o,d){try{var
i=a(e[17][5],d[2]),f=b(aJ[8][22],i,J)}catch(a){a=E(a);if(a===W)throw[0,Q,xV];throw a}var
h=f[2],k=f[1];return function(i){var
q=a(z[8],i),f=[0,a(z[2],i)],j=a(e[7],d[5]),s=[0,k,T(0,j)],t=a(c[21],s),u=T(0,j),v=a(c[21],[0,d[7],u]),w=bh(q,f,d[6],t,v),y=b(c[37],w,j);function
n(d){var
a=b(c[82],f[1],d)[1];if(b(c[55],f[1],a))return b(c[74],f[1],a)[1];throw[0,Q,xW]}var
A=n(k),B=n(d[7]);function
C(b){return a(x(a(r[68],[0,[0,xY,[1,A]],[0,[0,xX,[1,B]],0]])),b)}var
D=[0,C,[0,as(xZ,m(o[9],d[9])),0]],E=[0,x(r[28]),D],F=au(a(l[7],E)),G=x(g(r[c5],0,[0,h],F)),H=x(a(r[78],0)),I=au(b(l[5],H,G)),J=g(r[gJ],[0,h],y,I),K=a(c[10],h),L=[0,x(b(cT[3],0,K)),[0,p,0]],M=[0,x(J),L],N=[0,a(cx[11],f[1]),M];return b(l[7],N,i)}},D=a(e[17][1],t);if(a(e[17][1],s)===D){var
G=n(e[17][23],C,l[1],s,t),H=[0,G,[0,as(x0,a(l[21],q)),[0,p,[0,F,0]]]],I=[0,x(r[28]),H];return as(x1,x(v(au(a(l[7],I)))))}throw[0,Q,x2]}var
k=1;break;case
3:var
h=0,k=0;break;default:var
h=1,k=0}else
var
k=1;if(k)switch(d[0]){case
0:var
u=d[4],L=d[1][1];if(0!==u[0]){var
N=a(b8,b(e[17][7],L,u[1]-1|0));return x(v(w(a(K[10][16],N))))}var
h=1;break;case
3:var
h=0;break;default:var
h=1}break;case
1:var
O=i[4];switch(d[0]){case
1:var
P=d[4],R=d[2];return as(x5,function(d){var
r=a(z[7],d),s=a(z[2],d),i=b(c[3],s,r);if(9===i[0]){var
j=i[2];if(3===j.length-1){var
u=j[3],h=a(z[2],d),k=b(c[82],h,u),y=k[2],A=k[1];if(cn[1])var
B=b(c[78],h,A)[3],o=B,n=l[1];else
var
M=a(e[17][9],y),o=b(e[17][7],M,R-1|0),n=q;var
C=b(c[82],h,o)[1],D=b(c[67],h,C),E=a(e[19][11],O),F=function(a){return a},G=b(e[17][70],F,E),H=a(e[19][11],P),I=function(a){return a},J=b(e[17][70],I,H),K=function(c){var
d=b(e[17][7],G,c-1|0),f=[0,n,[0,p,[0,m(d,b(e[17][7],J,c-1|0)),0]]];return a(l[7],f)},L=x(w(D));return a(x(v(au(b(l[8],L,K)))),d)}}var
t=a(f[3],x4);return g(l[24],0,t,d)});case
3:var
h=0;break;default:var
h=1}break;case
2:return as(x6,m(b(e[17][7],i[6],0)[5],d));case
4:var
S=m(i[2],d),U=x(hC(0));return as(x7,b(l[5],U,S));case
5:var
V=i[3];switch(d[0]){case
3:var
h=0;break;case
5:var
y=d[2],X=d[3],Y=a(e[7],y[1]),Z=y[5],A=function(d){var
v=a(z[7],d),w=a(z[2],d),i=b(c[3],w,v);if(9===i[0]){var
h=i[2];if(3===h.length-1){var
B=h[2],C=h[3],e=a(z[2],d),k=b(c[73],e,B),D=k[2],E=k[1],n=b(c[73],e,C),G=n[2],H=n[1],I=eH(o,b(c[5],e,E),D)[3],s=eH(o,b(c[5],e,H),G),t=s[3],J=s[1],u=b(z[20],Y,d);if(b(aJ[3],J,Z)){var
K=[0,q,[0,m(V,X),0]],L=a(r[76],[0,u,0]),N=[0,a(j[70][8],L),K],O=[0,x(M(r[bZ],0,[0,u],t,0,ds[4])),N],P=au(a(l[7],[0,F,0])),Q=[0,x(g(cT[13],I,t,P)),O];return b(l[7],Q,d)}return b(l[7],[0,q,[0,p,[0,A,0]]],d)}}var
y=a(f[3],x8);return g(l[24],0,y,d)},_=[0,p,[0,as(x9,A),0]],$=[0,x(r[28]),_];return x(v(au(a(l[7],$))));default:var
h=1}break;default:var
h=0}if(!h)if(3===d[0]){var
d=d[2];continue}throw[0,Q,x3]}}try{var
ad=a(r[68],[0,[0,x$,[1,d]],[0,[0,x_,[1,h]],0]]),ae=0,af=[0,V,[0,function(c){b(y[52],[0,d],1);b(y[52],[0,h],1);return a(m(I,H),c)},ae]],ag=[0,x(ad),af],ah=[0,x(r[28]),ag],ai=[0,x(dd(0)),ah],aj=b(l[7],ai,G);b(y[52],[0,d],0);b(y[52],[0,h],0);return aj}catch(a){a=E(a);b(y[52],[0,d],0);b(y[52],[0,h],0);throw a}}function
jQ(q,m,y,d,x){var
o=ga[7],p=[0,ya,[0,d[2],0]];function
i(a){return M(o,0,0,0,a,p)}function
k(f,d){function
m(m){var
s=a(j[66][5],m),o=a(j[66][6],m),A=a(j[66][3],m),p=b(c[3],o,A);if(0===f){if(1===y){var
C=aD(q,a(e[17][9],d)),t=g(U[17],s,o,C),B=0,z=function(b){var
c=a(j[66][6],b),d=a(j[66][5],b),e=n(ao[2],0,d,c,t)[1],f=[0,i(0),0],g=[0,r[61],f],h=[0,a(r[86],t),g],k=[0,a(j[64][1],e),h];return a(l[66][20],k)},D=[0,a(j[66][10],z),B];return a(l[66][20],[0,r[61],[0,r[28],D]])}var
u=aD(q,a(e[17][9],d)),v=n(ao[2],0,s,o,u),E=b(c[90],v[1],v[2])[2],F=i(0),G=r[61],H=a(r[86],u),I=b(j[71][2],H,G),J=b(j[71][2],I,F),K=[0,a(l[66][8],J),0],L=i(0),M=a(r[mn],0),N=a(l[66][59],x),O=b(j[71][1],N,M),P=b(j[71][2],O,L),R=b(j[71][2],r[16],P),S=[0,a(l[66][8],R),K],T=a(j[37],S),V=a(r[143],E),W=b(j[71][2],r[61],r[28]),X=b(j[71][2],W,V);return b(j[71][2],X,T)}switch(p[0]){case
6:var
Y=0,Z=function(b){return k(f-1|0,[0,a(c[10],b),d])},_=[0,a(l[66][43],Z),Y];return a(l[66][20],[0,r[16],_]);case
8:var
w=p[2],$=p[4],aa=[0,k(f-1|0,[0,w,d]),0],ab=b(c[h][5],w,$),ac=[0,b(r[5],ab,2),aa];return a(l[66][20],ac);default:throw[0,Q,yb]}}return a(j[66][10],m)}try{var
t=k(m,0);return t}catch(c){var
s=a(f[3],yc);return b(l[66][4],0,s)}}var
gf=[0,wm,wn,wo,wp,wq,wr,ws,wt,wu,wv,ww,wx,wy,wz,wA,wB,wC,wD,wE,wF,wG,jE,wH,wI];aG(1076,[0,jD,gf,jF,jG,wO,dB,gb,wR,gc,wS,cS,gd,eH,jJ,dC,eI,jK,jL,jM,eJ,ge,bV,jO,jP,jQ],"Principles_proofs");function
jR(c){var
a=c[5];if(a){var
b=a[1];if(0!==b[0])return b[1]?yd:ye}return 0}function
jS(a){return typeof
a==="number"?0===a?1:0:a[1]}function
jT(a){return a[1]}function
yf(a){return a[2]}function
jU(f,d,b){function
e(h){var
a=h;for(;;){if(a<b.length-1){if(a<d.length-1){var
i=S(b,a)[a+1],j=S(d,a)[a+1];if(g(c[94],f,j,i))return[0,a,e(a+1|0)];var
a=a+1|0;continue}var
a=a+1|0;continue}return[0,a,0]}}return e(0)}function
eL(b,a){function
d(h,c,g){var
b=h,a=g;for(;;){if(c)if(a){var
e=a[2],f=c[1],i=a[1],j=c[2];if(b<f){var
b=b+1|0,a=e;continue}if(b===f)return[0,i,d(b+1|0,j,e)];throw[0,Q,yg]}return a}}return d(0,b,a)}function
jV(u,l){var
f=[0,l[1],l[2],l[3]],j=0,e=0,d=0;for(;;){var
m=f[3],r=f[2],i=f[1];if(i){var
s=i[1];if(0===s[0]){var
n=i[2];if(n){var
p=n[1];if(0!==p[0]){var
t=n[2],q=p[2],k=[0,c[14],[0,q,0]],v=b(c[94],u,q);if(b(o[28],v,d)){var
x=b(c[h][3],k,0),y=b(o[17],x,d),z=de(0,k,e),f=[0,t,r-2|0,g(c[h][3],k,j,m)],e=z,d=y;continue}var
A=b(c[h][3],k,0),B=b(o[17],A,[0,q,d]),f=[0,t,r,m],j=j+2|0,e=b(o[11],e,[0,s,[0,p,0]]),d=B;continue}}}}var
w=j+a(o[1],i)|0;return[0,b(o[11],e,i),w,m]}}function
jW(d,a){return b(c[82],d,a)[1]}function
yh(f,e){var
c=0,a=e;for(;;){if(a){var
g=a[2],d=b(f,c,a[1]);if(d)return d[1];var
c=c+1|0,a=g;continue}throw W}}function
yi(j,d,i){if(d){var
e=d[1];if(0===e[0]){var
k=e[1];try{var
a=b(o[7],k,j)[2];if(0===a[0])var
f=a[1],c=0;else{var
h=a[1];if(h)var
f=h[1],c=0;else
var
g=1,c=1}if(!c)var
g=f<i?1:0;return g}catch(a){a=E(a);if(a[1]!==a7)if(a[1]!==eh)throw a;return 1}}}return 1}function
gg(f,e,w,v,d,k){var
Y=e?e[1]:1,Z=a(o[1],d);function
l(a){return a[1][1]}var
M=b(o[17],l,d);function
_(h,e){function
i(n,d){var
i=d[4],j=d[3],k=d[2],l=d[1],p=l[2],r=b(q[69],f,l[1])[1];if(g(c[94],f,r,h))return yi(n,w,a(o[1],e))?[0,[0,j,i,eL(p,e)]]:0;if(k){var
m=k[1],s=m[2],t=jW(f,b(q[69],f,m[1])[1]);return g(c[94],f,t,h)?[0,[0,j,i,eL(s,e)]]:0}return 0}try{var
j=[0,yh(i,d)];return j}catch(a){a=E(a);if(a===W)return 0;throw a}}function
j(e,k,l){var
d=b(c[3],f,l);switch(d[0]){case
6:var
P=d[3],ac=d[2],ad=d[1],ae=a(c[9],1);if(!g(q[37],f,ae,P)){var
z=j(e,k,ac),A=z[2],af=z[3],ag=z[1],B=j(e,k,b(c[h][5],c[14],P)),D=B[2],ah=B[1],ai=b(c[h][1],1,B[3]),aj=g(c[h][2],A,D+2|0,ai),ak=[0,ad,b(c[h][1],D,af),aj],al=a(c[18],ak),am=aV(A,ah);return[0,b(C[25],am,ag),A+D|0,al]}break;case
7:var
Q=d[2],R=d[1],E=j(e+1|0,[0,[0,R,0,Q],k],d[3])[1];if(E){var
ao=E[2],ap=a(bj,E[1]),aq=[0,R,Q,b(c[37],ap,ao)],ar=a(c[18],aq);return[0,[0,[0,0,ar],0],1,b(c[h][1],1,l)]}return[0,0,0,l];case
8:var
S=d[3],T=d[2],U=d[1],as=d[4],G=j(e,k,T),p=G[2],at=G[3],au=G[1],H=j(e+1|0,[0,[0,U,[0,T],S],k],as),V=H[2],av=H[3],aw=dc(1,p,H[1]),ax=[0,an(U,[0,at],b(c[h][1],p,S)),0],ay=b(C[25],ax,au),az=b(C[25],aw,ay);return[0,az,(p+V|0)+1|0,g(c[h][2],p,V+2|0,av)];case
9:var
aA=d[2],aB=d[1],aC=function(d,m){var
f=d[2],n=d[3],p=d[1],i=j(e,k,m),l=i[2],q=i[3],r=aV(f,i[1]),s=a(c[h][1],l),t=b(o[17],s,n),u=[0,g(c[h][2],f,l+1|0,q),t];return[0,b(C[25],r,p),l+f|0,u]},I=g(aK[17],aC,yj,aA),r=I[2],W=I[1],s=a(o[9],I[3]),t=b(c[h][1],r,aB),N=_(t,s);if(N)var
x=N[1],m=[0,[0,x[1],x[2],x[3]]];else{if(w){var
O=w[1];if(0===O[0])var
n=0;else{var
y=O[1];if(ee(f,y,t))if(0===y[0])var
m=0,n=1;else
var
$=y[1],aa=a(F[gQ],s),m=[0,[0,Z-1|0,$[2],aa]],n=1;else
var
n=0}}else
var
n=0;if(!n)var
m=0}if(m){var
J=m[1],X=J[3],aD=J[1],aE=a(c[8],J[2]),aF=a(o[9],X),aG=b(c[h][4],aF,aE),aH=[0,t,a(aK[12],s)],aI=[0,a(c[21],aH)],aJ=an([0,a(i[1][6],yk)],aI,aG),aL=[0,a(c[9],1)],aM=a(aK[12],X),aN=a(c[h][1],1),aO=b(aK[15],aN,aM),aP=[0,a(c[9],(((aD+v|0)+r|0)+2|0)+e|0),aO],aQ=[0,a(c[21],aP),aL],aR=a(c[21],aQ),aS=[0,[0,a(i[1][6],yl)],aR],aT=a(c[9],2);return[0,b(C[25],[0,aS,[0,aJ,0]],W),r+2|0,aT]}var
aU=[0,t,a(aK[12],s)];return[0,W,r,a(c[21],aU)];case
13:var
aW=d[4],aX=d[2],aY=d[1],K=j(e,k,d[3]),u=K[2],aZ=K[3],a0=K[1],a1=a(c[h][1],u),a2=b(aK[15],a1,aW),a3=[0,aY,b(c[h][1],u,aX),aZ,a2],a4=a(c[30],a3);return[0,a0,u,g(c[h][3],M,(v+1|0)+u|0,a4)];case
16:var
a5=d[1],L=j(e,k,d[2]),a6=L[2],a7=L[1];return[0,a7,a6,a(c[24],[0,a5,L[3]])]}var
ab=Y?g(c[h][3],M,v+e|0,l):l;return[0,0,0,ab]}return jV(f,j(0,0,k))}function
jX(a,j,i,d){function
f(e,d){var
h=b(c[3],a,d);switch(h[0]){case
1:if(g(c[94],a,j,d))return g(i,e,d,[0]);break;case
9:var
k=h[1],m=h[2];if(g(c[94],a,j,k)){var
o=function(a){return a+1|0},p=n(c[cm],a,o,f,e);return g(i,e,k,b(aK[15],p,m))}break}function
l(a){return a+1|0}return M(c[cm],a,l,f,e,d)}return f(0,d)}function
jY(f,e,d,b){return jX(f,d,function(h,f,b){var
d=[0,e,g(aK[7],b,0,b.length-1-1|0)];return a(c[21],d)},b)}function
jZ(d,c,b,a){return dA(function(a){return jY(d,c,b,a)},a)}function
eM(a){return b(R[1],0,[1,a])}function
j0(e,o,d){function
f(p){var
k=p;for(;;){var
d=b(c[3],e,k);switch(d[0]){case
6:var
j=d[3],l=d[2],m=d[1],n=b(c[82],e,l)[1];if(b(c[46],e,n)){var
r=b(c[76],e,n)[1][1];if(b(i[23][13],r,o)){var
s=a(c[9],1);if(g(q[37],e,s,j))throw[0,Q,ym];var
k=b(c[h][5],c[14],j);continue}var
t=[0,m,l,f(j)];return a(c[18],t)}var
u=[0,m,l,f(j)];return a(c[18],u);case
8:var
v=d[3],w=d[2],x=d[1],y=[0,x,w,v,f(d[4])];return a(c[20],y);default:return k}}}return b(d8,f,d)}function
j1(g,f){var
d=a(at[26],g);if(0===d[0]){var
e=d[1],i=a(bj,b(o[7],f,e-1|0));return b(c[h][1],e,i)}return c[14]}function
j2(l,e,R,P,O,v,k,N,u,M,L){var
w=b(c[90],e[1],L),x=w[2],y=w[1];function
S(a){return jS(a[2][8][1])}var
V=b(o[35],S,k),z=a(o[1],V);if(1===z)var
W=a(o[1],u)+2|0,p=b(F[dQ],W,y);else
var
p=y;if(1===z)var
X=b(c[h][4],[0,c[14],[0,M,0]],x),r=b(c[37],X,u);else
var
J=function(i,g,f){var
d=b(c[90],e[1],i),j=d[2],k=b(F[dQ],2,d[1]),l=[0,f,T(0,g)],m=[0,a(c[21],l),0],n=b(c[h][4],[0,c[14],m],j);return b(c[37],n,k)},K=function(f,l){var
d=l;for(;;){var
g=b(c[3],e[1],f);if(d){var
i=d[1][2][8][1];if(typeof
i==="number")if(0!==i){var
d=d[2];continue}if(9===g[0]){var
h=g[2];if(2===h.length-1){var
k=d[1][2],m=g[1],n=h[1],o=k[4],p=k[1][1],q=K(h[2],d[2]),r=[0,m,[0,J(n,o,p),q]];return a(c[21],r)}}if(d[2])throw[0,Q,yx];var
j=d[1][2];return J(f,j[4],j[1][1])}return f}},r=K(x,k);var
s=j0(e[1],O,p);if(1===v){var
Y=b(c[37],r,s);return[0,a(o[1],s),Y]}var
Z=hj(e),$=a(o[1],p)-v|0,A=b(F[h],$,s),aa=A[1],B=a(F[cm],A[2]),ab=B[2],ac=B[1];function
ad(I,u,t){var
f=t[2],v=f[7],w=f[6],x=f[5],m=f[4],y=f[3],J=t[1];if(1===f[8][1]){var
K=a(ah,u)[1],p=a(o[1],m),z=[0,_([0,0,0,x]),m],L=0,M=function(k,e){var
d=(p-e[2]|0)+1|0,f=e[1],g=a(bj,b(o[7],m,(d-1|0)-1|0)),i=b(c[h][1],d,g),j=a(c[9],d);return[0,d,i,b(c[h][1],1,f),j]},j=g(F[75],M,L,v),d=a(o[1],j),N=al(e,hf(0)),A=function(f,d,l,k,j,g){var
m=b(c[h][1],1,g),o=a(c[9],1),p=b(c[h][1],1,d),r=n(q[51],e[1],p,o,m),s=[0,[0,a(i[1][6],yn)],f,r],t=[0,N,[0,f,d,a(c[19],s),j,l,k]];return a(c[21],t)};if(j){if(j[2])throw[0,Q,yo];var
s=j[1],r=s[3],B=s[2],O=s[4],S=function(f,k){var
l=k[2],o=k[1],i=b(c[h][1],d,O),t=j1(b(c[5],e[1],f),m),j=b(c[h][1],d+1|0,t);if(g(q[37],e[1],i,j)){if(b(c[44],e[1],r))var
p=b(c[h][1],d+1|0,f);else
var
z=b(c[h][1],d+1|0,f),C=a(c[9],1),D=b(c[h][1],d,r),p=A(b(c[h][1],d,B),i,D,C,z,j);if(b(c[44],e[1],r))var
s=b(c[h][1],d+3|0,f);else
var
u=b(c[h][1],2,j),v=b(c[h][1],d+3|0,f),w=a(c[9],1),x=a(c[9],2),y=b(c[h][1],2,i),s=A(b(c[h][1],d+2|0,B),y,x,w,v,u);return[0,[0,p,o],[0,[0,i,s],l]]}var
E=b(c[h][1],d+1|0,f),F=b(c[h][1],d,r);return[0,[0,n(q[51],e[1],F,i,E),o],l]},C=g(o[21],S,w,yp),E=C[1],D=C[2]}else
var
ap=a(c[h][1],d+1|0),E=b(o[17],ap,w),D=0;var
T=b(c[h][1],(d*2|0)+1|0,x),V=[0,a(c[9],d+1|0),T],W=function(p,f){var
r=f[4],k=f[3],m=f[2],j=p[2],s=p[1],t=f[1]+(2*d|0)|0,u=a(c[9],t);if(g(q[37],e[1],u,j)){var
v=b(c[h][1],d+1|0,r),w=a(c[9],1),x=bh(l,e,b(c[h][1],d+1|0,m),w,v),y=a(c[9],1),z=a(c[9],t),A=n(q[51],e[1],z,y,j),B=b(c[h][1],1,A),C=function(b,a){return n(q[51],e[1],a[1],a[2],b)},E=g(o[20],C,B,D),F=al(e,hg(0)),G=a(c[9],1),H=b(c[h][1],d,k),I=[0,[0,a(i[1][6],yq)],x,E],J=a(c[19],I),K=b(c[h][1],d,m),L=[0,[0,a(i[1][6],yr)],K,J],M=a(c[19],L),N=b(c[h][1],d,r),O=[0,F,[0,b(c[h][1],d,m),N,M,s,H,G]],P=a(c[21],O);return[0,P,b(c[h][5],k,j)]}return[0,s,b(c[h][5],k,j)]},X=g(o[20],W,V,j)[1];if(y){var
Y=y[2],$=1,G=b(g7(function(b,a){return iX(a[2][3],Y)?[0,(J+1|0)-b|0]:0}),$,k);if(G){var
aa=a(c[9],G[1]),ab=aD(aD(b(c[h][1],(p+1|0)+d|0,aa),E),[0,X,0]),ac=function(a){return bh(l,e,a[2],a[3],a[4])},ad=b(o[17],ac,j),ag=function(a){var
d=b(c[h][1],1,a[1]),f=g(U[17],l,e[1],d);return dc(p+2|0,-(I-1|0)|0,gg(e[1],yt,R,p,P,f)[1])},ai=b(o[17],ag,v),H=a(o[13],ai),aj=aV(d,H),ak=a(o[1],H),am=b(c[h][1],ak,ab),an=fr(l,e[1],am,aj),ae=function(d,b){var
e=[0,[0,a(i[1][6],ys)],d,b];return a(c[18],e)},af=g(o[21],ae,ad,an),ao=b(c[38],af,z);return _([0,K,[0,ao],b(c[37],Z,z)])}throw[0,Q,yu]}throw[0,Q,yv]}return u}var
ae=a(o[6],k),af=a(o[9],ae),D=n(F[76],ad,1,ab,af),f=N,d=a(o[9],aa),j=0,m=0;for(;;){if(f){var
E=f[1][1];if(typeof
E==="number")if(1===E){if(d){var
ag=f[2],f=ag,d=df(c[14],d[2]),j=j+1|0;continue}var
t=1}else
var
t=0;else
var
t=0;if(!t)if(d){var
f=f[2],ai=[0,d[1],m],d=d[2],m=ai;continue}throw[0,Q,yw]}if(d)var
aj=a(o[9],d),H=[0,j,b(C[25],aj,m)];else
var
H=[0,j,m];var
I=H[2],ak=b(C[25],D,[0,ac,0]),am=b(C[25],I,ak),an=b(c[h][1],-j|0,r),ao=b(c[37],an,am),ap=function(b){var
c=a(ft,b);return a(G[3],c)},aq=b(o[35],ap,D),ar=a(o[1],aq);return[0,(a(o[1],I)+ar|0)+1|0,ao]}}function
gh(e,a){function
d(f,a){var
d=a[1],g=a[2];return[0,d+1|0,[0,b(bb,b(c[h][10],d,e),f),g]]}return g(o[21],d,a,yy)}function
yz(h,e,l,d){var
m=d[9],n=d[8],o=d[7],p=d[5],r=d[4],s=d[1],j=b(az[21],l,h),k=b(c[aY],r,h),t=fR(k,e,0,m),u=a(f[5],0),v=ay(k,e,p),w=a(f[5],0),x=a(f[3],yA),y=g(q[V],j,e,n),z=a(f[3],yB),A=a(i[1][9],s),B=a(f[3],yC),C=a(f[5],0),D=g(q[V],j,e,o),E=b(f[12],D,C),F=b(f[12],E,B),G=b(f[12],F,A),H=b(f[12],G,z),I=b(f[12],H,y),J=b(f[12],I,x),K=b(f[12],J,w),L=b(f[12],K,v),M=b(f[12],L,u);return b(f[12],M,t)}function
j3(a){function
c(a){return a[7]}return b(o[17],c,a)}function
dD(c,a){return b(q[69],c,a)[2]}function
j4(e,d){var
f=[0,[0,0,[1,b(c[74],e,d)[1]]],0];return x(a(r[68],f))}function
gi(p,d,l,j,s,i,f,e){function
t(e,i){if(s){var
f=b(c[90],d,i),g=f[1],k=f[2];if(j)var
h=aD(e,b(c[82],d,k)[2]);else
var
l=a(o[1],g),m=b(q[9],0,l),n=a(F[gQ],m),h=aD(e,b(C[25],n,0));return b(c[38],h,g)}return e}function
k(i,f,a){var
j=$(a[1]);function
k(e,a){var
f=e[1],j=a[2],k=a[1];try{var
g=b(q[7],k,f),i=g[1],l=ai(0,p,[0,d],cH(0,p,d,i,[2,t(j,b(c[h][1],i,g[3]))],f),e);return l}catch(a){a=E(a);if(a===W)return e;throw a}}var
e=g(o[20],k,j,f);return[0,e,ai(0,p,[0,d],ai(0,p,[0,d],e,a),i)]}function
m(i,f,q,e){switch(e[0]){case
0:var
y=e[2],r=e[1],ad=e[4],ae=e[3],z=k(i,f,r),u=z[1],af=z[2],A=fQ(y),$=r[3],aa=r[2],ab=r[1],ac=bE(a(o[1],A),aa),B=k(i,f,[0,b(C[25],A,ab),ac,$])[1],ag=function(e){var
l=e[9],m=e[8],n=e[7],p=e[6],q=e[5],r=e[3],s=e[2],v=e[1],j=[0,0,e[4]];function
k(f,e){var
c=e[1],d=f[2],g=f[1],i=e[2];try{var
h=t(i,a(bD,b(h3,c,d))[3]),j=[0,[0,[0,c,h],g],h4(c,h,d)];return j}catch(a){a=E(a);if(a===W)return[0,g,d];throw a}}var
i=g(o[20],k,j,f),w=i[2],x=i[1],y=P(d,u,p),z=P(d,u,n),A=P(d,u,m);return[0,v,s,r,w,q,y,z,A,dA(function(a){return b(c[h][9],x,a)},l)]},ai=b(o[17],ag,y),aj=jB(d,B,ad);return[0,af,ai,P(d,B,ae),aj];case
1:var
ak=e[4],al=e[3],am=e[2],H=k(i,f,e[1]),I=H[1],an=H[2],ao=P(d,I,a(c[9],am)),ap=b(c[65],d,ao),aq=function(a){return m(i,f,q,a)},ar=a(G[16],aq),as=b(aK[15],ar,ak);return[1,an,ap,P(d,I,al),as];case
2:var
at=e[6],au=e[5],av=e[4],aw=e[3],ax=e[2],ay=k(i,f,e[1])[2],aA=function(a){var
b=a[4],c=a[3],d=a[2],e=a[1];return[0,e,d,c,b,m(i,f,q,a[5])]};return[2,ay,ax,aw,av,au,b(o[17],aA,at)];case
3:var
aB=e[2],aC=k(i,f,e[1])[2];return[3,aC,m(i,f,q,aB)];case
4:var
aE=e[1];return[4,aE,m(i,f,q,e[2])];default:var
j=e[2],J=e[1],L=j[8],M=j[7],N=j[6],O=N[2],Q=j[3],v=j[1],aF=e[3],aG=j[10],aH=N[1],aI=j[5],aJ=j[2],aL=v[3],aM=v[2],aN=v[1],R=k(i,f,J),l=R[1],aO=R[2],w=function(d){var
i=[0,0,0,a(o[1],d),0];function
j(p,d){var
e=d[4],g=d[3],k=d[2],l=d[1],i=a(ah,p),m=i[3],j=i[1],q=i[2];if(j){var
n=j[1];if(b(o[42],n,f))return[0,l,k,g-1|0,[0,t(b(o[38],n,f),m),e]]}var
r=b(c[h][4],e,m),s=a(c[h][4],e),u=_([0,j,b(G[16],s,q),r]),v=a(c[h][1],1),w=b(o[17],v,e);return[0,[0,g,l],[0,u,k],g-1|0,[0,a(c[9],1),w]]}var
e=g(o[21],j,d,i),k=e[2],l=e[1];function
m(a){return[0,a]}return[0,d,b(o[17],m,l),k]},aP=k(w(M[3]),f,M)[2],S=w(L[3]),T=k(S,f,L),aQ=T[2],aR=T[1],aS=j[9],aT=k(w(j[9][3]),f,aS)[2],x=a(G[2],s)?fe(0):aI,U=[0,x,q];if(a(G[2],s))var
V=[0,0],aU=0,aV=0,aW=function(h,g,e){if(h===Q)V[1]=a(o[1],g);if(b(c[44],d,e)){var
i=b(c[65],d,e)-1|0,j=a(b8,b(o[7],J[1],i)),k=a(K[10][16],j);return b(o[42],k,f)?g:[0,P(d,l,e),g]}return[0,P(d,l,e),g]},aX=n(F[90],aW,aV,aU,O),aY=a(az[9],p),aZ=function(b){var
d=a(D[2][1][1],b);return a(c[10],d)},a0=b(o[17],aZ,aY),a1=a(aK[12],a0),a2=V[1],a3=a(o[9],aX),Y=[0,a(c[12],[0,x,a1]),a3],X=a2;else
var
a8=function(a){return P(d,l,a)},a9=b(o[17],a8,O),a_=a(o[1],f),Z=b(F[h],a_,a9),a$=Z[2],ba=Z[1],bb=Q-a(o[1],f)|0,Y=[0,aD(P(d,l,aH),ba),a$],X=bb;var
a4=P(d,aR,aG),a5=P(d,l,aJ),a6=P(d,l,aL),a7=[0,[0,aN,P(d,l,aM),a6],a5,X,U,x,Y,aP,aQ,aT,a4];return[5,aO,a7,m(S,f,U,aF)]}}return m(i,f,0,e)}function
eN(m,j,i,f,q,p,d){var
k=[0,aJ[8][1]];if(i){var
n=i[1];if(0===n[0]){var
r=k[1];return[0,gi(m,j[1],f,0,0,q,p,d),r]}var
l=n[1],s=0===l[0]?a(c[10],l[1][2]):a(c[22],l[1][3]),t=jZ(j[1],f,s,d),e=function(i,f,d){switch(d[0]){case
0:var
t=d[4],u=d[3],v=d[2],w=d[1],x=function(d){var
f=b(c[aY],d[4],i),l=cA(f,j[1],0,d[8]),m=l[2];j[1]=l[1];var
r=d[7],s=d[4];function
t(b){var
d=aZ(b);return a(c[10],d)}var
u=b(o[17],t,s),n=b(c[h][4],u,r),p=b(c[75],j[1],m)[1],q=e(f,n,d[9]),v=b(K[5],d[1],yD);k[1]=g(aJ[8][4],p,[0,n,v,q],k[1]);var
w=d[8],x=d[6],y=d[5],z=d[4],A=d[3],B=[0,p,a(o[6],d[2])];return[0,d[1],B,A,z,y,x,m,w,q]};return[0,w,b(o[17],x,v),u,t];case
1:var
y=d[4],z=d[3],A=d[2],B=d[1],C=function(a){return e(i,f,a)},D=a(G[16],C);return[1,B,A,z,b(aK[15],D,y)];case
2:var
E=d[6],F=d[5],H=d[4],I=d[3],J=d[2],L=d[1],M=function(a){var
b=a[4],c=a[3],d=a[2],g=a[1];return[0,g,d,c,b,e(i,f,a[5])]};return[2,L,J,I,H,F,b(o[17],M,E)];case
3:var
N=d[1];return[3,N,e(i,f,d[2])];case
4:var
m=d[2],q=d[1];if(2===m[0]){var
n=m[6];if(n)if(!n[2]){var
p=n[1],r=p[4],O=p[5],P=p[3],Q=0===l[0]?yE:[0,l[1][4]],s=e(i,f,gi(i,j[1],f,0,Q,P,[0,[0,q,f],0],O));return r?[3,r[1],s]:s}}return[4,q,e(i,f,m)];default:var
R=d[2],S=d[1];return[5,S,R,e(i,f,d[3])]}},u=e(m,f,t);return[0,u,k[1]]}return[0,d,k[1]]}function
j5(b,d){switch(b[0]){case
0:return a(c[10],b[1]);case
1:return a(c[23],[0,b[1],d]);case
2:return a(c[26],[0,b[1],d]);default:return a(c[28],[0,b[1],d])}}function
yL(e,m,N,d,J,ak,I,h,aj,H,ai,ah,ag,L,p,as){var
r=cR(e[1]),n=a(i[1][6],e[1][2]),O=b(K[5],n,yM),P=a(o[1],H),R=b(q[9],0,P);if(m)var
v=m[1],j=v[1],z=v[3],x=[0,p];else
var
j=L,z=p,x=0;var
an=a(c[34],[0,j,R]);function
l(h){var
f=h[2],j=f[8][1],k=f[4],l=f[2],m=h[1],q=f[1][1];if(typeof
j==="number")if(0===j)var
g=0;else
var
n=0,g=1;else
var
g=0;if(!g)var
n=1;if(n){var
r=l?l[1][1][1]:q,o=aQ(0,k),s=a(c[34],[0,r,o]);if(0===m)var
p=yN;else
var
y=a(C[21],m),p=b(C[16],yP,y);var
t=b(C[16],yO,p),u=a(i[1][6],e[1][2]),v=al(d,fg(b(K[5],u,t))),w=[0,v,b(C[25],o,[0,s,0])],x=a(c[34],w);return[0,b7(N,d[1],x,k)]}return 0}if(h){var
S=h[1];if(h[2])var
A=function(f){var
b=f;for(;;){var
c=a(F[cm],b),d=c[2],e=l(c[1]);if(e)return[0,e[1],d];var
b=d;continue}}(h),T=A[2],U=A[1],V=function(j,f){var
g=l(j);if(g){var
b=fj[9],e=t(b),m=g[1],h=u===e?b[1]:k===e?a(s[2],b):b,i=[0,al(d,h),[0,m,f]];return a(c[21],i)}return f},B=g(o[21],V,T,U);else
var
ar=l(S),B=a(G[7],ar);var
W=function(aw,s,av){var
p=a(y[2],0);g(aH[22],0,[0,e[1][2],0],[1,[0,[0,0,r,[0,s]],0]]);var
f=e[1],k=a(i[1][6],f[2]),q=a(o[1],ai);if(1<q)var
u=0;else
if(0===cs(0))var
ae=eM(b(K[5],k,yK)),l=b(cp[3],0,ae),u=1;else
var
u=0;if(!u)var
l=ag;var
Q=a(y[2],0),v=b(y[48],Q,l),x=v[2],z=v[1],R=a(y[2],0);d[1]=a(w[17],R);if(cR(f)){var
S=f[1],T=a(y[2],0),U=dj(b(y[48],T,S)[2])[2],V=dj(x)[2],W=b(aP[40][7],U,V);d[1]=M(w[lT],0,0,w[aY],d[1],W);var
A=a(aP[37][4],x),X=b(dt[24],A,z),C=j5(l,a(c[2][1],A)),B=X}else
var
C=j5(l,c[2][3]),B=z;var
D=j2(p,d,J,ak,ah,q,h,aj,H,an,a(c[8],B)),E=D[2],Y=D[1];function
Z(x,m,v){var
d=a(y[2],0),n=a(w[17],d),o=a(aN[34],k),g=a4(n,a(am[9],o)),h=g[2],i=a4(g[1],m),j=i[2],l=hl(i[1]),e=l[1],p=l[2],q=b5(Y),r=[0,a(c[8],q),[0,j,0]],s=[0,h,[0,M(ax[2],0,0,d,e,j),r]],t=[0,M(ax[2],0,0,d,e,h),s],u=b(K[6],yI,k);c7(u,cR(f),e,0,p,t);return 0}var
_=jQ(C,q,a(o[1],I),f,s);g(fh,a(y[2],0),d,E);var
$=[0,a(bQ[1],Z)],aa=[0,f[3]],ab=a(w[bq],d[1]),ac=b(c[5],d[1],E),ad=b(K[5],k,yJ);bI(aW[7],ad,0,ac,ab,0,0,aa,[0,_],0,$,0,[0]);var
af=a(w[17],p),al=a(aN[34],n),F=a4(af,a(am[9],al)),t=F[2],G=a4(F[1],s),L=G[2],N=hk(G[1]),j=N[1],ao=N[2],ap=[0,t,[0,M(ax[2],0,0,p,j,L),[0,L,0]]],aq=[0,M(ax[2],0,0,p,j,t),ap];c7(b(K[6],yQ,n),r,j,0,ao,aq);var
O=dS[1];if(O){var
ar=gj[3],as=[0,b(c[74],j,t)[1]];b(y[52],as,ar);if(m){var
at=gj[3],au=[0,b(c[74],j,m[1][1])[1]];return b(y[52],au,at)}var
P=0}else
var
P=O;return P};if(r)var
D=d[1];else
var
ap=a(y[2],0),D=a(w[17],ap);var
X=a(w[bq],D);try{var
ab=[0,a(bQ[1],W)],ac=[0,jO(J,b(c[5],d[1],j),e,n,z,x,I)],ad=[0,e[1][3]],ae=b(c[5],d[1],B);bI(aW[7],O,0,ae,X,0,0,ad,ac,0,ab,0,[0]);var
ao=0;return ao}catch(c){c=E(c);var
Y=b(af[16],0,c),Z=a(f[5],0),_=a(f[3],yR),$=b(f[12],_,Z),aa=b(f[12],$,Y);return b(aq[8],0,aa)}}throw[0,Q,yS]}function
dE(D,f,e,k,t){var
u=a(o[5],t),p=u[3],H=u[2],I=u[1],v=p[3],z=p[1],j=H[4],J=I[6],A=H[1],V=p[5],X=p[2],Y=I[2];function
Z(i){var
d=i[3],j=i[1],q=[0,jR(j),0],m=d[5],n=d[4],p=d[3],v=d[2];function
l(i,k,j,n,w,u){var
m=w,d=u;for(;;)switch(d[0]){case
0:var
q=d[2],x=d[4],y=d[3],z=d[1],A=function(d){var
m=cz(d[4]),f=m[2],n=m[1],w=b(o[17],c[10],f),j=b(c[h][4],w,d[7]);try{var
I=a(o[5],d[2]),k=b(aJ[8][22],I,v),u=k[1],J=k[3],K=k[2],L=dD(e,u),M=[0,[0,[0,u,jU(e,dD(e,j),L)],K,J]],p=M}catch(a){a=E(a);if(a!==W)throw a;var
p=0}var
x=b(c[aY],d[4],i),y=l(x,d[5],j,0,yF,d[9]);function
z(a){var
i=a[3],l=a[9],m=a[8],o=a[7],p=a[6],q=a[5],r=a[4],s=a[2],j=gh(f,a[1]),d=j[1],t=b(C[25],j[2],n);if(i)var
e=i[1],u=e[3],v=e[2],k=[0,[0,g(c[h][10],d,f,e[1]),v,u]];else
var
k=0;function
w(a){return a}var
x=f_(b(c[h][10],d,f),w,m),y=g(c[h][10],d,f,p),z=g(c[h][10],d,f,q);return[0,t,g(c[h][10],d,f,s),k,r,z,y,o,x,l]}var
A=b(o[17],z,y),q=gh(f,d[5][1]),r=q[1],B=b(C[25],q[2],n),s=g(c[h][10],r,f,j);function
t(h,g){var
d=h,a=g;for(;;){if(a){var
f=a[2];if(b(c[44],e,a[1]))return[0,d,t(d+1|0,f)];var
d=d+1|0,a=f;continue}return 0}}var
D=dD(e,s),F=t(0,a(aK[11],D)),G=g(c[h][10],r,f,d[6]),H=b(o[19],bc,d[5][2]);return[0,[0,s,F],p,d[3],B,G,H,0,A]},B=b(o[17],A,q),p=ai(0,i,[0,e],z,k),D=j3(q),F=p[1],G=function(b){var
c=a(b8,b);return a(K[10][16],c)},H=b(o[17],G,F),I=function(a){return g(c[h][10],1,H,a)},r=b(o[17],I,D),J=function(a){return a},L=f_(function(a){var
d=b(c[h][4],r,a);return g(U[17],i,e,d)},J,x),M=b(o[19],bc,p[2]),N=[0,2,m[2]],O=b(c[h][4],r,y);return[0,[0,p[1],j,n,M,O,j,N,L,[0,B]],0];case
1:var
Q=d[4],R=0,S=function(c,a){if(a){var
d=l(i,k,j,n,m,a[1]);return b(C[25],c,d)}return c};return g(aK[17],S,R,Q);case
2:var
T=d[6],V=0,X=function(c,a){var
d=a[5],f=ai(0,i,[0,e],a[3],k),g=l(i,f,j,n,[0,m[1],0],d);return b(C[25],c,g)};return g(o[20],X,V,T);case
3:var
Y=d[2];ai(0,i,[0,e],k,d[1]);var
d=Y;continue;case
4:var
m=[0,m[1],0],d=d[2];continue;default:var
f=d[2],s=d[1],Z=d[3],_=f[1][2],t=ai(0,i,[0,e],s,k),$=b(o[19],bc,t[2]),aa=ai(0,i,[0,e],f[9],t)[2],ab=b(o[19],bc,aa),ac=[0,dD(e,f[6][1]).length-1,0],ad=l(i,f[8],f[6][1],0,yG,Z),ae=f[3],af=[0,[0,P(e,f[9],_),ae],0],ag=[0,[0,[0,[0,f[6][1],ac],0,f[4],f[8][1],f[10],ab,af,ad],0]],ah=[0,a(c[34],f[6])];return[0,[0,s[1],j,n,$,f[2],j,yH,ah,ag],0]}}return[0,j,d,l(f,n,a(c[8],p),k,q,m)]}var
$=b(o[17],Z,t);function
L(a){function
c(a,f){var
c=a[9],d=a[7],p=f[2],q=f[1],g=a[8],h=a[6],i=a[5],j=a[4],k=a[3],l=a[2],m=a[1];if(c)var
n=c[1],o=function(a){var
c=a[7],e=a[6],f=a[5],g=a[4],h=a[3],i=a[2],j=a[1],b=L(a[8]);return[0,[0,[0,j,i,h,g,f,e,c,d],b[1]],b[2]]},e=b(F[121],o,n);else
var
e=0;return[0,[0,[0,m,l,k,j,i,h,d,g],q],b(C[25],e,p)]}return g(o[21],c,a,yT)}function
aa(d,j){var
g=d[2],e=d[1],h=L(d[3]),l=h[2],m=h[1];if(k)var
f=k[1],i=[0,[0,[0,f[1],0],f[2],f[3]]];else
var
i=0;var
n=[0,jR(e),0],p=b(o[19],bc,g[4][2]),q=e[3],r=e[2],s=[0,[0,[0,a(c[8],g[3]),0],i,0,r,q,p,0,n],m];return[0,s,b(C[25],l,j)]}var
M=g(o[21],aa,$,0);function
ab(a){return a[1]}var
N=b(o[17],ab,M),O=a(o[1],N),ad=1;function
ae(g,a){var
d=a[2],f=a[1],h=a[5],i=f[2],j=b(q[60],e,f[1]),k=d?[0,d[1][1]]:0;return[0,[0,j,i],k,O-g|0,b(c[5],e,h)]}var
Q=g(F[75],ae,ad,N),d=[0,e],s=cR(j);function
af($,h){var
e=h[1],i=e[2],m=h[2],p=e[8],r=e[7],s=e[6],t=e[5],u=e[4],w=e[3],x=e[1];if(i)var
j=i[1][1],k=[0,j,0,w,u,t,s,r,p],H=j[2];else
var
k=e,H=x[2];function
y(e){var
h=e[8],p=e[7],r=e[5],s=e[4],t=e[3],i=e[2],j=e[1],I=p[2],K=p[1];if(t)var
u=t[1],L=u[1],M=g9(u[2]),x=L,w=b(cT[3],0,M);else{var
Z=a(c[8],v);if(g(c[94],d[1],i,Z))var
_=a(c[8],v),G=au(j4(d[1],_));else
var
G=l[66][2];var
x=i,w=G}var
y=aD(x,s);if(0===h[0])var
z=bh(f,d,r,y,g(U[17],f,d[1],h[1]));else
var
Y=[0,ht(d),[0,r,y]],z=a(c[21],Y);var
A=b(c[37],z,j);n(ao[3],0,f,d,A);if(0===h[0]){var
N=h[1],B=a(o[1],j),P=g(U[17],f,d[1],N),k=gg(d[1],0,J,B,Q,P),m=k[2],R=k[3],S=k[1],D=a(c[9],(B+(O-$|0)|0)+m|0);if(I)var
E=D;else
var
X=b(q[60],d[1],i),E=aD(D,eL(H,fs(m,b(c[82],d[1],X)[2])));var
T=fs(m,s),V=aD(E,b(C[25],T,[0,R,0])),W=fr(f,d[1],V,S),F=[0,hO(d[1],W,j)]}else
var
F=0;return[0,K,w,A,F]}return[0,k,b(o[17],y,m)]}var
T=g(F[75],af,0,M),ag=0;function
ah(b,a){var
c=a[2],d=a[1],e=1;function
f(b,a){return[0,b,a]}return[0,b,d,g(F[75],f,e,c)]}var
m=g(F[75],ah,ag,T);function
aj(a){return a[2]}var
ak=b(o[17],aj,T),al=a(o[13],ak),B=[0,gf[1]];function
am(e){var
k=e[3],h=e[2],l=h[5],i=h[4],m=e[1],u=h[3];if(0===m)var
p=yU;else
var
M=a(C[21],m),p=b(C[16],yX,M);var
j=b(K[5],z,p);function
v(b){var
c=a(b8,b),d=a(K[10][16],c);return a(at[2],d)}var
w=b(o[19],v,i);B[1]=g(gf[4],u,[0,j,w],B[1]);function
x(e){var
f=e[2][4],g=a(c[5],d[1]);return b(G[16],g,f)}var
y=b(F[70],x,k);function
A(c){var
d=c[2],e=d[4],f=d[1],g=c[1];function
h(h){var
c=a(C[21],g),d=1===f?yV:yW,e=b(C[16],d,c);return b(K[5],j,e)}return b(G[16],h,e)}var
D=b(F[70],A,k);if(0===cs(0))var
q=c[14];else
var
I=[0,_([0,0,0,l]),i],J=d[1],r=[0,f,aP[5]],s=function(e,d){var
f=d[1],g=d[2],h=a(bj,e),i=n(ax[3],0,f,J,h),j=a(j6[14],i),k=b(aP[12],j,g);return[0,b(c[bt],e,f),k]},t=g(o[21],s,I,r)[2],L=a(j6[15],t),q=a(c[13],L);var
E=a(c[18],[0,0,l,q]),H=b(c[37],E,i);return[0,j,b(c[5],d[1],H),0,D,y]}d[1]=a(w[k8],d[1]);if(!s){var
ap=a(w[148],d[1]);b(bf[14],0,ap);var
aq=a(y[2],0);d[1]=a(w[17],aq)}function
an(e){var
h=e[3],q=e[1],E=a2(a(o[1],h),0);if(0===q)var
n=z;else
var
u=a(C[21],q),G=b(C[16],y9,u),n=b(K[5],z,G);function
p(h){var
e=h[2],G=h[1],ao=e[4],p=e[3],u=e[2],z=a(C[21],G),H=b(C[16],y8,z),I=b(K[5],n,H);function
L(av,H,au){if(0===ao)g(ac[41],0,1,H);else{var
ar=a(y[2],0),as=[0,g(aO[31],0,ar,H),1,0],at=[0,b(R[1],0,as),0];b(cf[1],j[2],at)}var
I=G-1|0;S(E,I)[I+1]=1;function
ap(a){return a}var
L=b(bF[31],ap,E);if(L){g(ac[32],[1,A],0,0);if(k){var
aq=[0,b(c[74],d[1],k[1][1])[1]];b(y[52],aq,1)}b(y[52],[0,A],1);var
M=D?(q+1|0)===a(o[1],m)?1:0:D;if(M){var
e=b(o[17],am,m),l=b(w[l_],s,d[1]);g(co[11],yZ,yY,0);var
h=g(jd[2],[0,0,0,0,e,l,0],aO[8],0);g(co[11],y1,y0,1);var
r=cs(0);switch(r){case
0:var
n=y2;break;case
1:var
n=y6;break;default:var
n=y7}var
O=0,P=function(d,c){var
f=1===a(o[1],e)?n:y3,g=b(K[5],c[1],f);return[0,b(R[1],0,g),0,[0,h,d],r]},u=g(F[75],P,O,e);a(j7[5],u);if(1===a(o[1],e))var
T=b(C[16],y4,n),U=a(i[1][6],j[2]),W=eM(b(K[5],U,T)),x=b(cp[3],0,W);else{var
af=a(i[1][6],j[2]),z=b(K[5],af,y5),ag=function(b,a){var
c=b[1];return[0,c,jS(a[2][8][1])]},ah=g(o[23],ag,u,m),ai=function(a){var
b=a[1];return a[2]?[0,b]:0},aj=b(F[70],ai,ah),ak=b(R[1],0,z);b(j7[8],ak,aj);var
an=eM(z),x=b(cp[3],0,an)}switch(l[0]){case
0:var
p=a(c[25],[0,h,0]);break;case
1:var
$=a(aP[36][4],l[1]),aa=[0,[0,h,0],a(c[2][1],$)],p=a(c[26],aa);break;default:var
ab=a(aP[38][4],l[1]),ad=a(aP[36][4],ab),ae=[0,[0,h,0],a(c[2][1],ad)],p=a(c[26],ae)}var
Z=function(b,a){var
c=a[5],d=1;function
e(a,c){return[0,aH[4],s,1,0,[0,[3,[0,[0,h,b],a]]]]}var
f=[0,g(F[75],e,d,c)];return g(aH[22],0,[0,j[2],0],f)};b(o[16],Z,e);var
_=[0,j,B[1],X];return yL(_,k,f,d,J,Q,t,m,al,Y,e,h,x,a(c[8],v),V,p)}var
N=M}else
var
N=L;return N}var
M=[0,x(hK([1,A])),0],N=[0,x(u),M],O=[0,x(r[28]),N],P=a(l[7],O);if(!s){var
$=a(y[2],0);d[1]=a(w[17],$)}var
T=[0,a(bQ[1],L)],U=[0,au(P)],W=[0,j[3]],Z=a(w[bq],d[1]),_=b(c[5],d[1],p);bI(aW[7],I,0,_,Z,0,0,W,U,0,T,0,[0]);return 0}return b(o[15],p,h)}return b(o[15],an,m)}aG(lR,[0,jT,yf,jU,eL,jV,jW,gg,jX,jY,jZ,eM,j0,j1,j2,gh,yz,j3,dD,j4,gi,eN,dE],"Principles");function
j8(b,a,e,f,d,c){if(hR(b,a,e,d)){var
g=dZ(b,a,d,c);return[0,bh(b,a,e,f,c),g]}var
h=fm(b,a,d,c);return[0,fl(b,a,e,f,d,c),h]}function
j9(m,x,d,l,w,k,j,i,f,v,u){var
y=a(A[1],0),B=a(e[17][1],f),C=b(z[19],m,d),D=a(c[10],d);if(w)var
E=a(c[9],1),F=b(c[h][1],1,i),I=a(z[8],m),n=j8(b(c[H],k,I),x,F,E,C,D),J=n[1],K=[0,n[2]],L=[0,0,J,b(c[h][1],1,l)],p=a(c[18],L),o=K;else
var
p=l,o=[0];var
r=[0,0,1];function
s(e,a){var
d=a[2],f=a[1];return[0,[0,b(c[h][1],d,e),f],d+1|0]}var
t=g(e[17][19],s,f,r)[1];function
M(a){return[0,0,a]}var
N=b(e[17][15],M,t),O=b(c[h][1],B,p),P=b(c[37],O,N),Q=an([0,d],j,i),R=b(c[35],Q,P),S=b(c[37],R,k),T=[0,a(c[11],y),2,S],U=a(c[17],T),V=[0,U,a(e[19][12],v)],q=a(c[21],V),W=[0,q,[0,a(c[10],d)]],X=a(c[21],W);function
Y(a){return q}var
Z=g(G[24],Y,X,j),_=[0,Z,a(e[19][12],u)],$=[0,a(c[21],_),o];return a(c[21],$)}function
j_(k,j,d,h,c){if(a(i[1][10][2],c))return 0;var
e=[0,c,0];return g(d7,function(f,e){var
d=f[2],a=f[1],c=aZ(e);if(b(i[1][10][3],c,h))return[0,a,d];if(b(i[1][10][3],c,a))return[0,a,[0,c,d]];var
l=g(q[99],k,j,e),m=i[1][10][1],n=b(i[1][10][9],l,a);return b(i[1][10][11],n,m)?[0,a,d]:[0,b(i[1][10][4],c,a),[0,c,d]]},e,d)[2]}var
gk=[e6,y_,e3(0)];function
gl(f,d,c){var
a=[0,d];try{var
h=function(c){var
d=cv(f,y$,i[1][10][1],c),e=a[1];function
h(c,a){if(b(i[1][10][3],c,a))throw gk;return b(i[1][10][4],c,a)}a[1]=g(i[1][10][15],h,d,e);return 0};b(e[19][13],h,c);var
j=1;return j}catch(a){a=E(a);if(a===gk)return 0;throw a}}function
j$(f,h){var
d=f[2];b(z[20],h,f);var
j=a(bD,b(z[18],f,h)),k=j[2],q=j[3];if(k)var
l=b(c[82],d,k[1]),m=l[1],g=l[2];else
var
p=b(c[82],d,q),m=p[1],g=p[2];if(0===g)return 0;var
n=d3(d,m,a(e[19][12],g)),o=n[2];if(gl(d,cv(d,za,i[1][10][1],n[1]),o)){var
r=function(a){return 1-b(c[45],d,a)};return b(e[19][29],r,o)}return 1}function
ka(d,N,B,o,L,y,x){var
f=a(z[2],d),m=[0,f],C=a(z[8],d),D=a(z[7],d);if(B)var
p=B;else
var
ae=a(c[10],o),p=g(q[37],f,ae,D);var
r=[0,i[1][10][1]];function
O(j,k){var
l=j[10],p=j[9],s=j[8],w=j[7],x=j[6],y=j[5],z=j[4],B=j[3],t=j[2],C=n(U[66],l,f,1,j[1]),D=C[2],E=a(ah,a(e[17][5],C[1])),o=E[3],F=E[1],S=b(cw,d,k),T=M(dY[5],0,0,0,zc,l),G=g(A[65],T,m,S),H=a(e[17][1],t),I=b(c[h][1],H,G),J=n(q[54],f,1,I,o),K=b(c[3],f,k);if(1===K[0]){var
u=K[1];if(J)if(!b(i[1][10][3],u,s)){var
af=b(i[1][10][6],u,p),ag=b(i[1][10][4],u,s),ai=a(c[21],[0,z,[0,k]]);return[0,b(c[h][5],k,D),t,B,ai,y,x,w,ag,af,l]}}var
R=F?F[1]:a(i[1][6],zb),v=ha(r[1],R,d);r[1]=b(i[1][10][4],v,r[1]);var
L=[0,[0,v],o],N=[0,L,t],V=[0,a(c[9],1)],W=[0,b(c[h][1],1,z),V],X=a(c[21],W),Z=a(e[17][1],N),O=b(c[h][1],Z,k),Y=[0,k,y];if(J)var
_=dZ(l,m,b(c[h][1],-H|0,o),k),$=a(c[9],1),Q=bh(l,m,b(c[h][1],1,o),$,O),P=_;else
var
ad=fm(l,m,G,k),ae=a(c[9],1),Q=fl(l,m,b(c[h][1],1,o),ae,I,O),P=ad;var
aa=[0,Q,hv(x)],ab=cv(f,0,p,k),ac=b(i[1][10][7],ab,p);return[0,D,N,b(c[bt],L,B),X,Y,aa,[0,P,w],s,ac,l]}var
E=d3(f,y,x),k=E[2],s=E[1];if(gl(f,cv(f,zd,i[1][10][1],s),k)){var
P=function(d,a){return 1-b(c[45],f,a)},F=b(e[19][36],P,k);if(F)var
G=b(e[19][51],F[1],k),Q=G[2],u=1,l=a(c[21],[0,s,G[1]]),t=Q;else
var
u=0,l=s,t=k}else
var
u=1,l=y,t=x;if(u){var
R=i[1][10][1],S=i[1][10][1],T=[0,b(cw,d,l),0,C,l,0,0,0,S,R,C],j=g(e[19][17],O,T,t),v=j[4],H=j[2],V=j[9],W=j[8],X=j[6],Y=j[5],Z=j[3],_=a(e[17][9],j[7]),$=a(e[17][9],Y);if(N)var
aa=b(i[1][10][4],o,W),ab=a(z[9],d),ac=a(z[2],d),I=j_(a(z[8],d),ac,ab,aa,V);else
var
I=0;if(L)var
K=[0,v],J=M(ax[2],0,0,Z,w[16],v);else
var
K=0,J=v;var
ad=a(e[17][1],H)+1|0;return[0,[0,j9(d,m,o,D,p,H,K,J,X,$,_),p,ad,I]]}return 0}function
ze(o,n,f,d){var
D=o?o[1]:1,E=n?n[1]:0;a(aA[3],zf);var
p=d[2],q=b(z[20],f,d),s=a(bD,b(z[18],d,f)),t=s[2],F=s[3];if(t)var
u=b(c[82],p,t[1]),w=u[1],i=u[2],v=1;else
var
C=b(c[82],p,F),w=C[1],i=C[2],v=0;if(0===i)return a(l[1],d);var
y=ka(d,D,E,f,v,w,a(e[19][12],i));if(y){var
h=y[1],k=h[4],A=h[3],B=h[1];if(h[2])var
G=a(c[10],q),H=[0,x(b(r[lP],zg,G)),0],I=x(r[16]),J=[0,b(l[26],A,I),H],K=a(r[82],[0,[0,f,q],0]),L=[0,a(j[70][8],K),J],M=[0,a(z[39],B),L],m=a(l[7],M);else
var
O=x(r[16]),P=[0,b(l[26],A,O),0],Q=[0,x(a(r[75],[0,f,0])),P],R=[0,a(z[39],B),Q],m=a(l[7],R);if(0===k)return a(m,d);var
N=function(d){var
e=0;function
f(d){var
e=a(c[10],d),f=x(b(r[lP],zh,e));return a(l[21],f)}var
g=[0,b(l[30],f,k),e],h=a(r[83],k),i=[0,a(j[70][8],h),g];return b(l[19],i,d)};return g(l[5],m,N,d)}return a(l[1],d)}function
gm(l,f,d){var
s=l?l[1]:1,h=d[2],m=b(z[17],d,f),k=b(c[3],h,m);if(9===k[0])var
C=d3(h,k[1],k[2])[2],n=a(e[19][11],C);else
var
n=0;function
o(e){var
f=b(c[3],h,e);if(1===f[0])return f[1];var
j=a(z[2],d),k=a(z[8],d),l=g(aU[8],k,j,e),m=a(i[1][6],l);return b(z[20],m,d)}var
t=a(z[8],d);function
u(f,b){var
h=b[3],i=b[2],j=b[1],k=f[1],l=a(z[2],d),e=M(kb[6],t,l,zi,j,k),m=e[2];return[0,g(c[39],i,h,e[1]),m]}function
v(a){var
c=b(cw,d,a);return[0,a,o(a),c]}var
p=b(e[17][17],v,n),q=s?[0,[0,f,o(f),m],p]:p,w=a(z[2],d),x=[0,a(z[7],d),w],y=g(e[17][18],u,x,q)[1],A=aD(y,b(e[17][17],e[7],q)),B=b(r[5],A,2);return b(j[70][8],B,d)}function
kc(D,d){var
E=d[1],F=a(am[41],[2,d]),s=a(y[27],d),k=s[2],t=s[1],G=t[1];function
H(b,d){return a(c[25],[0,E,b])}var
I=b(e[19][16],H,G),J=a(e[19][11],I),L=a(e[17][9],J),m=t[6],j=b(e[17][15],c[V],k[2]),N=a(e[17][1],j)-m|0,u=b(e[17][h],N,j),v=u[2],l=u[1],o=a(e[17][1],l),O=T(0,j),P=[0,a(c[25],d),O],p=a(c[21],P),Q=a(y[2],0),f=[0,a(w[17],Q)],R=a(y[2],0),S=g(A[11],0,R,f),U=b(c[37],S,[0,[0,0,p],l]),q=k[9].length-1,W=k[9],X=k[4];function
Y(g,H,o){var
p=a(c[8],o),r=b(c[h][4],L,p),k=b(c[90],f[1],r),l=k[1],s=b(c[82],f[1],k[2])[2],t=b(e[17][h],m,s)[2],j=a(e[17][1],l)-m|0,n=aV(g+1|0,b(e[17][h],j,l)[1]),u=T(0,n),w=T((j+g|0)+1|0,v),x=b(e[19][5],w,u),y=[0,a(c[27],[0,d,g+1|0]),x],z=[0,a(c[21],y),0],A=b(e[18],t,z),B=aD(a(c[9],(j+g|0)+1|0),A),D=a(c[9],(1+q|0)-g|0),E=b(c[37],B,n),F=a(C[21],g),G=b(C[16],zj,F);return[0,[0,[0,a(i[1][6],G)],E],D]}var
r=g(e[19][55],Y,X,W),_=a(y[2],0),$=g(Z[76],_,d,4);function
x(f){var
g=T(0,l),h=T((o+q|0)+f|0,v),i=b(e[19][5],h,g),j=[0,a(c[25],d),i];return a(c[21],j)}var
z=[0,[0,0,x(2+o|0)],l],aa=T(0,z),ab=[0,a(c[9],(o+q|0)+3|0),aa],ac=a(c[21],ab),ad=b(c[38],ac,z);function
ae(a){return a[2]}var
af=b(e[19][15],ae,r),ag=[0,$,ad,a(c[9],1),af],ah=a(c[30],ag),ai=x(1),aj=f[1],ak=a(y[2],0),al=n(aU[10],ak,aj,ai,0),an=1+(r.length-1)|0,ao=[0,[0,[0,a(i[1][6],zk)],U],j];function
ap(a){return a[1]}var
aq=b(e[19][15],ap,r),ar=a(e[19][11],aq),as=a(e[17][9],ar),at=b(e[18],as,ao),au=[0,[0,al,b(c[h][1],an,p)],at],av=b(c[38],ah,au),aw=b(w[c5],D,f[1]),ax=b(c[5],f[1],av),ay=aF(bf[2],0,0,0,0,[0,aw],0,ax),az=b(K[5],F,zl),aA=[1,M(bf[3],0,0,az,0,[0,[0,ay],zm])],B=a(y[2],0);return[0,B,a(w[17],B),j,p,aA]}function
kd(w,l,f,k){var
g=k[1],d=kc(f,g),h=d[3],m=d[5],n=d[4],o=d[2],p=d[1],q=a(am[41],[2,g]),e=[0,o],r=b(K[6],zn,q),s=hm(e),i=al(e,m),t=M(ax[2],0,0,p,e[1],i),j=T(0,h),u=[0,a(c[21],[0,i,j]),0],v=[0,n,[0,fA(l,t,j),u]];return c7(r,f,e[1],h,s,v)}function
zo(d,c,b,a){kd(d,c,b,a);return 0}b_([0,zp,function(a,b){return b9(zo,a,b)}]);function
ke(k,f,d){var
s=k?k[1]:1,l=a(z[8],d),h=a(z[2],d),t=b(cw,d,f),u=a(z[9],d),v=a(q[77],u),w=a(i[1][10][35],v),m=b(c[3],h,f),x=9===m[0]?a(e[19][11],m[2]):0;function
n(d){var
e=b(c[3],h,d);if(1===e[0])return e[1];var
f=g(aU[8],l,h,d),j=a(i[1][6],f);return b(aU[25],j,w)}function
y(e,b){var
f=b[3],h=b[2],i=b[1],j=a(z[2],d),k=M(kb[6],l,j,zq,i,e)[1];return g(c[39],h,f,k)}function
A(a){var
c=b(cw,d,a);return[0,a,n(a),c]}var
o=b(e[17][17],A,x),p=s?[0,[0,f,n(f),t],o]:o,B=a(z[7],d),C=g(e[17][18],y,B,p),D=aD(C,b(e[17][17],e[7],p)),E=b(r[5],D,2);return b(j[70][8],E,d)}function
kf(h,g){var
a=b(c[3],h,g);switch(a[0]){case
10:var
d=a[1];return[0,[1,d[1]],d[2]];case
11:var
e=a[1];return[0,[2,e[1]],e[2]];case
12:var
f=a[1];return[0,[3,f[1]],f[2]];default:throw[0,a7,zr]}}function
cU(d,h,f){if(!b(c[45],d,f))if(!b(c[44],d,f)){var
i=b(c[3],d,h),j=b(c[3],d,f);if(9===i[0])if(9===j[0]){var
k=j[2],l=i[2],m=i[1];if(g(c[95],d,m,j[1]))if(b(c[56],d,m)){var
p=b(c[77],d,m)[1],e=a(Z[47],p);if(e<=l.length-1)if(e<=k.length-1){var
q=g(bF[7],l,l.length-1-e|0,e),r=g(bF[7],k,k.length-1-e|0,e),s=function(a,b){return cU(d,a,b)};return g(bF[32],s,q,r)}var
t=function(a,b){return cU(d,a,b)};return n(c[99],d,t,h,f)}}var
o=function(a,b){return cU(d,a,b)};return n(c[99],d,o,h,f)}return 1}function
zs(y,q){var
B=a(z[8],q),ae=b(z[19],q,y),d=[0,a(z[2],q)];function
I(e,a,d,c,b){var
f=d2(b,a),g=d2(c,a);return M(eG[4],e,0,d,g,f)}var
w=0,j=0,o=a(c[10],y),v=ae;for(;;){var
C=b(c[3],d[1],v);if(6===C[0]){var
J=C[3],K=C[2],af=C[1],L=b(c[3],d[1],K);if(9===L[0]){var
m=L[2],V=m.length-1,D=L[1];if(3===V){var
E=m[2],N=m[3],W=t(cq),am=m[1],ao=u===W?cq[1]:k===W?a(s[2],cq):cq;if(g(c[ba],d[1],ao,D)){var
ap=a(e[17][1],j);if(n(c[h][14],d[1],1,ap,E))var
S=1;else{var
av=a(e[17][1],j);if(n(c[h][14],d[1],1,av,N))var
S=1;else
var
R=1,S=0}if(S){var
aq=kf(d[1],D)[2],ar=a(e[17][1],j);if(n(c[h][14],d[1],1,ar,E))var
F=E,O=N;else
var
F=N,O=E;var
X=t(cr),as=u===X?cr[1]:k===X?a(s[2],cr):cr,at=[0,dh(d[1],[0,as,aq]),[0,am,F]],Y=a(c[21],at);if(cU(d[1],F,O))if(I(B,j,d,O,F)){var
au=b(c[h][5],Y,J),w=1,o=a(c[21],[0,o,[0,Y]]),v=au;continue}var
p=1,R=0}}else
var
R=1;if(R)var
p=0}else
if(4===V){var
Z=m[1],G=m[2],_=m[3],P=m[4],$=t(bw),aw=u===$?bw[1]:k===$?a(s[2],bw):bw;if(g(c[ba],d[1],aw,D)){var
ax=a(e[17][1],j);if(n(c[h][14],d[1],1,ax,G))var
U=1;else{var
aD=a(e[17][1],j);if(n(c[h][14],d[1],1,aD,P))var
U=1;else
var
T=1,U=0}if(U){var
ay=kf(d[1],D)[2],az=a(e[17][1],j);if(n(c[h][14],d[1],1,az,G))var
aa=Z,H=G,Q=P;else
var
aa=_,H=P,Q=G;var
ab=t(bx),aA=u===ab?bx[1]:k===ab?a(s[2],bx):bx,aB=[0,dh(d[1],[0,aA,ay]),[0,aa,H]],ac=a(c[21],aB);if(cU(d[1],H,Q))if(I(B,j,d,Z,_))if(I(B,j,d,Q,H)){var
aC=b(c[h][5],ac,J),w=1,o=a(c[21],[0,o,[0,ac]]),v=aC;continue}var
p=1,T=0}}else
var
T=1;if(T)var
p=0}else
var
p=0}else
var
p=0;if(!p)if(!w){var
ag=d2(K,j),ai=ch(A[7],B,d,0,0,0,0,0,0,ag),aj=[0,a(c[9],1)],ak=[0,b(c[h][1],1,o),aj],al=a(c[21],ak),w=0,j=[0,an(af,[0,ai],K),j],o=al,v=J;continue}}var
aE=b(A[41],d[1],j),aF=function(f){var
e=a(ah,f),g=e[2],h=e[3],i=e[1];if(g)if(b(c[47],d[1],g[1]))return[0,i,h];return f},ad=b(e[17][15],aF,aE),aG=b(c[37],v,ad),aH=b(c[38],o,ad),aI=b(A[35],d[1],aH),aJ=b(A[35],d[1],aG);if(w){var
aK=x(a(r[42],aI)),aL=x(b(r[k6],y,aJ));return g(l[9],aL,aK,q)}var
aM=a(i[1][9],y),aN=a(f[3],zt),aO=b(f[12],aN,aM);return g(l[24],0,aO,q)}}function
kg(c,b){try{a(x(a(r[75],[0,c,0])),b);var
h=0,d=h}catch(b){b=E(b);if(!a(af[20],b))throw b;var
d=1}if(d){var
e=a(f[3],zu);return g(l[24],0,e,b)}return zs(c,b)}function
gn(p,n){function
d(k){var
u=a(j[66][5],k),v=a(az[41],u),w=a(j[66][4],k),m=n[2],r=n[1];function
x(b){var
c=a(D[2][1][1],b);return a(q[b2],c)}var
s=b(F[c0],x,w),o=s[1],d=b(c[aY],s[2],v);function
y(w){var
q=cz(o),n=q[1],x=q[2],y=cy(zw,function(a){throw[0,Q,zv]},n)[2],z=a(j[66][3],k),A=a(j[66][6],k),B=b(c[h][11],x,z),s=[0,b(R[1],0,zx),0];function
C(j){if(be[1]){var
k=a(b$(d),j),l=a(f[5],0),m=a(f[3],zy),o=b(f[12],m,l),p=b(f[12],o,k);b(aq[6],0,p)}var
q=a6[1],r=[0,a(i[1][6],zz),0,q],s=$(n),t=g(D[1][13],c[9],0,n);function
u(g){var
f=[0,g],i=[0,f8(0,f,d,et(d,f,r,j,0,s,B))[3],t],k=b(U[55],f[1],i),l=a(e[17][9],y),m=b(c[h][4],l,k);return[0,f[1],m]}return b(cc[2],1,u)}if(p)var
E=p[1],F=[0,[0,i[1][10][1]]],H=function(a){return ef(d,F,a)},I=b(e[17][15],H,E),J=function(c){var
d=c[1],f=c[2];function
g(e){var
c=a(D[2][1][1],e);return b(i[1][1],c,m)?[0,d,f]:[0,0,[0,c,0]]}var
h=b(e[17][17],g,o);return[0,b(G[25],r,d),h,s]},K=b(e[17][15],J,I),t=a(j[16],K);else{var
u=er([0,d,[0,A]],w,n);if(u)var
L=a(e[19][11],u[1][3]),M=a(G[30][2],L),N=function(a){return it(0,0,a)},O=b(e[17][15],N,M),P=function(a){return[0,r,a,s]},S=b(e[17][15],P,O),v=a(j[16],S);else
var
T=a(i[1][9],m),V=a(f[3],zA),W=b(f[12],V,T),v=b(l[66][5],0,W);var
t=v}return b(j[71][1],t,C)}try{var
C=function(f,e){var
d=f,c=e;for(;;){if(c){var
g=c[2],h=a(D[2][1][1],c[1]);if(b(i[1][1],m,h))return d;var
d=d+1|0,c=g;continue}throw W}}(1,o),H=a(j[16],C),t=H}catch(c){c=E(c);if(c!==W)throw c;var
z=a(i[1][9],m),A=a(f[3],zB),B=b(f[12],A,z),t=b(l[66][5],0,B)}return b(j[71][1],t,y)}return a(j[66][9],d)}aG(1082,[0,j8,j9,j_,gk,gl,j$,ka,ze,gm,kc,kd,ke,kg,cU,gn],ll);function
kh(a){var
c=a[4];function
d(a){return b(R[1],0,[1,a[3]])}var
f=b(e[17][15],d,c);return b(cL[2][88],1,f)}function
ki(b,a){return g(aA[1],zC,b,a)}function
zD(b){var
a=ki(zF,zE);if(1===a[0])return a[1];throw[0,Q,zG]}var
zH=aO[50];function
kj(c,f){function
d(c){var
a=b(e[17][15],g,c);if(b(e[17][25],G[3],a))return 0;function
d(a){if(a)if(0!==a[1])return 1;return 0}return b(e[17][26],d,a)?zL:zM}function
g(e){var
a=e[3];switch(a[0]){case
0:return b(cD[34],c,a[1])?zI:0;case
2:return zJ;case
3:var
f=a[2];return b(cD[34],c,a[1])?zK:d(f);default:return 0}}function
h(a){return d(a[2])}var
a=b(e[17][15],h,f);if(b(e[17][25],G[3],a))return 0;function
i(a){if(a)if(0!==a[1])return 1;return 0}return b(e[17][26],i,a)?zN:zO}function
kk(m,s,l){var
h=a(y[2],0),d=[0,a(w[17],h)];function
x(e,d){var
f=d[1],g=b(y[47],h,[1,d[2][1]])[1],i=[0,a(c[8],g)];return _([0,[0,f[1]],i,e])}var
z=g(e[17][21],x,s,l),t=a(e[17][9],z);function
A(p){var
j=p[2],f=p[1],ap=p[2],aq=p[1];function
V(c){var
b=a(ah,c),d=b[1],e=a(G[7],b[2]);return[0,a(K[10][16],d),e]}var
W=b(e[17][15],V,t),n=f[1],l=f[2],r=f[3],X=f[4],z=b(y[47],h,[1,j[1]]),A=z[1],Y=z[2];if(m[1]){var
B=dj(Y),Z=B[1];d[1]=M(w[lT],0,0,w[b1],d[1],B[2]);var
_=[0,a(ar[16],A),Z],k=a(aO[35],_)}else
var
k=A;var
E=f[6];if(E){var
F=E[1];if(0===F[0]){var
v=f[5];if(v){var
x=v[1];if(0===x[0])var
q=0;else
if(x[1])var
q=0;else
var
U=function(c){var
d=a(D[1][1][1],c),e=a(K[10][16],d);return 1-b(i[1][1],e,f[1])},H=b(e[17][33],U,t),q=1}else
var
q=0;if(!q)var
H=t;var
o=$(l),aa=o[3],ab=o[2],ac=[0,b(e[18],o[1],H),ab,aa],ad=j[3],ae=a(c[8],k),I=eN(h,d,f[6],ae,ac,W,ad),u=[0,[0,n,I[2],k,o,I[1]]]}else{var
af=F[1],J=$(l),ag=j[3],ai=[0,[0,n,a(c[8],k)],0],aj=a(c[8],k),L=eN(h,d,f[6],aj,J,ai,ag),s=L[2],N=L[1],O=b(K[5],n,zP),ak=function(ae,t,h,ad){var
z=h[5],A=b(e[18],j[4][4],h[4]),i=[0,h[1],j[4][2],h[3],A,z];kh(i);var
u=h[1];if(1===u[0]){var
o=u[1],v=al(d,h[1]),B=function(e,d){var
f=b(t,e,d);return a(c[8],f)},p=a(f$(d[1],B),N),x=b(K[5],O,zR),D=function(E,D,B){b(y[52],[0,o],gj[3]);function
h(n,d){var
e=a(aN[34],d[2]),f=[1,a(am[11],e)],h=a(y[2],0),c=g(aO[31],0,h,f),j=[0,b(R[1],0,[0,c,1,0]),0],k=b(C[16],i[2],zS);b(cf[1],k,j);var
l=[0,b(R[1],0,[0,c,0,0]),0],m=b(C[16],i[2],zT);return b(cf[1],m,l)}b(aJ[8][10],h,s);var
e=a(y[2],0);if(1-m[1])d[1]=a(w[17],e);var
q=[0,n,l,r,r,0,0,f[7]],u=[0,[0,q,[0,o,t,p,i],[0,n,s,b(c[5],d[1],v),J,p]],0],z=j[3],A=[0,[0,a(c[8],k),x,z]];return dE(m[3],e,d[1],A,u)},E=a(y[2],0),q=[0,a(w[17],E)],F=[0,v,T(0,l)],G=a(c[21],F),H=T(0,l),I=[0,a(c[8],k),H],L=a(c[21],I),M=bh(a(y[2],0),q,r,L,G),P=b(c[37],M,l),S=j[3],U=j[1],V=function(a){return jP(i,s,af,U,o,S,p,a)},W=[0],X=0,Y=[0,a(bQ[1],D)],Z=[0,function(a){return a}],_=[0,au(V)],$=[0,i[3]],aa=[0,f[7]],ab=a(w[bq],q[1]),ac=b(c[5],q[1],P);bI(aW[7],x,0,ac,ab,0,aa,$,_,Z,Y,X,W);return 0}throw[0,Q,zQ]};f9(0,0,m[1],f[7],zU,d,h,[0,O,l,X],0,N,ak);var
u=0}}else
var
P=$(l),an=j[3],ao=a(c[8],k),S=eN(h,d,f[6],ao,P,0,an),u=[0,[0,n,S[2],k,P,S[1]]];return[0,aq,ap,u]}var
f=b(e[17][15],A,l);if(f){var
j=f[1],n=j[3],o=j[2],k=j[1];if(n){if(!f[2]){var
p=n[1],q=d[1],r=k[6];return r?0===r[1][0]?dE(m[3],h,q,0,[0,[0,k,o,p],0]):0:dE(m[3],h,q,0,[0,[0,k,o,p],0])}}else
if(!f[2])return 0}function
u(b){var
c=b[2],d=b[1];return[0,d,c,a(G[7],b[3])]}var
v=b(e[17][15],u,f);return dE(m[3],h,d[1],0,v)}function
go(b){var
a=b[5];if(a)if(0!==a[1][0])return 1;return 0}function
kl(H,x,aa){function
v(a){return b(e[17][29],a,H)?0:1}try{var
aO=function(a){return 1===a[0]?[0,a[1]]:0};b(e[17][b2],aO,H)}catch(a){a=E(a);if(a!==W)throw a}var
ab=v(zX),ac=v(zY),I=1-v(zZ),z=I?1-cn[1]:I,p=a(y[2],0),l=a(g$[31],0),o=[0,l,ac,ab],d=[0,a(w[17],p)];function
ad(I){var
m=I[1],J=m[2],j=m[1][2],$=m[4],aa=m[3],ab=n(a6[25],0,0,0,p),L=g(A[65],ab,d,aa)[2],o=L[2],N=L[1],ac=N[2],ad=a(a6[16],N[1]);function
ae(a){return b(ad,a,0)}var
r=g(A[65],ae,d,$),h=b(A[41],d[1],ac),af=b(A[35],d[1],r),s=kj(j,x);function
O(b,a){return 0===b?[1,[0,a]]:[0,a]}if(J){var
P=J[1],Q=P[2],B=P[1];if(Q){var
C=Q[1];try{var
aP=b(q[7],C[2],h)[1],aQ=[0,O(B,a(e[17][1],h)-aP|0)],S=aQ}catch(c){c=E(c);if(c!==W)throw c;var
ag=a(f[3],z0),ah=a(i[1][9],C[2]),ai=a(f[3],z1),aj=b(f[12],ai,ah),ak=b(f[12],aj,ag),S=bL([0,[0,C[1]],z2,ak])}var
T=S}else
var
aR=kj(j,[0,I,0])?[0,O(B,a(e[17][1],h)-1|0)]:0===B?Ab:[0,[0,a(e[17][1],h)-1|0]],T=aR;var
t=T}else{if(s)if(0===s[1])var
t=[0,[0,a(e[17][1],h)-1|0]],H=1;else
var
H=0;else
var
H=0;if(!H)var
t=0}var
D=b(c[38],af,h);n(km[13],p,w[16],d[1],D);if(z){n(km[13],p,w[16],d[1],D);var
F=b(K[5],j,z3),am=[0,[0,dU(0,[0,l],d,0,D)],z4],k=M(bf[3],0,0,F,0,am),an=b(A[35],d[1],r),ao=b(A[41],d[1],h);if(l)var
U=d[1];else
var
aO=a(y[2],0),U=a(w[17],aO);d[1]=U;var
ap=al(d,[1,k]),aq=[0,ap,a0(0,a(e[17][1],ao))],ar=a(c[21],aq);d_(k,0,z5);d_(k,0,z6);d_(k,0,z7);n(ca[26],1,[1,k],0,[0,o,0]);var
as=0,at=[1,F],au=R[1],av=[0,function(a){return b(au,0,a)}(at),as];b(cL[2][88],1,av);var
V=[0,[0,F,k]],v=ar,u=an}else
var
V=0,v=r,u=r;if(s){var
aw=s[1],X=b(K[5],j,z8),ax=[0,_([0,[0,a(i[1][6],z9)],0,v]),h],ay=a(c[9],1),Y=b(c[38],ay,ax);g(fh,a(y[2],0),d,Y);var
az=b(w[c5],l,d[1]),aA=b(c[5],d[1],Y),aB=[0,[0,aF(bf[2],0,0,0,0,[0,az],0,aA)],z_],Z=M(bf[3],0,0,X,0,aB),aC=z?[0,[0,[0,a(e[17][1],h)+1|0,0],z$],0]:0,aD=[0,b(e[18],o,aC),0];n(ca[26],1,[1,Z],0,aD);var
aE=0,aG=[1,X],aH=R[1],aI=[0,function(a){return b(aH,0,a)}(aG),aE];b(cL[2][88],1,aI);var
aJ=a(e[17][1],h)+1|0,aK=b(c[5],d[1],v),aL=function(a){return a[2]},aM=[1,[0,b(G[16],aL,V),aK,Z,aJ]],aN=aw?[0,[1,aM]]:Aa;return[0,j,h,v,u,t,aN,o]}return[0,j,h,u,u,t,0,o]}var
J=b(e[17][15],ad,x);function
ae(a){var
b=a[5],c=0,d=b?b[1]:Ac;return[0,a[1],d,c]}var
af=b(e[17][15],ae,J);function
ag(a){var
b=a[6];if(b)if(0===b[1][0])return[0,a[1],a[2],a[3],a[4],a[5],[0,[0,af]],a[7]];return a}var
m=b(e[17][15],ag,J);function
ah(a){return a[2]}var
L=b(e[17][15],ah,x),j=a(y[2],0);function
ai(a){var
d=b(c[37],a[4],a[2]),e=b(c[37],a[3],a[2]);return[0,a[1],[0,d,e],a[7]]}var
N=b(e[17][15],ai,m),B=a(e[17][b1],N),O=B[2],P=B[1],aj=B[3];function
an(a){return a[1]}var
ao=b(e[17][15],an,O),U=aF(a6[3],j,d[1],0,0,P,ao,aj);function
ap(f){var
b=t(aB),g=f[2],h=u===b?aB[1]:k===b?a(s[2],aB):aB,e=t(aE),i=al(d,h),j=u===e?aE[1]:k===e?a(s[2],aE):aE,l=[0,0,al(d,j),i,g];return a(c[20],l)}var
V=b(e[17][15],ap,O);function
aq(b,a){return _([0,[0,b],0,a])}var
as=g(e[17][21],aq,P,V),at=a(e[17][9],as);function
au(a){return M(ca[20],j,d[1],a[2][1],0,a[3])}var
av=b(e[17][15],au,N),aw=0;function
ax(d){var
a=b(kn[9],j,U);b(e[17][14],a,aa);function
c(d,a,c){var
f=a[6],g=a[1];function
h(a){return ih(g,f,j,d,a)}return b(e[17][15],h,c)}return n(e[17][77],c,av,m,L)}var
ay=b(kn[14],ax,aw),r=b(A[41],d[1],at);function
az(c,q){var
g=b(A[41],d[1],c[2]);if(id(c[6])){var
h=c[5];if(h){var
k=h[1];if(0===k[0])var
f=0;else
if(k[1])var
f=0;else
var
o=function(d){var
e=a(D[1][1][1],d),f=a(K[10][16],e);return 1-b(i[1][1],f,c[1])},p=b(e[17][33],o,r),l=$(b(e[18],g,p)),f=1}else
var
f=0;if(!f)var
l=$(b(e[18],g,r));var
m=l}else
var
m=$(g);b(A[35],d[1],c[4]);var
n=b(A[35],d[1],c[3]);return et(j,d,[0,c[1],z,U],q,0,m,n)}var
aA=g(e[17][21],az,m,ay),X=i[61],Y=t(aE),aC=X[2],aD=X[1],aG=u===Y?aE[1]:k===Y?a(s[2],aE):aE,aI=a(ar[8],aG),aJ=a(e[17][5],m)[1],aK=a(i[1][8],aJ),aL=[0,aD,b(i[18][8],aI,aC)];n(aH[17],0,aK,aL,1);var
C=a2(a(e[17][1],L),0),Z=[0,0];function
aM(f,x){var
p=f[6];if(p){var
q=p[1];if(0===q[0])var
m=0;else
var
s=[0,q[1]],m=1}else
var
m=0;if(!m)var
s=0;var
t=f[5];if(t){var
u=t[1];if(0===u[0])var
k=0;else
if(u[1])var
k=0;else
var
B=function(c){var
d=a(D[1][1][1],c),e=a(K[10][16],d);return 1-b(i[1][1],e,f[1])},v=b(e[17][33],B,r),k=1}else
var
k=0;if(!k)var
v=r;var
E=Z[1];function
z(aw,D,k,ax){kh(k);var
F=0,H=k[5];function
I(e,d){var
f=a(aN[34],e),b=[0,[1,a(am[11],f)]],c=cR(k);return[0,[0,aH[4],c,1,0,b],d]}var
J=[0,g(i[1][10][15],I,H,F)],K=[0,gb(k),0];g(aH[22],0,K,J);var
u=k[1];if(1===u[0]){var
an=u[1];d[1]=a(w[18],ax);var
ao=function(e,d){var
f=b(D,e,d);return a(c[8],f)},ap=a(f$(d[1],ao),aw),q=d[1],L=f[7],M=f[6],N=f[5],O=b(A[35],q,f[4]),P=b(A[35],q,f[3]),R=b(A[41],q,f[2]),aq=[0,[0,[0,f[1],R,P,O,N,M,L],[0,an,D,ap,k]]];S(C,E)[E+1]=aq;var
ar=function(b){return 1-a(G[3],b)},v=b(bF[31],ar,C);if(v){var
as=a(A[35],d[1]),at=b(e[17][15],as,V),au=function(b){return a(G[7],b)},j=b(e[19][49],au,C);if(j)if(j[2])var
r=0;else
var
x=j,r=1;else
var
r=0;if(!r)var
U=function(a){return 1-go(a[1])},l=b(e[17][33],U,j),W=function(f){var
b=f[1],c=b[5];if(c){var
d=c[1];if(0===d[0])return d[1]}return a(e[17][1],b[2])-1|0},X=b(e[19][50],W,l),Y=a(y[2],0),m=[0,a(w[17],Y)],Z=function(o){var
g=o[1],u=[0,g[1]],v=b(A[14],m,[1,o[2][1]]),w=b(c[37],g[3],g[2]),k=a(e[17][1],l),f=0,i=j;for(;;){if(i){var
n=i[2],p=i[1],q=p[2],d=p[1],r=d[5];if(r){var
s=r[1];if(0!==s[0]){var
t=s[1];if(t){var
y=t[1],z=a(e[17][1],l),B=a0(a(e[17][1],d[2]),z),C=[0,a(c[22],q[1]),B],D=a(c[21],C),E=b(c[h][1],1,D),F=a2(1,[0,d[1]]),G=a2(1,b(c[37],d[3],d[2])),H=[0,E,a0(a(e[17][1],d[2]),1)],I=a(c[21],H),J=[0,I,T(0,d[2])],K=a(c[21],J),L=[0,[0,a2(1,y),0],[0,F,G,a2(1,b(c[38],K,d[2]))]],f=[0,a(c[31],L),f],i=n;continue}var
M=a0(0,a(e[17][1],l)),N=[0,a(c[22],q[1]),M],f=[0,a(c[21],N),f],i=n;continue}}var
x=[0,a(c[9],k),f],k=k-1|0,f=x,i=n;continue}var
O=a(e[17][9],f),P=[0,v,a(e[19][12],O)],Q=a(c[21],P),R=T(0,g[2]),S=a(e[17][1],g[2]),U=[0,b(c[h][1],S,Q),R],V=a(c[21],U);return[0,u,w,b(c[38],V,g[2])]}},_=b(e[17][15],Z,l),p=a(e[17][b1],_),$=p[2],aa=p[1],ab=a(e[19][12],p[3]),ac=a(e[19][12],$),ad=[0,a(e[19][12],aa),ac,ab],ae=function(h,f){var
e=f[2],d=f[1];if(go(d))return[0,d,e];var
i=a(c[31],[0,[0,X,h],ad]),j=b(c[37],d[3],d[2]),g=bu(d[1],i,[0,j],o[1],m[1],zV);n(ca[26],1,[1,g],0,[0,d[7],0]);return[0,d,[0,g,e[2],e[3],e[4]]]},s=b(e[17][16],ae,j),af=function(a){return go(a[1])},t=b(e[17][35],af,s),ag=t[2],ah=t[1],ai=function(j){var
f=j[2],d=j[1],t=b(c[37],d[3],d[2]),k=d[5];if(k){var
l=k[1];if(0===l[0])var
i=0;else
var
g=l[1],i=1}else
var
i=0;if(!i)var
g=0;var
u=al(m,[1,f[1]]);function
v(e){var
f=e[2];if(ak.caml_equal(e[1][1],d[1])){var
h=function(b){return a(c[9],1)};return b(G[16],h,g)}return[0,al(m,[1,f[1]])]}var
w=b(e[17][70],v,s),x=[0,u,a(e[19][12],w)],y=a(c[21],x),z=T(0,d[2]),A=a(e[17][1],d[2]),B=[0,b(c[h][1],A,y),z],C=a(c[21],B),p=b(c[38],C,d[2]);if(g)var
D=[0,a2(1,g[1]),0],E=a2(1,[0,d[1]]),F=[0,D,[0,E,a2(1,b(c[37],d[3],d[2])),a2(1,p)]],q=a(c[31],F);else
var
q=p;var
r=bu(d[1],q,[0,t],o[1],m[1],zW);n(ca[26],1,[1,r],0,[0,d[7],0]);return[0,d,[0,r,f[2],f[3],f[4]]]},aj=b(e[17][15],ai,ah),x=b(e[18],ag,aj);var
av=o[2],z=av||o[3];if(z)return kk(o,at,x);var
B=z}else
var
B=v;return B}throw[0,Q,Ae]}f9(f[6],v,l,f[7],Ad,d,j,[0,f[1],f[2],f[4]],s,x,z);Z[1]++;return 0}return g(F[20],aM,m,aA)}function
ko(d,a,c){function
f(c){var
a=c[1][1],d=b(R[1],[0,a[1]],a[2]);return g(dk[14],d,0,Af)}b(e[17][14],f,a);return kl(d,a,c)}function
kp(A,y,d){var
B=a(z[7],d);function
j(e,h){var
c=a(at[26],e);switch(c[0]){case
6:var
k=c[1];if(k){var
p=c[3],q=k[1],s=a(z[8],d),m=dX(i[1][10][1],q,s),t=a(at[2],m),f=j(b(dt[14],t,p),[0,m]),u=f[3],v=f[2],w=f[1],y=x(r[16]);return[0,b(l[5],y,w),v,u]}break;case
8:var
n=c[1];if(n){var
o=n[1],A=c[4],B=c[2];if(ak.caml_string_equal(a(i[1][8],o),Ag))return[0,l[1],h,e];var
C=a(z[8],d),D=[0,dX(i[1][10][1],o,C)],g=j(b(dt[14],B,A),D),E=g[3],F=g[2],G=g[1],H=x(r[16]);return[0,b(l[5],H,G),F,E]}break}return[0,l[1],h,e]}var
C=a(z[2],d),k=j(b(c[5],C,B),0),o=k[2],D=k[3],E=k[1];if(o)var
F=o[1],p=function(a){return x(b(r[81],a,[1,F]))};else
var
p=function(a){return l[1]};var
G=a(c[8],D),H=a(z[2],d),g=b(c[3],H,G);if(8===g[0]){var
u=g[1];if(u){var
W=g[4],X=g[2],Y=u[1],Z=a(z[2],d),h=b(c[3],Z,W);if(8===h[0]){var
w=h[1];if(w)var
_=h[4],$=h[2],aa=w[1],ab=a(z[2],d),ac=dW(b(c[5],ab,$)),ad=a(z[2],d),v=[0,Y,aa,dW(b(c[5],ad,X)),ac,_],n=1;else
var
n=0}else
var
n=0;if(!n)var
v=a5(Aj);var
f=v,m=1}else
var
m=0}else
var
m=0;if(!m)var
f=a5(Ah);var
I=f[5],J=f[4],K=f[3],L=f[2],N=f[1];function
q(g,f){if(0===g)return[0,0,f];var
j=a(z[2],d),e=b(c[3],j,f);if(8===e[0]){var
h=e[1];if(h){var
k=e[3],l=e[2],m=h[1],i=q(g-1|0,e[4]);return[0,[0,[0,m,l,k],i[1]],i[2]]}}return a5(Ai)}var
s=q(J,I)[1],t=[0,N,[0,L,b(e[17][15],jT,s)]],O=x(a(r[75],t)),P=x(a(r[25],t)),Q=b(l[5],P,O),R=x(r[16]),S=b(l[26],K+1|0,R);function
T(a){var
c=a[1],d=a[3],e=a[2],f=p(c),g=b(l[5],f,y),h=x(M(r[bZ],0,[0,c],e,[0,d],cu));return b(l[5],h,g)}var
U=b(e[17][15],T,s),V=[0,E,[0,Q,[0,S,[0,b(l[11],A,U),0]]]];return b(l[7],V,d)}aG(1085,[0,ki,zD,zH,kl,kk,ko,kp,function(c,f,e,d){var
a=b(az[79],c,e),h=g(d7,function(a,d){var
e=aZ(d),h=g(q[99],c,f,d);return b(i[1][10][3],e,a)?b(i[1][10][7],h,a):a},a,d);return[0,a,b(i[1][10][9],h,a)]}],N);function
kq(k,h,g){function
d(d){var
l=a(j[66][6],d),e=b(c[82],l,g),f=e[1],m=e[2],n=b(z[42][21],d,f),o=a(aK[12],m),p=[0,a(c[9],1),o],q=a(c[21],p),s=[0,[0,a(i[1][6],Ak)],n,q],t=a(c[19],s),u=M(r[bZ],0,[0,h],t,0,ds[5]),v=M(r[bZ],0,[0,k],f,0,ds[5]);return b(j[18],v,u)}return a(j[66][10],d)}function
kr(c){if(1===c[0]){var
d=a(i[17][9],c[1]),e=a(i[6][5],d);return b(An[7],[0,Am,[0,e,0]],ds[6])}throw[0,Q,Al]}function
ks(o){function
d(h){var
x=a(j[66][5],h),e=a(j[66][6],h),y=a(j[66][3],h),A=ax[2];function
B(a){return g(A,0,0,a)}var
C=g(z[42][1],B,h,o),p=i[61],q=[0,e],f=x,l=y,r=C;for(;;){var
k=b(c[3],e,l),d=b(c[3],e,r);if(6===k[0]){var
w=k[2],P=k[3],Q=k[1];switch(d[0]){case
6:var
R=d[3];if(M(eG[4],f,[0,p],q,w,d[2])){var
S=_([0,Q,0,w]),f=b(c[bt],S,f),l=P,r=R;continue}return a5(Aq);case
9:var
m=0;break;default:var
m=1}}else
var
m=0;if(!m)if(9===d[0]){var
s=d[1],D=d[2];if(b(c[47],e,s)){var
E=b(c[75],e,s),F=q[1],t=n(h8,f,F,E,a(aK[11],D)),u=t[2],G=t[1],H=u[2],I=function(a){return 0},J=b(bF[49],I,H),v=cY(eG[10],p,f,G,u,J,l),K=v[1];if(v[2]){var
L=function(a){return[0,a,o]},N=b(cc[2],0,L),O=a(j[64][1],K);return b(j[71][2],O,N)}return a5(Ap)}}return a5(Ao)}}return a(j[66][9],d)}aG(1087,[0,kq,kr,ks],k7);a(aX[10],aj);var
eO=j[70][1],Ar=0;function
As(c,b,a,d){return kq(c,b,a)}var
Au=a(i[1][7],At),Av=[0,[5,a(m[16],B[13])],Au],Aw=[1,b(J[11],0,Av),0],Ay=a(i[1][7],Ax),Az=[0,[5,a(m[16],B[8])],Ay],AA=[1,b(J[11],0,Az),Aw],AC=a(i[1][7],AB),AD=[0,[5,a(m[16],B[8])],AC],AF=[0,[0,[0,AE,[1,b(J[11],0,AD),AA]],As],Ar];n(L[10][8],aj,AG,0,AF);var
AH=0;function
AI(a,b){return kr(a)}var
AK=a(i[1][7],AJ),AL=[0,[5,a(m[16],B[23])],AK],AN=[0,[0,[0,AM,[1,b(J[11],0,AL),0]],AI],AH];n(L[10][8],aj,AO,0,AN);var
AP=0;function
AQ(c,a,d){return b(cN[5],c,a)}var
AS=a(i[1][7],AR),AT=[0,[5,a(m[16],B[8])],AS],AU=[1,b(J[11],0,AT),0],AW=a(i[1][7],AV),AX=[0,[5,a(m[16],B[9])],AW],AZ=[0,[0,[0,AY,[1,b(J[11],0,AX),AU]],AQ],AP];n(L[10][8],aj,A0,0,AZ);var
A1=0;function
A2(b,c){return a(cN[4],b)}var
A4=a(i[1][7],A3),A5=[0,[5,a(m[16],B[9])],A4],A8=[0,[0,[0,A7,[0,A6,[1,b(J[11],0,A5),0]]],A2],A1];n(L[10][8],aj,A9,0,A8);var
A_=0,Ba=[0,[0,A$,function(a){return cN[2]}],A_];function
Bb(b,c){return a(cN[1],b)}var
Bd=a(i[1][7],Bc),Be=[0,[5,a(m[16],B[9])],Bd],Bg=[0,[0,[0,Bf,[1,b(J[11],0,Be),0]],Bb],Ba];n(L[10][8],aj,Bh,0,Bg);var
Bi=0;function
Bj(a,b){return i_(a)}var
Bl=a(i[1][7],Bk),Bm=[0,[5,a(m[16],B[8])],Bl],Bo=[0,[0,[0,Bn,[1,b(J[11],0,Bm),0]],Bj],Bi];n(L[10][8],aj,Bp,0,Bo);var
Bq=0;function
Br(c,a,d){return b(cN[3],c,a)}var
Bt=a(i[1][7],Bs),Bu=[0,[5,a(m[16],B[8])],Bt],Bv=[1,b(J[11],0,Bu),0],Bx=a(i[1][7],Bw),By=[0,[5,a(m[16],B[13])],Bx],BA=[0,[0,[0,Bz,[1,b(J[11],0,By),Bv]],Br],Bq];n(L[10][8],aj,BB,0,BA);var
BC=0;function
BD(a,e){var
c=0;function
d(b){return gm(c,a,b)}return b(j[70][1],0,d)}var
BF=a(i[1][7],BE),BG=[0,[5,a(m[16],B[13])],BF],BJ=[0,[0,[0,BI,[0,BH,[1,b(J[11],0,BG),0]]],BD],BC];n(L[10][8],aj,BK,0,BJ);var
BL=0;function
BM(a,d){function
c(b){return gm(BN,a,b)}return b(j[70][1],0,c)}var
BP=a(i[1][7],BO),BQ=[0,[5,a(m[16],B[13])],BP],BU=[0,[0,[0,BT,[0,BS,[0,BR,[1,b(J[11],0,BQ),0]]]],BM],BL];n(L[10][8],aj,BV,0,BU);var
BW=0;function
BX(a,e){var
c=0;function
d(b){return ke(c,a,b)}return b(j[70][1],0,d)}var
BZ=a(i[1][7],BY),B0=[0,[5,a(m[16],B[13])],BZ],B2=[0,[0,[0,B1,[1,b(J[11],0,B0),0]],BX],BW];n(L[10][8],aj,B3,0,B2);var
B4=0,B6=[0,[0,0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f){var
g=f[2];if(g){var
h=g[2];if(h){var
i=h[2];if(i){var
j=i[2];if(j){var
k=j[2];if(k){var
l=k[2];if(l)if(!l[2]){var
n=l[1],o=k[1],p=j[1],q=i[1],r=h[1],s=g[1],t=f[1],u=e[1],v=d[1],w=c[1],x=a(m[4],B[12]),y=b(m[8],x,w),z=a(m[4],B[24]),A=b(m[8],z,v),D=a(m[4],B[24]),E=b(m[8],D,u),F=a(m[4],B[24]),G=b(m[8],F,t),H=a(m[4],B[24]),I=b(m[8],H,s),J=a(m[4],B[24]),K=b(m[8],J,r),L=a(m[4],B[24]),M=b(m[8],L,q),N=a(m[4],B[24]),O=b(m[8],N,p),P=a(m[4],B[24]),Q=b(m[8],P,o),R=a(m[4],B[24]),S=b(m[8],R,n);return function(l,c){function
b(b){var
c=a(am[18],b);return a(hb[4],c)}var
d=b(S),e=b(Q),f=b(O),g=b(M),h=b(K),i=b(I),j=b(G),k=b(E);hc([0,b(A),k,j,i,y,h,g,f,e,d]);return c}}}}}}}}}}}return a(C[2],B5)}],B4];function
B7(b,a){return g(gp[2],a[1],[0,B8,b],a[2])}b(F[87],B7,B6);var
B9=0,B$=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f){var
g=f[2];if(g){var
h=g[2];if(h){var
i=h[2];if(i){var
j=i[2];if(j){var
k=j[2];if(k)if(!k[2])return function(a){return cV[4]}}}}}}}}}}return a(C[2],B_)},B9];function
Ca(c,a){return b(cV[3],[0,Cb,c],a)}b(F[87],Ca,B$);var
Cc=[6,a(v[12],B[24])],Cd=[0,[0,a(m[4],B[24])],Cc],Ce=[0,[1,b(J[11],0,Cd)],0],Cf=[6,a(v[12],B[24])],Cg=[0,[0,a(m[4],B[24])],Cf],Ch=[0,[1,b(J[11],0,Cg)],Ce],Ci=[6,a(v[12],B[24])],Cj=[0,[0,a(m[4],B[24])],Ci],Ck=[0,[1,b(J[11],0,Cj)],Ch],Cl=[6,a(v[12],B[24])],Cm=[0,[0,a(m[4],B[24])],Cl],Cn=[0,[1,b(J[11],0,Cm)],Ck],Co=[6,a(v[12],B[24])],Cp=[0,[0,a(m[4],B[24])],Co],Cq=[0,[1,b(J[11],0,Cp)],Cn],Cr=[6,a(v[12],B[24])],Cs=[0,[0,a(m[4],B[24])],Cr],Ct=[0,[1,b(J[11],0,Cs)],Cq],Cu=[6,a(v[12],B[24])],Cv=[0,[0,a(m[4],B[24])],Cu],Cw=[0,[1,b(J[11],0,Cv)],Ct],Cx=[6,a(v[12],B[24])],Cy=[0,[0,a(m[4],B[24])],Cx],Cz=[0,[1,b(J[11],0,Cy)],Cw],CA=[6,a(v[12],B[24])],CB=[0,[0,a(m[4],B[24])],CA],CC=[0,[1,b(J[11],0,CB)],Cz],CD=[6,a(v[12],B[12])],CE=[0,[0,a(m[4],B[12])],CD],CH=[0,[0,CG,[0,CF,[0,[1,b(J[11],0,CE)],CC]]],0];function
CI(b,a){return g(gq[1],[0,CJ,b],0,a)}b(F[87],CI,CH);var
CK=0;function
CL(c,e){function
d(b){if(j$(b,c))return a(cx[9],b);var
d=a(f[3],CM);return g(cx[41],0,d,b)}return b(j[70][1],0,d)}var
CO=a(i[1][7],CN),CP=[0,[5,a(m[16],B[9])],CO],CR=[0,[0,[0,CQ,[1,b(J[11],0,CP),0]],CL],CK];n(L[10][8],aj,CS,0,CR);var
CT=0;function
CU(d,c,a){var
e=x(b(L[13][24],a,c)),f=x(b(L[13][24],a,d));return b(eO,0,function(a){return kp(f,e,a)})}var
CW=a(i[1][7],CV),CX=[0,[5,a(m[16],L[2][1])],CW],CY=[1,b(J[11],0,CX),0],C0=a(i[1][7],CZ),C1=[0,[5,a(m[16],L[2][1])],C0],C3=[0,[0,[0,C2,[1,b(J[11],0,C1),CY]],CU],CT];n(L[10][8],aj,C4,0,C3);var
C5=0;function
C6(e,d,f){return b(eO,0,gc(d,a(hx,b(o[17],c[lq][1],e))))}var
C8=a(i[1][7],C7),C9=[0,[5,a(m[16],B[25])],C8],C_=[1,b(J[11],0,C9),0],Da=a(i[1][7],C$),Db=[0,[2,[5,a(m[16],B[13])]],Da],Dd=[0,[0,[0,Dc,[1,b(J[11],0,Db),C_]],C6],C5];function
De(c,a,d){return b(eO,0,gc(a,c))}var
Dg=a(i[1][7],Df),Dh=[0,[5,a(m[16],B[25])],Dg],Di=[1,b(J[11],0,Dh),0],Dk=a(i[1][7],Dj),Dl=[0,[0,[5,a(m[16],B[22])]],Dk],Dn=[0,[0,[0,Dm,[1,b(J[11],0,Dl),Di]],De],Dd];n(L[10][8],aj,Do,0,Dn);var
bG=a(m[2],Dp);function
Dq(b,a){return[0,b,a]}b(a_[9],bG,Dq);function
Dr(b,a){return a}b(a_[10],bG,Dr);function
Ds(g,c){var
d=a(m[6],bG),e=a(X[3],d),f=b(X[1][8],e,c);return a(bW[1],f)}b(X[7],bG,Ds);b(X[4],bG,0);var
Dt=a(m[4],bG),eP=g(v[13],v[9],Du,Dt),Dv=0,Dw=0;function
Dx(b,a){return Dy}var
DA=[0,[0,[0,0,[0,a(bn[10],Dz)]],Dx],Dw];function
DB(b,a){return DC}var
DE=[0,[0,[0,0,[0,a(bn[10],DD)]],DB],DA];function
DF(b,c,a){return[1,[0,[0,a,b]]]}var
DG=[6,v[14][2]],DI=[0,[0,[0,[0,0,[0,a(bn[10],DH)]],DG],DF],DE];function
DJ(b,a){return DK}var
DM=[0,[0,[0,0,[0,a(bn[10],DL)]],DJ],DI];function
DN(b,a){return DO}var
DQ=[0,[0,[0,0,[0,a(bn[10],DP)]],DN],DM];function
DR(b,a){return DS}var
DU=[0,[0,[0,0,[0,a(bn[10],DT)]],DR],DQ];function
DV(b,a){return DW}var
DY=[0,[0,[0,0,[0,a(bn[10],DX)]],DV],DU];function
DZ(b,a){return D0}var
D2=[0,0,[0,[0,0,0,[0,[0,[0,0,[0,a(bn[10],D1)]],DZ],DY]],Dv]];g(v[22],eP,0,D2);n(L[5][1],bG,ec,ec,ec);var
D3=[0,eP,0];function
D4(c){var
d=c[2],e=a(m[4],bG);return[0,b(m[7],e,d)]}g(L[10][5],D5,D4,D3);var
a$=a(m[2],D6);function
D7(b,a){return[0,b,a]}b(a_[9],a$,D7);function
D8(b,a){return a}b(a_[10],a$,D8);function
D9(g,c){var
d=a(m[6],a$),e=a(X[3],d),f=b(X[1][8],e,c);return a(bW[1],f)}b(X[7],a$,D9);b(X[4],a$,0);var
D_=a(m[4],a$),gr=g(v[13],v[9],D$,D_),Ea=0,Eb=0;function
Ec(d,a,c,b){return a}var
Ee=[0,a(bn[10],Ed)],Eg=[0,[0,[0,[0,[0,0,[0,a(bn[10],Ef)]],[1,[6,eP]]],Ee],Ec],Eb],Eh=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Eg]],Ea]];g(v[22],gr,0,Eh);n(L[5][1],a$,ed,ed,ed);var
Ei=[0,gr,0];function
Ej(c){var
d=c[2],e=a(m[4],a$);return[0,b(m[7],e,d)]}g(L[10][5],Ek,Ej,Ei);function
eQ(e,d,c,b){return a(i[1][9],b[2])}var
bH=a(m[2],El);function
Em(b,a){return[0,b,a]}b(a_[9],bH,Em);function
En(b,a){return a}b(a_[10],bH,En);function
Eo(g,c){var
d=a(m[6],bH),e=a(X[3],d),f=b(X[1][8],e,c);return a(bW[1],f)}b(X[7],bH,Eo);b(X[4],bH,0);var
Ep=a(m[4],bH),eR=g(v[13],v[9],Eq,Ep),Er=0,Es=0;function
Et(b,a){return[0,a,b]}g(v[22],eR,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,v[14][2]]],Et],Es]],Er]]);n(L[5][1],bH,eQ,eQ,eQ);var
Eu=[0,eR,0];function
Ev(c){var
d=c[2],e=a(m[4],bH);return[0,b(m[7],e,d)]}g(L[10][5],Ew,Ev,Eu);var
Ex=0,Ey=0;function
kt(e,d,c,b){return a(f[7],0)}function
ku(e,d,c,b){return a(f[7],0)}function
kv(e,d,c,b){return a(f[7],0)}var
eS=a(m[3],Ez),EA=a(m[4],eS),gs=g(v[13],v[8],EB,EA),EC=b(X[4],eS,0);n(L[5][1],eS,kt,ku,kv);var
eT=a(m[3],ED),EE=b(X[4],eT,0);function
kw(e,d,c,b){return a(f[7],0)}function
kx(e,d,c,b){return a(f[7],0)}function
ky(e,d,c,b){return a(f[7],0)}var
EF=a(m[4],eT),dF=g(v[13],v[10],EG,EF);n(L[5][1],eT,kw,kx,ky);var
eU=a(m[3],EH),EI=b(X[4],eU,0);function
kz(e,d,c,b){return a(f[7],0)}function
kA(e,d,c,b){return a(f[7],0)}function
kB(e,d,c,b){return a(f[7],0)}var
EJ=a(m[4],eU),dG=g(v[13],v[9],EK,EJ);n(L[5][1],eU,kz,kA,kB);var
cg=a(m[3],EL),EM=b(X[4],cg,0);function
kC(e,d,c,b){return a(f[7],0)}function
kD(e,d,c,b){return a(f[7],0)}function
kE(e,d,c,b){return a(f[7],0)}var
EN=a(m[4],cg),kF=g(v[13],v[10],EO,EN);n(L[5][1],cg,kC,kD,kE);function
EP(b,a){return a}function
kG(e,d){var
c=a(m[2],d);b(X[4],c,e);return c}var
ER=a(m[6],B[4]),eV=kG([0,a(X[3],ER)],EQ);function
kH(b,a){return[0,b,a]}function
kI(b,a){return a}function
kJ(c,b){return a(bW[1],b)}function
kK(c,d){function
e(f,e){function
g(d){var
e=a(m[6],c),f=a(X[3],e),g=b(X[1][8],f,d);return a(bW[1],g)}var
h=b(d,f,e);return b(bW[2],h,g)}return b(X[7],c,e)}function
kL(a){b(a_[9],a,kH);b(a_[10],a,kI);return kK(a,kJ)}kL(eV);var
ES=a(m[4],eV),kM=g(v[13],v[9],ET,ES),a1=v[1][4][1],cW=a(a1,EU),kN=a(a1,EV),kO=a(a1,EW),dH=a(a1,EX),kP=a(a1,EY),eW=a(a1,EZ),kQ=a(a1,E0),gt=a(a1,E1),kR=a(a1,E2),gu=a(a1,E3),kS=a(a1,E4),gv=a(a1,E5),eX=a(a1,E6),E7=0,E8=0,E_=[0,[0,0,0,[0,[0,E9,function(a,b){return a}],E8]],E7];g(v[1][6],kM,0,E_);var
E$=0,Fa=0;function
Fb(a,b){return a}g(v[1][6],gs,0,[0,[0,0,0,[0,[0,[0,[2,v[15][16]],0],Fb],Fa]],E$]);var
Fc=0,Fd=0,Ff=[0,[0,0,0,[0,[0,[0,[7,[2,kN],Fe,0],0],function(a,b){return a}],Fd]],Fc];g(v[1][6],dF,0,Ff);var
Fg=0,Fh=0,Fl=[0,[0,0,0,[0,[0,[0,Fk,[0,[5,[2,eW],Fj,0],Fi]],function(d,a,c,b){return a}],Fh]],Fg];g(v[1][6],dG,0,Fl);var
Fm=0,Fn=0;function
Fo(c,b){return[0,a(v[29],b),c]}g(v[1][6],cW,0,[0,[0,0,0,[0,[0,[0,[2,v[15][6]],0],Fo],Fn]],Fm]);var
Fp=0,Fq=0,Fr=[0,[0,[0,[2,cW],[0,[6,[2,kO]],[0,[2,gv],0]]],function(c,b,a,d){return[0,[0,a],[0,b],c]}],Fq],Fu=[0,[0,0,0,[0,[0,[0,Ft,[0,[7,[2,eW],Fs,0],[0,[2,gv],0]]],function(b,a,d,c){return[0,0,[1,a],b]}],Fr]],Fp];g(v[1][6],kN,0,Fu);var
Fv=0,Fw=0,FA=[0,[0,[0,Fz,[0,[2,cW],[0,Fy,[0,[2,dH],Fx]]]],function(f,b,e,a,d,c){return[0,[0,a],b]}],Fw],FB=[0,[0,0,0,[0,[0,[0,[2,dH],0],function(a,b){return[0,0,a]}],FA]],Fv];g(v[1][6],kO,0,FB);var
FC=0,FD=0;function
FE(c,b){var
d=[0,[0,a(v[29],b),c],0];return[0,a(v[29],b),d]}var
FF=[0,[0,[0,[2,v[14][19]],0],FE],FD],FH=[0,[0,FG,function(c,b){return[0,a(v[29],b),0]}],FF],FK=[0,[0,[0,FJ,[0,[2,eW],FI]],function(d,a,c,b){return a}],FH];function
FL(e,c,d,b){return[0,a(v[29],b),[1,c]]}var
FO=[0,[0,[0,FN,[0,[2,v[15][3]],FM]],FL],FK];function
FP(c,b){return[0,a(v[29],b),[2,c]]}g(v[1][6],dH,0,[0,[0,0,0,[0,[0,[0,[3,v[15][11],FQ],0],FP],FO]],FC]);var
FR=0,FS=0;function
FT(c,b){return[0,a(v[29],b),c]}g(v[1][6],kP,0,[0,[0,0,0,[0,[0,[0,[2,v[14][19]],0],FT],FS]],FR]);var
FU=0,FV=0,FW=[0,[0,[0,[2,kP],[0,[4,[2,dH]],0]],function(d,c,b){return[0,a(v[29],b),[0,c,d]]}],FV],FX=[0,[0,0,0,[0,[0,[0,[2,dH],0],function(a,b){return a}],FW]],FU];g(v[1][6],eW,0,FX);var
FY=0,FZ=0;function
F0(g,f){function
c(a){return a}var
b=g;for(;;){if(b){var
d=b[2],e=b[1];if(d){var
c=function(c,d){return function(b){return c([3,d,[0,[0,0,[1,[0,[0,a(v[29],f),0],0]],b],0]])}}(c,e),b=d;continue}return function(a){return c([3,e,a])}}throw[0,Q,F1]}}g(v[1][6],kQ,0,[0,[0,0,0,[0,[0,[0,[7,[2,v[15][3]],F2,0],0],F0],FZ]],FY]);var
F3=0,F4=0,F8=[0,[0,[0,F7,[0,F6,[0,[2,cW],F5]]],function(e,a,d,c,b){return[0,a]}],F4],F9=[0,[0,0,0,[0,[0,0,function(a){return 0}],F8]],F3];g(v[1][6],gt,0,F9);var
F_=0,F$=0,Gb=[0,[0,[0,Ga,[0,[2,gt],0]],function(a,c,b){return[0,[0,0,a]]}],F$],Gd=[0,[0,[0,Gc,[0,[2,gt],0]],function(a,c,b){return[0,[0,1,a]]}],Gb],Ge=[0,[0,0,0,[0,[0,0,function(a){return 0}],Gd]],F_];g(v[1][6],kR,0,Ge);var
Gf=0,Gg=0;function
Gh(e,h,d,g,c,b,a,f){return[0,[0,b,a,c,d],e]}g(v[1][6],gu,0,[0,[0,0,0,[0,[0,[0,[2,kR],[0,[2,eR],[0,[2,gs],[0,Gj,[0,[2,v[15][3]],[0,Gi,[0,[2,eX],0]]]]]]],Gh],Gg]],Gf]);var
Gk=0,Gl=0,Gn=[0,[0,[0,Gm,[0,[6,[2,gu]],0]],function(a,c,b){return a}],Gl],Go=[0,[0,0,0,[0,[0,0,function(a){return 0}],Gn]],Gk];g(v[1][6],kS,0,Go);var
Gp=0,Gq=0,Gs=[0,[0,[0,Gr,[0,[2,cW],0]],function(a,c,b){return[1,a]}],Gq];function
Gt(b,a,d,c){return[0,a,b]}var
Gu=[0,[2,v[15][3]],[0,[2,kS],0]],Gv=0,Gx=[0,[0,Gw,function(a,b){return a}],Gv],Gz=[0,[0,Gy,function(a,b){return a}],Gx],GA=[0,[0,[0,a(eY[2],Gz),Gu],Gt],Gs];function
GB(c,f,b,e,d){return a(b,c)}var
GC=[0,[2,eX],0],GD=0,GF=[0,[0,GE,function(a,b){return a}],GD],GH=[0,[0,GG,function(a,b){return a}],GF],GI=[0,[2,kQ],[0,a(eY[2],GH),GC]],GJ=0,GL=[0,[0,GK,function(a,b){return a}],GJ],GN=[0,[0,GM,function(a,b){return a}],GL],GO=[0,[0,[0,a(eY[2],GN),GI],GB],GA];function
GP(b,f,a,e,d,c){return[4,[0,a],b]}var
GT=[0,[0,[0,GS,[0,GR,[0,[2,L[6][18]],[0,GQ,[0,[2,eX],0]]]]],GP],GO];function
GU(d,h,c,b,a,g,f,e){return[2,a,b,c,d]}var
GV=[0,[2,dF],0],GW=0,GY=[0,[0,GX,function(a,b){return a}],GW],G0=[0,[0,GZ,function(a,b){return a}],GY],G1=[0,[8,[2,cW]],[0,a(eY[2],G0),GV]];g(v[1][6],gv,0,[0,[0,0,0,[0,[0,[0,G3,[0,G2,[0,[2,v[15][1]],[0,[8,[2,v[15][1]]],G1]]]],GU],GT]],Gp]);var
G4=0,G5=0,G8=[0,[0,[0,G7,[0,[2,dF],G6]],function(d,a,c,b){return a}],G5],G9=[0,[0,0,0,[0,[0,[0,[2,dF],0],function(a,b){return a}],G8]],G4];g(v[1][6],eX,0,G9);var
G_=0,G$=0,Ha=[0,[0,0,0,[0,[0,[0,[6,[2,gu]],0],function(a,b){return a}],G$]],G_];g(v[1][6],kF,0,Ha);var
Hb=0,Hd=[0,[0,0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(m[4],a$),h=b(m[8],g,f),i=a(m[4],cg),j=b(m[8],i,e);return function(b,a){ko(h,j,0);return a}}}return a(C[2],Hc)}],Hb];function
He(b,a){return g(gp[2],a[1],[0,Hf,b],a[2])}b(F[87],He,Hd);var
Hg=0,Hi=[0,function(b){if(b){var
c=b[2];if(c)if(!c[2])return function(a){return cV[5]}}return a(C[2],Hh)},Hg];function
Hj(c,a){return b(cV[3],[0,Hk,c],a)}b(F[87],Hj,Hi);var
Hl=[6,a(v[12],cg)],Hm=[0,[0,a(m[4],cg)],Hl],Hn=[0,[1,b(J[11],0,Hm)],0],Ho=[6,a(v[12],a$)],Hp=[0,[0,a(m[4],a$)],Ho],Hr=[0,[0,Hq,[0,[1,b(J[11],0,Hp)],Hn]],0];function
Hs(b,a){return g(gq[1],[0,Ht,b],0,a)}b(F[87],Hs,Hr);function
eZ(e,d,c,b){return a(f[7],0)}var
bo=a(m[2],Hu);function
Hv(b,a){return[0,b,a]}b(a_[9],bo,Hv);function
Hw(b,a){return a}b(a_[10],bo,Hw);function
Hx(g,c){var
d=a(m[6],bo),e=a(X[3],d),f=b(X[1][8],e,c);return a(bW[1],f)}b(X[7],bo,Hx);b(X[4],bo,0);b(v[11],bo,dG);n(L[5][1],bo,eZ,eZ,eZ);var
Hy=[0,dG,0];function
Hz(c){var
d=c[2],e=a(m[4],bo);return[0,b(m[7],e,d)]}g(L[10][5],HA,Hz,Hy);var
HB=0;function
HC(c,b,d){return gn([0,b],[0,a(J[3],HD),c])}var
HF=a(i[1][7],HE),HG=[0,[5,a(m[16],bo)],HF],HI=[0,HH,[1,b(J[11],0,HG),0]],HK=a(i[1][7],HJ),HL=[0,[5,a(m[16],B[8])],HK],HO=[0,[0,[0,HN,[0,HM,[1,b(J[11],0,HL),HI]]],HC],HB];function
HP(b,c){return gn(0,[0,a(J[3],HQ),b])}var
HS=a(i[1][7],HR),HT=[0,[5,a(m[16],B[8])],HS],HW=[0,[0,[0,HV,[0,HU,[1,b(J[11],0,HT),0]]],HP],HO];n(L[10][8],aj,HX,0,HW);var
HY=0;function
HZ(e,g){function
d(g){var
h=a(j[66][6],g),d=b(c[3],h,e);if(1===d[0])if(a(q[b2],d[1]))return a(j[16],0);var
i=a(f[3],H0);return b(l[66][4],0,i)}return a(j[66][10],d)}var
H2=a(i[1][7],H1),H3=[0,[5,a(m[16],B[13])],H2],H5=[0,[0,[0,H4,[1,b(J[11],0,H3),0]],HZ],HY];n(L[10][8],aj,H6,0,H5);var
H7=0;function
H8(a,b){return ks(a)}var
H_=a(i[1][7],H9),H$=[0,[5,a(m[16],B[15])],H_],Ib=[0,[0,[0,Ia,[1,b(J[11],0,H$),0]],H8],H7];n(L[10][8],aj,Ic,0,Ib);var
Id=0;function
Ie(a,d){function
c(b){return kg(a,b)}return b(j[70][1],0,c)}var
Ig=a(i[1][7],If),Ih=[0,[5,a(m[16],B[8])],Ig],Ij=[0,[0,[0,Ii,[1,b(J[11],0,Ih),0]],Ie],Id];n(L[10][8],aj,Ik,0,Ij);var
Il=0;function
Im(b,a,c){return hV(b,a)}var
Io=a(i[1][7],In),Ip=[0,[5,a(m[16],B[13])],Io],Iq=[1,b(J[11],0,Ip),0],Is=a(i[1][7],Ir),It=[0,[5,a(m[16],B[8])],Is],Iv=[0,[0,[0,Iu,[1,b(J[11],0,It),Iq]],Im],Il];n(L[10][8],aj,Iw,0,Iv);var
Ix=0,Iz=[0,[0,0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
f=d[1],g=c[1],h=a(m[18],B[8]),j=a(m[4],h),k=b(m[8],j,g),l=a(m[18],B[24]),n=a(m[4],l),o=b(m[8],n,f);return function(c,a){function
d(a){var
c=b(cp[3],0,a);return[0,a[2],c]}var
f=b(e[17][15],d,o),g=b(e[17][15],i[1][8],k);ia(c[3],g,f);return a}}}return a(C[2],Iy)}],Ix];function
IA(b,a){return g(gp[2],a[1],[0,IB,b],a[2])}b(F[87],IA,Iz);var
IC=0,IE=[0,function(b){if(b){var
c=b[2];if(c)if(!c[2])return function(a){return cV[5]}}return a(C[2],ID)},IC];function
IF(c,a){return b(cV[3],[0,IG,c],a)}b(F[87],IF,IE);var
IH=[3,[6,a(v[12],B[24])]],II=a(m[18],B[24]),IJ=[0,[0,a(m[4],II)],IH],IL=[0,IK,[0,[1,b(J[11],0,IJ)],0]],IM=[1,[6,a(v[12],B[8])]],IN=a(m[18],B[8]),IO=[0,[0,a(m[4],IN)],IM],IQ=[0,[0,IP,[0,[1,b(J[11],0,IO)],IL]],0];function
IR(b,a){return g(gq[1],[0,IS,b],0,a)}b(F[87],IR,IQ);var
e0=a(m[3],IT),IU=b(X[4],e0,0);function
kT(c,b,a){return dz}function
kU(c,b,a){return dz}function
kV(c,b,a){return dz}var
IV=a(m[4],e0),cX=g(v[13],v[9],IW,IV);n(L[5][1],e0,kT,kU,kV);var
e1=v[1][4][1],kW=a(e1,IX),kX=a(e1,IY),kY=a(e1,IZ),kZ=a(e1,I0),I1=0,I2=0,I3=[0,[0,0,0,[0,[0,[0,[6,[2,kW]],0],function(a,b){return a}],I2]],I1];g(v[1][6],cX,0,I3);var
I4=0,I5=0,I6=[0,[0,0,0,[0,[0,[0,[2,kX],0],function(c,b){return[0,[0,a(v[29],b)],c]}],I5]],I4];g(v[1][6],kW,0,I6);var
I7=0,I8=0,I9=[0,[0,[0,[2,kY],0],function(a,b){return[0,a]}],I8],I$=[0,[0,I_,function(b,a){return 0}],I9],Jb=[0,[0,Ja,function(b,a){return 1}],I$],Jd=[0,[0,0,0,[0,[0,Jc,function(b,a){return 2}],Jb]],I7];g(v[1][6],kX,0,Jd);var
Je=0,Jf=0,Ji=[0,[0,Jh,function(b,a){return Jg}],Jf],Jl=[0,[0,Jk,function(b,a){return Jj}],Ji],Jo=[0,[0,Jn,function(b,a){return Jm}],Jl],Js=[0,[0,[0,Jr,[0,Jq,[0,[2,cX],Jp]]],function(e,a,d,c,b){return[2,a]}],Jo],Jt=[0,[0,0,0,[0,[0,[0,[2,kZ],0],function(a,b){return[1,a]}],Js]],Je];g(v[1][6],kY,0,Jt);var
Ju=0,Jv=0,Jx=[0,[0,Jw,function(b,a){return 0}],Jv],Jz=[0,[0,0,0,[0,[0,Jy,function(b,a){return 1}],Jx]],Ju];g(v[1][6],kZ,0,Jz);function
e2(c,b,a){return dz}var
bp=a(m[2],JA);function
JB(b,a){return[0,b,a]}b(a_[9],bp,JB);function
JC(b,a){return a}b(a_[10],bp,JC);function
JD(g,c){var
d=a(m[6],bp),e=a(X[3],d),f=b(X[1][8],e,c);return a(bW[1],f)}b(X[7],bp,JD);b(X[4],bp,0);b(v[11],bp,cX);n(L[5][1],bp,e2,e2,e2);var
JE=[0,cX,0];function
JF(c){var
d=c[2],e=a(m[4],bp);return[0,b(m[7],e,d)]}g(L[10][5],JG,JF,JE);var
JH=0,JJ=[0,[0,JI,function(a){return f6(0)}],JH];function
JK(a,b){return f6(a)}var
JM=a(i[1][7],JL),JN=[0,[5,a(m[16],bp)],JM],JP=[0,[0,[0,JO,[1,b(J[11],0,JN),0]],JK],JJ];n(L[10][8],aj,JQ,0,JP);var
JR=0;function
JS(b,a,c){return gd(b,a)}var
JU=a(i[1][7],JT),JV=[0,[2,[5,a(m[16],B[3])]],JU],JW=[1,b(J[11],0,JV),0],JY=a(i[1][7],JX),JZ=[0,[2,[5,a(m[16],eV)]],JY],J1=[0,[0,[0,J0,[1,b(J[11],0,JZ),JW]],JS],JR];n(L[10][8],aj,J2,0,J1);aG(1097,[0,aj,eO,bG,eP,a$,gr,eQ,bH,eR,Ex,Ey,kt,ku,kv,eS,gs,EC,eT,EE,kw,kx,ky,dF,eU,EI,kz,kA,kB,dG,cg,EM,kC,kD,kE,kF,EP,kG,eV,kH,kI,kJ,kK,kL,kM,eZ,bo,dG,e0,IU,kT,kU,kV,cX,e2,bp,cX],"G_equations");a(aX[10],J3);a(aX[10],J4);a(aX[10],J5);a(aX[10],J6);a(aX[10],J7);a(aX[10],J8);a(aX[10],J9);a(aX[10],J_);a(aX[10],J$);a(aX[10],Ka);a(aX[10],Kb);a(aX[10],Kc);a(aX[10],Kd);aG(1098,[0],"Equations_plugin_mod");return}
