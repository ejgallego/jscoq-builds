function(T3){"use strict";var
c0=";",$="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2stdlib.ml",aE=",",E="(",mE="pattern:(",m0="Init",c3="pattern",fc="list",nb="!",mn="This expression has type",_="|",b2="&",hT="[]",hS="refine",na=163,hA="..",mm="reference:(",mZ="hyp",e6=129,bK="with",a0="]",mY="destruction_arg",b5="=>",m_="Constructor name should start with an uppercase letter ",m$="Type ",dJ="6",hz="exn",mD="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2interp.ml",cn="0",m9=128,ml="ltac",hR="int",mC="Cannot set both constants to unfold and constants not to unfold",mX=" arguments",dI=248,mW="Invalid parsing token",mk=203,mV="Multiple definition of the type name ",mU="Tactic definition must be a syntactical value",mT="x",m8="thunk",mj="fun",m7="terminal",e5="->",hE=246,mS="next",mi="bindings",b1="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2print.ml",b4="constr",m6="of",mh=121,hM="'",a$="[",mg="None",al="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2intern.ml",c1="ident",mf="Unknown tactic ",mR="is not an empty type",c2="1",e4=103,dH=120,mQ="<-",e3="unit",mB="-",mA="rec",mz="list0",cm="::",mP="keyword",m5="lident",my="case",mO=".(",hL="open_constr",me="Some",mx="self",mN="ltac1val",md="MatchPattern",hy="end",hD="Type",co="*",dL="}",hC="in",m4="Multiple definitions of the constructor ",hQ="@",mM=107,fb="match",ah=143,ba="Ltac2",hP="Unexpected value shape",mL=109,fe=142,cq="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2core.ml",bL="5",e$="{",e_="",aa="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2ffi.ml",mc="Syntax error",ma="with_bindings",mb=" arguments, but is applied to ",mv="Constructor already defined in this module ",mw=" expects ",m3="Unbound value ",l$="move_location",hx=", ",mu="opt",dK=135,m2="MatchContext",fd="at",hK="F",m1=".",b6="$",mK="induction_clause",hB="Field ",mt="array",mJ="clause",l_="...",ms="pose",hI="?",hJ="false",l9="hintdb",l8="constr:(",mI=106,hH="string",mq="dispatch",mr=" is not a projection",mH="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2entries.ml",l7="intropatterns",s=")",mp="let",l6="t",T=":",cl="|-",e9="reference",hO="Pattern not handled yet",cp="ltac2",mG="conversion",b3="_",mF=" cannot be recursive",dG="()",l5=134,ag=":=",hN="ltac1",fa="as",hG="true",e8="_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2extffi.ml",l4=" |- ",hw="preterm",mo="list1",e7=250,hF="tactic",K=T3.jsoo_runtime,y=K.caml_check_bound,dF=K.caml_fresh_oo_id,cZ=K.caml_make_vect,hv=K.caml_ml_bytes_length,b=K.caml_new_string,hu=K.caml_obj_tag,aD=K.caml_register_global,e1=K.caml_string_notequal,v=K.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):K.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):K.caml_call_gen(a,[b,c])}function
f(a,b,c,d){return a.length==3?a(b,c,d):K.caml_call_gen(a,[b,c,d])}function
p(a,b,c,d,e){return a.length==4?a(b,c,d,e):K.caml_call_gen(a,[b,c,d,e])}function
a_(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):K.caml_call_gen(a,[b,c,d,e,f])}function
e2(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):K.caml_call_gen(a,[b,c,d,e,f,g])}function
T1(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):K.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
T2(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):K.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}var
k=K.caml_get_global_data(),jE=[0,1,1,1,1,0,0],jZ=[0,b(cp),b("ltac2_eval")],hU=k.Dyn,m=k.Assert_failure,j=k.Proofview,e=k.Util,h=k.Names,bb=k.Exninfo,dV=k.Stdlib__char,d=k.Pp,l=k.CErrors,r=k.Libnames,D=k.Not_found,h$=k.Summary,bO=k.Nametab,ao=k.Genarg,bh=k.Geninterp,bR=k.Printer,ce=k.Stdlib__bytes,bi=k.Global,cy=k.Int,bP=k.Stdlib,i=k.CAst,W=k.Genintern,cF=k.Environ,di=k.Ltac_plugin__Tacintern,w=k.Option,aI=k.Mod_subst,f0=k.Namegen,iv=k.CWarnings,iL=k.CArray,aJ=k.Evd,el=k.CLexer,av=k.Lib,ja=k.Goptions,ep=k.Hook,i_=k.Goal_select,jc=k.Pfedit,eo=k.Proof,i9=k.Proof_global,cJ=k.Feedback,gf=k.Loc,g=k.Pcoq,aK=k.Libobject,x=k.EConstr,jk=k.Stdlib__list,cg=k.Context,gh=k.IStream,gi=k.CamlinternalLazy,a6=k.Constr_matching,gq=k.Constrexpr_ops,j0=k.Ftactic,az=k.Ltac_plugin__Tacinterp,gB=k.Typing,jV=k.Ltac_plugin__Pptactic,eI=k.Ltac_plugin__Tacarg,gD=k.Detyping,jU=k.Constrintern,jP=k.Pretyping,jS=k.Stdarg,eH=k.Ltac_plugin__Tacenv,bC=k.Tacticals,jM=k.Tacmach,gC=k.Evar,jO=k.Evarutil,jN=k.Stdlib__sys,jG=k.Univ,jW=k.GlobEnv,jX=k.Genprint,kS=k.Contradiction,gO=k.Inv,t=k.Tactics,kP=k.Class_tactics,gN=k.Eauto,eL=k.Auto,kJ=k.Autorewrite,dz=k.Equality,bJ=k.Vernacextend,dE=k.Attributes,k8=k.Stdlib__stream,rw=k.Flags,uX=k.Vernacentries,tK=k.Stdlib__printf,vg=k.Mltop,vm=k.CList,vh=k.Reductionops,zV=k.Globnames,zK=k.Patternops,zt=k.Logic_monad,zr=k.Glob_ops,yV=k.Abstract,yQ=k.Refine,yr=k.Termops,yb=k.Inductiveops,As=k.Ltac_plugin__Tacentries,BM=k.Unification,BK=k.Redexpr,Ct=k.Locusops,E0=k.G_prim,Fg=k.Ltac_plugin__Pltac,TK=k.Pvernac,TO=k.Ltac_plugin__G_ltac;aD(1193,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"Ltac2_plugin");var
dM=c(hU[1],[0]),nc=dM[3],nd=dM[4],ne=dM[1],ad=c(hU[1],[0]),nS=[0,0],nT=[0,b(aa),189,7],og=[0,b(aa),405,11],of=[0,b(aa),364,7],oc=[0,b(aa),341,7],oa=[0,b(aa),330,7],n_=[0,b(aa),319,7],n9=[0,b(aa),307,7],n8=[0,b(aa),297,7],n7=[0,b(aa),287,7],n6=[0,b(aa),282,7],n5=[0,b(aa),267,7],n4=[0,0],n3=[0,b(aa),252,7],nW=[0,b(aa),214,7],nU=[0,b(aa),201,85],nR=[0,b(aa),174,7],nQ=[0,b(aa),na,7],nP=[0,b(aa),152,7],nN=[0,0],nO=[0,1],nM=[0,b(aa),139,7],nL=[0,b(aa),m9,7],nK=[0,0],nG=[0,b(aa),mL,10],nn=b(hP),nl=b(hP),nj=b(hP),nq=b(hz),nr=b(b4),ns=b(c1),nt=b(c3),nu=b(hw),nv=b("pp"),nw=b("sort"),nx=b("cast"),ny=b("inductive"),nz=b("constant"),nA=b("constructor"),nB=b("projection"),nC=b(my),nD=b("universe"),nE=b("free"),nF=b(hN),nH=b("Ltac2_plugin.Tac2ffi.LtacError"),nX=[0,b(m0),[0,b(ba),0]],n0=b("Internal"),oE=[0,b("_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2env.ml"),299,2],oC=b(hJ),oD=b(hG),os=b("Unknown object type "),oh=b("ltac2-state"),on=b("ltac2-nametab"),ot=[0,b(m0),[0,b(ba),0]],ov=[0,b("Std"),[0,b(ba),0]],ox=[0,b("Ltac1"),[0,b(ba),0]],oz=b("ltac2:value"),oA=b("ltac2:in-constr"),oB=b("ltac2:quotation"),pB=b("<poly>"),pC=b("<fun>"),pG=b(dG),pD=b(s),pE=b(E),pF=b("<unknown>"),pH=b(s),pI=b(E),pJ=[0,b(b1),385,6],pK=b("<abstr>"),pL=b(s),pM=b(E),pN=b(ag),pO=b(dL),pP=b(e$),pQ=b(a0),pR=b(a$),qc=b("|]"),qd=b("[|"),qb=[0,b(b1),487,9],p7=b(s),p8=b("err:("),p4=b(s),p5=b("message:("),p0=b(l_),p1=b(s),p2=b(mE),pW=b(l_),pX=b(s),pY=b(l8),pU=b(hQ),pj=b(fa),pk=b(b5),pl=b(_),pc=b(b5),pd=b(_),o7=b(aE),o6=b(aE),o2=b(bK),o1=b(ag),oY=b(b5),oZ=b(mj),o0=b(mA),o3=b(hC),o4=b(mp),o5=b(dG),o8=b(b5),o9=b(_),pb=[0,b(b1),mk,52],o_=b(hy),o$=b(bK),pa=b(fb),pe=[0,b(b1),e7,50],pf=b(m1),pg=[0,b(b1),260,50],ph=b(ag),pi=b(m1),pm=b(b3),pn=b(hy),po=b(bK),pp=b(fb),pq=b("@external"),pv=b(ag),pr=b(cm),ps=b(a0),pt=b(a$),pw=b(dL),px=b(e$),pu=[0,b(b1),327,31],oX=[0,b(b1),137,9],oW=[0,b(b1),dH,10],oV=b(b3),oU=b(e_),oS=b(hx),oQ=b(" * "),oO=b(hM),oP=b(e5),oR=b(s),oT=b(E),oF=b(s),oG=b(E),oH=b(fc),oK=b(e3),pS=b(hR),pT=b(hH),pV=b(c1),pZ=b(b4),p3=b(c3),p6=b("message"),p9=b("err"),p_=b(mt),qJ=b("Unbound type constructor "),qI=[0,b(al),241,27],qE=b("argument(s)"),qF=b(" argument(s), but is here applied to "),qG=b(mw),qH=b("The type constructor "),rf=b(hO),rs=b(" is bound several times in this matching"),rt=b("Variable "),rq=[2,[1,[0,0]],0],rr=[0,0],rm=b("Missing hardwired primitive "),rn=b("Missing hardwired alias "),rp=[0,b(al),694,43],ro=[0,0,0],ru=b("Field is not mutable"),rv=[2,[0,0],0],rx=[0,b(al),816,44],rz=b("This kind of expression is not allowed as right-hand side of a recursive binding"),ry=b("This kind of pattern is forbidden in let-rec bindings"),rA=[0,0,0],rK=b("TODO: Unhandled match case"),rL=b("Missing default case"),rE=[0,0,0],rF=[0,b(al),953,21],rG=b(hO),rH=[0,b(al),984,51],rI=b("Unhandled match case for constructor "),rB=b(hO),rC=[0,b(al),879,11],rD=[0,1,0,[0,0,0]],rJ=[0,b(al),897,56],rM=[0,b(al),1070,2],rO=b(" is defined several times"),rP=b(hB),rQ=b(" does not pertain to record definition "),rR=b(hB),rU=b("Cannot infer the corresponding record type"),rN=[0,b(al),1093,9],rS=b(" is undefined"),rT=b(hB),rZ=b("Cannot globalize generic arguments of type"),r0=[0,b(al),1505,15],r7=b(m3),r3=[0,b(al),1545,15],rY=b(mr),rW=[0,0],rX=[0,0,0],rV=[0,b(al),1129,15],rg=b("p"),rb=b("tuple of size "),rc=b("type "),rd=b(", found a pattern for "),re=b("Invalid pattern, expected a pattern for "),q$=b(mr),q_=b("Unbound constructor "),q9=b(m3),q4=b("Cannot infer an empty type for this expression"),q7=b(mR),q8=b(hD),q5=b(mR),q6=b(hD),q1=b("The following clause is redundant."),qX=b("The following expression should have type unit."),qV=[0,b(al),395,17],qR=b("and is applied to too many arguments"),qS=b("This function has type"),qT=b("and is not a function"),qU=b(mn),qO=b("but an expression was expected of type"),qP=b(mn),qK=[0,b(al),283,9],qB=b(mX),qC=b(mb),qD=b("Type expects "),qx=b(mX),qy=b(mb),qz=b(mw),qA=b("Constructor "),qw=b("Unbound type parameter "),qv=b(e_),qt=[0,b(al),113,2],qr=[0,b(al),mI,2],qq=[0,b(al),mM,2],qo=[0,1,0],qn=[1,0],qg=b(hR),qi=b(hH),qk=b(b4),ql=b(l6),qm=b(hw),qL=b("Ltac2_plugin.Tac2intern.Occur"),qM=b("Ltac2_plugin.Tac2intern.CannotUnify"),qY=b(ml),qZ=b("not-unit"),q2=b(ml),q3=b("redundant-clause"),sd=b("Ill-formed recursive function"),sf=[0,b(mD),195,22],se=b("Term is not a syntactical value"),sj=[0,b(mD),221,10],sb=b("Unbound reference"),r$=b("Unbound variable "),sg=b("ltac2:env"),sh=b("@@ltac2_env@@"),va=b(e3),vb=[0,[0,0],0],vc=[0,0],vd=b(cm),ve=[0,b(hT),0],vf=b(fc),u6=b(ba),u7=[0,b("Default"),[0,b("Proof"),[0,b("Mode"),0]]],uY=[0,0,0],uQ=b("Unknown constructor "),uR=b(T),uS=b("Constructor"),uT=b(mf),uU=b(ag),uV=b(T),uW=b("Alias to ..."),uP=b("Backtrace:"),uM=b(hz),uN=b("Uncaught Ltac2 exception:"),uD=b("Call "),uE=b(dL),uF=b("Call {"),uG=b(">"),uH=b(T),uI=b("Prim <"),uJ=b(T),uK=b("Extn "),ux=b(cp),uu=[0,b(mH),805,40],uv=b("="),uw=b("- : "),un=b(mf),ut=b("Cannot redefine syntactic abbreviations"),uo=b(" is not declared as mutable"),up=b("The tactic "),uq=b(mU),ur=b(" is not a subtype of "),us=b(m$),ue=[0,5],t$=[0,1],tZ=b(mW),tW=[2,[1,[0,0]]],tY=b("Unknown scope"),tX=b(mW),tU=b("Types can only be extended one by one"),tV=b("Extensions cannot be recursive"),tR=b(m_),tQ=[0,b(mH),521,13],tP=b(mv),tM=b("Unbound type "),tS=b(" is not an open type"),tT=b(m$),tO=b(m4),tN=b("Extensions only accept inductive constructors"),tJ=[0,[12,dH,[4,3,0,0,0]],b("x%i")],tH=b("External tactic must have at least one argument"),tI=b("Unregistered primitive "),tF=b(mv),tE=b(m_),tx=b(" occurs several times"),ty=b("The type parameter "),tz=b(mF),tA=b("The open type declaration "),tB=b(mF),tC=b("The type abbreviation "),tD=b(m4),tG=b("Multiple definitions of the projection "),tw=b(mV),tv=b(mV),tu=b("Identifier expected"),tr=b(mU),ts=b(" already exists"),tt=b("Tactic "),tq=b("Tactic definition must have a name"),to=b(" must be lowercase"),tp=b("The identifier "),tn=b(mT),tm=b("Recursive tactic definitions must be functions"),te=[0,1],s$=[0,1],s4=[0,1],sk=b("tactic:tac2expr"),sm=b("tactic:tac2expr_in_env"),so=b("tactic:q_ident"),sq=b("tactic:q_bindings"),ss=b("tactic:q_with_bindings"),su=b("tactic:q_intropattern"),sw=b("tactic:q_intropatterns"),sy=b("tactic:q_destruction_arg"),sA=b("tactic:q_induction_clause"),sC=b("tactic:q_conversion"),sE=b("tactic:q_rewriting"),sG=b("tactic:q_clause"),sI=b("tactic:q_dispatch"),sK=b("tactic:q_occurrences"),sM=b("tactic:q_reference"),sO=b("tactic:q_strategy_flag"),sQ=b("tactic:q_constr_matching"),sS=b("tactic:q_goal_matching"),sU=b("tactic:q_hintdb"),sW=b("tactic:q_move_location"),sY=b("tactic:q_pose"),s0=b("tactic:q_assert"),s7=b("TAC2-DEFINITION"),tc=b("TAC2-TYPE-DEFINITION"),tj=b("TAC2-TYPE-EXTENSION"),t1=b("ltac2-notation"),t6=b("TAC2-NOTATION"),uc=b("TAC2-ABBREVIATION"),uh=b("TAC2-REDEFINITION"),uz=[0,b(ba),[0,b("Backtrace"),0]],uA=b("print Ltac2 backtrace"),u1=[0,0,[0,0,[0,[0,[2,[0,0],0]]]]],u2=b(fc),u8=b("TAC2-INIT"),u_=b("ltac2_plugin"),vi=b("Ltac2_plugin.Tac2match.Not_coherent_metas"),vj=b("No matching clauses for match."),vl=[0,b("tactic matching")],vL=b(cm),vM=b(hT),vT=b("IntroForthcoming"),vU=b("IntroNaming"),vV=b("IntroAction"),vW=b("IntroAnonymous"),vX=b("IntroIdentifier"),vY=b("IntroFresh"),vZ=b("IntroWildcard"),v0=b("IntroOrAndPattern"),v1=b("IntroInjection"),v2=b("IntroRewrite"),v3=b("IntroOrPattern"),v4=b("IntroAndPattern"),wT=b("AssertType"),wU=b("AssertValue"),wP=b("MoveFirst"),wQ=b("MoveLast"),wR=b("MoveAfter"),wS=b("MoveBefore"),wN=b(md),wO=b(m2),wL=b(md),wM=b(m2),wE=b("rConst"),wF=b("rDelta"),wG=b("rZeta"),wH=b("rCofix"),wI=b("rFix"),wJ=b("rMatch"),wK=b("rBeta"),wB=b(mC),wC=b(mC),wD=[0,0,0,0,0,0,0,0],wA=[2,[1,[0,0]]],wz=b(hS),wx=b(hS),ww=b(mZ),wt=b("rew_equatn"),wu=b("rew_repeat"),wv=b("rew_orient"),wr=b("LTR"),ws=b("RTL"),wn=b("RepeatStar"),wo=b("RepeatPlus"),wp=b("Precisely"),wq=b("UpTo"),wm=[0,0],wk=b("get"),wl=[0,0,0],wj=b("Invalid pattern binding name "),wf=b("indcl_in"),wg=b("indcl_as"),wh=b("indcl_eqn"),wi=b("indcl_arg"),wc=b("ElimOnConstr"),wd=b("ElimOnIdent"),we=b("ElimOnAnonHyp"),wa=b("on_concl"),wb=b("on_hyps"),v8=b("AllOccurrences"),v9=b("NoOccurrences"),v_=b("AllOccurrencesBut"),v$=b("OnlyOccurrences"),v5=b("InHyp"),v6=b("InHypTypeOnly"),v7=b("InHypValueOnly"),vQ=b("NoBindings"),vR=b("ImplicitBindings"),vS=b("ExplicitBindings"),vO=b("AnonHyp"),vP=b("NamedHyp"),vJ=b(hG),vK=b(hJ),vI=b("Invalid identifier"),vG=b(me),vH=b(mg),vF=[2,[1,[0,0]]],vE=[2,[1,[0,2]]],vC=b(e3),vD=[0,0],vv=[0,b(ba),0],vn=b(c3),vo=b(e9),vp=b(c1),vq=b(b4),vr=b(hL),vs=b(hw),vt=b(hN),vu=b(mN),vw=b("Control"),vy=b("Pattern"),vA=b("Array"),AH=b(hx),AJ=b(b3),AG=b(s),AI=b(E),By=[0,0,0],Bz=b("Recursive symbols (self / next) are not allowed in local rules"),A_=b(b4),A8=b(m8),A5=b(hF),A6=b(bL),A4=b(hF),A2=b(mS),A0=b(mx),AY=b(mu),AW=b(mo),AU=b(mz),AS=b(m7),AQ=b(mP),AL=b(hx),AK=b(s),AM=b(E),AN=b(" in scope "),AO=b("Invalid arguments "),AE=b(l4),AA=b(hK),AB=[22,0],Au=[0,b(cq),1352,29],Av=[0,b(cq),1347,14],Aw=[0,b(cq),1341,10],Ao=b(" "),Al=b("s "),Am=b(", probably an ill-typed expression"),An=b("Missing notation term for variable"),Ai=b(b6),Ae=[0,b(cq),1275,13],Ad=b(cp),Ab=b(l4),z$=b(s),Aa=b("ltac1val:("),z7=b(cl),z5=b(s),z6=b("ltac1:("),z0=b(s),z1=b(mm),zX=b(s),zY=b(b2),zZ=b(mm),zR=b(s),zS=b("preterm:("),zM=b(s),zN=b(mE),zI=[0,0],zE=b(s),zF=b("ident:("),zA=b(s),zB=b("open_constr:("),zw=b(s),zx=b(l8),zi=b(mT),zj=b(hK),zk=b(hK),yM=b(" not found"),yN=b("Hypothesis "),yf=b("Variable already exists"),x5=[0,b(cq),538,9],xA=b(cp),xv=[0,b(cq),111,9],xu=[0,b(cq),100,9],xt=[0,1],xs=[0,0],wV=[0,0,1,0,1,0,0],wW=b(hR),wY=b(hH),w0=b(mt),w2=b(e3),w4=b(fc),w6=b(b4),w8=b(c3),w_=b(c1),xa=b("option"),xc=b(hz),xe=b(e9),xf=b(l6),xg=b(hT),xi=b(cm),xk=b(mg),xm=b(me),xo=b(hG),xq=b(hJ),xw=b("Not_focussed"),xx=b("Out_of_bounds"),xy=b("Not_found"),xz=b("Match_failure"),xC=b("print"),xD=b("message_of_int"),xE=b("message_of_string"),xF=b("message_of_constr"),xG=b("message_of_ident"),xH=b("message_of_exn"),xI=b("message_concat"),xJ=b("array_make"),xK=b("array_length"),xL=b("array_set"),xM=b("array_get"),xN=b("ident_equal"),xO=b("ident_to_string"),xP=b("ident_of_string"),xQ=b("int_equal"),xR=b("int_compare"),xS=b("int_add"),xT=b("int_sub"),xU=b("int_mul"),xV=b("int_neg"),xW=b("char_of_int"),xX=b("char_to_int"),xY=b("string_make"),xZ=b("string_length"),x0=b("string_set"),x1=b("string_get"),x2=b("constr_type"),x3=b("constr_equal"),x4=b("constr_kind"),x6=b("constr_make"),x7=b("constr_check"),x9=b("constr_substnl"),x$=b("constr_closenl"),yc=b("constr_case"),ye=b("constr_constructor"),yg=b("constr_in_context"),yi=b("constr_pretype"),yj=b("pattern_empty_context"),yk=b("pattern_matches"),yl=b("pattern_matches_subterm"),ym=b("pattern_matches_vect"),yn=b("pattern_matches_subterm_vect"),yq=b("pattern_matches_goal"),ys=b("pattern_instantiate"),yt=b("throw"),yu=b("zero"),yv=b("plus"),yw=b("once"),yy=b(mq),yB=b("extend"),yC=b("enter"),yD=b(my),yE=b("focus"),yG=b("shelve"),yI=b("shelve_unifiable"),yJ=b("new_goal"),yL=b("goal"),yO=b(mZ),yP=b("hyps"),yR=b(hS),yS=b("with_holes"),yT=b("progress"),yW=b("abstract"),yY=b("time"),y0=b("check_interrupt"),y3=b("fresh_free_union"),y5=b("fresh_free_of_ids"),y6=b("fresh_free_of_constr"),y8=b("fresh_fresh"),y_=b("env_get"),za=b("env_expand"),zb=b("env_path"),zc=b("env_instantiate"),zf=b("ltac1_ref"),zg=b("ltac1_run"),zl=b("ltac1_apply"),zm=b("ltac1_of_constr"),zn=b("ltac1_to_constr"),zp=b("ltac1_of_list"),zq=b("ltac1_to_list"),Ar=b(cp),At=b("ltac2:ltac2_eval"),AP=[2,[1,[0,0]]],AR=b(mP),AT=b(m7),AV=b(mz),AX=b(mo),AZ=b(mu),A1=b(mx),A3=b(mS),A7=b(hF),A9=b(m8),A$=b(b4),Bb=b(c1),Bc=b(mi),Bd=b(ma),Be=b("intropattern"),Bf=b(l7),Bg=b(mY),Bh=b(mK),Bi=b(mG),Bj=b("rewriting"),Bk=b(mJ),Bl=b(l9),Bm=b("occurrences"),Bn=b(mq),Bo=b("strategy"),Bp=b(e9),Bq=b(l$),Br=b(ms),Bs=b("assert"),Bt=b("constr_matching"),Bu=b("goal_matching"),Bv=b(hL),Bw=b(c3),Bx=b("Ltac2_plugin.Tac2core.SelfSymbol"),BA=b("seq"),BE=[0,b(e8),40,7],BD=[0,b(e8),34,7],BC=[0,b(e8),24,7],BB=[0,b(e8),17,49],BO=[1,0],BP=[0,b("_vendor+v8.11+32bit/coq/user-contrib/Ltac2/tac2tactics.ml"),432,14],BQ=b("Inversion only accept disjunctive patterns"),BN=[0,2],BL=[0,0],BI=b("to an evaluable reference."),BJ=b("Cannot coerce"),BF=b("ltac2_delayed"),BY=[0,b($),89,7],BZ=[0,b($),95,7],B0=[0,b($),mM,7],B1=[0,b($),114,7],CI=[0,0],Cy=[1,0],Cz=b("Invalid pattern for remember"),Cf=b(cp),Cd=[0,b($),210,7],Cc=[0,b($),mk,7],Ca=[0,b($),194,7],B_=[0,b($),186,7],B9=[0,b($),179,7],B7=[0,b($),171,7],B6=[0,b($),na,7],B4=[0,b($),154,7],B3=[0,b($),141,2],B2=[0,b($),e6,7],BX=[0,b($),75,7],BV=[0,b($),56,9],BW=[0,b($),60,7],BU=[0,b($),49,7],BT=[0,b($),41,7],BR=[0,b($),22,49],Cg=b("tac_intros"),Cj=b("tac_apply"),Cl=b("tac_elim"),Cm=b("tac_case"),Co=b("tac_generalize"),Cp=b("tac_assert"),Cs=b("tac_enough"),Cu=b("tac_pose"),Cw=b("tac_set"),CB=b("tac_remember"),CE=b("tac_destruct"),CH=b("tac_induction"),CJ=b("tac_red"),CK=b("tac_hnf"),CM=b("tac_simpl"),CN=b("tac_cbv"),CO=b("tac_cbn"),CP=b("tac_lazy"),CR=b("tac_unfold"),CT=b("tac_fold"),CV=b("tac_pattern"),CX=b("tac_vm"),CZ=b("tac_native"),C1=b("eval_red"),C2=b("eval_hnf"),C4=b("eval_simpl"),C5=b("eval_cbv"),C6=b("eval_cbn"),C7=b("eval_lazy"),C9=b("eval_unfold"),C$=b("eval_fold"),Db=b("eval_pattern"),Dd=b("eval_vm"),Df=b("eval_native"),Di=b("tac_change"),Dl=b("tac_rewrite"),Do=b("tac_inversion"),Dp=b("tac_reflexivity"),Dq=b("tac_move"),Dt=b("tac_intro"),Du=b("tac_assumption"),Dv=b("tac_transitivity"),Dw=b("tac_etransitivity"),Dx=b("tac_cut"),Dy=b("tac_left"),Dz=b("tac_right"),DA=b("tac_introsuntil"),DB=b("tac_exactnocheck"),DC=b("tac_vmcastnocheck"),DD=b("tac_nativecastnocheck"),DE=b("tac_constructor"),DF=b("tac_constructorn"),DH=b("tac_specialize"),DI=b("tac_symmetry"),DJ=b("tac_split"),DL=b("tac_rename"),DN=b("tac_revert"),DO=b("tac_admit"),DP=b("tac_fix"),DQ=b("tac_cofix"),DS=b("tac_clear"),DU=b("tac_keep"),DW=b("tac_clearbody"),DY=b("tac_discriminate"),D1=b("tac_injection"),D2=b("tac_absurd"),D4=b("tac_contradiction"),D7=b("tac_autorewrite"),D9=b("tac_subst"),Ea=b("tac_substall"),Ed=b("tac_trivial"),Ei=b("tac_eauto"),Em=b("tac_auto"),Eq=b("tac_newauto"),Eu=b("tac_typeclasses_eauto"),Tx=[1,[0,0,0]],Tj=[0,[2,b(cn)]],Qv=[0,[0,0,0],0],PF=[0,0],PC=[0,1],Pa=[0,0],NP=[0,0],NM=[0,1],Nu=[2,0],Nr=[2,1],KL=[0,0,[0,0]],IT=[0,0],IK=b("Invalid pattern"),IA=b("Invalid bound Ltac2 identifier "),Gv=[2,[1,[0,0]]],Gs=[2,[1,[0,0]]],Gd=[1,[1,[0,0]],0],F2=b(mc),FR=[1,[1,[0,0]],0],FO=[0,0],Fh=b(mc),Ev=b(e_),Ew=b(e_),Ex=b(b6),EA=b(ag),ED=b(E),EG=b("test_lpar_idnum_coloneq"),EH=b(T),EJ=b(E),EM=b("test_lpar_id_colon"),EN=b(ag),EP=b(E),ES=b("test_lpar_id_coloneq"),ET=b(s),EV=b(E),EY=b("test_lpar_id_rpar"),E1=b(b2),E4=b("test_ampersand_ident"),E5=b(b6),E7=b("test_dollar_ident"),E8=b(cl),E_=b("test_ltac1_env"),E$=b("tactic:tac2type"),Fa=b("tactic:tac2def_val"),Fb=b("tactic:tac2def_typ"),Fc=b("tactic:tac2def_ext"),Fd=b("tactic:tac2def_syn"),Fe=b("tactic:tac2def_mut"),Ff=b("vernac:ltac2_command"),Fi=b("tac2pat"),Fj=b("atomic_tac2pat"),Fk=b("branches"),Fl=b("branch"),Fm=b("rec_flag"),Fn=b("mut_flag"),Fo=b("typ_param"),Fp=b("tactic_atom"),Fq=b("ltac1_expr_in_env"),Fr=b("let_clause"),Fs=b("let_binder"),Ft=b("locident"),Fu=b("binder"),Fv=b("input_fun"),Fw=b("tac2def_body"),Fx=b("tac2typ_knd"),Fy=b("tac2alg_constructors"),Fz=b("tac2alg_constructor"),FA=b("tac2rec_fields"),FB=b("tac2rec_field"),FC=b("tac2rec_fieldexprs"),FD=b("tac2rec_fieldexpr"),FE=b("tac2typ_prm"),FF=b("tac2typ_def"),FG=b("tac2type_body"),FH=b("syn_node"),FI=b("sexpr"),FJ=b("syn_level"),FK=b(m5),FL=b("globref"),FP=[0,0,[0,[0,b(b3)]]],FS=[0,0,[0,[0,b(dG)]]],FW=[0,[0,b(s)]],FX=[0,0,[0,[0,b(E)]]],FY=[0,b(cn)],F3=b(cn),F7=[0,[0,0,[0,[0,b(a$)]]],[0,[0,b(a0)]]],F9=[0,[0,b(cm)]],F_=[0,2],F$=[0,b(c2)],Gf=[0,[0,b(T)]],Gh=[0,[0,b(aE)]],Gi=[0,[0,b(aE)]],Gn=[0,[0,[0,0,[0,[0,b(E)]]],0],[0,[0,b(s)]]],Gp=[0,[0,b(s)]],Gq=[0,[0,[0,0,[0,[0,b(E)]]],0],[0,[0,b(T)]]],Gt=[0,0,[0,[0,b(dG)]]],Gw=[0,[0,0,[0,[0,b(E)]]],[0,[0,b(s)]]],Gy=[0,[0,b(a0)]],Gz=[0,[0,b(c0)]],GA=b(bL),GB=[0,0,[0,[0,b(a$)]]],GD=[0,[0,b(dL)]],GE=[0,0,[0,[0,b(e$)]]],GG=[0,b(cn)],GJ=b(cn),GM=[0,[0,b(s)]],GN=[0,[0,0,0],[0,[0,b(mO)]]],GQ=b(bL),GR=[0,[0,b(ag)]],GS=[0,[0,b(s)]],GT=[0,[0,0,0],[0,[0,b(mO)]]],GU=[0,2],GV=[0,b(c2)],GY=[0,[0,b(cm)]],GZ=[0,1],G0=[0,b(cm)],G3=[0,[0,[0,0,0],[0,[0,b(aE)]]],[2,1,[0,[0,b(aE)]]]],G4=[0,[0,b("4")],[0,2],0],G7=b(dJ),G8=[0,[0,b(b5)]],G9=[0,0,[0,[0,b(mj)]]],G$=b(dJ),Ha=[0,[0,b(hC)]],Hb=[0,[0,b(bK)]],Hc=[0,0,[0,[0,b(mp)]]],He=[0,[0,b(hy)]],Hf=[0,[0,b(bK)]],Hg=b(bL),Hh=[0,0,[0,[0,b(fb)]]],Hi=[0,b(bL)],Hl=[0,[0,[0,0,0],[0,[0,b(c0)]]],0],Hm=[0,1],Hn=[0,b(dJ)],Hs=[0,[0,b(_)]],Ht=[0,0,[0,[0,b(_)]]],Hv=[0,[0,b(_)]],Hz=b(dJ),HA=[0,[0,b(b5)]],HB=b(c2),HF=[0,0,[0,[2,[0,b(mA)]]]],HK=[0,0,[0,[2,[0,b("mutable")]]]],HQ=[0,0,[0,[0,b(hM)]]],H0=[0,0,[0,[0,b(hQ)]]],H2=[0,0,[0,[0,b(b2)]]],H5=[0,0,[0,[0,b(hM)]]],H8=[0,[0,b(s)]],H9=[0,[0,[0,0,[0,[2,[0,b(b4)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],Ia=[0,[0,b(s)]],Ib=[0,[0,[0,0,[0,[2,[0,b(hL)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],Id=[0,[0,b(s)]],Ie=[0,[0,[0,0,[0,[2,[0,b(c1)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],Ih=[0,[0,b(s)]],Ii=[0,[0,[0,0,[0,[2,[0,b(c3)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],Ik=[0,[0,b(s)]],Il=[0,[0,[0,0,[0,[2,[0,b(e9)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],In=[0,[0,b(s)]],Io=[0,[0,[0,0,[0,[2,[0,b(hN)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],Iq=[0,[0,b(s)]],Ir=[0,[0,[0,0,[0,[2,[0,b(mN)]]]],[0,[0,b(T)]]],[0,[0,b(E)]]],Iv=[0,[0,b(cl)]],IB=[0,[0,b(cl)]],IG=[0,[0,b(ag)]],IO=[0,[0,b(s)]],IP=b(bL),IQ=[0,0,[0,[0,b(E)]]],IU=[0,0,[0,[0,b(b3)]]],IZ=[0,[0,b(s)]],I0=[0,[0,b(aE)]],I1=b(bL),I2=[0,0,[0,[0,b(E)]]],I3=[0,b(cn)],I7=[0,0,0],I8=[0,2],I9=[0,b(c2)],Ja=[0,[0,b(co)]],Jb=b(c2),Jc=[0,[0,b(co)]],Jd=[0,b("2")],Jg=[0,[0,b(e5)]],Jh=[0,1],Ji=[0,b(bL)],Jp=[0,0,[0,[0,b(b3)]]],Ju=b(cn),Jy=[0,[0,b(ag)]],JC=[0,[0,b(bK)]],JH=[0,[0,b(ag)]],JI=[0,0,[0,[0,b("Set")]]],JM=[0,[0,[0,0,[0,[0,b(a$)]]],[0,[0,b(hA)]]],[0,[0,b(a0)]]],JO=[0,[0,b(a0)]],JP=[0,0,[0,[0,b(a$)]]],JR=[0,[0,b(dL)]],JS=[0,0,[0,[0,b(e$)]]],JW=[0,[0,b(_)]],JX=[0,0,[0,[0,b(_)]]],JZ=[0,[0,b(_)]],J6=[0,[0,b(s)]],J7=[0,[0,b(aE)]],J8=[0,[0,b(E)]],J$=[0,[0,b(c0)]],Kb=[0,[0,b(c0)]],Ki=[0,[0,b(T)]],Kl=[0,[0,b(c0)]],Kn=[0,[0,b(c0)]],Ku=b(c2),Kv=[0,[0,b(ag)]],KB=[0,[0,b(s)]],KC=[0,[0,b(aE)]],KE=[0,0,[0,[0,b(E)]]],KN=[0,0,[0,[0,b(ag)]]],KP=[0,0,[0,[0,b("::=")]]],KT=[0,[0,b(bK)]],KU=[0,0,[0,[0,b(hD)]]],KZ=[0,[0,b(ag)]],K0=b(bL),K1=[0,[0,b(T)]],K2=[0,[0,0,[0,[0,b(hQ)]]],[0,[2,[0,b("external")]]]],K5=[0,0,[0,[0,b(b3)]]],Ld=[0,[0,b(s)]],Le=[0,[0,b(aE)]],Lf=[0,[0,b(E)]],Ll=[0,0,[0,[0,b(T)]]],Lo=[0,[0,b(ag)]],Lp=[0,0,[0,[0,b("Notation")]]],Lx=[0,0,[0,[0,b(b2)]]],LB=b("anti"),LC=b("ident_or_anti"),LD=b(m5),LE=b("lnatural"),LF=b("qhyp"),LG=b("simple_binding"),LH=b(mi),LI=b(l7),LJ=b("or_and_intropattern"),LK=b("equality_intropattern"),LL=b("naming_intropattern"),LM=b("nonsimple_intropattern"),LN=b("simple_intropattern"),LO=b("simple_intropattern_closed"),LP=b("nat_or_anti"),LQ=b("eqn_ipat"),LR=b(ma),LS=b("constr_with_bindings"),LT=b(mY),LU=b("as_or_and_ipat"),LV=b("occs_nums"),LW=b("occs"),LX=b("hypident"),LY=b("hypident_occ"),LZ=b("in_clause"),L0=b(mJ),L1=b("concl_occ"),L2=b(mK),L3=b(mG),L4=b("orient"),L5=b("rewriter"),L6=b("oriented_rewriter"),L7=b("tactic_then_last"),L8=b("tactic_then_gen"),L9=b("red_flag"),L_=b("refglobal"),L$=b("refglobals"),Ma=b("delta_flag"),Mb=b("strategy_flag"),Mc=b(l9),Md=b("match_pattern"),Me=b("match_rule"),Mf=b("match_list"),Mg=b("gmatch_hyp_pattern"),Mh=b("gmatch_pattern"),Mi=b("gmatch_rule"),Mj=b("gmatch_list"),Mk=b(l$),Ml=b("as_name"),Mm=b(ms),Mn=b("as_ipat"),Mo=b("by_tactic"),Mp=b("assertion"),Mt=[0,0,[0,[0,b(b6)]]],My=[0,0,[0,[0,b(b6)]]],MQ=[0,[0,b(s)]],MR=[0,[0,b(ag)]],MS=[0,0,[0,[0,b(E)]]],M8=[0,[0,b(a0)]],M9=[0,[0,b(_)]],M_=[0,0,[0,[0,b(a$)]]],Na=[0,0,[0,[0,b(dG)]]],Nc=[0,[0,b(s)]],Nd=[0,0,[0,[0,b(E)]]],Nf=[0,[0,b(s)]],Ng=[0,[0,b(aE)]],Nh=[0,[0,b(aE)]],Ni=[0,0,[0,[0,b(E)]]],Nk=[0,[0,b(s)]],Nl=[0,[0,b(b2)]],Nm=[0,[0,b(b2)]],Nn=[0,0,[0,[0,b(E)]]],Ns=[0,0,[0,[0,b(e5)]]],Nv=[0,0,[0,[0,b(mQ)]]],Nx=[0,[0,b(a0)]],Ny=[0,0,[0,[0,b("[=")]]],NC=[0,0,[0,0]],NE=[0,0,[0,[0,b("?$")]]],NG=[0,0,[0,[0,b(hI)]]],NN=[0,0,[0,[0,b(co)]]],NQ=[0,0,[0,[0,b("**")]]],NZ=[0,0,[0,[0,b(b3)]]],Oa=[0,0,[0,[0,b(b6)]]],Od=[0,[0,0,[0,[2,[0,b("eqn")]]]],[0,[0,b(T)]]],Oi=[0,0,[0,[0,b(bK)]]],Oy=[0,0,[0,[0,b(fa)]]],OE=[0,0,[0,[0,b(mB)]]],OI=[0,0,[0,[0,b(fd)]]],OO=[0,[0,b(s)]],OP=[0,[0,[0,0,[0,[0,b(E)]]],[0,[2,[0,b("type")]]]],[0,[2,[0,b(m6)]]]],OR=[0,[0,b(s)]],OS=[0,[0,[0,0,[0,[0,b(E)]]],[0,[2,[0,b("value")]]]],[0,[2,[0,b(m6)]]]],OZ=[0,0,[0,[0,b(co)]]],O1=[0,[0,0,[0,[0,b(co)]]],[0,[0,b(cl)]]],O3=[0,[0,b(cl)]],O4=[0,[0,b(aE)]],O6=[0,[0,b(aE)]],O_=[0,0,[0,[0,b(hC)]]],Pb=[0,0,[0,[0,b(fd)]]],Pi=[0,0,[0,[0,b(co)]]],Pw=[0,[0,b(bK)]],PD=[0,0,[0,[0,b(e5)]]],PG=[0,0,[0,[0,b(mQ)]]],PL=[0,0,[0,[0,b(nb)]]],PQ=[1,0,[0,[0,b(hI)]]],PS=[1,0,[0,0]],PU=[0,[0,b(nb)]],PZ=[1,0,[0,[0,b(hI)]]],P1=[1,0,[0,0]],Qc=[0,[0,b(_)]],Qd=b(dJ),Qe=[0,0,[0,[0,b(_)]]],Qk=[0,[0,b(_)]],Qn=[0,[0,b(hA)]],Qp=[0,0,[0,[0,b(hA)]]],Qt=[0,0,[0,[0,b(_)]]],QF=[0,0,[0,[2,[0,b("beta")]]]],QH=[0,0,[0,[2,[0,b("iota")]]]],QJ=[0,0,[0,[2,[0,b(fb)]]]],QL=[0,0,[0,[2,[0,b("fix")]]]],QN=[0,0,[0,[2,[0,b("cofix")]]]],QP=[0,0,[0,[2,[0,b("zeta")]]]],QR=[0,0,[0,[2,[0,b("delta")]]]],QW=[0,0,[0,[0,b(b2)]]],Q1=[0,0,[0,[0,b(b6)]]],Q_=[0,[0,b(a0)]],Q$=[0,[0,0,[0,[0,b(mB)]]],[0,[0,b(a$)]]],Rb=[0,[0,b(a0)]],Rc=[0,0,[0,[0,b(a$)]]],Ro=[0,0,[0,[0,b(co)]]],Rx=[0,[0,b(a0)]],Ry=[0,[0,b(a$)]],Rz=[0,0,[0,[2,[0,b("context")]]]],RF=[0,[0,b(b5)]],RI=[0,[0,b(_)]],RK=[0,[0,b(_)]],RL=[0,0,[0,[0,b(_)]]],RT=[0,[0,b(T)]],RW=[0,[0,b(a0)]],RX=[0,[0,b(cl)]],RY=[0,[0,b(aE)]],RZ=[0,0,[0,[0,b(a$)]]],R4=[0,[0,b(b5)]],R7=[0,[0,b(_)]],R9=[0,[0,b(_)]],R_=[0,0,[0,[0,b(_)]]],Sf=[0,[0,0,[0,[0,b(fd)]]],[0,[2,[0,b("top")]]]],Sh=[0,[0,0,[0,[0,b(fd)]]],[0,[2,[0,b("bottom")]]]],Sj=[0,0,[0,[2,[0,b("after")]]]],Sl=[0,0,[0,[2,[0,b("before")]]]],St=[0,0,[0,[0,b(fa)]]],Sy=[0,[0,b(s)]],Sz=[0,[0,b(ag)]],SA=[0,[0,b(E)]],SI=[0,0,[0,[0,b(fa)]]],SO=[0,0,[0,[0,b("by")]]],SU=[0,[0,b(s)]],SV=[0,[0,b(ag)]],SW=[0,[0,b(E)]],SZ=[0,[0,b(s)]],S0=[0,[0,b(T)]],S1=[0,[0,b(E)]],S9=[0,[0,b(s)]],S_=[0,[0,b(E)]],S$=[0,[0,b(T)]],Ta=[0,[2,[0,b(cp)]]],Td=[0,[0,b(b2)]],Tg=[0,[0,b(b6)]],Tr=b("ltac2_entry"),Tv=b("ltac2_expr"),TB=b("Eval"),TC=b(ba),TG=b(ba),TI=b("VernacDeclareTactic2Definition"),TJ=b(ba),TR=b("VernacLtac2"),TV=b(ba),TW=b("Print"),T0=b("Ltac2Print");function
hV(b){var
a=c(dM[7],[0]);return[0,a[1],a[2],a[3],a[4],a[5]]}var
aW=[0,ne,nc,nd];aD(1195,[0,aW,hV,ad],"Ltac2_plugin__Tac2dyn");var
as=0;function
nf(a){return[0,a]}function
ng(b,a){return[0,b,a]}function
nh(a){return 0===a[0]?1:0}function
ni(a){if(1===a[0])return a[1];var
b=c(d[3],nj);return f(l[2],0,0,b)}function
nk(b,a){if(1===b[0])return y(b[2],a)[1+a];var
e=c(d[3],nl);return f(l[2],0,0,e)}function
nm(b,a,e){if(1===b[0]){y(b[2],a)[1+a]=e;return 0}var
g=c(d[3],nn);return f(l[2],0,0,g)}function
no(b,a){return[1,b,a]}var
aX=[0,nh,ni,nk,nm,no,function(a){return[0,a]}];function
np(b,a){return c(b[1],a)}function
G(b,a){return c(b[2],a)}function
ff(b,a){return[0,b,a,0]}var
dN=c(ad[1],nq),dO=c(ad[1],nr),dP=c(ad[1],ns),dQ=c(ad[1],nt),fg=c(ad[1],nu),dR=c(ad[1],nv),fh=c(ad[1],nw),fi=c(ad[1],nx),b7=c(ad[1],ny),dS=c(ad[1],nz),cr=c(ad[1],nA),fj=c(ad[1],nB),dT=c(ad[1],nC),fk=c(ad[1],nD),b8=c(ad[1],nE),a1=c(ad[1],nF),bn=[dI,nH,dF(0)],nI=1;function
nJ(a){return a}var
cs=[0,function(a){return a},nJ,nI];function
dU(a){return nK}function
hW(a){if(0===a[0])if(0===a[1])return 0;throw[0,m,nL]}var
am=[0,dU,hW,0];function
fl(a){return[0,a]}function
O(a){if(0===a[0])return a[1];throw[0,m,nM]}var
I=[0,fl,O,0];function
c4(a){return a?nN:nO}function
a2(a){if(0===a[0]){var
b=a[1];if(0===b)return 1;if(1===b)return 0}throw[0,m,nP]}var
M=[0,c4,a2,0];function
hX(a){return[0,a]}function
hY(a){if(0===a[0])return c(dV[1],a[1]);throw[0,m,nQ]}var
dW=[0,hX,hY,0];function
hZ(a){return[2,a]}function
fm(a){if(2===a[0])return a[1];throw[0,m,nR]}var
b9=[0,hZ,fm,0];function
bo(b,a){if(a){var
d=a[1],e=bo(b,a[2]);return[1,0,[0,c(b,d),e]]}return nS}function
aN(d,a){switch(a[0]){case
0:if(0===a[1])return 0;break;case
1:if(0===a[1]){var
b=a[2];if(2===b.length-1){var
e=b[1],f=aN(d,b[2]);return[0,c(d,e),f]}}break}throw[0,m,nT]}function
z(a){var
b=0;function
c(b){return aN(a[2],b)}return[0,function(b){return bo(a[1],b)},c,b]}function
h0(a){return[3,a]}function
c5(a){if(3===a[0])return a[1];throw[0,m,nU]}var
U=[0,h0,c5,0];function
nV(b,a){return[5,b,a]}function
V(c,b){if(5===b[0]){var
d=b[2];if(a(ad[3],c,b[1]))return d;throw[0,m,nG]}throw[0,m,nW]}function
aO(a){var
b=0;function
c(b){return V(a,b)}return[0,function(b){return[5,a,b]},c,b]}function
u(a){return[5,dO,a]}function
H(a){return V(dO,a)}var
o=aO(dO);function
aF(a){return[5,dP,a]}function
aG(a){return V(dP,a)}var
F=aO(dP);function
h1(a){return[5,dQ,a]}function
h2(a){return V(dQ,a)}var
bp=aO(dQ),nY=a(e[22][68],h[1][6],nX),nZ=[0,c(h[5][4],nY)],n1=c(h[1][6],n0),n2=c(h[8][5],n1),h3=a(h[15][1],nZ,n2);function
c6(b){var
a=b[1];return a[1]===bn?[4,a[2],a[3]]:[4,h3,[0,[5,dN,b]]]}function
fn(b){if(4===b[0]){var
c=b[2],d=b[1];return a(h[15][10],d,h3)?V(dN,y(c,0)[1]):[0,[0,bn,d,c],bb[2]]}throw[0,m,n3]}var
fo=[0,c6,fn,0];function
bc(b,a){return a?[1,0,[0,c(b,a[1])]]:n4}function
a3(d,a){switch(a[0]){case
0:if(0===a[1])return 0;break;case
1:if(0===a[1]){var
b=a[2];if(1===b.length-1)return[0,c(d,b[1])]}break}throw[0,m,n5]}function
A(a){var
b=0;function
c(b){return a3(a[2],b)}return[0,function(b){return bc(a[1],b)},c,b]}function
b_(a){return[5,dR,a]}function
h4(a){return V(dR,a)}var
dX=aO(dR);function
bd(a){return[1,0,a]}function
bq(a){if(1===a[0])if(0===a[1])return a[2];throw[0,m,n6]}function
h5(d,b,a){var
e=a[1],f=c(b,a[2]);return[1,0,[0,c(d,e),f]]}function
fp(e,d,a){if(1===a[0])if(0===a[1]){var
b=a[2];if(2===b.length-1){var
f=b[1],g=c(d,b[2]);return[0,c(e,f),g]}}throw[0,m,n7]}function
be(b,a){var
c=0;function
d(c){return fp(b[2],a[2],c)}return[0,function(c){return h5(b[1],a[1],c)},d,c]}function
at(c,b){return[1,0,a(e[24][15],c,b)]}function
bf(c,b){if(1===b[0])if(0===b[1])return a(e[24][15],c,b[2]);throw[0,m,n8]}function
fq(a){var
b=0;function
c(b){return bf(a[2],b)}return[0,function(b){return at(a[1],b)},c,b]}function
h6(a){return[1,a[1],a[2]]}function
aP(a){if(1===a[0])return[0,a[1],a[2]];throw[0,m,n9]}var
dY=[0,h6,aP,0];function
c7(a){return[4,a[1],a[2]]}function
dZ(a){if(4===a[0])return[0,a[1],a[2]];throw[0,m,n_]}var
n$=[0,c7,dZ,0];function
h7(a){return[6,a]}function
fr(a){if(6===a[0])return a[1];throw[0,m,oa]}var
ob=[0,h7,fr,0];function
h8(a){return[7,a]}function
fs(a){if(7===a[0])return a[1];throw[0,m,oc]}var
od=[0,h8,fs,0];function
ft(a){return[5,dS,a]}function
fu(a){return V(dS,a)}var
oe=aO(dS);function
c8(a){switch(a[0]){case
0:return[1,0,[0,aF(a[1])]];case
1:return[1,1,[0,ft(a[1])]];case
2:return[1,2,[0,[5,b7,a[1]]]];default:return[1,3,[0,[5,cr,a[1]]]]}}function
fv(a){if(1===a[0]){var
b=a[1];if(!(3<b>>>0))switch(b){case
0:var
c=a[2];if(1===c.length-1)return[0,aG(c[1])];break;case
1:var
d=a[2];if(1===d.length-1)return[1,fu(d[1])];break;case
2:var
e=a[2];if(1===e.length-1)return[2,V(b7,e[1])];break;default:var
f=a[2];if(1===f.length-1)return[3,V(cr,f[1])]}}throw[0,m,of]}var
d0=[0,c8,fv,0];function
fw(b,a){return U}function
d1(c,b,a){return c5(a)}function
h9(t,s,r){var
b=t,d=s,h=r;for(;;){if(h){var
g=h[2],e=h[1];if(g){var
i=g[2],k=g[1];if(i){var
l=i[2],m=i[1];if(l){if(!l[2])if(b){var
n=b[1];if(n){var
o=n[1];if(o)if(!o[1])return p(d,e,k,m,l[1])}}}else
if(b){var
q=b[1];if(q)if(!q[1])return f(d,e,k,m)}}else
if(b)if(!b[1])return a(d,e,k)}else
if(!b)return c(d,e);if(b){var
u=b[1],b=u,d=c(d,e),h=g;continue}var
v=function(b){var
a=c5(b);return h9(a[1],a[2],g)},w=c(d,e);return a(j[72][1],w,v)}return c(j[16],[3,[0,b,d]])}}function
bM(a,b){return h9(a[1],a[2],b)}function
h_(d,b){if(1===d)return[0,0,function(d,a){return c(b,c(e[22][9],[0,a,d]))}];var
f=h_(a(e[5],d,1),b),g=f[2],h=f[1];return[0,[0,h],function(b,a){return c(g,[0,a,b])}]}function
d2(a,d){if(0<a){var
b=h_(a,d),e=b[1];return[0,e,c(b[2],0)]}throw[0,m,og]}function
c9(f,e,d,b){function
g(a){var
b=c(d[2],a);return c(j[16],b)}var
h=bM(f,[0,c(e[1],b),0]);return a(j[72][1],h,g)}aD(1204,[0,as,nf,ng,aX,np,G,ff,dU,hW,am,fl,O,I,c4,a2,M,hX,hY,dW,hZ,fm,b9,bo,aN,z,u,H,o,c6,fn,fo,aF,aG,F,h0,c5,U,h6,aP,dY,at,bf,fq,bd,bq,h5,fp,be,bc,a3,A,h1,h2,bp,b_,h4,dX,ft,fu,oe,c8,fv,d0,nV,V,aO,c7,dZ,n$,h7,fr,ob,h8,fs,od,c9,d1,fw,cs,dO,dP,dQ,fg,dR,fh,fi,b7,dS,cr,fj,dT,fk,b8,a1,dN,bM,d2,bn],"Ltac2_plugin__Tac2ffi");var
au=f(h$[4],0,oh,[0,h[18][1],h[18][1],h[18][1],h[18][1],h[18][1]]);function
d3(d,b){var
a=c(e[3],au),g=a[5],i=a[4],j=a[3],k=a[2];au[1]=[0,f(h[18][4],d,b,a[1]),k,j,i,g];return 0}function
bN(b){return a(h[18][23],b,au[1][1])}function
fx(d,b){var
a=c(e[3],au),g=a[5],i=a[4],j=a[3],k=f(h[18][4],d,b,a[2]);au[1]=[0,a[1],k,j,i,g];return 0}function
b$(b){return a(h[18][23],b,au[1][2])}function
ia(d,b){var
a=c(e[3],au),g=a[5],i=a[4],j=f(h[18][4],d,b,a[3]);au[1]=[0,a[1],a[2],j,i,g];return 0}function
fy(b){return a(h[18][23],b,au[1][3])}function
c_(d,b){var
a=c(e[3],au),g=a[5],i=f(h[18][4],d,b,a[4]);au[1]=[0,a[1],a[2],a[3],i,g];return 0}function
ae(b){return a(h[18][23],b,au[1][4])}function
fz(d,b){var
a=c(e[3],au),g=f(h[18][4],d,b,a[5]);au[1]=[0,a[1],a[2],a[3],a[4],g];return 0}function
fA(b){return a(h[18][23],b,au[1][5])}var
oi=[0,function(c,b){var
d=a(e[20][33],c[1],b[1]);return 0===d?a(e[20][33],c[2],b[2]):d}],fB=c(e[26][1],oi),fC=[0,fB[1]];function
a4(b,a){var
d=c(e[3],fC);fC[1]=f(fB[4],b,a,d);return 0}function
fD(b){var
d=c(e[3],fC);return a(fB[23],b,d)}var
oj=r[14],ok=r[20],ib=[0,oj,ok,function(b){var
a=c(r[16],b),d=a[2];return[0,d,c(h[5][5],a[1])]}];function
ic(c,b){if(0===c[0]){var
d=c[1];return 0===b[0]?a(h[15][9],d,b[1]):-1}var
e=c[1];return 0===b[0]?1:a(h[15][9],e,b[1])}function
ol(b,a){return 0===ic(b,a)?1:0}var
om=[0,h[15][10]],an=c(c(bO[50],ib),om),c$=c(c(bO[50],ib),[0,ol]),fE=c(e[26][1],[0,ic]),P=f(h$[4],0,on,[0,c$[1],fE[1],an[1],h[18][1],an[1],h[18][1],an[1],h[18][1]]);function
da(g,d,b){var
a=c(e[3],P),h=p(c$[2],g,d,b,a[1]),i=f(fE[4],b,d,a[2]);P[1]=[0,h,i,a[3],a[4],a[5],a[6],a[7],a[8]];return 0}function
ct(b){var
d=c(e[3],P)[1];return a(c$[3],b,d)}function
oo(b){var
d=c(e[3],P)[1];return a(c$[8],b,d)}function
fF(d){var
b=c(e[3],P),f=a(fE[23],d,b[2]);return p(c$[7],0,h[1][10][1],f,b[1])}function
fG(g,d,b){var
a=c(e[3],P),i=p(an[2],g,d,b,a[3]),j=f(h[18][4],b,d,a[4]);P[1]=[0,a[1],a[2],i,j,a[5],a[6],a[7],a[8]];return 0}function
fH(b){var
d=c(e[3],P)[3];return a(an[3],b,d)}function
op(b){var
d=c(e[3],P)[3];return a(an[8],b,d)}function
fI(d){var
b=c(e[3],P),f=a(h[18][23],d,b[4]);return p(an[7],0,h[1][10][1],f,b[3])}function
fJ(b){var
d=c(e[3],P);try{a(an[3],b,d[3]);var
f=1;return f}catch(a){a=v(a);if(a===D)return 0;throw a}}function
db(g,d,b){var
a=c(e[3],P),i=p(an[2],g,d,b,a[5]),j=f(h[18][4],b,d,a[6]);P[1]=[0,a[1],a[2],a[3],a[4],i,j,a[7],a[8]];return 0}function
d4(b){var
d=c(e[3],P)[5];return a(an[3],b,d)}function
oq(b){var
d=c(e[3],P)[5];return a(an[8],b,d)}function
d5(f,d){var
b=c(e[3],P),g=a(h[18][23],d,b[6]);return p(an[7],f,h[1][10][1],g,b[5])}function
id(g,d,b){var
a=c(e[3],P),i=p(an[2],g,d,b,a[7]),j=f(h[18][4],b,d,a[8]);P[1]=[0,a[1],a[2],a[3],a[4],a[5],a[6],i,j];return 0}function
fK(b){var
d=c(e[3],P)[7];return a(an[3],b,d)}function
or(b){var
d=c(e[3],P)[7];return a(an[8],b,d)}function
ie(d){var
b=c(e[3],P),f=a(h[18][23],d,b[8]);return p(an[7],0,h[1][10][1],f,b[7])}var
fL=hV([0]),fM=[0,fL[1]];function
br(b,a){var
d=c(e[3],fM);fM[1]=f(fL[2],b,[0,a],d);return 0}function
cu(g){try{var
b=c(e[3],fM),m=a(fL[4],g,b)[1];return m}catch(b){b=v(b);if(b===D){var
h=c(aW[3],g),i=c(d[3],h),j=c(d[3],os),k=a(d[12],j,i);return f(l[2],0,0,k)}throw b}}var
ou=a(e[22][68],h[1][6],ot),bg=[0,c(h[5][4],ou)],ow=a(e[22][68],h[1][6],ov),fN=[0,c(h[5][4],ow)],oy=a(e[22][68],h[1][6],ox),fO=[0,c(h[5][4],oy)],ca=c(ao[2],oz),cb=c(ao[2],oA),cc=c(ao[2],oB);a(bh[4],ca,0);a(bh[4],cc,0);function
d6(b){var
a=c(h[1][8],b);if(0<K.caml_ml_string_length(a)){if(e1(a,oC))if(e1(a,oD))return 25<(K.caml_string_get(a,0)-65|0)>>>0?0:1;return 1}throw[0,m,oE]}function
bs(a){return d6(c(r[24],a)[2])}aD(1211,[0,d3,bN,c_,ae,fx,b$,ia,fy,fz,fA,da,ct,oo,fF,fG,fJ,fH,op,fI,db,d4,oq,d5,id,fK,or,ie,a4,fD,br,cu,bg,fN,fO,ca,cb,cc,d6,bs],"Ltac2_plugin__Tac2env");function
cv(d,b){var
e=c(h[15][3],d),f=c(h[8][5],b);return a(h[15][1],e,f)}function
aH(b){var
e=c(d[3],oF),f=c(d[3],oG),g=a(d[12],f,b),h=a(d[12],g,e);return a(d[26],2,h)}var
oI=c(h[1][6],oH),oJ=c(h[8][5],oI),ig=a(h[15][1],bg,oJ),oL=c(h[1][6],oK),oM=c(h[8][5],oL),oN=a(h[15][1],bg,oM);function
cw(a){var
b=d5(0,a);return c(r[26],b)}function
ih(k,g,b){function
e(g,b){switch(b[0]){case
0:var
l=c(k,b[1]),m=c(d[3],l),n=c(d[3],oO);return a(d[12],n,m);case
1:var
o=b[2],p=b[1],q=1===g?function(a){return a}:aH,s=e(1,o),t=c(d[13],0),u=c(d[3],oP),v=c(d[13],0),w=e(0,p),x=a(d[12],w,v),y=a(d[12],x,u),z=a(d[12],y,t);return q(a(d[12],z,s));default:var
i=b[1];if(0===i[0]){if(0===i[1])if(!b[2]){var
F=d5(0,oN);return c(r[26],F)}var
A=b[2],B=2<=g?aH:function(a){return a},C=2,D=function(a){return e(C,a)},E=function(a){return c(d[3],oQ)};return B(f(d[39],E,D,A))}var
h=b[2],j=i[1];if(h){if(h[2]){var
G=4<=g?aH:function(a){return a},H=cw(j),I=c(d[13],0),J=c(d[3],oR),K=function(a){return e(g,a)},L=function(a){return c(d[3],oS)},M=f(d[39],L,K,h),N=c(d[3],oT),O=a(d[12],N,M),P=a(d[12],O,J),Q=a(d[12],P,I);return G(a(d[12],Q,H))}var
R=h[1],S=4<=g?aH:function(a){return a},T=cw(j),U=c(d[13],0),V=e(3,R),W=a(d[12],V,U);return S(a(d[12],W,T))}return cw(j)}}var
h=e(g,b);return a(d[26],0,h)}function
cx(b,a){return ih(b,1,a)}function
d7(d){var
b=[0,cy[3][1]];return function(d){var
j=c(e[3],b);if(a(cy[3][3],d,j)){var
k=c(e[3],b);return a(cy[3][23],d,k)}var
l=c(e[3],b),g=c(cy[3][17],l),h=g/26|0,m=a(e[4],97,g%26|0),n=c(dV[1],m),o=a(e[20][1],1,n),p=0===h?oU:c(bP[22],h),i=a(bP[17],o,p),q=c(e[3],b);b[1]=f(cy[3][4],d,i,q);return i}}function
bQ(a){var
b=fI(a);return c(r[26],b)}function
d8(a){var
b=ie(a);return c(r[26],b)}function
dc(a){return a?c(h[1][9],a[1]):c(d[3],oV)}function
ii(h,d,g){var
b=h,a=g;for(;;){if(a){var
c=a[1];if(c[2]){var
e=a[2];if(d){var
a=e;continue}if(0===b)return c;var
b=b-1|0,a=e;continue}var
f=a[2];if(d){if(0===b)return c;var
b=b-1|0,a=f;continue}var
a=f;continue}throw[0,m,oW]}}function
d9(b,d,c){var
a=ae(b)[2];if(typeof
a!=="number"&&1===a[0])return bQ(cv(b,ii(d,c,a[1][1])[1]));throw[0,m,oX]}function
ij(i,b){function
g(i,b){switch(b[0]){case
0:var
q=b[1];return 0===q[0]?c(d[16],q[1]):c(d[19],q[1]);case
1:return c(h[1][9],b[1]);case
2:var
X=fF([0,b[1]]);return c(r[26],X);case
3:var
Y=b[2],Z=a(d[45],dc,b[1]),_=0===i?function(a){return a}:aH,$=g(0,Y),aa=c(d[13],0),ab=c(d[3],oY),ac=c(d[13],0),ad=c(d[13],0),af=c(d[3],oZ),ag=a(d[12],af,ad),ah=a(d[12],ag,Z),ai=a(d[26],2,ah),aj=a(d[12],ai,ac),ak=a(d[12],aj,ab),al=a(d[12],ak,aa),am=a(d[12],al,$);return _(a(d[26],0,am));case
4:var
an=b[2],ao=b[1],ap=5<=i?aH:function(a){return a},aq=5,ar=function(a){return g(aq,a)},as=a(d[45],ar,an),at=c(d[13],0),au=g(4,ao),av=a(d[12],au,at),aw=a(d[12],av,as);return ap(a(d[26],2,aw));case
5:var
ax=b[3],ay=b[2],az=b[1],aA=0===i?function(a){return a}:aH;if(az)var
aB=c(d[13],0),aC=c(d[3],o0),D=a(d[12],aC,aB);else
var
D=c(d[7],0);var
aD=function(b){var
e=b[2],f=b[1],h=c(d[13],0),i=g(0,e),j=a(d[26],2,i),k=c(d[13],0),l=c(d[3],o1),m=c(d[13],0),n=dc(f),o=a(d[12],n,m),p=a(d[12],o,l),q=a(d[12],p,k),r=a(d[12],q,j);return a(d[12],r,h)},aE=function(f){var
b=c(d[13],0),e=c(d[3],o2);return a(d[12],e,b)},aF=f(d[39],aE,aD,ay),aG=g(0,ax),aI=c(d[13],0),aJ=c(d[3],o3),aK=c(d[13],0),aL=c(d[3],o4),aM=a(d[12],aL,aK),aN=a(d[12],aM,D),aO=a(d[12],aN,aF),aP=a(d[12],aO,aJ),aQ=a(d[26],2,aP),aR=a(d[12],aQ,aI),aS=a(d[12],aR,aG);return aA(a(d[25],0,aS));case
6:var
s=b[1];if(0===s[0]){if(0===s[1])return c(d[3],o5);var
aT=b[3],aU=4<=i?aH:function(a){return a},aV=4,aW=function(a){return g(aV,a)},aX=function(f){var
b=c(d[13],0),e=c(d[3],o6);return a(d[12],e,b)};return aU(f(d[39],aX,aW,aT))}var
j=b[3],E=b[2],l=s[1],z=ae(l)[2];if(a(h[15][10],l,ig)){var
k=0,n=[6,[1,l],E,j];for(;;){if(6===n[0])if(0===n[2]){var
A=n[3];if(A){var
B=A[2];if(B){if(!B[2]){var
k=[0,A[1],k],n=B[1];continue}var
o=0}else
var
o=0}else
var
T=[0,k,0],o=1}else
var
o=0;else
var
o=0;if(!o)var
T=[0,k,[0,n]];var
U=T[2];if(U){var
cL=U[1],cM=3<=i?aH:function(a){return a},cN=function(a){return g(4,a)},cO=function(h){var
b=c(d[13],0),e=c(d[3],pr),f=c(d[13],0),g=a(d[12],f,e);return a(d[12],g,b)},cP=c(e[22][9],[0,cL,k]),cQ=f(d[39],cO,cN,cP);return cM(a(d[26],2,cQ))}var
cR=function(a){return g(1,a)},cS=c(d[3],ps),cT=c(e[22][9],k),cU=f(d[39],d[29],cR,cT),cV=c(d[3],pt),cW=a(d[12],cV,cU),cX=a(d[12],cW,cS);return a(d[26],2,cX)}}if(typeof
z!=="number")switch(z[0]){case
1:var
cY=5<=i?c(e[22][48],j)?function(a){return a}:aH:function(a){return a},cZ=d9(l,E,c(e[22][48],j));if(j)var
c0=5,c1=function(a){return g(c0,a)},c2=a(d[45],c1,j),c3=c(d[13],0),V=a(d[12],c3,c2);else
var
V=c(d[7],0);var
c4=a(d[12],cZ,V);return cY(a(d[26],2,c4));case
2:var
c5=a(e[22][dH],z[1],j),c6=function(b){var
e=b[2],f=cv(l,b[1][1]),h=g(4,e),i=c(d[13],0),j=c(d[3],pv),k=c(d[13],0),m=d8(f),n=a(d[12],m,k),o=a(d[12],n,j),p=a(d[12],o,i);return a(d[12],p,h)},c7=f(d[39],d[29],c6,c5),c8=c(d[3],pw),c9=c(d[13],0),c_=c(d[13],0),c$=c(d[3],px),da=a(d[12],c$,c_),db=a(d[12],da,c7),dd=a(d[12],db,c9),de=a(d[12],dd,c8);return a(d[25],0,de)}throw[0,m,pu];case
7:var
F=b[4],G=b[3],t=b[2],aY=g(0,b[1]);if(0===t[0]){if(0===t[1])var
I=[0],H=y(G,0)[1];else
var
K=y(F,0)[1],I=K[1],H=K[2];var
aZ=g(0,H),a0=function(f){var
b=c(d[13],0),e=c(d[3],o7);return a(d[12],e,b)},a1=f(d[42],a0,dc,I),a2=c(d[13],0),a3=c(d[3],o8),a4=c(d[13],0),a5=aH(a1),a6=a(d[12],a5,a4),a7=a(d[12],a6,a3),a8=a(d[26],0,a7),a9=c(d[13],0),a_=c(d[3],o9),a$=a(d[12],a_,a9),ba=a(d[12],a$,a8),bb=a(d[12],ba,a2),bc=a(d[12],bb,aZ),J=a(d[26],4,bc)}else{var
L=t[1],u=ae(L)[2];if(typeof
u==="number")var
C=0;else
if(1===u[0])var
bu=u[1][1],p=function(d,b,a){if(a){var
f=a[1],g=f[1];if(f[2]){var
i=p(d,b+1|0,a[2]),h=y(F,b)[1+b],j=h[2];return[0,[0,g,c(e[24][11],h[1]),j],i]}var
k=p(d+1|0,b,a[2]);return[0,[0,g,0,y(G,d)[1+d]],k]}return 0},W=p(0,0,bu),bv=function(b){var
e=b[2],h=b[3],i=bQ(cv(L,b[1]));if(e)var
j=a(d[45],dc,e),k=c(d[13],0),f=a(d[12],k,j);else
var
f=c(d[7],0);var
l=c(d[13],0),m=g(0,h),n=a(d[26],2,m),o=c(d[13],0),p=c(d[3],pc),q=c(d[13],0),r=a(d[12],i,f),s=a(d[12],r,q),t=a(d[12],s,p),u=a(d[26],0,t),v=c(d[13],0),w=c(d[3],pd),x=a(d[12],w,v),y=a(d[12],x,u),z=a(d[12],y,o),A=a(d[12],z,n),B=a(d[26],4,A);return a(d[12],B,l)},J=a(d[37],bv,W),C=1;else
var
C=0;if(!C)throw[0,m,pb]}var
bd=c(d[3],o_),be=c(d[13],0),bf=c(d[13],0),bg=c(d[3],o$),bh=c(d[13],0),bj=c(d[13],0),bk=c(d[3],pa),bl=a(d[12],bk,bj),bm=a(d[12],bl,aY),bn=a(d[12],bm,bh),bo=a(d[12],bn,bg),bp=a(d[25],0,bo),bq=a(d[12],bp,bf),br=a(d[12],bq,J),bs=a(d[12],br,be),bt=a(d[12],bs,bd);return a(d[24],0,bt);case
8:var
M=b[1],bw=b[3],bx=b[2],v=ae(M)[2];if(typeof
v!=="number"&&2===v[0]){var
by=d8(cv(M,a(e[22][7],v[1],bw)[1])),bz=g(5,bx),bA=aH(by),bB=c(d[3],pf),bC=a(d[12],bz,bB),bD=a(d[12],bC,bA);return a(d[26],0,bD)}throw[0,m,pe];case
9:var
N=b[1],bE=b[4],bF=b[3],bG=b[2],w=ae(N)[2];if(typeof
w!=="number"&&2===w[0]){var
bH=d8(cv(N,a(e[22][7],w[1],bF)[1])),bI=g(5,bG),bJ=g(4,bE),bK=c(d[13],0),bL=c(d[3],ph),bM=c(d[13],0),bN=aH(bH),bO=c(d[3],pi),bP=a(d[12],bI,bO),bR=a(d[12],bP,bN),bS=a(d[12],bR,bM),bT=a(d[12],bS,bL),bU=a(d[12],bT,bK),bV=a(d[12],bU,bJ);return a(d[26],0,bV)}throw[0,m,pg];case
10:var
bW=b[2],bX=b[1],bY=5<=i?aH:function(a){return a},bZ=bQ(bX),b0=5,b1=function(a){return g(b0,a)},b2=a(d[45],b1,bW),b3=c(d[13],0),b4=a(d[12],bZ,b3),b5=a(d[12],b4,b2);return bY(a(d[26],0,b5));case
11:var
x=b[1],b6=g(0,x[1]),O=function(j,b,i,f){if(b)var
k=c(h[1][9],b[1]),l=c(d[13],0),m=c(d[3],pj),n=c(d[13],0),o=a(d[12],n,m),p=a(d[12],o,l),e=a(d[12],p,k);else
var
e=c(d[7],0);var
q=c(d[13],0),r=g(0,f),s=a(d[26],2,r),t=c(d[13],0),u=c(d[3],pk),v=c(d[13],0),w=a(d[12],j,i),x=a(d[12],w,e),y=a(d[12],x,v),z=a(d[12],y,u),A=a(d[26],0,z),B=c(d[13],0),C=c(d[3],pl),D=a(d[12],C,B),E=a(d[12],D,A),F=a(d[12],E,t),G=a(d[12],F,s),H=a(d[26],4,G);return a(d[12],H,q)},b7=function(f){var
b=f[2],i=b[3],j=b[2],k=b[1],l=bQ(f[1]),g=c(e[24][11],j);if(g)var
m=a(d[45],dc,g),n=c(d[13],0),h=a(d[12],n,m);else
var
h=c(d[7],0);return O(l,k,h,i)},b8=c(h[18][18],x[2]),b9=a(d[37],b7,b8),P=x[3],b_=P[2],b$=P[1],ca=c(d[7],0),cb=O(c(d[3],pm),b$,ca,b_),cc=a(d[12],b9,cb),cd=c(d[3],pn),ce=c(d[13],0),cf=c(d[3],po),cg=c(d[13],0),ch=c(d[13],0),ci=c(d[3],pp),cj=a(d[12],ci,ch),ck=a(d[12],cj,b6),cl=a(d[12],ck,cg),cm=a(d[12],cl,cf),cn=a(d[25],0,cm),co=a(d[12],cn,ce),cp=a(d[12],co,cc),cq=a(d[12],cp,cd);return a(d[24],0,cq);case
12:var
cr=b[2],cs=cu(b[1]),ct=c(bi[2],0),cw=a(cs[4],ct,cr);return a(d[26],0,cw);default:var
Q=b[2],R=b[1];if(Q)var
cx=5,cy=function(a){return g(cx,a)},cz=a(d[45],cy,Q),cA=c(d[13],0),S=a(d[12],cA,cz);else
var
S=c(d[7],0);var
cB=c(d[19],R[2]),cC=c(d[13],0),cD=c(d[19],R[1]),cE=c(d[13],0),cF=c(d[3],pq),cG=a(d[12],cF,cE),cH=a(d[12],cG,cD),cI=a(d[12],cH,cC),cJ=a(d[12],cI,cB),cK=a(d[12],cJ,S);return a(d[26],0,cK)}}var
j=g(i,b);return a(d[26],0,j)}function
d_(a){return ij(0,a)}function
cz(c,b){switch(b[0]){case
0:var
d=b[1];return y(c,d)[1+d];case
1:var
f=b[1],g=cz(c,b[2]);return[1,cz(c,f),g];default:var
h=b[2],i=b[1],j=function(a){return cz(c,a)};return[2,i,a(e[22][68],j,h)]}}function
py(j){var
a=j;for(;;){switch(a[0]){case
0:return[0,a[1]];case
2:var
h=a[1];if(0!==h[0]){var
k=a[2],d=ae(h[1])[2];if(typeof
d==="number")var
b=0;else
if(0===d[0]){var
g=d[1];if(g)var
i=g[1],f=[0,cz(c(e[24][12],k),i)],b=1;else
var
b=0}else
var
b=0;if(!b)var
f=0;if(f){var
a=f[1];continue}return a}break}return a}}var
fP=[0,h[18][1]];function
fQ(b,a){var
d=c(e[3],fP);fP[1]=f(h[18][4],b,a,d);return 0}function
pA(h,g,e,b){function
i(a){return bt(h,g,a,b)}var
j=c(d[3],pQ),k=f(d[39],d[29],i,e),l=c(d[3],pR),m=a(d[12],l,k);return a(d[12],m,j)}function
pz(k,j,i,g,b){var
l=c(e[24][12],i);function
m(a){var
b=a[1];return[0,b,cz(l,a[3])]}var
n=a(e[22][68],m,b),o=c(e[24][11],g),p=a(e[22][dH],n,o);function
q(b){var
e=b[1],f=e[1],g=bt(k,j,b[2],e[2]),i=c(d[13],0),l=c(d[3],pN),m=c(d[13],0),n=c(h[1][9],f),o=a(d[12],n,m),p=a(d[12],o,l),q=a(d[12],p,i);return a(d[12],q,g)}var
r=c(d[3],pO),s=c(d[13],0),t=f(d[39],d[29],q,p),u=c(d[13],0),v=c(d[3],pP),w=a(d[12],v,u),x=a(d[12],w,t),y=a(d[12],x,s);return a(d[12],y,r)}function
bt(i,g,b,z){var
j=py(z);switch(j[0]){case
0:return c(d[3],pB);case
1:return c(d[3],pC);default:var
o=j[1];if(0===o[0]){if(0===o[1])if(!j[2])return c(d[3],pG);var
s=j[2],A=aP(b)[2],t=c(e[24][11],A),B=c(e[22][1],s);if(c(e[22][1],t)===B){var
C=function(b,a){return bt(i,g,b,a)},E=f(e[22][69],C,t,s),F=c(d[3],pD),H=function(a){return a},I=f(d[39],d[28],H,E),J=c(d[3],pE),K=a(d[12],J,I),L=a(d[12],K,F);return a(d[25],2,L)}return c(d[3],pF)}var
k=j[2],l=o[1];try{var
ak=c(e[3],fP),al=[0,a(h[18][23],l,ak)],q=al}catch(a){a=v(a);if(a!==D)throw a;var
q=0}if(q)return p(q[1][1],i,g,b,k);var
n=ae(l)[2];if(a(h[15][10],l,ig)){var
M=c(e[22][5],k);return pA(i,g,aN(function(a){return G(cs,a)},b),M)}if(typeof
n==="number"){var
u=dZ(b),w=u[2],r=u[1];if(0===w.length-1)return bQ(r);var
N=ik(i,g,k,w,b$(r)[3]),P=c(d[3],pH),Q=c(d[3],pI),R=c(d[13],0),S=bQ(r),T=a(d[12],S,R),U=a(d[12],T,Q),V=a(d[12],U,N),W=a(d[12],V,P);return a(d[25],2,W)}else
switch(n[0]){case
0:if(n[1])throw[0,m,pJ];return c(d[3],pK);case
1:var
X=n[1];if(c(aX[1],b))return d9(l,O(b),1);var
x=aP(b),Y=x[2],y=ii(x[1],0,X[1]),Z=y[2],_=cv(l,y[1]),$=ik(i,g,k,Y,Z),aa=c(d[3],pL),ab=c(d[3],pM),ac=c(d[13],0),ad=bQ(_),af=a(d[12],ad,ac),ag=a(d[12],af,ab),ah=a(d[12],ag,$),ai=a(d[12],ah,aa);return a(d[25],2,ai);default:var
aj=n[1];return pz(i,g,k,aP(b)[2],aj)}}}function
ik(j,i,h,g,b){var
k=c(e[24][12],h);function
l(a){return cz(k,a)}var
m=a(e[22][68],l,b),n=c(e[24][11],g),o=a(e[22][dH],n,m);function
p(a){return bt(j,i,a[1],a[2])}return f(d[39],d[28],p,o)}function
cd(d,b){var
e=c(h[8][4],d),g=a(h[15][1],bg,e);return fQ(g,[0,function(d,c,a,e){return f(b,d,c,a)}])}cd(pS,function(f,e,a){var
b=O(a);return c(d[16],b)});cd(pT,function(h,g,a){var
b=fm(a),e=c(ce[6],b),f=c(d[3],e);return c(d[21],f)});cd(pV,function(j,i,b){var
e=aG(b),f=c(h[1][9],e),g=c(d[3],pU);return a(d[12],g,f)});cd(pZ,function(h,g,e){var
i=H(e);try{var
m=f(bR[12],h,g,i),b=m}catch(a){var
b=c(d[3],pW)}var
j=c(d[3],pX),k=c(d[3],pY),l=a(d[12],k,b);return a(d[12],l,j)});cd(p3,function(h,g,e){var
i=h2(e);try{var
m=f(bR[28],h,g,i),b=m}catch(a){var
b=c(d[3],p0)}var
j=c(d[3],p1),k=c(d[3],p2),l=a(d[12],k,b);return a(d[12],l,j)});cd(p6,function(j,i,b){var
e=c(d[3],p4),f=h4(b),g=c(d[3],p5),h=a(d[12],g,f);return a(d[12],h,e)});cd(p9,function(k,j,b){var
e=V(dN,b),f=c(d[3],p7),g=c(l[10],e),h=c(d[3],p8),i=a(d[12],h,g);return a(d[12],i,f)});var
p$=c(h[8][4],p_),qa=a(h[15][1],bg,p$);fQ(qa,[0,function(h,g,e,b){if(b)if(!b[2]){var
i=b[1],j=aP(e)[2],k=c(d[3],qc),l=c(d[13],0),n=function(a){return bt(h,g,a,i)},o=f(d[42],d[29],n,j),p=c(d[13],0),q=c(d[3],qd),r=a(d[12],q,p),s=a(d[12],r,o),t=a(d[12],s,l);return a(d[12],t,k)}throw[0,m,qb]}]);aD(1217,[0,cw,ih,cx,bQ,d9,d8,ij,d_,fQ,bt,d7],"Ltac2_plugin__Tac2print");function
d$(b){var
d=c(h[8][4],b);return a(h[15][1],bg,d)}var
qh=d$(qg),qj=d$(qi),il=d$(qk),qe=c(h[8][4],ql),qf=a(h[15][1],fO,qe),im=d$(qm);function
dd(a,c){var
b=y(c[1],a)[1+a];if(0===b[0])return[0,a,b[1],b[2]];var
d=b[1],e=dd(d,c),f=e[1];if(1-(f===d?1:0))y(c[1],a)[1+a]=[1,f];return e}function
de(c,b){var
a=dd(c,b);return[0,a[1],a[3]]}function
qp(o,n,b){var
d=dd(o,b),j=d[2],f=dd(n,b),k=f[2];if(j<k)var
h=d,g=f;else
var
h=f,g=d;var
l=h[1],i=g[1],p=g[3];if(c(w[3],h[3])){if(c(w[3],p)){y(b[1],l)[1+l]=[1,i];var
q=[0,a(e[4],j,k),0];y(b[1],i)[1+i]=q;return 0}throw[0,m,qq]}throw[0,m,qr]}function
qs(f,e,b){var
a=dd(f,b),d=a[1],g=a[2];if(c(w[3],a[3])){y(b[1],d)[1+d]=[0,g,[0,e]];return 0}throw[0,m,qt]}var
df=cy[3],fR=df[23],io=df[3],cA=df[4],cB=df[1],qu=df[14];function
bu(a){return[0,h[1][11][1],[0,[0],0],[0,h[1][11][1]],1,h[1][11][1],1]}var
cC=c(W[1][1],0);function
fS(b){return a(W[1][5],b,cC)}function
ap(f){var
b=f[2];if(b[1].length-1===b[2]){var
d=cZ(a(e[4],2*b[2]|0,1),qn);a_(e[24][10],b[1],0,d,0,b[1].length-1);b[1]=d}var
c=b[2];y(b[1],c)[1+c]=qo;b[2]=a(e[4],c,1);return c}function
ip(e,b){var
g=e[1],j=e[2];try{var
p=a(h[1][11][23],g,b[3][1]);return p}catch(e){e=v(e);if(e===D){if(b[4]){var
i=ap(b),k=f(h[1][11][4],g,i,b[3][1]);b[3][1]=k;return i}var
m=c(h[1][9],g),n=c(d[3],qw),o=a(d[12],n,m);return f(l[5],j,0,o)}throw e}}function
bv(b,c,a){if(b){var
d=a[6],e=a[5],g=a[4],i=a[3],j=a[2];return[0,f(h[1][11][4],b[1],c,a[1]),j,i,g,e,d]}return a}function
ea(h,g,e,b){var
i=fI(g),j=c(d[3],qx),k=c(d[16],b),m=c(d[3],qy),n=c(d[16],e),o=c(d[3],qz),p=c(r[26],i),q=c(d[3],qA),s=a(d[12],q,p),t=a(d[12],s,o),u=a(d[12],t,n),v=a(d[12],u,m),w=a(d[12],v,k),x=a(d[12],w,j);return f(l[5],h,0,x)}function
iq(g,e,b){var
h=c(d[3],qB),i=c(d[16],b),j=c(d[3],qC),k=c(d[16],e),m=c(d[3],qD),n=a(d[12],m,k),o=a(d[12],n,j),p=a(d[12],o,i),q=a(d[12],p,h);return f(l[5],g,0,q)}function
af(d,b){switch(b[0]){case
0:return c(d,b[1]);case
1:var
f=b[1],g=af(d,b[2]);return[1,af(d,f),g];default:var
h=b[2],i=b[1],j=function(a){return af(d,a)};return[2,i,a(e[22][68],j,h)]}}function
bw(b,t){var
k=t[2],g=t[1];switch(g[0]){case
0:var
u=g[1];return u?[0,ip(a(i[1],k,u[1]),b)]:[0,ap(b)];case
1:var
I=g[1],J=bw(b,g[2]);return[1,bw(b,I),J];default:var
w=g[2],j=g[1];if(0===j[0]){var
n=j[1],x=c(r[34],n);if(c(r[32],n))if(a(h[1][11][3],x,b[5]))var
y=a(h[1][11][23],x,b[5]),z=[0,[1,y[1]],y[2]],s=1;else
var
s=0;else
var
s=0;if(!s){try{var
aa=d4(n),p=aa}catch(b){b=v(b);if(b!==D)throw b;var
Z=c(r[26],n),_=c(d[3],qJ),$=a(d[12],_,Z),p=f(l[5],k,0,$)}var
z=[0,[1,p],ae(p)[1]]}var
o=z}else{var
q=j[1];if(0===q[0])var
F=q[1],G=[0,[0,F],F];else
var
H=q[1],G=[0,[1,H],ae(H)[1]];var
o=G}var
A=o[2],K=o[1],B=c(e[22][1],w);if(1-(A===B?1:0)){if(0===j[0])var
C=j[1];else{var
E=j[1];if(0===E[0])throw[0,m,qI];var
C=d5(k,E[1])}var
L=c(d[22],qE),M=c(d[16],B),N=c(d[22],qF),O=c(d[16],A),P=c(d[22],qG),Q=c(r[26],C),R=c(d[22],qH),S=a(d[12],R,Q),T=a(d[12],S,P),U=a(d[12],T,O),V=a(d[12],U,N),W=a(d[12],V,M),X=a(d[12],W,L);f(l[5],k,0,X)}var
Y=function(a){return bw(b,a)};return[2,K,a(e[22][68],Y,w)]}}function
fT(c,b){var
d=b[2],f=b[1];function
g(a){return ap(c)}var
h=a(e[24][2],f,g);return af(function(a){return[0,y(h,a)[1+a]]},d)}function
ir(c,b){var
d=b[2],f=b[1];function
g(a){return ap(c)}var
h=a(e[24][2],f,g);return af(function(a){if(0===a[0])return[0,a[1]];var
b=a[1];return[0,y(h,b)[1+b]]},d)}function
is(d,b){var
f=0===b[0]?b[1]:ae(b[1])[1];function
g(a){return ap(d)}var
c=a(e[24][2],f,g);function
h(a){return[0,a]}return[0,c,[2,b,a(e[24][56],h,c)]]}function
bS(q,p){var
a=p;for(;;){switch(a[0]){case
0:var
h=de(a[1],q[2]),i=h[2],r=h[1];if(i){var
a=i[1];continue}return[0,r];case
2:var
j=a[1];if(0!==j[0]){var
k=j[1],s=a[2],d=ae(k)[2];if(typeof
d==="number")var
b=0;else
if(0===d[0])if(d[1])var
l=1,b=1;else
var
b=0;else
var
b=0;if(!b)var
l=0;if(l){var
f=ae(k)[2];if(typeof
f!=="number"&&0===f[0]){var
g=f[1];if(g){var
n=g[1],o=c(e[24][12],s),a=af(function(b){return function(a){return y(b,a)[1+a]}}(o),n);continue}}throw[0,m,qK]}return a}break}return a}}function
eb(c,g){var
b=g;for(;;)switch(b[0]){case
0:var
d=de(b[1],c[2]),f=d[2],h=d[1];if(f){var
b=f[1];continue}return[0,h];case
1:var
i=b[2],j=eb(c,b[1]);return[1,j,eb(c,i)];default:var
k=b[2],l=b[1],m=function(a){return eb(c,a)};return[2,l,a(e[22][68],m,k)]}}function
cD(d,i){var
j=eb(d,i);function
g(d,b,a){return f(cA,b,c(h[1][8],d),a)}var
b=[0,f(h[1][11][12],g,d[3][1],cB)];return cx(function(h){if(a(io,h,c(e[3],b)))return a(fR,h,c(e[3],b));var
d=0;for(;;){var
i=d/26|0,j=a(e[4],97,d%26|0),k=c(dV[1],j),l=a(e[20][1],1,k),m=0===i?qv:c(bP[22],i),g=a(bP[17],l,m),n=c(e[3],b);if(a(qu,function(c){return function(d,b){return a(e[20][34],c,b)}}(g),n)){var
d=d+1|0;continue}b[1]=f(cA,h,g,c(e[3],b));return g}},j)}var
it=[dI,qL,dF(0)];function
fU(d,c,h){var
f=h;for(;;){var
b=bS(d,f);switch(b[0]){case
0:var
g=c===b[1]?1:0;if(g)throw it;return g;case
1:var
i=b[2];fU(d,c,b[1]);var
f=i;continue;default:var
j=b[2],k=function(a){return fU(d,c,a)};return a(e[22][11],k,j)}}}var
dg=[dI,qM,dF(0)];function
qN(c,a,b){var
d=bS(c,b);if(0===d[0]){var
e=d[1],f=1-(a===e?1:0);return f?qp(a,e,c[2]):f}try{fU(c,a,b);var
g=qs(a,b,c[2]);return g}catch(c){c=v(c);if(c===it)throw[0,dg,[0,a],b];throw c}}function
iu(d,c,b){if(0===c[0]){var
e=c[1];if(0===b[0])return e===b[1]?1:0}else{var
f=c[1];if(0!==b[0])return a(d,f,b[1])}return 0}function
ec(c,m,l){var
i=m,g=l;for(;;){var
b=bS(c,i),a=bS(c,g);switch(b[0]){case
0:var
k=a,j=b[1],d=2;break;case
1:var
n=b[2],o=b[1];switch(a[0]){case
0:var
d=0;break;case
1:var
p=a[2];ec(c,o,a[1]);var
i=n,g=p;continue;default:var
d=1}break;default:var
q=b[2],r=b[1];switch(a[0]){case
0:var
d=0;break;case
1:var
d=1;break;default:var
s=a[2];if(iu(h[15][10],r,a[1])){var
t=function(b,a){return ec(c,b,a)};return f(e[22][17],t,q,s)}throw[0,dg,i,g]}}switch(d){case
0:var
k=b,j=a[1];break;case
1:throw[0,dg,i,g]}return qN(c,j,k)}}function
aY(i,e,h,g){try{var
b=ec(e,h,g);return b}catch(b){b=v(b);if(b[1]===dg){var
j=cD(e,g),k=c(d[13],0),m=c(d[3],qO),n=c(d[13],0),o=cD(e,h),p=c(d[13],0),q=c(d[3],qP),r=a(d[12],q,p),s=a(d[12],r,o),t=a(d[12],s,n),u=a(d[12],t,m),w=a(d[12],u,k),x=a(d[12],w,j);return f(l[5],i,0,x)}throw b}}function
qQ(i,e,h,o){var
k=h,b=o,j=0;for(;;){var
g=bS(e,k);if(b)switch(g[0]){case
0:var
p=b[2],q=b[1][2],r=g[1],m=[0,ap(e)];aY(i,e,[0,r],[1,q,m]);var
k=m,b=p,j=1;continue;case
1:var
n=b[1],s=b[2],t=g[2];aY(n[1],e,n[2],g[1]);var
k=t,b=s,j=1;continue;default:if(j){var
u=c(d[3],qR),v=c(d[13],0),w=cD(e,h),x=c(d[13],0),y=c(d[3],qS),z=a(d[12],y,x),A=a(d[12],z,w),B=a(d[12],A,v),C=a(d[12],B,u);return f(l[5],i,0,C)}var
D=c(d[3],qT),E=c(d[13],0),F=cD(e,h),G=c(d[13],0),H=c(d[3],qU),I=a(d[12],H,G),J=a(d[12],I,F),K=a(d[12],J,E),L=a(d[12],K,D);return f(l[5],i,0,L)}return g}}function
cf(b){switch(b[0]){case
0:var
f=0===b[1][0]?0:1;break;case
6:var
h=b[1];if(0===h[0])return a(e[22][21],cf,b[3]);if(b[3]){var
k=b[3],c=ae(h[1])[2];if(typeof
c==="number")var
g=0;else
switch(c[0]){case
0:throw[0,m,qV];case
2:var
i=c[1],j=function(a){return 1-a[2]},d=a(e[22][21],j,i),g=1;break;default:var
g=0}if(!g)var
d=1;return d?a(e[22][21],cf,k):d}return 1;case
10:return a(e[22][21],cf,b[2]);case
4:case
5:var
f=1;break;case
1:case
2:case
3:var
f=0;break;default:return 0}return f?0:1}function
ed(d,h,g){var
b=h,c=g;for(;;)switch(b[0]){case
0:return a(d,b[1],c);case
1:var
i=b[1],j=ed(d,b[2],c),b=i,c=j;continue;default:var
k=b[2],l=function(b,a){return ed(d,a,b)};return f(e[22][15],l,c,k)}}function
bx(a){return[0,0,af(function(a){return[0,[0,a]]},a)]}function
qW(a){return c(d[22],qX)}var
iw=p(iv[1],qZ,qY,0,qW);function
q0(a){return c(d[22],q1)}var
ee=p(iv[1],q3,q2,0,q0);function
fV(j,i,h){var
c=bS(i,h);switch(c[0]){case
0:var
d=1;break;case
1:var
d=0;break;default:var
f=c[1];if(0===f[0])if(0===f[1])if(c[2])var
b=0;else
var
g=1,b=1;else
var
b=0;else
var
b=0;if(!b)var
g=0;var
d=g}var
e=1-d;return e?a(iw,j,0):e}function
ix(j,i){var
e=bu(0),c=bS(e,fT(e,i));switch(c[0]){case
0:var
d=1;break;case
1:var
d=0;break;default:var
g=c[1];if(0===g[0])if(0===g[1])if(c[2])var
b=0;else
var
h=1,b=1;else
var
b=0;else
var
b=0;if(!b)var
h=0;var
d=h}var
f=1-d;return f?a(iw,j,0):f}function
fW(b){return b?a(ee,b[1][1][2],0):0}function
iy(j,e){if(0===e[0]){var
b=e[1],g=c(r[34],b);if(c(r[32],b))if(c(j,g))return[1,a(i[1],b[2],g)];try{var
o=ct(b),h=o}catch(e){e=v(e);if(e!==D)throw e;var
k=c(r[26],b),m=c(d[3],q9),n=a(d[12],m,k),h=f(l[5],b[2],0,n)}return[0,h]}return[0,e[1]]}function
fX(c,b){return iy(function(b){return a(h[1][11][3],b,c[1])},b)}function
cE(m,b){if(0===b[0]){var
e=b[1];try{var
k=[0,fH(e)],g=k}catch(a){a=v(a);if(a!==D)throw a;var
g=0}if(g)return[1,g[1]];var
h=c(r[26],e),i=c(d[3],q_),j=a(d[12],i,h);return f(l[5],e[2],0,j)}return b[1]}function
fY(b){if(0===b[0]){var
e=b[1];try{var
k=fK(e),g=k}catch(b){b=v(b);if(b!==D)throw b;var
h=c(d[3],q$),i=c(r[26],e),j=a(d[12],i,h),g=f(l[5],e[2],0,j)}return fy(g)}return fy(b[1])}function
ra(b,a){return 0===a[0]?[0,[0,[0,a[1]]],[2,[1,qh],0]]:[0,[0,[1,a[1]]],[2,[1,qj],0]]}function
fZ(h,g,e){function
b(b){if(0===b[0]){var
e=c(d[16],b[1]),f=c(d[3],rb);return a(d[12],f,e)}var
g=cw(b[1]),h=c(d[3],rc);return a(d[12],h,g)}var
i=b(e),j=c(d[3],rd),k=b(g),m=c(d[3],re),n=a(d[12],m,k),o=a(d[12],n,j),p=a(d[12],o,i);return f(l[5],h,0,p)}function
ef(h,g){var
b=g[1],i=g[2];switch(b[0]){case
0:return[0,b[1]];case
1:var
j=b[2],k=cE(h,b[1]),m=function(a){return ef(h,a)};return[1,k,a(e[22][68],m,j)];default:var
n=c(d[3],rf);return f(l[5],i,0,n)}}function
iz(c,b){return b?a(h[1][10][4],b[1],c):c}function
dh(c,i){var
d=i;for(;;){var
b=d[1];switch(b[0]){case
0:var
g=b[1];return g?a(h[1][10][4],g[1],c):c;case
1:return f(e[22][15],dh,c,b[2]);default:var
d=b[1];continue}}}function
iA(b){var
a=b[1];return 2===a[0]?[0,a[1],[0,a[2]]]:[0,b,0]}function
iB(g,d){function
j(f,m){var
b=m[1],g=f[1],i=b[1],n=f[2];if(0===i[0])var
d=i[1],j=0;else
var
k=function(b){var
c=a(h[1][10][3],b,g);if(c)return c;try{ct(a(r[31],0,b));var
d=1;return d}catch(a){a=v(a);if(a===D)return 0;throw a}},l=c(h[1][6],rg),e=a(f0[25],l,k),d=[0,e],j=[0,[0,a(r[31],b[2],e)]];return[0,iz(dh(g,b),d),[0,[0,d,b,j],n]]}var
b=f(e[22][15],j,[0,g,0],d)[2];function
k(d,c){var
e=c[3],g=c[2];if(e){var
b=e[1],f=0===b[0]?b[1][2]:0,h=[8,a(i[1],f,[1,b]),[0,[0,g,d],0]];return a(i[1],f,h)}return d}function
l(a){return f(e[22][15],k,a,b)}function
m(a){return a[1]}return[0,a(e[22][14],m,b),l]}function
rh(c,b){var
a=fX(c,b);if(0===a[0])if(0!==a[1][0])return 1;return 0}function
rl(o,n,t){function
u(b){var
a=b[1],c=b[2],d=0===a[0]?a[1][2]:0;return[0,d,fY(a),c]}var
g=a(e[22][68],u,t);if(g)var
b=g[1][2][2];else
var
I=c(d[3],rU),b=f(l[5],n,0,I);var
p=ae(b),j=p[2],q=p[1];if(typeof
j!=="number"&&2===j[0]){var
k=j[1],v=function(a){return ap(o)},r=a(e[24][2],q,v),i=cZ(c(e[22][1],k),0),x=function(m){var
g=m[2],n=m[1],p=m[3];if(a(h[15][10],b,g[2])){var
j=g[5];if(y(i,j)[1+j]){var
q=a(e[22][7],k,g[5])[1],s=c(d[3],rO),t=c(h[1][9],q),u=c(d[3],rP),v=a(d[12],u,t),w=a(d[12],v,s);return f(l[5],n,0,w)}var
x=g[3],z=[0,bT(o,p,af(function(a){return[0,y(r,a)[1+a]]},x))];y(i,j)[1+j]=z;return 0}var
A=cw(g[2]),B=c(d[3],rQ),C=c(d[3],rR),D=a(d[12],C,B),E=a(d[12],D,A);return f(l[5],n,0,E)};a(e[22][11],x,g);var
z=function(b,a){return c(w[3],a)},s=a(e[24][41],z,i);if(s){var
A=a(e[22][7],k,s[1])[1],B=c(d[3],rS),C=c(h[1][9],A),D=c(d[3],rT),E=a(d[12],D,C),F=a(d[12],E,B);f(l[5],n,0,F)}var
G=a(e[24][56],w[7],i),H=function(a){return[0,y(r,a)[1+a]]};return[0,[6,[1,b],0,G],[2,[1,b],a(e[22][56],q,H)]]}throw[0,m,rN]}function
iC(g,r,i,d){if(0===i[0]){var
h=i[1];if(h===c(e[22][1],d)){var
s=function(a){return[0,ap(g)]},k=a(e[22][56],h,s),t=function(b,a){return bT(g,b,a)};return[0,[6,[0,h],0,f(e[22][69],t,d,k)],[2,[0,h],k]]}throw[0,m,rM]}var
j=i[1],b=b$(j),l=c(e[22][1],b[3]);if(l===c(e[22][1],d)){var
u=function(a){return ap(g)},n=a(e[24][2],b[1],u),v=function(a){return[0,y(n,a)[1+a]]},w=b[3],x=function(a){return af(v,a)},z=a(e[22][68],x,w),A=function(a){return[0,y(n,a)[1+a]]},B=a(e[22][56],b[1],A),o=[2,[1,b[2]],B],C=function(b,a){return bT(g,b,a)},p=f(e[22][69],C,d,z),q=b[4];return q?[0,[6,[1,b[2]],q[1],p],o]:[0,[10,j,p],o]}return ea(r,j,l,c(e[22][1],d))}function
rk(b,n,P,k){var
aj=X(b,P),o=aj[2],x=aj[1];function
ak(a,e){var
b=c(d[3],rB);return f(l[5],a,0,b)}if(k){var
ah=k[1],v=k[2];for(;;){var
N=ef(b,ah[1]);if(0===N[0]){if(v){var
ah=v[1],v=v[2];continue}var
i=1}else{var
ai=N[1];if(0===ai[0])var
i=[0,[0,c(e[22][1],N[2])]];else
var
O=b$(ai[1]),i=c(w[3],O[4])?[1,O[2]]:[0,[1,O[2]]]}break}}else
var
i=0;if(typeof
i==="number"){if(0===i){var
ac=bS(b,o);switch(ac[0]){case
0:var
aP=c(d[3],q4),z=f(l[5],n,0,aP),u=1;break;case
2:var
ad=ac[1];if(0===ad[0])var
u=0;else{var
ag=ad[1],M=ae(ag)[2];if(typeof
M==="number")var
L=1;else
if(1===M[0])if(M[1][1])var
L=1;else
var
z=ag,u=1,L=0;else
var
L=1;if(L)var
a0=c(d[3],q7),a1=c(d[13],0),a2=cD(b,o),a3=c(d[13],0),a4=c(d[3],q8),a5=a(d[12],a4,a3),a6=a(d[12],a5,a2),a7=a(d[12],a6,a1),a8=a(d[12],a7,a0),z=f(l[5],n,0,a8),u=1}break;default:var
u=0}if(!u)var
aQ=c(d[3],q5),aR=c(d[13],0),aS=cD(b,o),aT=c(d[13],0),aU=c(d[3],q6),aV=a(d[12],aU,aT),aW=a(d[12],aV,aS),aX=a(d[12],aW,aR),aZ=a(d[12],aX,aQ),z=f(l[5],n,0,aZ);return[0,[7,x,[1,z],[0],[0]],[0,ap(b)]]}var
al=c(e[22][5],k),a9=al[2],am=ef(b,al[1]);if(0===am[0]){var
an=am[1];fW(c(e[22][6],k));var
ao=X(bv(an,bx(o),b),a9);return[0,[5,0,[0,[0,an,x],0],ao[1]],ao[2]]}throw[0,m,rC]}else{if(0===i[0]){var
g=i[1],aq=is(b,g),a_=aq[1];aY(P[2],b,o,aq[2]);if(0===g[0])var
ar=g[1],a$=0===ar?rD:[0,0,1,[0,ar,0]],A=a$;else{var
W=ae(g[1])[2];if(typeof
W==="number")var
ab=0;else
if(1===W[0])var
Y=W[1],br=Y[1],bs=function(a){return c(e[22][1],a[2])},bt=a(e[22][68],bs,br),A=[0,Y[2],Y[3],bt],ab=1;else
var
ab=0;if(!ab)throw[0,m,rJ]}var
r=cZ(A[1],0),s=cZ(A[2],0),ba=A[3],as=[0,ap(b)],B=k;for(;;){if(B){var
at=B[2],au=B[1],Q=au[2],R=au[1],C=R[1];switch(C[0]){case
0:if(C[1])var
av=ak(R[2],0);else{fW(at);var
aw=X(b,Q),bb=aw[2],bc=aw[1],bd=function(f){return function(e,d){var
a=e[2],b=e[1];if(0===d){var
g=y(r,b)[1+b];if(c(w[3],g))y(r,b)[1+b]=[0,f];return[0,b+1|0,a]}var
h=y(s,a)[1+a];if(c(w[3],h))y(s,a)[1+a]=[0,[0,cZ(d,0),f]];return[0,b,a+1|0]}}(bc);f(e[22][15],bd,rE,ba);var
av=bb}var
S=av;break;case
1:var
ax=C[2],D=R[2],q=cE(b,C[1]);if(0===q[0])var
ay=q[1],be=function(a){return[0,a]},E=[0,[0,ay],0,a(e[22][56],ay,be)];else
var
V=b$(q[1]),bm=c(w[7],V[4]),E=[0,[1,V[2]],bm,V[3]];var
az=E[3],j=E[2],aA=E[1];if(1-iu(h[15][10],g,aA))fZ(D,g,aA);var
bf=function(a){var
b=a[1];return 0===b[0]?b[1]:ak(a[2],0)},T=a(e[22][68],bf,ax),U=c(e[22][1],T),aB=c(e[22][1],az);if(0===q[0]){if(q[1]!==U)throw[0,m,rF]}else{var
bl=q[1];if(1-(U===aB?1:0))ea(D,bl,aB,U)}var
bg=function(c,b,a){return bv(b,bx(af(function(a){return[0,y(a_,a)[1+a]]},a)),c)},aC=X(p(e[22][19],bg,b,T,az),Q),aD=aC[1],bh=aC[2];if(c(e[22][48],ax)){var
bi=y(r,j)[1+j];if(c(w[3],bi))y(r,j)[1+j]=[0,aD];else
a(ee,D,0)}else{var
bj=c(e[24][12],T),bk=y(s,j)[1+j];if(c(w[3],bk))y(s,j)[1+j]=[0,[0,bj,aD]];else
a(ee,D,0)}var
S=bh;break;default:var
bn=c(d[3],rG),S=f(l[5],n,0,bn)}aY(Q[2],b,S,as);var
B=at;continue}var
aE=function(h,e,b){if(b)return b[1];if(0===g[0])throw[0,m,rH];var
i=d9(g[1],h,e),j=c(d[3],rI),k=a(d[12],j,i);return f(l[5],n,0,k)},bo=function(b,a){return aE(b,1,a)},bp=a(e[24][16],bo,r),bq=function(b,a){return aE(b,0,a)};return[0,[7,x,g,bp,a(e[24][16],bq,s)],as]}}var
F=i[1],aF=is(b,[1,F]),aG=aF[2],bu=aF[1];aY(P[2],b,o,aG);var
Z=[0,ap(b)],t=h[18][1],G=k;for(;;){if(G){var
aH=G[2],aI=G[1],aJ=aI[2],aK=aI[1],H=ef(b,aK);if(0!==H[0]){var
_=H[1],bw=H[2],by=function(a){if(0===a[0])return a[1];var
b=c(d[3],rK);return f(l[5],n,0,b)},I=aK[2],J=0===_[0]?fZ(I,[1,F],[0,_[1]]):_[1],$=a(e[22][68],by,bw),K=b$(J);if(1-a(h[15][10],F,K[2]))fZ(I,[1,F],[1,K[2]]);var
aM=c(e[22][1],$),aN=c(e[22][1],K[3]);if(1-(aM===aN?1:0))ea(I,J,aN,aM);var
bz=function(c,b,a){return bv(b,bx(af(function(a){return[0,y(bu,a)[1+a]]},a)),c)},bA=bT(p(e[22][19],bz,b,$,K[3]),aJ,Z);if(a(h[18][3],J,t)){a(ee,I,0);var
aO=t}else
var
bB=[0,0,c(e[24][12],$),bA],aO=f(h[18][4],J,bB,t);var
t=aO,G=aH;continue}var
aL=H[1];fW(aH);var
aa=[0,t,[0,aL,bT(bv(aL,bx(aG),b),aJ,Z)]]}else
var
bC=c(d[3],rL),aa=f(l[5],n,0,bC);return[0,[11,[0,x,aa[1],aa[2]]],Z]}}}function
rj(k,j,q,i,h){function
m(e,a){var
g=a[1],h=g[1],k=a[3],m=a[2];if(0===h[0])var
b=h[1];else
var
n=c(d[3],ry),b=f(l[5],g[2],0,n);var
i=ap(e);return[0,bv(b,bx([0,i]),e),[0,j,b,m,k,i]]}var
b=f(e[22][89],m,k,i),a=b[1],n=b[2];function
o(b,g){var
h=b[4],i=b[3],j=b[1],n=g[2],o=g[1],p=b[5],q=b[2],r=h[2],k=X(a,h),e=k[2],m=k[1],s=3===m[0]?1:0;if(1-s){var
t=c(d[3],rz);f(l[5],r,0,t)}aY(j,a,e,[0,p]);if(i)aY(j,a,e,bw(a,i[1]));return[0,[0,[0,q,m],o],[0,e,n]]}var
p=f(e[22][16],o,n,rA)[1],g=X(a,h);return[0,[5,1,p,g[1]],g[2]]}function
ri(b,w,k,j,i){var
l=c(h[1][11][30],b[1]),n=a(h[1][10][7],k,l);function
o(b,c){var
d=c[1],g=b[2],i=c[2],j=b[3],h=iB(d,[0,[0,b[1],g],0]),a=h[1],k=h[2];if(a)if(!a[2]){var
l=a[1];return[0,f(e[22][15],iz,d,a),[0,[0,l,k,g,j],i]]}throw[0,m,rx]}var
p=f(e[22][16],o,j,[0,n,0])[2];function
q(d,k){var
o=d[4],p=d[3],q=d[1],B=k[3],C=k[2],E=k[1],F=d[2];if(p)var
r=bw(b,p[1]),m=bT(b,o,r),l=r;else
var
t=X(b,o),m=t[1],l=t[2];if(cf(m))var
g=function(e,a){var
c=de(e,b[2]),d=c[2],h=c[1];return d?ed(g,d[1],a):f(cA,h,0,a)},u=function(d,b,a){var
c=b[2];return ed(function(b,a){return 0===b[0]?g(b[1],a):a},c,a)},w=f(h[1][11][12],u,b[1],cB),x=function(c,b,a){return g(b,a)},y=c(e[3],b[3]),i=[0,0],j=[0,cB],z=f(h[1][11][12],x,y,w),n=function(l){var
g=de(l,b[2]),h=g[2],d=g[1];if(h)return af(n,h[1]);if(a(io,d,z))return[0,[0,d]];try{var
m=a(fR,d,c(e[3],j));return m}catch(a){a=v(a);if(a===D){var
k=[0,[1,c(e[3],i)]];i[1]++;j[1]=f(cA,d,k,c(e[3],j));return k}throw a}},A=af(n,l),s=[0,c(e[3],i),A];else
var
s=bx(l);return[0,c(F,E),[0,[0,q,m],C],[0,[0,q,s],B]]}var
d=f(e[22][16],q,p,[0,i,0,0]),r=d[3],s=d[2],t=d[1];function
u(b,a){return bv(a[1],a[2],b)}var
g=X(f(e[22][15],u,b,r),t);return[0,[5,0,s,g[1]],g[2]]}function
X(b,ai){var
o=ai;for(;;){var
j=o[2],g=o[1];switch(g[0]){case
0:return ra(b,g[1]);case
1:var
s=fX(b,g[1]);if(0===s[0]){var
t=s[1];if(0===t[0]){var
u=t[1];try{var
am=bN(u),C=am}catch(b){b=v(b);if(b!==D)throw b;var
aj=c(h[15][6],u),ak=c(d[3],rm),al=a(d[12],ak,aj),C=f(l[2],0,0,al)}return[0,[2,u],fT(b,C[2])]}var
E=t[1];try{var
ar=fA(E),F=ar}catch(b){b=v(b);if(b!==D)throw b;var
an=c(h[15][6],E),ao=c(d[3],rn),aq=a(d[12],ao,an),F=f(l[2],0,0,aq),bn=b}var
o=F;continue}var
G=s[1][1];return[0,[1,G],ir(b,a(h[1][11][23],G,b[1]))];case
2:return iC(b,j,cE(b,g[1]),0);case
3:var
as=g[2],H=a(e[22][68],iA,g[1]),at=function(c){var
a=c[2];return a?bw(b,a[1]):[0,ap(b)]},I=a(e[22][68],at,H),J=iB(c(h[1][11][30],b[1]),H),K=J[1],au=J[2],av=function(c,b,a){return bv(b,bx(a),c)},aw=p(e[22][19],av,b,K,I),L=X(aw,c(au,as)),ax=L[2],ay=L[1],az=function(b,a){return[1,b,a]};return[0,[3,K,ay],f(e[22][16],az,I,ax)];case
4:var
q=g[1],w=q[1];switch(w[0]){case
1:var
O=w[1],aG=g[2];if(rh(b,O)){var
P=fX(b,O);if(0===P[0]){var
Q=P[1];if(0!==Q[0]){var
aH=fA(Q[1]),aI=function(c){var
b=c[2],d=a(i[1],b,rq),e=[2,a(i[1],b,rr),d],f=[3,[0,a(i[1],b,e),0],c];return a(i[1],b,f)},aJ=[4,aH,a(e[22][68],aI,aG)],o=a(i[1],j,aJ);continue}}throw[0,m,rp]}break;case
2:var
aK=g[2],aL=q[2];return iC(b,aL,cE(b,w[1]),aK)}var
aA=g[2],aB=q[2],M=X(b,q),aC=M[2],aD=M[1],aE=function(c,a){var
e=a[2],f=a[1],g=c[2],d=X(b,c);return[0,[0,d[1],f],[0,[0,g,d[2]],e]]},N=f(e[22][16],aE,aA,ro),aF=N[1];return[0,[4,aD,aF],qQ(aB,b,aC,N[2])];case
5:var
R=g[3],aM=g[2],aN=g[1],aO=function(a){var
c=a[2],b=iA(a[1]);return[0,b[1],b[2],c]},x=a(e[22][68],aO,aM),aP=function(b,j){var
e=j[1],g=dh(h[1][10][1],e),i=a(h[1][10][8],g,b);if(c(h[1][10][2],i))return a(h[1][10][7],g,b);var
k=c(h[1][10][26],i),m=c(d[3],rs),n=c(h[1][9],k),o=c(d[3],rt),p=a(d[12],o,n),q=a(d[12],p,m);return f(l[5],e[2],0,q)},S=f(e[22][15],aP,h[1][10][1],x);return aN?rj(b,j,S,x,R):ri(b,j,S,x,R);case
6:var
aQ=g[2],T=X(b,g[1]),aR=T[2],aS=T[1],U=bw(b,aQ);aY(j,b,aR,U);return[0,aS,U];case
7:var
V=g[1],aT=g[2],aU=V[2],Y=X(b,V),aV=Y[2],aW=Y[1],Z=X(b,aT),aX=Z[2],aZ=Z[1];fV(aU,b,aV);return[0,[5,0,[0,[0,0,aW],0],aZ],aX];case
8:return rk(b,j,g[1],g[2]);case
9:return rl(b,j,g[1]);case
10:var
_=g[1],n=fY(g[2]),a0=_[2],$=X(b,_),a1=$[2],a2=$[1],a3=function(a){return ap(b)},aa=a(e[24][2],n[1],a3),a4=function(a){return[0,a]},a5=a(e[24][56],a4,aa);aY(a0,b,a1,[2,[1,n[2]],a5]);var
a6=function(a){return[0,y(aa,a)[1+a]]},a7=af(a6,n[3]);return[0,[8,n[2],a2,n[5]],a7];case
11:var
z=g[2],a8=g[3],a9=g[1],k=fY(z);if(1-k[4]){var
a_=0===z[0]?z[1][2]:0,a$=c(d[3],ru);f(l[5],a_,0,a$)}var
ba=function(a){return ap(b)},ab=a(e[24][2],k[1],ba),bb=function(a){return[0,a]},bc=a(e[24][56],bb,ab),bd=bT(b,a9,[2,[1,k[2]],bc]),be=function(a){return[0,y(ab,a)[1+a]]},bf=bT(b,a8,af(be,k[3]));return[0,[9,k[2],bd,k[5],bf],rv];default:var
ac=g[2],ad=g[1],ae=function(d,c){var
b=a(W[1][4],d[3],cC),e=b?b[1]:bu(0);return X(e,c)},ag=cu(ad),bg=c(bi[54],cF[38]),r=c(W[2],bg),bh=r[4],bj=f(W[1][3],r[3],cC,b),ah=[0,r[1],r[2],bj,bh];if(b[6])var
bk=function(a){return f(ag[1],ae,ah,ac)},A=f(rw[26],di[13],bk,0);else
var
A=f(ag[1],ae,ah,ac);var
B=A[1],bl=A[2],bm=0===B[0]?[12,ad,B[1]]:B[1];return[0,bm,bl]}}}function
bT(b,a,d){var
c=X(b,a),e=c[1];aY(a[2],b,c[2],d);return e}function
f1(g,d,j){var
b=d[2],h=d[1];function
k(d){try{var
j=a(fR,d,c(e[3],b));return j}catch(a){a=v(a);if(a===D){if(g[4]){var
i=[0,c(e[3],h)];h[1]++;b[1]=f(cA,d,i,c(e[3],b));return i}throw[0,m,rV]}throw a}}function
i(c){var
a=de(c,g[2]),b=a[2],d=a[1];return b?af(i,b[1]):k(d)}return af(i,j)}function
dj(h,g){var
a=bu(0),b=h?a:[0,a[1],a[2],a[3],a[4],a[5],0],d=X(b,g),f=[0,0],i=d[1],j=f1(b,[0,f,[0,cB]],d[2]);return[0,i,[0,c(e[3],f),j]]}function
f2(r,k){var
d=k[2],s=k[1],g=bu(0),b=[0,g[1],g[2],g[3],g[4],r,g[6]];function
t(a){return ip(a,b)}var
l=a(e[22][68],t,s),m=[0,c(e[22][1],l)],i=[0,cB];function
u(b,a){i[1]=f(cA,a,[0,b],c(e[3],i));return 0}a(e[22][12],u,l);var
n=[0,b[1],b[2],b[3],0,b[5],b[6]];function
j(a){return f1(n,[0,m,i],bw(n,a))}var
h=c(e[3],m);if(typeof
d==="number")return[0,h,0];else
switch(d[0]){case
0:var
o=d[1];return o?[0,h,[0,[0,j(o[1])]]]:[0,h,rW];case
1:var
v=d[1],w=function(b){var
c=b[1];return[0,c,a(e[22][68],j,b[2])]},p=a(e[22][68],w,v),x=function(a,d){var
b=a[2],c=a[1];return d[2]?[0,c,b+1|0]:[0,c+1|0,b]},q=f(e[22][15],x,rX,p);return[0,h,[1,[0,p,q[1],q[2]]]];default:var
y=d[1],z=function(a){var
b=a[2],c=a[1];return[0,c,b,j(a[3])]};return[0,h,[2,a(e[22][68],z,y)]]}}function
iD(d){var
a=bu(0),b=[0,0],f=f1(a,[0,b,[0,cB]],bw(a,d));return[0,c(e[3],b),f]}function
iE(d,b){var
c=bu(0),f=fT(c,d);function
g(b){return[2,[0,a(e[4],b,1)],0]}var
h=b[2],i=a(e[24][2],b[1],g),j=af(function(a){return y(i,a)[1+a]},h);try{ec(c,f,j);var
k=1;return k}catch(a){a=v(a);if(a[1]===dg)return 0;throw a}}function
f3(b){if(0===b[0]){var
e=b[1];try{var
k=fK(e),g=k}catch(b){b=v(b);if(b!==D)throw b;var
h=c(d[3],rY),i=c(r[26],e),j=a(d[12],i,h),g=f(l[5],e[2],0,j)}return g}return b[1]}function
ai(g,k){var
j=k[2],b=k[1];switch(b[0]){case
0:return k;case
1:var
r=b[1],m=iy(function(b){return a(h[1][10][3],b,g)},r);return 0===m[0]?a(i[1],j,[1,[1,m[1]]]):k;case
2:var
s=[2,[1,cE(0,b[1])]];return a(i[1],j,s);case
3:var
t=b[2],u=b[1],v=function(b,a){var
c=b[1],d=dh(b[2],a);return[0,[0,dk(g,a),c],d]},n=f(e[22][15],v,[0,0,g],u),w=n[2],x=c(e[22][9],n[1]),y=[3,x,ai(w,t)];return a(i[1],j,y);case
4:var
z=b[2],A=ai(g,b[1]),B=function(a){return ai(g,a)},C=[4,A,a(e[22][68],B,z)];return a(i[1],j,C);case
5:var
o=b[2],p=b[1],D=b[3],E=function(b,a){return dh(b,a[1])},F=f(e[22][15],E,h[1][10][1],o),q=a(h[1][10][7],F,g),G=ai(q,D),H=function(a){var
c=a[2],d=a[1],b=p?q:g,e=dk(b,d);return[0,e,ai(b,c)]},I=[5,p,a(e[22][68],H,o),G];return a(i[1],j,I);case
6:var
J=b[2],K=[6,ai(g,b[1]),J];return a(i[1],j,K);case
7:var
L=b[2],M=ai(g,b[1]),N=[7,M,ai(g,L)];return a(i[1],j,N);case
8:var
O=b[2],P=ai(g,b[1]),Q=function(a){var
b=a[1],c=ai(g,a[2]);return[0,dk(g,b),c]},R=[8,P,a(e[22][68],Q,O)];return a(i[1],j,R);case
9:var
S=b[1],T=function(a){var
b=a[2],c=f3(a[1]);return[0,[1,c],ai(g,b)]},U=[9,a(e[22][68],T,S)];return a(i[1],j,U);case
10:var
V=b[2],W=ai(g,b[1]),X=[10,W,[1,f3(V)]];return a(i[1],j,X);case
11:var
Y=b[3],Z=b[2],_=ai(g,b[1]),$=f3(Z),aa=[11,_,[1,$],ai(g,Y)];return a(i[1],j,aa);default:var
ab=c(aW[3],b[1]),ac=c(d[3],ab),ad=c(d[13],0),ae=c(d[3],rZ),af=a(d[12],ae,ad),ag=a(d[12],af,ac);return f(l[5],j,0,ag)}}function
dk(d,c){var
f=c[2],b=c[1];switch(b[0]){case
0:return c;case
1:var
g=b[2],h=[1,cE(0,b[1])],j=function(a){return dk(d,a)},k=[1,h,a(e[22][68],j,g)];return a(i[1],f,k);default:var
l=b[2],m=[2,dk(d,b[1]),l];return a(i[1],f,m)}}function
eg(f,e,b){if(0===b[0])return b;var
c=b[1],d=a(f,e,c);return d===c?b:[1,d]}function
by(c,b){switch(b[0]){case
0:return b;case
1:var
d=b[2],f=b[1],g=by(c,f),h=by(c,d);if(g===f)if(h===d)return b;return[1,g,h];default:var
i=b[2],j=b[1],k=eg(aI[37],c,j),m=function(a){return by(c,a)},l=a(e[22][ah][1],m,i);if(k===j)if(l===i)return b;return[2,k,l]}}function
Q(c,b){switch(b[0]){case
2:return[2,a(aI[37],c,b[1])];case
3:var
J=b[1];return[3,J,Q(c,b[2])];case
4:var
K=b[2],L=b[1],M=function(a){return Q(c,a)},N=a(e[22][68],M,K);return[4,Q(c,L),N];case
5:var
O=b[3],P=b[2],R=b[1],S=function(a){var
b=a[1];return[0,b,Q(c,a[2])]},T=a(e[22][68],S,P);return[5,R,T,Q(c,O)];case
6:var
i=b[3],j=b[1],U=b[2],k=eg(aI[37],c,j),V=function(a){return Q(c,a)},l=a(e[22][ah][1],V,i);if(k===j)if(l===i)return b;return[6,k,U,l];case
7:var
W=b[4],X=b[3],Y=b[2],Z=b[1],_=function(a){return Q(c,a)},$=a(e[24][15],_,X),aa=function(a){var
b=a[1];return[0,b,Q(c,a[2])]},ab=a(e[24][15],aa,W),ac=eg(aI[37],c,Y);return[7,Q(c,Z),ac,$,ab];case
8:var
m=b[2],n=b[1],ad=b[3],o=a(aI[37],c,n),p=Q(c,m);if(o===n)if(p===m)return b;return[8,o,p,ad];case
9:var
q=b[4],r=b[2],s=b[1],ae=b[3],t=a(aI[37],c,s),u=Q(c,r),v=Q(c,q);if(t===s)if(u===r)if(v===q)return b;return[9,t,u,ae,v];case
10:var
w=b[2],x=b[1],y=a(aI[37],c,x),af=function(a){return Q(c,a)},z=a(e[22][ah][1],af,w);if(y===x)if(z===w)return b;return[10,y,z];case
11:var
d=b[1],A=d[3],B=A[2],g=d[2],C=d[1],ag=A[1],D=Q(c,C),E=Q(c,B),ai=function(d,b,e){var
g=b[3],k=b[2],l=b[1],i=a(aI[37],c,d),j=Q(c,g);if(i===d)if(j===g)return e;var
m=a(h[18][6],d,e);return f(h[18][4],i,[0,l,k,j],m)},F=f(h[18][12],ai,g,g);if(D===C)if(F===g)if(E===B)return b;return[11,[0,D,F,[0,ag,E]]];case
12:var
G=b[2],H=b[1],I=a(cu(H)[2],c,G);return I===G?b:[12,H,I];default:return b}}function
iF(g,f){var
b=f[2],p=f[1];if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
h=b[1],m=function(a){return by(g,a)},i=a(w[28][1],m,h),c=i===h?b:[0,i];break;case
1:var
d=b[1],n=function(b){var
c=b[2],f=b[1];function
h(a){return by(g,a)}var
d=a(e[22][ah][1],h,c);return d===c?b:[0,f,d]},j=a(e[22][ah][1],n,d[1]),c=j===d[1]?b:[1,[0,j,d[2],d[3]]];break;default:var
k=b[1],o=function(a){var
b=a[3],d=a[2],e=a[1],c=by(g,b);return c===b?a:[0,e,d,c]},l=a(e[22][ah][1],o,k),c=l===k?b:[2,l]}return c===b?f:[0,p,c]}function
iG(d,a){var
b=a[2],e=a[1],c=by(d,b);return c===b?a:[0,e,c]}function
f4(d,a){if(0===a[0])return a;var
b=a[1],c=eg(aI[37],d,b);return c===b?a:[1,c]}function
dl(d,b){var
f=b[2],c=b[1];switch(c[0]){case
0:return b;case
1:var
g=c[2],h=c[1],j=dl(d,h),k=dl(d,g);if(j===h)if(k===g)return b;return a(i[1],f,[1,j,k]);default:var
l=c[2],m=c[1],n=f4(d,m),p=function(a){return dl(d,a)},o=a(e[22][ah][1],p,l);if(n===m)if(o===l)return b;return a(i[1],f,[2,n,o])}}function
f5(e,b){if(0===b[0])return b;var
c=b[1],d=a(aI[37],e,c);return d===c?b:[1,d]}function
dm(d,b){var
f=b[2],c=b[1];switch(c[0]){case
0:return b;case
1:var
g=c[2],h=c[1],p=function(a){return dm(d,a)},j=a(e[22][ah][1],p,g),k=f4(d,h);if(j===g)if(k===h)return b;return a(i[1],f,[1,k,j]);default:var
l=c[2],m=c[1],n=dm(d,m),o=dl(d,l);if(n===m)if(o===l)return b;return a(i[1],f,[2,n,o])}}function
aj(b,d){var
f=d[2],c=d[1];switch(c[0]){case
0:return d;case
1:var
g=c[1];if(0===g[0])var
h=g;else{var
j=g[1];if(0===j[0])var
k=j[1],l=a(aI[37],b,k),h=l===k?g:[1,[0,l]];else
var
n=j[1],o=a(aI[37],b,n),h=o===n?g:[1,[1,o]]}return h===g?d:a(i[1],f,[1,h]);case
2:var
p=c[1],q=f4(b,p);return q===p?d:a(i[1],f,[2,q]);case
3:var
r=c[2],s=c[1],$=function(a){return dm(b,a)},t=a(e[22][ah][1],$,s),u=aj(b,r);if(t===s)if(u===r)return d;return a(i[1],f,[3,t,u]);case
4:var
v=c[2],w=c[1],x=aj(b,w),aa=function(a){return aj(b,a)},y=a(e[22][ah][1],aa,v);if(x===w)if(y===v)return d;return a(i[1],f,[4,x,y]);case
5:var
z=c[3],A=c[2],ab=c[1],ac=function(a){var
c=a[2],d=a[1],e=dm(b,d),f=aj(b,c);if(e===d)if(f===c)return a;return[0,e,f]},B=a(e[22][ah][1],ac,A),C=aj(b,z);if(B===A)if(C===z)return d;return a(i[1],f,[5,ab,B,C]);case
6:var
D=c[2],E=c[1],F=aj(b,E),G=dl(b,D);if(G===D)if(F===E)return d;return a(i[1],f,[6,F,G]);case
7:var
H=c[2],I=c[1],J=aj(b,I),K=aj(b,H);if(J===I)if(K===H)return d;return a(i[1],f,[7,J,K]);case
8:var
L=c[2],M=c[1],ad=function(a){var
c=a[2],d=a[1],e=dm(b,d),f=aj(b,c);if(e===d)if(f===c)return a;return[0,e,f]},N=aj(b,M),O=a(e[22][ah][1],ad,L);if(N===M)if(O===L)return d;return a(i[1],f,[8,N,O]);case
9:var
P=c[1],ae=function(a){var
c=a[2],d=a[1],e=f5(b,d),f=aj(b,c);if(e===d)if(f===c)return a;return[0,e,f]},Q=a(e[22][ah][1],ae,P);return Q===P?d:a(i[1],f,[9,Q]);case
10:var
R=c[2],S=c[1],T=f5(b,R),U=aj(b,S);if(T===R)if(U===S)return d;return a(i[1],f,[10,U,T]);case
11:var
V=c[3],W=c[2],X=c[1],Y=f5(b,W),Z=aj(b,X),_=aj(b,V);if(Y===W)if(Z===X)if(_===V)return d;return a(i[1],f,[11,Z,Y,_]);default:throw[0,m,r0]}}function
r1(g,d){var
h=d[2],n=d[1];function
o(a){return a[1]}var
i=a(e[22][68],o,n),j=a(W[1][4],g[3],cC);if(j)var
k=j[1];else
var
b=bu(0),s=c(e[3],di[13])?b:[0,b[1],b[2],b[3],b[4],b[5],0],k=s;function
p(b,a){return bv([0,a],bx([2,[1,qf],0]),b)}var
l=f(e[22][15],p,k,i),q=h[2],m=X(l,h),r=m[1];fV(q,l,m[2]);return[0,g,[0,i,r]]}a(W[9],ca,r1);function
r2(d,g){var
i=a(W[1][4],d[3],cC);if(i)var
j=i[1];else
var
b=bu(0),s=c(e[3],di[13])?b:[0,b[1],b[2],b[3],b[4],b[5],0],j=s;function
o(b,g,c){var
d=c[2],e=c[1];if(1-a(h[1][11][3],b,d[1])){var
f=bv([0,b],bx([2,[1,im],0]),d);return[0,a(h[1][10][4],b,e),f]}throw[0,m,r3]}var
k=f(h[1][11][12],o,d[4][2],[0,h[1][10][1],j]),l=k[2],p=k[1],q=g[2],n=X(l,g),r=n[1];fV(q,l,n[2]);return[0,d,[0,p,r]]}a(W[9],cb,r2);function
r4(b,a){var
c=a[1];return[0,c,Q(b,a[2])]}a(W[10],ca,r4);function
r5(b,a){var
c=a[1];return[0,c,Q(b,a[2])]}a(W[10],cb,r5);function
r6(j,m){var
i=m[2],k=m[1],n=a(W[1][4],j[3],cC);if(n)var
g=n[1];else
var
b=bu(0),t=c(e[3],di[13])?b:[0,b[1],b[2],b[3],b[4],b[5],0],g=t;if(a(h[1][11][3],i,j[4][2]))aY(k,g,[2,[1,im],0],[2,[1,il],0]);try{var
s=a(h[1][11][23],i,g[1]),o=s}catch(b){b=v(b);if(b!==D)throw b;var
p=c(h[1][9],i),q=c(d[3],r7),r=a(d[12],q,p),o=f(l[5],k,0,r)}aY(k,g,ir(g,o),[2,[1,il],0]);return[0,j,i]}a(W[9],cc,r6);function
r8(b,a){return a}a(W[10],cc,r8);aD(1227,[0,dj,f2,iD,cf,ix,iE,by,Q,iF,iG,aj,ai,ea,iq,fS],"Ltac2_plugin__Tac2intern");var
iH=c(aJ[86][1],0),cG=[0,0];function
r9(d){var
e=c(aJ[87],d),b=a(aJ[86][4],e,iH);return b?c(j[16],b[1]):c(j[16],0)}var
f6=a(j[72][1],j[54],r9);function
iI(d){function
b(b){var
e=c(aJ[87],b),g=f(aJ[86][3],e,iH,d),h=a(aJ[88],g,b);return c(j[65][1],h)}return a(j[72][1],j[54],b)}function
eh(f,b){if(c(e[3],cG)){var
d=function(d){function
e(f){function
e(b){function
e(a){return c(j[16],b)}var
f=iI(d);return a(j[72][1],f,e)}return a(j[72][1],b,e)}var
g=iI([0,f,d]);return a(j[72][1],g,e)};return a(j[72][1],f6,d)}return b}var
ei=[0,h[1][11][1]];function
cH(b,a,c){return a?[0,f(h[1][11][4],a[1],c,b[1])]:b}function
r_(b,e){try{var
k=a(h[1][11][23],e,b[1]);return k}catch(b){b=v(b);if(b===D){var
g=c(h[1][9],e),i=c(d[3],r$),j=a(d[12],i,g);return f(l[2],0,0,j)}throw b}}function
sa(k,e){try{var
b=bN(e)[1];return b}catch(b){b=v(b);if(b===D){var
g=c(h[15][6],e),i=c(d[3],sb),j=a(d[12],i,g);return f(l[2],0,0,j)}throw b}}var
a5=j[16];function
L(w,v){var
g=w,b=v;for(;;)switch(b[0]){case
0:var
i=b[1];return 0===i[0]?c(a5,[0,i[1]]):c(a5,[2,c(ce[5],i[1])]);case
1:return c(a5,r_(g,b[1]));case
2:var
m=b[1];return c(a5,iJ([0,m],sa(g,m)));case
3:return c(a5,[3,f7([0,g[1],b[1],b[2],0])]);case
4:var
x=b[2],z=b[1],A=function(b){function
c(a){return bM(c5(b),a)}function
d(a){return L(g,a)}var
e=a(j[20][5][1],d,x);return a(j[72][1],e,c)},B=L(g,z);return a(j[72][1],B,A);case
5:if(0===b[1]){var
C=b[3],D=b[2],E=function(d,b){var
e=b[2],f=b[1];function
h(a){return c(a5,cH(d,f,a))}var
i=L(g,e);return a(j[72][1],i,h)},F=function(a){return L(a,C)},G=f(j[20][5][4],E,g,D);return a(j[72][1],G,F)}var
H=b[3],I=b[2],J=function(i){return function(b){var
a=b[2],g=b[1];if(3===a[0]){var
e=[0,i[1],a[1],a[2],0];return[0,g,e,[3,f7(e)]]}var
h=c(d[3],sd);return f(l[2],0,0,h)}}(g),n=a(e[22][68],J,I),K=function(b,a){var
c=a[1],d=a[3];return c?[0,f(h[1][11][4],c[1],d,b[1])]:b},o=f(e[22][15],K,g,n),M=function(b){return function(a){a[2][1]=b[1];return 0}}(o);a(e[22][11],M,n);var
g=o,b=H;continue;case
6:var
q=b[3],r=b[2];if(q){var
N=function(b){var
d=c(e[24][12],b);return c(a5,a(aX[5],r,d))},P=function(a){return L(g,a)},Q=a(j[20][5][1],P,q);return a(j[72][1],Q,N)}return c(a5,c(aX[6],r));case
7:var
R=b[4],S=b[3],T=b[1],U=function(a){if(c(aX[1],a)){var
b=O(a);return L(g,y(S,b)[1+b])}var
d=aP(a),e=d[1],h=d[2],f=y(R,e)[1+e],i=f[2];return L(p(iL[51],cH,g,f[1],h),i)},V=L(g,T);return a(j[72][1],V,U);case
8:var
W=b[3],X=b[2],Y=function(b){return c(a5,a(aX[3],b,W))},Z=L(g,X);return a(j[72][1],Z,Y);case
9:var
_=b[4],$=b[3],aa=b[2],ab=function(b){function
d(a){f(aX[4],b,$,a);return c(a5,c(aX[6],0))}var
e=L(g,_);return a(j[72][1],e,d)},ac=L(g,aa);return a(j[72][1],ac,ab);case
10:var
ad=b[2],ae=b[1],af=function(a){return c(a5,c7([0,ae,c(e[24][12],a)]))},ag=function(a){return L(g,a)},ah=a(j[20][5][1],ag,ad);return a(j[72][1],ah,af);case
11:var
k=b[1],ai=k[3],aj=k[2],ak=k[1],al=function(a){return sc(g,a,aj,ai)},am=L(g,ak);return a(j[72][1],am,al);case
12:var
s=b[2],t=b[1];return eh([3,t,s],a(cu(t)[3],g,s));default:var
u=b[1],an=b[2],ao=function(a){return eh([2,u],bM(fD(u),a))},ap=function(a){return L(g,a)},aq=a(j[20][5][1],ap,an);return a(j[72][1],aq,ao)}}function
f7(a){function
b(d){var
b=a[4],c=a[3],f=a[1],g=a[2],h=b?[0,b[1]]:[1,c];return eh(h,L(p(e[22][19],cH,[0,f],g,d),c))}return d2(c(e[22][1],a[2]),b)}function
sc(f,b,i,e){var
g=dZ(b),j=g[2],k=g[1];try{var
q=[0,a(h[18][23],k,i)],c=q}catch(a){a=v(a);if(a!==D)throw a;var
c=0}if(c){var
d=c[1],l=d[3],m=d[2],n=cH(f,d[1],b);return L(p(iL[51],cH,n,m,j),l)}var
o=e[2];return L(cH(f,e[1],b),o)}function
iJ(p,o){var
g=p,b=o;for(;;){switch(b[0]){case
0:var
i=b[1];if(0===i[0])return c(aX[6],i[1]);break;case
2:var
j=b[1];try{var
r=bN(j)}catch(a){a=v(a);if(a===D)throw[0,m,sf];throw a;var
u=a}var
g=[0,j],b=r[1];continue;case
3:return[3,f7([0,h[1][11][1],b[1],b[2],g])];case
6:var
k=b[3],n=b[2];if(k){var
s=a(e[24][57],iK,k);return a(aX[5],n,s)}return c(aX[6],n);case
10:var
t=b[1];return c7([0,t,a(e[24][57],iK,b[2])])}var
q=c(d[3],se);return f(l[2],0,0,q)}}function
iK(a){return iJ(0,a)}var
iM=c(bh[1][1],sg),iN=c(h[1][7],sh);function
si(b){var
c=b[2];if(a(bh[1][2],b[1],iM))return c;throw[0,m,sj]}function
f8(b){try{var
c=si(a(h[1][11][23],iN,b));return c}catch(a){a=v(a);if(a===D)return ei;throw a}}function
ej(b,a){return f(h[1][11][4],iN,[0,iM,b],a)}aD(1230,[0,ei,L,f8,ej,bn,f6,eh,cG],"Ltac2_plugin__Tac2interp");var
sl=c(g[2][1],sk),sn=c(g[2][1],sm),sp=c(g[2][1],so),sr=c(g[2][1],sq),st=c(g[2][1],ss),sv=c(g[2][1],su),sx=c(g[2][1],sw),sz=c(g[2][1],sy),sB=c(g[2][1],sA),sD=c(g[2][1],sC),sF=c(g[2][1],sE),sH=c(g[2][1],sG),sJ=c(g[2][1],sI),sL=c(g[2][1],sK),sN=c(g[2][1],sM),sP=c(g[2][1],sO),sR=c(g[2][1],sQ),sT=c(g[2][1],sS),sV=c(g[2][1],sU),sX=c(g[2][1],sW),sZ=c(g[2][1],sY),q=[0,sl,sn,sp,sr,st,sv,sx,sz,sB,sD,sF,sH,sJ,sL,sN,sP,sR,sT,sV,sX,sZ,c(g[2][1],s0)];function
iO(e,b){var
a=b[2],c=b[1],d=c[2],f=c[1];if(1-a[1])da(e,f,[0,d]);return d3(d,[0,a[3],a[4],a[2]])}function
s1(b,a){return iO([0,b],a)}function
s2(b,a){return iO([1,b],a)}function
s3(b){var
a=b[2],c=b[1],d=c[2];da(s4,c[1],[0,d]);return d3(d,[0,a[3],a[4],a[2]])}function
s5(b){var
a=b[2],c=b[1],d=Q(c,a[3]),e=iG(c,a[4]);if(d===a[3])if(e===a[4])return a;return[0,a[1],a[2],d,e]}function
s6(a){return[0,a]}var
f9=c(aK[1],s7),iP=c(aK[4],[0,f9[1],s3,s1,s2,s6,s5,f9[7],f9[8]]);function
cI(d,b){var
e=c(h[15][3],d),f=c(h[8][5],b);return a(h[15][1],e,f)}function
f_(d,b){var
e=c(r[16],d)[1];return a(r[15],e,b)}function
iQ(d,c,b,g){var
f=g[2];if(typeof
f==="number")return db(d,c,b);else
switch(f[0]){case
0:return db(d,c,b);case
1:var
h=f[1][1],i=function(e){var
a=e[1],f=f_(c,a);return fG(d,f,cI(b,a))};db(d,c,b);return a(e[22][11],i,h);default:var
j=f[1],k=function(e){var
a=e[1],f=f_(c,a);return id(d,f,cI(b,a))};db(d,c,b);return a(e[22][11],k,j)}}function
iR(a){var
b=c(e[3],a);a[1]++;return b}function
iS(b,d){var
f=d[2],g=d[1];if(typeof
f==="number")return c_(b,d);else
switch(f[0]){case
0:return c_(b,d);case
1:var
h=f[1][1],i=[0,0],j=[0,0],k=function(a){var
d=a[2],f=cI(b,a[1]),h=c(e[22][48],d)?iR(i):iR(j);return fx(f,[0,g,b,d,[0,h]])};c_(b,d);return a(e[22][11],k,h);default:var
l=f[1],m=function(c,a){var
d=a[3],e=a[2];return ia(cI(b,a[1]),[0,g,b,d,e,c])};c_(b,d);return a(e[22][12],m,l)}}function
iT(e,b){var
a=b[2],c=b[1],d=c[2],f=c[1];if(1-a[1])iQ(e,f,d,a[2]);return iS(d,a[2])}function
s8(b,a){return iT([0,b],a)}function
s9(b,a){return iT([1,b],a)}function
s_(a){var
b=a[2],c=a[1],d=c[2];iQ(s$,c[1],d,b[2]);return iS(d,b[2])}function
ta(b){var
a=b[2],c=iF(b[1],a[2]);return c===a[2]?a:[0,a[1],c]}function
tb(a){return[0,a]}var
f$=c(aK[1],tc),ga=c(aK[4],[0,f$[1],s_,s8,s9,tb,ta,f$[7],f$[8]]);function
iU(f,d,c,b){function
g(a){var
b=f_(d,a[1]);return fG(f,b,cI(c,a[1]))}return a(e[22][11],g,b[4])}function
iV(c,b){function
d(a){var
d=cI(c,a[1]);return fx(d,[0,b[2],b[3],a[2],0])}return a(e[22][11],d,b[4])}function
td(a){var
b=a[2],c=a[1],d=c[2],e=c[1];iV(d,b);return iU(te,e,d,b)}function
iW(e,b){var
a=b[2],c=b[1],d=c[2],f=c[1];if(1-a[1])iU(e,f,d,a);return iV(d,a)}function
tf(b,a){return iW([0,b],a)}function
tg(b,a){return iW([1,b],a)}function
th(c){var
b=c[2],d=c[1];function
h(b){var
f=b[2];function
g(a){return by(d,a)}var
c=a(e[22][ah][1],g,f);return c===b[2]?b:[0,b[1],c]}var
f=a(aI[37],d,b[3]),g=a(e[22][ah][1],h,b[4]);if(f===b[3])if(g===b[4])return b;return[0,b[1],b[2],f,g]}function
ti(a){return[0,a]}var
gb=c(aK[1],tj),tk=c(aK[4],[0,gb[1],td,tf,tg,ti,th,gb[7],gb[8]]);function
tl(b){var
a=b[1];return 2===a[0]?[0,a[1],[0,a[2]]]:[0,b,0]}function
iX(b){var
e=b[1],i=b[2],g=bs(a(r[31],0,e));if(g){var
j=c(d[3],to),k=c(h[1][9],e),m=c(d[3],tp),n=a(d[12],m,k),o=a(d[12],n,j);return f(l[5],i,0,o)}return g}function
iY(k,j,u,t){var
w=k?k[1]:0,x=j?j[1]:0;function
y(g){var
h=g[1],b=h[2],j=h[1],k=g[2];if(j)var
e=j[1];else
var
m=c(d[3],tq),e=f(l[5],b,0,m);iX(a(i[1],b,e));return[0,a(i[1],b,e),k]}var
b=a(e[22][68],y,t);if(u)var
n=h[1][10][1],o=function(c,b){return a(h[1][10][4],b[1][1],c)},p=f(e[22][15],o,n,b),q=function(b){var
g=b[2],h=b[1],i=g[1];if(3===i[0])return[0,h,a(e[22][68],tl,i[1]),g];var
j=c(d[3],tm);return f(l[5],h[2],0,j)},g=a(e[22][68],q,b),s=function(b){var
j=b[1],m=b[3],n=b[2];function
o(d,j){var
e=d[1],k=j[1],l=d[2];function
f(b){var
c=a(h[1][10][3],b,e);if(c)return c;try{ct(a(r[31],0,b));var
d=1;return d}catch(a){a=v(a);if(a===D)return 0;throw a}}var
g=c(h[1][6],tn),b=a(f0[25],g,f),m=[0,a(i[1],k[2],b),l];return[0,a(h[1][10][4],b,e),m]}var
k=f(e[22][15],o,[0,p,0],n)[2];function
q(b){var
c=b[1],d=b[3];return[0,a(i[1],c[2],[0,[0,c[1]]]),d]}var
s=a(e[22][68],q,g);function
t(b){return a(i[1],b[2],[0,[0,b[1]]])}function
l(b){var
c=b[2],d=[1,[0,a(r[31],c,b[1])]];return a(i[1],c,d)}var
d=m[2],u=a(e[22][68],t,k),w=a(e[22][68],l,k),x=[4,l(j),w],y=[5,1,s,a(i[1],d,x)],z=[3,u,a(i[1],d,y)];return[0,j,a(i[1],d,z)]},m=a(e[22][68],s,g);else
var
m=b;function
z(e){var
g=e[1],i=g[2],b=g[1],j=dj(1,e[2]),k=j[1],n=j[2];if(1-cf(k)){var
o=c(d[3],tr);f(l[5],i,0,o)}var
p=c(av[21],b);try{bN(p);var
w=1,m=w}catch(a){a=v(a);if(a!==D)throw a;var
m=0}if(m){var
q=c(d[3],ts),r=c(h[1][9],b),s=c(d[3],tt),t=a(d[12],s,r),u=a(d[12],t,q);f(l[5],i,0,u)}return[0,b,k,n]}var
A=a(e[22][68],z,m);function
B(b){var
d=b[1],e=c(iP,[0,w,x,b[2],b[3]]);a(av[10],d,e);return 0}return a(e[22][11],B,A)}function
iZ(i,g,p,b){var
j=g[2],q=g[1],r=i?i[1]:0,k=iD(p);function
m(b){if(1===b[0]){var
c=m(b[2]);return a(e[4],1,c)}return 0}var
n=m(k[2]);if(0===n){var
s=c(d[3],tH);f(l[5],j,0,s)}try{fD(b)}catch(e){e=v(e);if(e!==D)throw e;var
t=c(d[3],b[2]),u=c(d[21],t),w=c(d[13],0),x=c(d[3],b[1]),y=c(d[21],x),z=c(d[3],tI),A=a(d[12],z,y),B=a(d[12],A,w),C=a(d[12],B,u);f(l[5],j,0,C)}function
E(b){var
d=a(tK[4],tJ,b);return c(h[1][6],d)}var
o=a(e[22][56],n,E);function
F(a){return[0,a]}var
G=a(e[22][68],F,o);function
H(a){return[1,a]}var
I=c(iP,[0,r,0,[3,G,[13,b,a(e[22][68],H,o)]],k]);a(av[10],q,I);return 0}function
tL(p,b,o){var
g=o[2],i=o[1],t=p?p[1]:0;try{var
O=d4(b),j=O}catch(e){e=v(e);if(e!==D)throw e;var
u=c(r[26],b),w=c(d[3],tM),x=a(d[12],w,u),j=f(l[5],b[2],0,x)}var
q=ae(j),k=q[1];if(typeof
q[2]!=="number"){var
J=c(d[3],tS),K=c(r[26],b),L=c(d[3],tT),M=a(d[12],L,K),N=a(d[12],M,J);f(l[5],b[2],0,N)}if(1-(c(e[22][1],i)===k?1:0)){var
y=c(e[22][1],i);iq(b[2],y,k)}if(typeof
g==="number")return 0;else{if(1===g[0]){var
n=g[1],A=function(c,b){return a(h[1][1],c[1],b[1])},s=a(e[22][dK],A,n);if(s){var
B=c(h[1][9],s[1][1]),C=c(d[3],tO),E=a(d[12],C,B);f(l[5],0,0,E)}var
F=function(g){var
h=g[1],i=c(av[17],0),b=f(r[23],0,i,h),e=fJ(b);if(e){var
j=c(r[26],b),k=c(d[3],tP),m=a(d[12],k,j);return f(l[5],0,0,m)}return e};a(e[22][11],F,n);var
G=function(c){var
a=f2(h[1][11][1],[0,i,[0,[0,c]]])[2];if(typeof
a!=="number"&&0===a[0]){var
b=a[1];if(b)return b[1]}throw[0,m,tQ]},H=function(g){var
b=g[1],i=g[2];if(1-d6(b)){var
j=c(h[1][9],b),k=c(d[3],tR),m=a(d[12],k,j);f(l[5],0,0,m)}return[0,b,a(e[22][68],G,i)]},I=c(tk,[0,t,k,j,a(e[22][68],H,n)]);return a(av[11],0,I)}var
z=c(d[3],tN);return f(l[5],b[2],0,z)}}function
i0(k,b,j){if(j){var
m=j[1];if(0!==m[2])if(!j[2]){var
q=m[1],H=m[3];if(b){var
I=c(d[3],tV);f(l[5],q[2],0,I)}return tL(k,q,H)}}function
G(e){var
b=e[1],k=e[3];if(e[2]){var
m=c(d[3],tU);f(l[5],b[2],0,m)}if(c(r[32],b))var
h=c(r[34],b),g=a(i[1],b[2],h);else
var
j=c(d[3],tu),g=f(l[5],b[2],0,j);return[0,g,k]}var
g=a(e[22][68],G,j),s=k?k[1]:0;function
t(c,b){return a(h[1][1],c[1][1],b[1][1])}var
n=a(e[22][dK],t,g);if(n){var
o=n[1][1],u=o[2],w=c(h[1][9],o[1]),x=c(d[3],tv),y=a(d[12],x,w);f(l[5],u,0,y)}function
z(e){var
g=e[1][1],h=c(av[17],0),b=f(r[23],0,h,g);try{d4(b);var
i=c(r[26],b),j=c(d[3],tw),k=a(d[12],j,i),m=f(l[5],0,0,k);return m}catch(a){a=v(a);if(a===D)return 0;throw a}}a(e[22][11],z,g);function
A(j){var
k=j[2],g=k[2],m=j[1],n=m[2],o=m[1],u=k[1];function
v(c,b){return a(h[1][1],c[1],b[1])}var
p=a(e[22][dK],v,u);if(p){var
q=p[1],w=q[2],x=q[1],y=c(d[3],tx),z=c(h[1][9],x),A=c(d[3],ty),B=a(d[12],A,z),C=a(d[12],B,y);f(l[5],w,0,C)}if(typeof
g==="number"){if(b){var
D=c(d[3],tz),E=c(h[1][9],o),F=c(d[3],tA),G=a(d[12],F,E),H=a(d[12],G,D);return f(l[5],n,0,H)}return b}else
switch(g[0]){case
0:if(b){var
I=c(d[3],tB),J=c(h[1][9],o),K=c(d[3],tC),L=a(d[12],K,J),M=a(d[12],L,I);return f(l[5],n,0,M)}return b;case
1:var
i=g[1],N=function(c,b){return a(h[1][1],c[1],b[1])},s=a(e[22][dK],N,i);if(s){var
O=c(h[1][9],s[1][1]),P=c(d[3],tD),Q=a(d[12],P,O);f(l[5],0,0,Q)}var
R=function(g){var
b=g[1],e=1-d6(b);if(e){var
i=c(h[1][9],b),j=c(d[3],tE),k=a(d[12],j,i);return f(l[5],0,0,k)}return e};a(e[22][11],R,i);var
S=function(g){var
h=g[1],i=c(av[17],0),b=f(r[23],0,i,h),e=fJ(b);if(e){var
j=c(r[26],b),k=c(d[3],tF),m=a(d[12],k,j);return f(l[5],0,0,m)}return e};a(e[22][11],S,i);return 0;default:var
T=g[1],U=function(c,b){return a(h[1][1],c[1],b[1])},t=a(e[22][dK],U,T);if(t){var
V=c(h[1][9],t[1][1]),W=c(d[3],tG),X=a(d[12],W,V);f(l[5],0,0,X)}return 0}}a(e[22][11],A,g);if(b)var
B=function(d,a){var
b=a[1][1],g=c(e[22][1],a[2][1]),i=[0,c(av[21],b),g];return f(h[1][11][4],b,i,d)},p=f(e[22][15],B,h[1][11][1],g);else
var
p=h[1][11][1];function
C(a){var
b=a[1][1];return[0,b,[0,s,f2(p,a[2])]]}var
E=a(e[22][68],C,g);function
F(b){var
d=b[1],e=c(ga,b[2]);a(av[10],d,e);return 0}return a(e[22][11],F,E)}var
ek=[0,h[1][11][1]];function
i1(b,a){var
d=c(e[3],ek);ek[1]=f(h[1][11][4],b,a,d);return 0}function
i2(a){switch(a[0]){case
0:return a[1][2];case
1:return a[1][2];default:return[0,a[1]]}}function
bz(b){switch(b[0]){case
0:var
m=b[1][1],n=a(i[1],0,tW);return[0,[0,[2,[0,m]]],function(a){return n}];case
2:var
j=b[2],k=j[1];if(k){var
g=k[1],q=b[3],r=j[2],s=c(e[3],ek);if(a(h[1][11][3],g,s)){var
t=c(e[3],ek);return f(h[1][11][23],g,t,q)}var
u=c(h[1][9],g),v=c(d[13],0),w=c(d[3],tY),x=a(d[12],w,v),y=a(d[12],x,u);return f(l[5],r,0,y)}break}var
o=i2(b),p=c(d[3],tX);return f(l[5],o,0,p)}function
i3(a){switch(a[0]){case
0:return[0,a[1][1]];case
2:var
b=a[3];if(b)if(!b[2]){var
e=a[2][1],i=b[1],j=e?[0,e[1]]:0;return[1,j,bz(i)]}break}var
g=i2(a),h=c(d[3],tZ);return f(l[5],g,0,h)}function
gc(b){if(b){var
d=b[1];if(0===d[0]){var
h=d[1],e=gc(b[2]),i=e[2],j=e[1],k=[0,j,[0,c(el[10],h)]];return[0,k,function(a,b){return c(i,a)}]}var
f=d[2],l=f[2],m=f[1],n=d[1],g=gc(b[2]),o=g[2],p=[0,g[1],m];return[0,p,function(d,b){return c(o,function(f,e){return a(d,f,[0,[0,n,c(l,b)],e])})}]}return[0,0,function(c,b){return a(c,b,0)}]}function
t0(b,g){var
d=gc(a(e[22][14],i3,b[1])),h=d[2],j=d[1],k=[0,j,c(h,function(d,c){function
f(b){var
c=b[2];return[0,a(i[1],c[2],[0,b[1]]),c]}var
g=a(e[22][68],f,c);return a(i[1],[0,d],[5,0,g,b[2]])})],f=b[3],l=f?[0,c(bP[22],f[1])]:0;return[0,[0,[0,q[1],0,[0,0,[0,[0,l,0,[0,k,0]],0]]],0],g]}var
i4=a(g[21],t1,t0);function
t2(b){return a(g[22],i4,b[2])}function
t3(d,c){var
b=1===d?1:0,e=c[2];return b?a(g[22],i4,e):b}function
t4(b){var
a=b[2],c=aj(b[1],a[2]);return c===a[2]?a:[0,a[1],c,a[3],a[4]]}function
t5(a){return a[4]?0:[0,a]}var
em=c(aK[1],t6),t7=c(aK[4],[0,em[1],t2,em[3],t3,t5,t4,em[7],em[8]]);function
i5(d,a){var
b=a[1],c=b[2],e=a[2];da(d,b[1],[1,c]);return fz(c,e[1])}function
t8(b,a){return i5([0,b],a)}function
t9(b,a){return i5([1,b],a)}function
t_(a){var
b=a[1],c=b[2],d=a[2];da(t$,b[1],[1,c]);return fz(c,d[1])}function
ua(b){var
a=b[2],c=aj(b[1],a[1]);return c===a[1]?a:[0,c]}function
ub(a){return[0,a]}var
gd=c(aK[1],uc),ud=c(aK[4],[0,gd[1],t_,t8,t9,ub,ua,gd[7],gd[8]]);function
i6(k,b,j,g){var
o=k?k[1]:0;if(b){var
d=b[1];if(2===d[0]){var
l=d[2],m=l[1];if(m)if(!d[3])if(!b[2])if(!j){var
n=m[1];iX(a(i[1],l[2],n));var
u=c(ud,[0,ai(h[1][10][1],g)]);a(av[10],n,u);return 0}}}var
p=a(e[22][68],i3,b);function
q(b,c){if(0===c[0])return b;var
d=c[1];return d?a(h[1][10][4],d[1],b):b}var
r=ai(f(e[22][15],q,h[1][10][1],p),g),s=j||ue,t=c(t7,[0,b,r,s,o]);return a(av[11],0,t)}function
i7(d){var
a=d[2],b=a[1],c=bN(b);return d3(b,[0,a[2],c[2],c[3]])}function
uf(c){var
b=c[2],d=c[1],e=a(aI[37],d,b[1]),f=Q(d,b[2]);if(e===b[1])if(f===b[2])return b;return[0,e,f]}function
ug(a){return[0,a]}var
en=c(aK[1],uh),ui=en[8],uj=en[7];function
uk(a){return i7}var
ul=c(aK[4],[0,en[1],i7,en[3],uk,ug,uf,uj,ui]);function
um(K,b,n){try{var
J=ct(b),g=J}catch(e){e=v(e);if(e!==D)throw e;var
o=c(r[26],b),p=c(d[3],un),q=a(d[12],p,o),g=f(l[5],b[2],0,q)}if(0===g[0])var
e=g[1];else
var
I=c(d[3],ut),e=f(l[5],b[2],0,I);var
h=bN(e);if(1-h[3]){var
s=c(d[3],uo),t=c(r[26],b),u=c(d[3],up),w=a(d[12],u,t),x=a(d[12],w,s);f(l[5],b[2],0,x)}var
i=dj(1,n),j=i[2],k=i[1];if(1-cf(k)){var
y=c(d[3],uq);f(l[5],b[2],0,y)}if(1-iE(j,h[2])){var
m=d7(0),z=cx(m,h[2][2]),A=c(d[3],ur),B=cx(m,j[2]),C=c(d[3],us),E=a(d[12],C,B),F=a(d[12],E,A),G=a(d[12],F,z);f(l[5],b[2],0,G)}var
H=c(ul,[0,e,k]);return a(av[11],0,H)}function
i8(i,s){var
k=c(bi[2],0),l=dj(0,s),n=l[2],e=L(ei,l[1]);if(i)var
t=c(i9[1],i[1]),b=c(i_[2],0),o=t;else
var
J=c(aJ[17],k),K=c(h[1][6],ux),b=1,o=p(eo[2],K,0,J,0);if(typeof
b==="number"){if(0===b)throw[0,m,uu];var
g=e}else
switch(b[0]){case
0:var
r=b[1],g=p(j[31],0,r,r,e);break;case
1:var
g=f(j[32],0,b[1],e);break;default:var
g=f(j[33],0,b[1],e)}var
u=c(bi[2],0),q=f(eo[24],u,g,o),v=q[3],w=c(eo[1],q[1])[1],x=d7(0),y=bt(k,w,v,n[2]),z=c(d[13],0),A=c(d[3],uv),B=c(d[13],0),C=cx(x,n[2]),D=c(d[3],uw),E=a(d[12],D,C),F=a(d[12],E,B),G=a(d[12],F,A),H=a(d[12],G,z),I=a(d[12],H,y);return a(cJ[7],0,I)}function
i$(b,a){switch(a[0]){case
0:return iY(b,[0,a[1]],a[2],a[3]);case
1:return i0(b,a[1],a[2]);case
2:return iZ(b,a[1],a[2],a[3]);case
3:return i6(b,a[1],a[2],a[3]);default:return um(b,a[1],a[2])}}function
uy(a){cG[1]=a;return 0}var
uB=[0,0,uA,uz,function(a){return c(e[3],cG)},uy];a(ja[4],0,uB);var
ge=c(bb[1],0);function
uC(b){switch(b[0]){case
0:var
g=fF([0,b[1]]),h=c(r[26],g),i=c(d[3],uD);return a(d[12],i,h);case
1:var
j=b[1],k=c(d[3],uE),l=d_(j),m=c(d[3],uF),n=a(d[12],m,l);return a(d[12],n,k);case
2:var
e=b[1],o=c(d[3],uG),p=c(d[3],e[2]),q=c(d[3],uH),s=c(d[3],e[1]),t=c(d[3],uI),u=a(d[12],t,s),v=a(d[12],u,q),w=a(d[12],v,p);return a(d[12],w,o);default:var
f=b[1],x=b[2],y=cu(f),z=c(bi[2],0),A=a(y[4],z,x),B=c(d[13],0),C=c(d[3],uJ),D=c(aW[3],f),E=c(d[3],D),F=c(d[3],uK),G=a(d[12],F,E),H=a(d[12],G,C),I=a(d[12],H,B);return a(d[12],I,A)}}function
uL(b){if(b[1]===bn){var
e=b[3],f=b[2],g=c(h[8][4],uM),i=a(h[15][1],bg,g),j=c7([0,f,e]),k=aJ[16],m=bt(c(bi[2],0),k,j,[2,[1,i],0]),n=a(d[26],0,m),o=c(d[13],0),p=c(d[3],uN),q=a(d[12],p,o),r=a(d[12],q,n);return a(d[26],0,r)}throw l[7]}c(l[8],uL);function
uO(g){if(c(e[3],cG)){var
b=a(bb[4],g,ge),h=b?b[1]:0,i=c(d[5],0),j=f(d[39],d[5],uC,h),k=c(d[5],0),l=c(d[3],uP),m=a(d[12],l,k),n=a(d[12],m,j),o=[0,a(d[12],n,i)];return[0,a(gf[12],0,o)]}return 0}c(l[14],uO);function
jb(b){if(bs(b)){try{var
y=fH(b),h=y}catch(e){e=v(e);if(e!==D)throw e;var
i=c(r[26],b),j=c(d[3],uQ),k=a(d[12],j,i),h=f(l[5],b[2],0,k)}b$(h);var
m=c(r[26],b),n=c(d[13],0),o=c(d[3],uR),p=c(d[13],0),q=c(d[3],uS),s=a(d[12],q,p),t=a(d[12],s,o),u=a(d[12],t,n),w=a(d[12],u,m),x=a(d[26],2,w);return a(cJ[7],0,x)}try{var
ad=ct(b),g=ad}catch(e){e=v(e);if(e!==D)throw e;var
z=c(r[26],b),A=c(d[3],uT),B=a(d[12],A,z),g=f(l[5],b[2],0,B)}if(0===g[0]){var
e=bN(g[1]),C=e[1],E=e[2][2],F=d7(0),G=d_(C),H=c(d[13],0),I=c(d[3],uU),J=c(d[13],0),K=c(r[26],b),L=a(d[12],K,J),M=a(d[12],L,I),N=a(d[12],M,H),O=a(d[12],N,G),P=a(d[26],2,O),Q=c(d[5],0),R=cx(F,E),S=c(d[13],0),T=c(d[3],uV),U=c(d[13],0),V=c(r[26],b),W=a(d[12],V,U),X=a(d[12],W,T),Y=a(d[12],X,S),Z=a(d[12],Y,R),_=a(d[26],2,Z),$=a(d[12],_,Q),aa=a(d[12],$,P),ab=a(d[26],0,aa);return a(cJ[7],0,ab)}var
ac=c(d[3],uW);return a(cJ[7],0,ac)}function
jd(h,i,d){var
k=d[2],e=dj(0,d),l=e[1];ix(k,e[2]);var
m=L(ei,l),n=c(j[19],m);function
f(e,d){var
f=i?[0,e]:0,g=c(i_[2],0),b=a_(jc[6],f,g,0,n,d),h=b[2];return[0,a(eo[25],uX[4],b[1]),h]}var
b=a(i9[16],f,h),g=b[1];if(1-b[2])p(cJ[4],0,0,0,3);return g}var
uZ=c(h[8][4],u2),u0=a(h[15][1],bg,uZ),je=a(ep[1],0,0),jf=je[2],jg=je[1];function
u3(b){return a(ep[2],jg,0)}function
u4(c,b){return a(ep[2],jg,0)}function
u5(b,a){return f(ja[13],0,u7,u6)}var
dn=c(aK[1],u8),u9=c(aK[4],[0,dn[1],u3,u4,u5,dn[5],dn[6],dn[7],dn[8]]);function
u$(q){var
l=c(ga,u1),m=c(h[1][6],va);a(av[10],m,l);var
n=[0,ve,[0,[0,vd,[0,vc,[0,[2,[1,u0],vb],0]]],0]],o=1,g=c(h[1][6],vf);function
i(a){var
b=a[2];return[0,c(h[1][7],a[1]),b]}var
b=a(e[22][68],i,n);function
j(a,d){var
b=a[2],c=a[1];return d[2]?[0,c,b+1|0]:[0,c+1|0,b]}var
d=f(e[22][15],j,uY,b),k=c(ga,[0,0,[0,o,[1,[0,b,d[1],d[2]]]]]);a(av[10],g,k);var
p=c(u9,0);return a(av[11],0,p)}a(vg[13],u$,u_);aD(1246,[0,iY,i0,iZ,i$,i6,i8,i1,bz,jb,jd,ge,q,jf],"Ltac2_plugin__Tac2entries");var
jh=h[1][11][2],ji=[dI,vi,dF(0)],vk=c(d[3],vj),gg=[0,l[4],vl,vk],jj=[0,gg,bb[2]];function
jl(e,g,F,E,n){var
o=c(x[e6],e),G=E?c(jk[9],o):o,q=h[1][11][1];function
r(b,a){if(c(jh,b))return a;if(c(jh,a))return b;function
d(f,c,a){if(c){var
b=c[1];if(a){if(a_(vh[81],0,e,g,b,a[1]))return[0,b];throw ji}var
d=b}else{if(!a)return 0;var
d=a[1]}return[0,d]}return f(h[1][11][7],d,b,a)}function
i(b,a){try{var
c=[0,[0,r(b[1],a[1])]];return c}catch(a){a=v(a);if(a===ji)return 0;throw a}}function
d(b){return[0,function(d,c){return a(d,b,c)}]}function
b(d,b){return[0,function(f,e){function
g(e,d){return a(c(b,e)[1],f,d)}return a(d[1],g,e)}]}function
s(c,b){return[0,function(e,d){function
f(d,c){return a(b[1],e,c)}return a(c[1],f,d)}]}var
t=[0,function(c,b){return a(j[21],0,gg)}];function
k(b,c){var
d=c[2],e=c[1];if(b){var
f=b[2],g=b[1];return[0,function(c,b){function
d(d){return a(k(f,d)[1],c,b)}var
e=a(c,g,b);return a(j[22],e,d)}]}return[0,function(c,b){return a(j[21],[0,d],e)}]}function
z(b){var
c=[0,b];return[0,function(e,d){var
b=i(c,d);return b?a(e,0,b[1]):a(j[21],0,gg)}]}function
l(b,k){if(0===b[0]){var
l=b[1];try{var
m=d(0),n=s(z(p(a6[3],e,g,l,k)),m);return n}catch(a){a=v(a);if(a===a6[1])return t;throw a}}var
o=b[1];function
f(p,b){var
h=b[2],k=b[1];return[0,function(e,d){var
g=c(gh[6],p);if(g){var
l=g[2],m=g[1],b=m[2],n=i(d,[0,m[1][2]]);if(n){var
q=n[1],r=function(b){return a(f(l,b)[1],e,d)},o=hu(b),s=e7===o?b[1]:hE===o?c(gi[2],b):b,t=a(e,[0,s],q);return a(j[22],t,r)}return a(f(l,[0,k,h])[1],e,d)}return a(j[21],[0,h],k)}]}return f(p(a6[8],e,g,[0,h[1][10][1],o],k),jj)}function
m(e,g,f){if(e){var
j=e[2],n=e[1],o=function(b){var
d=b[1],e=b[2];function
i(b){var
e=c(cg[11][1][2],b);return a(h[1][1],e,d)}return m(j,a(vm[97],i,g),[0,[0,d,e],f])},i=function(a){var
e=c(cg[11][1][2],a);function
f(a){return d([0,e,a])}return b(l(n,c(cg[11][1][4],a)),f)};return b(b(k(g,jj),i),o)}return d(f)}function
H(a){var
b=a[1];return c(j[16],[0,b[1],b[2],a[2][1]])}var
A=n[2],B=c(jk[9],n[1]);function
C(a){function
c(b){return d([0,b,a])}return b(m(B,G,0),c)}var
D=b(l(A,F),C),u=[0,q];function
w(b,a){return c(j[16],[0,b,a])}var
y=a(D[1],w,u);return a(j[72][1],y,H)}aD(1255,[0,jl],"Ltac2_plugin__Tac2match");var
cK=c(aW[1],vn),eq=c(aW[1],vo),gj=c(aW[1],vp),gk=c(aW[1],vq),dp=c(aW[1],vr),gl=c(aW[1],vs),gm=c(aW[1],vt),gn=c(aW[1],vu);function
go(b){var
d=a(e[22][68],h[1][6],[0,b,vv]);return[0,c(h[5][4],d)]}var
vx=go(vw),vz=go(vy),vB=go(vA);function
dq(d,b){var
e=c(h[1][7],b),f=c(h[8][5],e);return a(h[15][1],d,f)}function
jm(a){return dq(fN,a)}function
ch(a){return dq(bg,a)}function
gp(a){return dq(vx,a)}function
er(a){return dq(vz,a)}function
es(c,b){return a(i[1],c,[1,[1,[0,b]]])}function
a7(d,g,b){var
f=a(i[1],d,[2,[1,[1,g]]]);return c(e[22][48],b)?f:a(i[1],d,[4,f,b])}function
C(c,b,a){return a7(c,jm(b),a)}function
aq(b,a){return[1,jm(a)]}function
bA(c){var
d=ch(vC),b=c[2],e=a(i[1],b,[2,[1,[1,d]],0]),f=[2,a(i[1],b,vD),e],g=[3,[0,a(i[1],b,f),0],c];return a(i[1],b,g)}function
cL(g,f,b){var
d=b[2],e=b[1],h=e[1],j=[0,c(f,e[2]),0],k=[0,c(g,h),j],l=[4,a(i[1],d,vE),k];return a(i[1],d,l)}function
bj(d,b){if(b){if(b[2]){var
f=[2,[1,[0,c(e[22][1],b)]]],g=[4,a(i[1],d,f),b];return a(i[1],d,g)}return b[1]}return a(i[1],d,vF)}function
bU(b){return a(i[1],b[2],[0,[0,b[1]]])}function
aQ(b,d,a){if(a){var
e=[0,c(d,a[1]),0];return a7(b,ch(vG),e)}return a7(b,ch(vH),0)}function
cM(d,c,b){return a(i[1],d,[12,c,b])}function
et(e){var
b=e[2],g=a(r[31],b,e[1]);if(bs(g)){var
h=c(d[3],vI);return f(l[5],b,0,h)}return a(i[1],b,[1,[0,g]])}function
aw(b,a){return 0===a[0]?c(b,a[1]):et(a[1])}function
ar(a){return cM(a[2],gj,a[1])}function
ci(g,b){var
d=c(gq[5],b);function
j(e,b){var
f=[20,c(h[1][8],b),e];return a(i[1],d,f)}var
k=a(e[22][15],j,b);return cM(d,gk,f(w[22],k,b,g))}function
cN(a){return cM(c(gq[5],a),dp,a)}function
bV(b,a){var
c=a?ch(vJ):ch(vK);return a7(b,c,0)}function
N(d,b,a){if(a){var
e=a[1],f=[0,N(d,b,a[2]),0],g=[0,c(b,e),f];return a7(d,ch(vL),g)}return a7(0,ch(vM),0)}function
vN(b){var
c=b[2],a=b[1];return 0===a[0]?C(c,vO,[0,bU(a[1]),0]):C(c,vP,[0,ar(a[1]),0])}function
eu(c){var
a=c[2],b=c[1];if(typeof
b==="number")return C(a,vQ,0);else{if(0===b[0])return C(a,vR,[0,N(a,cN,b[1]),0]);var
d=b[1],e=0;return C(a,vS,[0,N(a,function(a){return cL(function(a){return aw(vN,a)},cN,a)},d),e])}}function
jn(a){return cL(cN,eu,a)}function
ev(f){var
e=f[2],c=f[1];switch(c[0]){case
0:return C(e,vT,[0,bV(0,c[1]),0]);case
1:return C(e,vU,[0,jo(c[1]),0]);default:var
g=c[1],a=g[2],b=g[1],h=0;if(typeof
b==="number")var
d=C(a,vZ,0);else
switch(b[0]){case
0:var
d=C(a,v0,[0,jp(b[1]),0]);break;case
1:var
d=C(a,v1,[0,dr(b[1]),0]);break;default:var
d=C(a,v2,[0,bV(a,b[1]),0])}return C(e,vV,[0,d,h])}}function
jo(c){var
b=c[2],a=c[1];return typeof
a==="number"?C(b,vW,0):0===a[0]?C(b,vX,[0,aw(ar,a[1]),0]):C(b,vY,[0,aw(ar,a[1]),0])}function
jp(c){var
a=c[2],b=c[1];return 0===b[0]?C(a,v3,[0,N(a,dr,b[1]),0]):C(a,v4,[0,dr(b[1]),0])}function
dr(a){return N(a[2],ev,a[1])}function
ew(c){var
a=c[2],b=c[1];if(typeof
b==="number")return 0===b?C(a,v8,0):C(a,v9,0);else{if(0===b[0]){var
d=b[1];return C(a,v_,[0,N(a,function(a){return aw(bU,a)},d),0])}var
e=b[1];return C(a,v$,[0,N(a,function(a){return aw(bU,a)},e),0])}}function
gr(c){var
b=c[2],d=c[1],e=d[1],f=aQ(b,function(a){return N(b,function(d){var
c=d[1],a=0,e=c[2],f=c[1],g=0;switch(d[2]){case
0:var
b=C(a,v5,0);break;case
1:var
b=C(a,v6,0);break;default:var
b=C(a,v7,0)}var
h=[0,ew(f),[0,b,g]];return bj(a,[0,aw(ar,e),h])},a)},e),g=ew(d[2]),h=[0,[0,aq(0,wa),g],0],j=[9,[0,[0,aq(0,wb),f],h]];return a(i[1],b,j)}function
gs(c){var
b=c[2],a=c[1];switch(a[0]){case
0:return C(b,wc,[0,bA(jn(a[1])),0]);case
1:return C(b,wd,[0,ar(a[1]),0]);default:return C(b,we,[0,bU(a[1]),0])}}function
jq(d){var
b=d[2],c=d[1],e=gs(c[1]),f=aQ(b,jo,c[2]),g=aQ(b,jp,c[3]),h=aQ(b,gr,c[4]),j=[0,[0,aq(0,wf),h],0],k=[0,[0,aq(0,wg),g],j],l=[0,[0,aq(0,wh),f],k],m=[9,[0,[0,aq(0,wi),e],l]];return a(i[1],b,m)}function
jr(g,b){var
e=bs(a(r[31],0,b));if(e){var
i=c(h[1][9],b),j=c(d[3],wj),k=a(d[12],j,i);return f(l[5],g,0,k)}return e}function
ex(b){function
f(k,g,d){var
b=d[1];switch(b[0]){case
13:var
e=b[1],c=1;break;case
14:if(b[2])var
c=0;else
var
e=b[1],c=1;break;default:var
c=0}if(c){jr(d[2],e);return a(h[1][10][4],e,g)}var
i=0;function
j(b,a){return 0}return a_(gq[28],j,f,i,g,d)}return f(0,h[1][10][1],b)}function
ds(b,g,d){function
l(a){return a?[0,a[1]]:0}try{var
p=[0,a(e[22][99],l,g)],c=p}catch(a){a=v(a);if(a!==D)throw a;var
c=0}if(c)var
h=c[1],m=function(f,d){var
g=f[2],c=f[1];if(d){var
j=es(b,dq(vB,wk)),k=[0,bU(a(i[1],b,c)),0],l=[4,j,[0,et(a(i[1],b,h)),k]],m=a(i[1],b,l),n=[0,[0,a(i[1],b,[0,d]),m],g];return[0,a(e[4],c,1),n]}return[0,a(e[4],c,1),g]},n=[5,0,f(e[22][15],m,wl,g)[2],d],k=[0,h],j=a(i[1],b,n);else
var
k=0,j=d;var
o=[3,[0,a(i[1],b,[0,k]),0],j];return a(i[1],b,o)}function
ey(a){return cM(a[2],cK,a)}function
js(f){var
b=f[2],d=f[1];if(0===d[0]){var
j=d[1],k=aQ(b,ey,0),l=ci(0,j),m=[3,[0,a(i[1],b,wm),0],l];return bj(b,[0,k,[0,a(i[1],b,m),0]])}var
g=d[1],n=d[2],o=ex(g),p=aQ(b,ey,[0,g]),q=ci(0,n),r=c(h[1][10][21],o);function
s(a){return[0,a]}return bj(0,[0,p,[0,ds(b,a(e[22][68],s,r),q),0]])}function
jt(f){var
d=f[1],g=d[1],e=g[2],j=f[2],k=g[1],l=aQ(e,function(a){return a?C(e,wr,0):C(e,ws,0)},k),h=d[2],c=h[2],b=h[1],m=typeof
b==="number"?0===b?C(c,wn,0):C(c,wo,0):0===b[0]?C(c,wp,[0,bU(b[1]),0]):C(c,wq,[0,bU(b[1]),0]),n=bA(jn(d[3])),o=[0,[0,aq(0,wt),n],0],p=[0,[0,aq(0,wu),m],o],q=[9,[0,[0,aq(0,wv),l],p]];return a(i[1],j,q)}function
gt(b,c){var
d=es(b,gp(ww)),e=[4,d,[0,ar(c),0]];return a(i[1],b,e)}function
ju(b,c){var
d=es(b,gp(wx)),e=[4,d,[0,bA(gt(b,c)),0]];return a(i[1],b,e)}function
wy(b,c){var
d=es(b,gp(wz)),e=[4,d,[0,bA(et(c)),0]];return a(i[1],b,e)}function
jv(d){var
b=d[2];function
c(c){return c?bA(c[1]):bA(a(i[1],b,wA))}function
e(d){var
e=a(i[1],b,d);return cL(c,function(a){return N(b,c,a)},e)}function
f(a){return aQ(b,e,a)}return cL(function(a){return N(b,c,a)},f,d)}function
gu(a){return aw(function(a){return cM(a[2],eq,a)},a)}function
jw(p){var
h=p[2],b=wD,k=p[1];for(;;){if(k){var
j=k[1][1],q=k[2];if(typeof
j==="number")switch(j){case
0:var
g=[0,1,b[2],b[3],b[4],b[5],b[6],b[7]];break;case
1:var
g=[0,b[1],1,1,1,b[5],b[6],b[7]];break;case
2:var
g=[0,b[1],1,b[3],b[4],b[5],b[6],b[7]];break;case
3:var
g=[0,b[1],b[2],1,b[4],b[5],b[6],b[7]];break;case
4:var
g=[0,b[1],b[2],b[3],1,b[5],b[6],b[7]];break;default:var
g=[0,b[1],b[2],b[3],b[4],1,b[6],b[7]]}else
if(0===j[0]){var
m=j[1],r=m[2],s=m[1];if(b[6]){var
t=c(d[3],wB);f(l[5],r,0,t)}var
u=a(e[23],b[7],s),g=[0,b[1],b[2],b[3],b[4],b[5],b[6],u]}else{var
n=j[1],o=0!==b[7]?1:0,v=n[2],w=n[1],x=o?1-b[6]:o;if(x){var
y=c(d[3],wC);f(l[5],v,0,y)}var
z=a(e[23],b[7],w),g=[0,b[1],b[2],b[3],b[4],b[5],1,z]}var
b=g,k=q;continue}var
A=N(h,gu,b[7]),B=[0,[0,aq(0,wE),A],0],C=bV(h,b[6]),D=[0,[0,aq(0,wF),C],B],E=bV(h,b[5]),F=[0,[0,aq(0,wG),E],D],G=bV(h,b[4]),H=[0,[0,aq(0,wH),G],F],I=bV(h,b[3]),J=[0,[0,aq(0,wI),I],H],K=bV(h,b[2]),L=[0,[0,aq(0,wJ),K],J],M=bV(h,b[1]),O=[9,[0,[0,aq(0,wK),M],L]];return a(i[1],h,O)}}function
jx(a){var
b=a[2],c=a[1];if(c){var
d=[0,c[1]];return aQ(b,function(a){return N(0,function(a){return aw(ar,a)},a)},d)}var
e=0;return aQ(b,function(a){return N(0,function(a){return aw(ar,a)},a)},e)}function
jy(c,a){if(a){var
b=a[1];jr(c,b);return[0,b]}return 0}function
jz(b){var
d=b[2],f=b[1];return N(d,function(g){var
b=g[2],j=g[1],k=j[1],d=k[1],m=j[2],n=k[2];if(0===d[0])var
o=d[1],f=[0,a7(b,er(wL),0),o,0];else
var
x=d[2],y=jy(b,d[1]),f=[0,a7(b,er(wM),0),x,y];var
l=f[2],p=f[3],q=f[1],r=ex(l),s=c(h[1][10][21],r);function
t(a){return[0,a]}var
u=ds(b,a(e[22][68],t,s),m),v=[3,[0,a(i[1],b,[0,p]),0],u],w=a(i[1],b,v);return bj(0,[0,q,[0,cM(n,cK,l),[0,w,0]]])},f)}function
jA(b){var
d=b[2],g=b[1];function
j(c){var
b=c[2],a=c[1];if(0===a[0]){var
d=a[1];return[0,0,d,a7(b,er(wN),0)]}var
e=a[2],f=jy(b,a[1]);return[0,f,e,a7(b,er(wO),0)]}return N(d,function(n){var
b=n[2],o=n[1],p=o[1],k=p[1],H=o[2],q=p[2],r=k[2],d=j(k[1]),l=d[2],s=d[3],t=d[1],u=ex(l);function
v(e,c){var
f=c[1],b=j(c[2]),d=b[2],g=b[3],i=b[1],k=ex(d);return[0,a(h[1][10][7],k,e),[0,f,i,d,g]]}var
m=f(e[22][89],v,u,r),g=m[2],w=m[1];function
x(a){var
b=a[4];return bj(0,[0,b,[0,ey(a[3]),0]])}var
y=[0,bj(0,[0,s,[0,ey(l),0]]),0],z=bj(0,[0,N(q,x,g),y]);function
A(a){return a[1][1]}var
B=a(e[22][68],A,g);function
C(a){return a[2]}var
D=a(e[22][68],C,g),E=c(h[1][10][21],w);function
F(a){return[0,a]}var
G=a(e[22][68],F,E),I=[3,[0,a(i[1],b,[0,t]),0],H];return bj(b,[0,z,[0,ds(b,B,ds(b,D,ds(b,G,a(i[1],b,I)))),0]])},g)}function
jB(c){var
b=c[2],a=c[1];return typeof
a==="number"?0===a?C(b,wP,0):C(b,wQ,0):0===a[0]?C(b,wR,[0,aw(ar,a[1]),0]):C(b,wS,[0,aw(ar,a[1]),0])}function
jC(a){return cL(function(a){return aQ(0,function(a){return aw(ar,a)},a)},cN,a)}function
jD(b){var
c=b[2],a=b[1];if(0===a[0]){var
d=a[3],e=a[2],f=aQ(0,ev,a[1]),g=ci(0,e);return C(c,wT,[0,f,[0,g,[0,aQ(0,bA,d),0]]])}var
h=a[2],i=aw(ar,a[1]);return C(c,wU,[0,i,[0,ci(0,h),0]])}aD(1257,[0,a7,bA,aw,bU,cL,bj,et,ar,ci,cN,N,eu,ev,dr,gr,gs,jq,js,jt,ew,jx,jB,gu,gt,ju,wy,jv,jw,jC,jD,jz,jA,cK,gj,eq,gk,dp,gl,gm,gn],"Ltac2_plugin__Tac2quote");function
gv(d,b){var
e=c(h[1][7],b),f=c(h[8][5],e);return a(h[15][1],d,f)}function
Y(a){return gv(bg,a)}var
wX=Y(wW),wZ=Y(wY),w1=Y(w0),w3=Y(w2),w5=Y(w4),w7=Y(w6),w9=Y(w8),w$=Y(w_),xb=Y(xa),xd=Y(xc),jF=gv(fN,xe),gw=gv(fO,xf),xh=Y(xg),xj=Y(xi),xl=Y(xk),xn=Y(xm),xp=Y(xo),xr=Y(xq),aL=dU(0),J=aX[5];function
ez(a){return a?bc(aF,[0,a[1]]):bc(aF,0)}function
eA(b){var
a=a3(aG,b);return a?[0,a[1]]:0}function
eB(b,a){var
d=a[1],e=0===a[2]?xs:xt;return bd([0,c(b,d),e])}function
eC(i,h){var
b=bq(h);if(2===b.length-1){var
f=b[2],j=c(i,b[1]);if(0===f[0]){var
e=f[1];if(0===e)var
g=0,d=1;else
if(1===e)var
g=1,d=1;else
var
d=0;if(d)return a(cg[4],j,g)}throw[0,m,xu]}throw[0,m,xv]}function
gx(a){var
b=c(x[fe][7],a),d=c(jG[29][4],b);return at(function(a){return[5,fk,a]},d)}function
gy(a){var
b=bf(function(a){return V(fk,a)},a),d=c(jG[29][3],b);return c(x[2][1],d)}function
jH(a){var
b=a[2],c=a[1],d=at(u,a[3]),e=at(u,b);return[0,at(function(a){return eB(ez,a)},c),e,d]}function
jI(a){var
b=a[2],c=a[1],d=bf(H,a[3]),e=bf(H,b);return[0,bf(function(a){return eC(eA,a)},c),e,d]}function
jJ(d,b){return 0===b[0]?a(J,0,[0,c(d,b[1])]):a(J,1,[0,c6(b[1])])}var
gz=[0,bn,Y(xw),[0]],cO=[0,bn,Y(xx),[0]],eD=[0,bn,Y(xy),[0]],eE=[0,bn,Y(xz),[0]];function
ax(a){return bM(a,[0,aL,0])}var
jK=c(bb[1],0);function
dt(b){if(c(e[3],cG)){var
d=function(a){var
d=f(bb[3],b,ge,a);return c(j[16],d)};return a(j[72][1],f6,d)}return c(j[16],b)}function
ay(b,d){var
e=b?b[1]:bb[2];function
g(b){var
e=[0,f(bb[3],b,jK,0)],g=a(j[69][16],e,d);return c(j[70],g)}var
h=dt(e);return a(j[72][1],h,g)}function
du(b,c){var
d=b?b[1]:bb[2];function
e(b){return a(j[21],[0,b],c)}var
f=dt(d);return a(j[72][1],f,e)}function
n(a){return c(j[16],a)}function
cP(b){function
d(a){return n(c(b,0))}var
e=n(0);return a(j[72][1],e,d)}function
gA(b){function
d(a){c(b,0);return n(aL)}var
e=n(0);return a(j[72][1],e,d)}function
xB(a){if(a)if(!a[2])return c(j[16],0);return ay(0,gz)}var
jL=a(j[72][1],j[67][9],xB);function
aR(d){function
b(b){if(b){var
e=b[1];if(b[2])return ay(0,gz);var
f=function(b){var
e=c(jM[32][3],b);return a(d,c(j[67][3],b),e)};return a(j[72][1],e,f)}function
g(b){function
c(c){return a(d,b,c)}return a(j[72][1],j[54],c)}return a(j[72][1],j[55],g)}return a(j[72][1],j[67][9],b)}function
eF(c,b,a){return a4([0,xA,c],[0,b,a])}function
cQ(b,a){return eF(b,as,function(b){return a})}function
B(d,b,a){return eF(d,as,function(d){return c(a,G(b,d))})}function
R(e,d,c,b){return eF(e,[0,as],function(f,e){var
g=G(c,e);return a(b,G(d,f),g)})}function
bB(e,d,c,b,a){return eF(e,[0,[0,as]],function(h,g,e){var
i=G(b,e),j=G(c,g);return f(a,G(d,h),j,i)})}B(xC,dX,function(b){return gA(function(c){return a(cJ[7],0,b)})});B(xD,I,function(a){return n(b_(c(d[16],a)))});B(xE,b9,function(a){var
b=c(ce[6],a);return n(b_(c(d[3],b)))});B(xF,o,function(a){return aR(function(c,b){return n(b_(f(bR[11],c,b,a)))})});B(xG,F,function(a){return n(b_(c(h[1][9],a)))});B(xH,cs,function(b){function
c(c){function
d(a){return n(b_(bt(c,a,b,[2,[1,xd],0])))}return a(j[72][1],j[54],d)}return a(j[72][1],j[55],c)});R(xI,dX,dX,function(c,b){return n(b_(a(d[10],c,b)))});R(xJ,I,cs,function(b,c){if(0<=b)if(!(jN[14]<b))return cP(function(d){return a(J,0,cZ(b,c))});return ay(0,cO)});B(xK,dY,function(a){return n([0,a[2].length-1])});bB(xL,dY,I,cs,function(d,a,c){var
b=d[2];if(0<=a)if(!(b.length-1<=a))return gA(function(d){y(b,a)[1+a]=c;return 0});return ay(0,cO)});R(xM,dY,I,function(c,a){var
b=c[2];if(0<=a)if(!(b.length-1<=a))return cP(function(c){return y(b,a)[1+a]});return ay(0,cO)});R(xN,F,F,function(c,b){return n(c4(a(h[1][1],c,b)))});B(xO,F,function(a){var
b=c(h[1][8],a);return n([2,c(ce[5],b)])});B(xP,b9,function(b){try{var
d=c(ce[6],b),e=[0,c(h[1][6],d)],a=e}catch(b){var
a=0}return n(bc(aF,a))});R(xQ,I,I,function(b,a){return n(c4(b===a?1:0))});function
eG(c,b){return R(c,I,I,function(d,c){return n([0,a(b,d,c)])})}eG(xR,K.caml_int_compare);eG(xS,e[4]);eG(xT,e[5]);eG(xU,function(b,a){return K.caml_mul(b,a)});B(xV,I,function(a){return n([0,-a|0])});B(xW,I,function(a){return cP(function(b){return[0,c(dV[1],a)]})});B(xX,dW,function(a){return cP(function(b){return[0,a]})});R(xY,I,dW,function(b,c){if(0<=b)if(!(jN[13]<b))return cP(function(d){return[2,a(ce[1],b,c)]});return ay(0,cO)});B(xZ,b9,function(a){return n([0,hv(a)])});bB(x0,b9,I,dW,function(b,a,c){if(0<=a)if(!(hv(b)<=a))return gA(function(d){return K.caml_bytes_set(b,a,c)});return ay(0,cO)});R(x1,b9,I,function(b,a){if(0<=a)if(!(hv(b)<=a))return cP(function(c){return[0,K.caml_bytes_get(b,a)]});return ay(0,cO)});B(x2,o,function(d){return aR(function(f,e){function
b(l){var
b=p(gB[2],0,f,e,d),g=b[1],h=u(b[2]),i=c(j[16],h),k=c(j[65][1],g);return a(j[72][2],k,i)}return c(j[71][10],b)})});R(x3,o,o,function(d,b){function
e(a){var
e=c4(f(x[104],a,d,b));return c(j[16],e)}return a(j[72][1],j[54],e)});B(x4,o,function(o){function
b(p){var
b=a(x[3],p,o);switch(b[0]){case
0:var
d=a(J,0,[0,[0,b[1]]]);break;case
1:var
d=a(J,1,[0,aF(b[1])]);break;case
2:var
d=a(J,2,[0,[0,b[1]]]);break;case
3:var
g=b[1],q=g[1],r=at(u,g[2]),d=a(J,3,[0,[0,c(gC[1],q)],r]);break;case
4:var
d=a(J,4,[0,[5,fh,b[1]]]);break;case
5:var
s=b[2],t=b[1],v=u(b[3]),d=a(J,5,[0,u(t),[5,fi,s],v]);break;case
6:var
w=b[2],y=b[1],z=u(b[3]),A=u(w),d=a(J,6,[0,eB(ez,y),A,z]);break;case
7:var
B=b[2],C=b[1],D=u(b[3]),E=u(B),d=a(J,7,[0,eB(ez,C),E,D]);break;case
8:var
F=b[3],G=b[2],H=b[1],I=u(b[4]),K=u(F),L=u(G),d=a(J,8,[0,eB(ez,H),L,K,I]);break;case
9:var
M=b[1],N=at(u,b[2]),d=a(J,9,[0,u(M),N]);break;case
10:var
h=b[1],O=h[1],P=gx(h[2]),d=a(J,10,[0,ft(O),P]);break;case
11:var
i=b[1],Q=i[1],d=a(J,11,[0,[5,b7,Q],gx(i[2])]);break;case
12:var
j=b[1],R=j[1],d=a(J,12,[0,[5,cr,R],gx(j[2])]);break;case
13:var
S=b[3],T=b[2],U=b[1],V=at(u,b[4]),W=u(S),d=a(J,13,[0,[5,dT,U],u(T),W,V]);break;case
14:var
k=b[1],l=k[1],X=l[2],Y=l[1],e=jH(k[2]),Z=e[3],_=e[2],$=e[1],d=a(J,14,[0,at(fl,Y),[0,X],$,_,Z]);break;case
15:var
m=b[1],aa=m[1],f=jH(m[2]),d=a(J,15,[0,[0,aa],f[1],f[2],f[3]]);break;case
16:var
ab=b[1],d=a(J,16,[0,[5,fj,ab],u(b[2])]);break;case
17:var
d=a(J,17,[0,[6,b[1]]]);break;default:var
d=a(J,18,[0,[7,b[1]]])}return n(d)}return a(j[72][1],j[54],b)});B(x6,cs,function(C){var
b=aP(C),t=b[1];if(!(18<t>>>0)){switch(t){case
0:var
v=b[2];if(1===v.length-1)var
D=O(v[1]),d=c(x[10],D),a=1;else
var
a=0;break;case
1:var
w=b[2];if(1===w.length-1)var
E=aG(w[1]),d=c(x[11],E),a=1;else
var
a=0;break;case
2:var
y=b[2];if(1===y.length-1)var
F=O(y[1]),d=c(x[12],F),a=1;else
var
a=0;break;case
3:var
l=b[2];if(2===l.length-1)var
G=l[2],I=O(l[1]),J=c(gC[2],I),K=[0,J,bf(H,G)],d=c(x[13],K),a=1;else
var
a=0;break;case
4:var
z=b[2];if(1===z.length-1)var
L=V(fh,z[1]),M=c(x[fe][6],L),d=c(x[14],M),a=1;else
var
a=0;break;case
5:var
i=b[2];if(3===i.length-1)var
N=i[2],P=i[3],Q=H(i[1]),R=V(fi,N),S=[0,Q,R,H(P)],d=c(x[19],S),a=1;else
var
a=0;break;case
6:var
j=b[2];if(3===j.length-1)var
T=j[2],U=j[3],W=eC(eA,j[1]),X=H(T),Y=[0,W,X,H(U)],d=c(x[20],Y),a=1;else
var
a=0;break;case
7:var
k=b[2];if(3===k.length-1)var
Z=k[2],_=k[3],$=eC(eA,k[1]),aa=H(Z),ab=[0,$,aa,H(_)],d=c(x[21],ab),a=1;else
var
a=0;break;case
8:var
f=b[2];if(4===f.length-1)var
ac=f[2],ad=f[3],ae=f[4],af=eC(eA,f[1]),ag=H(ac),ah=H(ad),ai=[0,af,ag,ah,H(ae)],d=c(x[22],ai),a=1;else
var
a=0;break;case
9:var
o=b[2];if(2===o.length-1)var
aj=o[2],ak=H(o[1]),al=[0,ak,bf(H,aj)],d=c(x[23],al),a=1;else
var
a=0;break;case
10:var
p=b[2];if(2===p.length-1)var
am=p[2],an=fu(p[1]),ao=[0,an,gy(am)],d=c(x[25],ao),a=1;else
var
a=0;break;case
11:var
q=b[2];if(2===q.length-1)var
ap=q[2],aq=V(b7,q[1]),ar=[0,aq,gy(ap)],d=c(x[28],ar),a=1;else
var
a=0;break;case
12:var
r=b[2];if(2===r.length-1)var
as=r[2],at=V(cr,r[1]),au=[0,at,gy(as)],d=c(x[30],au),a=1;else
var
a=0;break;case
13:var
g=b[2];if(4===g.length-1)var
av=g[2],aw=g[3],ax=g[4],ay=V(dT,g[1]),az=H(av),aA=H(aw),aB=[0,ay,az,aA,bf(H,ax)],d=c(x[32],aB),a=1;else
var
a=0;break;case
14:var
e=b[2];if(5===e.length-1)var
aC=e[2],aD=e[3],aE=e[4],aF=e[5],aH=bf(O,e[1]),aI=O(aC),aJ=[0,[0,aH,aI],jI([0,aD,aE,aF])],d=c(x[33],aJ),a=1;else
var
a=0;break;case
15:var
h=b[2];if(4===h.length-1)var
aK=h[2],aL=h[3],aM=h[4],aN=O(h[1]),aO=[0,aN,jI([0,aK,aL,aM])],d=c(x[34],aO),a=1;else
var
a=0;break;case
16:var
s=b[2];if(2===s.length-1)var
aQ=s[2],aR=V(fj,s[1]),aS=[0,aR,H(aQ)],d=c(x[26],aS),a=1;else
var
a=0;break;case
17:var
A=b[2];if(1===A.length-1)var
aT=fr(A[1]),d=c(x[37],aT),a=1;else
var
a=0;break;default:var
B=b[2];if(1===B.length-1)var
aU=fs(B[1]),d=c(x[38],aU),a=1;else
var
a=0}if(a)return n(u(d))}throw[0,m,x5]});B(x7,o,function(b){return aR(function(e,d){try{var
f=p(gB[2],0,e,d,b)[1],g=function(a){return n(jJ(u,[0,b]))},h=c(j[65][1],f),i=a(j[72][1],h,g);return i}catch(a){a=v(a);if(c(l[13],a))return n(jJ(u,[1,c(l[1],a)]));throw a}})});function
x8(c,b,a){return n(u(f(x[mh][3],c,b,a)))}bB(x9,z(o),I,o,x8);function
x_(c,b,a){return n(u(f(x[mh][10],b,c,a)))}bB(x$,z(F),I,o,x_);function
ya(b){function
d(a){try{var
d=n([5,dT,p(yb[76],a,b,0,4)]);return d}catch(a){a=v(a);if(c(l[13],a))return ay(0,eD);throw a}}return a(j[72][1],j[55],d)}B(yc,aO(b7),ya);function
yd(f,d){var
b=f[2],g=f[1];function
h(f){try{y(y(a(cF[80],g,f)[1],b)[1+b][4],d);var
h=n([5,cr,[0,[0,g,b],a(e[4],d,1)]]);return h}catch(a){a=v(a);if(c(l[13],a))return ay(0,eD);throw a}}return a(j[72][1],j[55],h)}R(ye,aO(b7),I,yd);bB(yg,F,o,U,function(b,h,p){function
f(f){if(f)if(!f[2]){var
g=f[1],i=function(f){var
g=c(j[67][3],f),q=c(j[67][4],f);try{a(cF[45],b,g);var
C=1,i=C}catch(a){a=v(a);if(a!==D)throw a;var
i=0}if(i){var
r=c(d[3],yf);return a(bC[57][5],0,r)}var
s=[0,a(cg[4],b,0),h],k=a(x[125],s,g),l=T1(jO[7],0,0,0,0,0,k,q,aJ[m9]),t=l[2][1],w=l[1],y=c(cF[14],k),m=T2(jO[5],0,0,0,0,0,0,0,y,w,t),o=m[2],z=m[1];function
A(l){function
d(k){function
d(m){function
d(p){var
d=c(x[e6],g);function
f(a){var
b=c(cg[11][1][2],a);return c(x[11],b)}var
i=a(e[22][68],f,d),j=[0,c(x[10],1),i],k=[0,o,c(e[24][12],j)],l=c(x[13],k),m=[0,a(cg[4],[0,b],0),h,l];return n(u(c(x[21],m)))}var
i=c(j[67][11],f),k=[0,c(j[9],i),0],l=c(j[65][5],k);return a(j[72][1],l,d)}var
i=ax(p);return a(j[72][1],i,d)}var
i=[0,c(j[9],o),0],k=c(j[65][5],i);return a(j[72][1],k,d)}var
B=c(j[65][1],z);return a(j[72][1],B,A)};return a(j[72][1],g,i)}return ay(0,gz)}return a(j[72][1],j[67][9],f)});function
yh(d){return aR(function(g,f){function
b(n){var
b=d[1],e=e2(jP[12],jE,g,f,[0,b[2],b[3],b[1],h[1][11][1]],1,d[2]),i=e[1],k=u(e[2]),l=c(j[16],k),m=c(j[65][1],i);return a(j[72][2],m,l)}return c(j[71][10],b)})}B(yi,aO(fg),yh);var
jQ=c(x[12],a6[2]);cQ(yj,n(u(jQ)));R(yk,bp,o,function(d,a){return aR(function(f,e){try{var
i=[0,p(a6[3],f,e,d,a)],b=i}catch(a){a=v(a);if(a!==a6[1])throw a;var
b=0}if(b){var
g=c(h[1][11][18],b[1]);return n(bo(function(a){var
b=a[1],c=u(a[2]);return bd([0,aF(b),c])},g))}return du(0,eE)})});R(yl,bp,o,function(d,b){function
e(i){var
d=c(gh[6],i);if(d){var
f=d[1],b=f[2],k=d[2],l=c(h[1][11][18],f[1][2]),m=bo(function(a){var
b=a[1],c=u(a[2]);return bd([0,aF(b),c])},l),g=hu(b),o=e7===g?b[1]:hE===g?c(gi[2],b):b,p=bd([0,u(o),m]),q=function(a){return e(k)},r=n(p);return a(j[22],r,q)}return du(0,eE)}return aR(function(c,a){return e(p(a6[8],c,a,[0,h[1][10][1],d],b))})});R(ym,bp,o,function(f,d){return aR(function(i,g){try{var
l=[0,p(a6[3],i,g,f,d)],b=l}catch(a){a=v(a);if(a!==a6[1])throw a;var
b=0}if(b){var
j=c(h[1][11][18],b[1]),k=function(a){return a[2]};return n(at(u,a(e[24][57],k,j)))}return du(0,eE)})});R(yn,bp,o,function(d,b){function
f(k){var
d=c(gh[6],k);if(d){var
g=d[1],b=g[2],l=d[2],m=c(h[1][11][18],g[1][2]),o=function(a){return a[2]},p=at(u,a(e[24][57],o,m)),i=hu(b),q=e7===i?b[1]:hE===i?c(gi[2],b):b,r=bd([0,u(q),p]),s=function(a){return f(l)},t=n(r);return a(j[22],t,s)}return du(0,eE)}return aR(function(c,a){return f(p(a6[8],c,a,[0,h[1][10][1],d],b))})});function
yo(i,g,f){function
b(d){function
b(b){var
k=c(j[67][3],b),l=c(j[67][4],b),m=c(j[67][1],b);function
d(a){var
b=a[2];return a[1]?[0,b]:[1,b]}var
n=d(f),o=[0,a(e[22][68],d,g),n];function
p(b){var
d=b[1],g=b[3],i=b[2];function
f(b){return u(a(w[23],jQ,b))}function
k(a){return a[1]}var
l=at(aF,a(e[24][57],k,d));function
m(a){return a[2]}var
n=at(f,a(e[24][57],m,d)),o=c(h[1][11][18],g);function
p(a){return a[2]}var
q=at(u,a(e[24][57],p,o)),r=bd([0,l,n,q,f(i)]);return c(j[16],r)}var
q=jl(k,l,m,i,o);return a(j[72][1],q,p)}return a(j[67][8],0,b)}return a(j[72][1],jL,b)}var
yp=be(M,bp);bB(yq,M,z(be(M,bp)),yp,yo);R(ys,o,o,function(d,b){var
e=c(x[fe][1],d),f=c(x[fe][1],b),g=a(yr[41],[0,[0,a6[2],f],0],e);return n(u(c(x[9],g)))});B(yt,fo,function(a){return ay([0,a[2]],a[1])});B(yu,fo,function(a){return du([0,a[2]],a[1])});R(yv,U,U,function(c,b){function
d(a){return bM(b,[0,c6(a),0])}var
e=ax(c);return a(j[22],e,d)});B(yw,U,function(a){var
b=ax(a);return c(j[25],b)});function
yx(b){function
d(a){var
b=ax(a);return c(j[19],b)}var
f=a(e[22][68],d,b);function
g(a){return n(aL)}var
h=c(j[36],f);return a(j[72][1],h,g)}B(yy,z(U),yx);function
yz(g,d,b){function
h(a){var
b=ax(a);return c(j[19],b)}var
i=a(e[22][68],h,g),k=ax(d),l=c(j[19],k);function
m(a){var
b=ax(a);return c(j[19],b)}var
o=a(e[22][68],m,b);function
p(a){return n(aL)}var
q=f(j[38],i,l,o);return a(j[72][1],q,p)}var
yA=z(U);bB(yB,z(U),U,yA,yz);B(yC,U,function(b){var
d=ax(b),e=c(j[19],d);function
f(a){return n(aL)}var
g=c(j[39],e);return a(j[72][1],g,f)});B(yD,U,function(b){function
d(b){if(0===b[0])return n(a(J,1,[0,c6(b[1])]));var
d=b[2],e=b[1];return n(a(J,0,[0,bd([0,e,[3,[0,as,function(e){var
b=fn(e),f=b[2],g=b[1];function
h(a){return c(d,[0,g,a])}var
i=dt(f);return a(j[72][1],i,h)}]]])]))}var
e=ax(b),f=c(j[28],e);return a(j[72][1],f,d)});bB(yE,I,I,U,function(c,b,a){var
d=ax(a);return p(j[31],0,c,b,d)});function
yF(a){return n(aL)}cQ(yG,a(j[72][1],j[41],yF));function
yH(a){return n(aL)}cQ(yI,a(j[72][1],j[44],yH));B(yJ,I,function(d){var
b=c(gC[2],d);function
e(d){if(a(aJ[26],d,b)){var
e=c(j[16],aL),f=[0,c(j[9],b),0],g=c(j[65][4],f);return a(j[72][2],g,e)}return ay(0,eD)}return a(j[72][1],j[54],e)});function
yK(d){function
b(a){return n(u(c(jM[32][17],a)))}return a(j[67][8],0,b)}cQ(yL,a(j[72][1],jL,yK));B(yO,F,function(b){return aR(function(f,p){try{a(cF[44],b,f);var
o=1,e=o}catch(a){a=v(a);if(a!==D)throw a;var
e=0}if(e)return n(u(c(x[11],b)));var
g=c(d[3],yM),i=c(h[1][9],b),j=c(d[21],i),k=c(d[3],yN),l=a(d[12],k,j),m=a(d[12],l,g);return a(bC[57][5],0,m)})});cQ(yP,aR(function(a,f){var
b=c(cF[13],a),d=c(e[22][9],b);return n(bo(function(a){if(0===a[0]){var
b=a[1],d=u(c(x[9],a[2])),e=bc(u,0);return bd([0,aF(b[1]),e,d])}var
f=a[3],g=a[1],h=c(x[9],a[2]),i=u(c(x[9],f)),j=bc(u,[0,h]);return bd([0,aF(g[1]),j,i])},d))}));B(yR,U,function(b){function
d(a){var
b=[0,0,H(a)];return c(j[16],b)}var
e=ax(b),g=a(j[72][1],e,d);function
h(a){return n(aL)}function
i(a){return f(yQ[2],1,g,a)}var
k=c(j[67][7],i);return a(j[72][1],k,h)});R(yS,U,U,function(d,b){function
e(e){function
g(d){function
g(g){function
h(c){var
a=bM(b,[0,d,0]);return f(bC[57][38],0,a,g)}var
i=c(j[65][1],e);return a(j[72][1],i,h)}return a(j[72][1],j[54],g)}var
h=ax(d);return a(j[72][1],h,g)}return a(j[72][1],j[54],e)});B(yT,U,function(a){var
b=ax(a);return c(j[59],b)});function
yU(d,b){function
e(a){return n(aL)}var
g=ax(b),h=c(j[19],g),i=f(yV[2],0,d,h);return a(j[72][1],i,e)}R(yW,A(F),U,yU);function
yX(c,b){var
d=a(w[16],ce[6],c),e=ax(b);return a(j[63],d,e)}R(yY,A(b9),U,yX);function
yZ(a){return n(aL)}cQ(y0,a(j[72][1],j[61],yZ));function
y1(c,b){return n([5,b8,a(h[1][10][7],c,b)])}var
y2=aO(b8);R(y3,aO(b8),y2,y1);function
y4(a){return n([5,b8,f(e[22][16],h[1][10][4],a,h[1][10][1])])}B(y5,z(F),y4);B(y6,o,function(d){function
b(b){function
c(e,d){var
f=a(x[3],b,d);return 1===f[0]?a(h[1][10][4],f[1],e):p(x[119],b,c,e,d)}return n([5,b8,c(h[1][10][1],d)])}return a(j[72][1],j[54],b)});function
y7(c,b){function
d(b){return a(h[1][10][3],b,c)}return n(aF(a(f0[25],b,d)))}R(y8,aO(b8),F,y7);function
y9(b){if(b){var
d=c(e[22][e4],b),i=d[1],j=c(e[22][9],d[2]),k=c(h[5][4],j),l=a(r[15],k,i);try{var
m=[0,c(bO[28],l)],f=m}catch(a){a=v(a);if(a!==D)throw a;var
f=0}var
g=f}else
var
g=0;return n(bc(c8,g))}B(y_,z(F),y9);function
y$(a){if(a)var
b=c(e[22][e4],a),g=b[1],i=c(e[22][9],b[2]),j=c(h[5][4],i),k=f(r[23],0,j,g),d=c(bO[23],k);else
var
d=0;return n(bo(c8,d))}B(za,z(F),y$);B(zb,d0,function(d){try{var
f=c(bO[38],d)}catch(a){a=v(a);if(a===D)return ay(0,eD);throw a}var
b=c(r[16],f),g=b[2],i=c(h[5][5],b[1]);return n(bo(aF,a(e[22][10],i,[0,g,0])))});B(zc,d0,function(d){function
b(e){function
b(f){var
b=e2(aJ[173],0,0,0,e,f,d),g=b[2],h=b[1];function
i(a){return n(u(g))}var
k=c(j[65][1],h);return a(j[72][1],k,i)}return a(j[72][1],j[54],b)}return a(j[72][1],j[55],b)});var
cR=aO(a1);function
zd(a){return[5,a1,a]}function
ze(b){if(b){var
d=c(e[22][e4],b),g=d[1],i=c(e[22][9],d[2]),j=c(h[5][4],i),f=a(r[15],j,g);if(c(eH[4],f)){var
k=a(r[29],0,f),l=c(eH[3],k),m=c(e[22][5],l),o=c(eH[12],m),p=c(az[31],0);return n([5,a1,a(az[2][6],p,o)])}throw D}throw D}B(zf,z(F),ze);B(zg,cR,function(b){function
d(a){return n(aL)}var
e=c(az[31],0),f=a(az[24],e,b);return a(j[72][1],f,d)});function
zh(l,k,g){function
m(a){var
b=bM(g,[0,[5,a1,a],0]);return c(j[19],b)}function
o(g,b){var
d=b[1],j=b[3],k=b[2],l=c(bP[22],d),m=a(bP[17],zi,l),e=c(h[1][6],m),n=[2,[1,a(i[1],0,e)]];return[0,d+1|0,[0,n,k],f(h[1][11][4],e,g,j)]}var
b=f(e[22][16],o,k,[0,0,0,h[1][11][1]]),p=b[3],q=b[2],r=c(h[1][6],zj),s=f(h[1][11][4],r,l,p),d=c(az[31],0),t=[0,s,d[2],d[3]],u=c(h[1][6],zk),v=[0,[1,a(i[1],0,u)],q],w=[3,a(i[1],0,v)],x=[29,a(i[1],0,w)];function
y(a){return n(aL)}var
z=f(az[11],t,x,m);return a(j[72][1],z,y)}bB(zl,cR,z(cR),U,zh);B(zm,o,function(a){return n([5,a1,c(az[2][1],a)])});B(zn,cR,function(a){return n(bc(u,c(az[2][2],a)))});function
zo(b){return n([5,a1,a(bh[1][8],[0,bh[1][5]],b)])}B(zp,z(cR),zo);B(zq,cR,function(a){var
b=c(az[2][5],a);return n(bc(function(a){return bo(zd,a)},b))});function
bD(a){return[2,[1,a],0]}function
jR(d,b,a){var
c=f(W[3],jS[11],b,a)[2][1];return[0,[0,c],bD(w7)]}function
zs(a){return a[1]===zt[1]?0:c(l[13],a)}function
jT(i,e,g){var
d=ej(e,h[1][11][1]),b=zr[36],f=[0,b[1],b[2],b[3],d];return aR(function(k,h){try{var
b=e2(jP[12],i,k,h,f,1,g),p=b[1],q=u(b[2]),r=function(a){return c(j[16],q)},s=c(j[65][1],p),t=a(j[72][1],s,r);return t}catch(b){b=v(b);if(zs(b)){var
d=c(l[1],b),e=d[1],m=d[2],n=function(b){return a(bb[4],b,jK)?ay([0,b],e):a(j[21],[0,b],e)},o=dt(m);return a(j[72][1],o,n)}throw b}})}function
zu(b,a){return jT(jE,b,a)}function
zv(e,b){var
f=c(d[3],zw),g=a(bR[26],e,b),h=c(d[3],zx),i=a(d[12],h,g);return a(d[12],i,f)}br(gk,[0,jR,function(b,a){var
d=c(bi[2],0);return f(gD[6],d,b,a)},zu,zv]);function
zy(b,a){return jT(wV,b,a)}function
zz(e,b){var
f=c(d[3],zA),g=a(bR[26],e,b),h=c(d[3],zB),i=a(d[12],h,g);return a(d[12],i,f)}br(dp,[0,jR,function(b,a){var
d=c(bi[2],0);return f(gD[6],d,b,a)},zy,zz]);function
zC(b,a){return n(aF(a))}function
zD(j,b){var
e=c(d[3],zE),f=c(h[1][9],b),g=c(d[3],zF),i=a(d[12],g,f);return a(d[12],i,e)}function
zG(b,a){return a}br(gj,[0,function(c,b,a){return[0,[0,a],bD(w$)]},zG,zC,zD]);function
zH(k,f,d){var
b=f[2],g=c(aJ[17],b),h=c(e[3],di[13])?function(a){return a}:jU[31],i=0,j=a(h,function(a){return a_(jU[20],b,g,zI,0,d)},i)[2];return[0,[0,j],bD(w9)]}function
zJ(d,b){var
a=c(bi[2],0),e=c(aJ[17],a);return p(zK[3],a,e,d,b)}function
zL(e,b){var
g=c(d[3],zM),h=f(bR[28],e,aJ[16],b),i=c(d[3],zN),j=a(d[12],i,h);return a(d[12],j,g)}br(cK,[0,zH,zJ,function(b,a){return n(h1(a))},zL]);function
zO(b,a){return n([5,fg,[0,[0,h[1][11][1],h[1][11][1],h[1][11][1]],a]])}function
zP(b,a){var
d=c(bi[2],0);return f(gD[6],d,b,a)}function
zQ(e,b){var
f=c(d[3],zR),g=a(bR[26],e,b),h=c(d[3],zS),i=a(d[12],h,g);return a(d[12],i,f)}br(gl,[0,function(d,b,a){return c(e[19][1],a)},zP,zO,zQ]);function
zT(i,h,e){var
a=e[1];if(0===a[0]){var
b=a[1];try{var
f=c(bO[12],b),d=f}catch(a){a=v(a);if(a!==D)throw a;var
d=c(bO[4],b)}return[0,[0,d],bD(jF)]}var
g=a[1];return[0,[0,[0,g]],bD(jF)]}function
zU(c,b){return a(zV[13],c,b)}function
zW(b,a){return n(c8(a))}br(eq,[0,zT,zU,zW,function(q,b){if(0===b[0]){var
e=b[1],f=c(d[3],zX),g=c(h[1][9],e),i=c(d[3],zY),j=c(d[3],zZ),k=a(d[12],j,i),l=a(d[12],k,g);return a(d[12],l,f)}var
m=c(d[3],z0),n=c(bR[39],b),o=c(d[3],z1),p=a(d[12],o,n);return a(d[12],p,m)}]);function
z2(p,b,d){var
g=d[2],i=d[1];function
j(a){return a[1]}var
c=a(e[22][68],j,i),k=fS(b[3]),l=f(e[22][16],h[1][10][4],c,b[1]),m=f(W[3],eI[9],[0,l,b[2],k,b[4]],g)[2];function
n(a,b){return[1,bD(gw),a]}var
o=bD(w3);return[0,[0,[0,c,m]],f(e[22][15],n,o,c)]}function
z3(l,b){var
d=b[1],k=b[2];function
g(g){function
i(c,b,a){var
d=V(a1,a);return f(h[1][11][4],b,d,c)}var
l=p(e[22][19],i,h[1][11][1],d,g),m=ej([0,h[1][11][1]],l),b=c(az[31],0),o=a(az[23],[0,m,b[2],b[3]],k);function
q(b){var
c=b[2],d=b[1];function
e(b){return a(j[21],[0,b],d)}var
f=dt(c);return a(j[72][1],f,e)}function
r(a){return n(aL)}var
s=a(j[22],o,q);return a(j[72][1],s,r)}var
i=c(e[22][1],d);return 0===i?g(0):n([3,d2(i,g)])}function
z4(b,a){var
c=a[1];return[0,c,f(W[5],eI[9],b,a[2])]}br(gm,[0,z2,z4,z3,function(i,b){var
f=b[1],j=b[2];if(c(e[22][48],f))var
g=c(d[7],0);else
var
p=c(d[13],0),q=c(d[3],z7),r=c(d[13],0),s=a(d[45],h[1][9],f),t=a(d[12],s,r),u=a(d[12],t,q),g=a(d[12],u,p);var
k=c(d[3],z5),l=a(jV[24],i,j),m=c(d[3],z6),n=a(d[12],m,g),o=a(d[12],n,l);return a(d[12],o,k)}]);function
z8(p,b,d){var
g=d[2],i=d[1];function
j(a){return a[1]}var
c=a(e[22][68],j,i),k=fS(b[3]),l=f(e[22][16],h[1][10][4],c,b[1]),m=f(W[3],eI[9],[0,l,b[2],k,b[4]],g)[2];function
n(a,b){return[1,bD(gw),a]}var
o=bD(gw);return[0,[0,[0,c,m]],f(e[22][15],n,o,c)]}function
z9(k,b){var
d=b[1],j=b[2];function
g(g){function
i(c,b,a){var
d=V(a1,a);return f(h[1][11][4],b,d,c)}var
k=p(e[22][19],i,h[1][11][1],d,g),l=ej([0,h[1][11][1]],k),b=c(az[31],0);return n([5,a1,a(az[2][6],[0,l,b[2],b[3]],j)])}var
i=c(e[22][1],d);return 0===i?g(0):n([3,d2(i,g)])}function
z_(b,a){var
c=a[1];return[0,c,f(W[5],eI[9],b,a[2])]}br(gn,[0,z8,z_,z9,function(i,b){var
f=b[1],j=b[2];if(c(e[22][48],f))var
g=c(d[7],0);else
var
p=c(d[3],Ab),q=a(d[45],h[1][9],f),g=a(d[12],q,p);var
k=c(d[3],z$),l=a(jV[24],i,j),m=c(d[3],Aa),n=a(d[12],m,g),o=a(d[12],n,l);return a(d[12],o,k)}]);function
Ac(i,g,f,e,d,a){var
k=a[2];if(c(h[1][10][2],a[1])){var
l=L(f8(i),k),n=c(j[19],l),o=c(h[1][6],Ad),b=e2(jc[11],o,g,f,e,d,n),p=b[2];return[0,c(x[9],b[1]),p]}throw[0,m,Ae]}a(jW[1],cb,Ac);function
Af(g,j,f,e,d,c){var
i=f8(g)[1],b=H(a(h[1][11][23],c,i));return[0,b,p(gB[4],f,e,b,d)]}a(jW[1],cc,Af);function
Ag(a){return[0,function(b,a){return c(d[7],0)}]}function
Ah(b){return[0,function(i,g){var
e=c(h[1][9],b),f=c(d[3],Ai);return a(d[12],f,e)}]}function
Aj(a){return[0,d[7]]}p(jX[4],cc,Ag,Ah,Aj);function
Ak(m,g){var
i=g[2],n=g[1];function
o(c,d,b){var
e=b[2],f=[12,gl,d[1]];return[0,a(h[1][10][6],c,b[1]),[0,[0,[0,c],f],e]]}var
j=f(h[1][11][12],o,m,[0,n,0]),k=j[2],b=j[1];if(1-c(h[1][10][2],b)){var
p=1<c(h[1][10][20],b)?Al:Ao,q=c(d[3],Am),r=c(h[1][10][21],b),s=a(d[45],h[1][9],r),t=c(d[3],p),u=c(d[3],An),v=a(d[12],u,t),w=a(d[12],v,s),x=a(d[12],w,q);f(l[5],0,0,x)}var
y=c(e[22][48],k)?i:[5,0,k,i];return[0,h[1][10][1],y]}a(W[11],cb,Ak);var
Ap=q[2];function
Aq(b){var
d=b[2],e=c(ao[4],ca);return[0,a(ao[7],e,d)]}f(As[5],Ar,Aq,[0,Ap,0]);var
jY=c(bh[1][1],At),Ax=[0,function(b,i){if(b){var
d=b[1],g=d[2];if(a(bh[1][2],d[1],jY)){var
k=g[2],l=g[1],n=function(c,b){try{var
d=a(h[1][11][23],b,i[1])}catch(a){a=v(a);if(a===D)throw[0,m,Au];throw a}return f(h[1][11][4],b,[5,a1,d],c)},o=L([0,f(e[22][15],n,h[1][11][1],l)],k);return c(j[19],o)}throw[0,m,Av]}throw[0,m,Aw]}];f(eH[16],0,jZ,Ax);var
Ay=[0,jZ,0];function
Az(b,d){var
f=d[1],l=d[2];if(f){var
g=c(h[1][6],AA),m=function(a){return[0,a]},n=a(e[22][68],m,f),k=function(b){return[2,[1,a(i[1],0,b)]]},o=a(e[22][68],k,f),p=[0,Ay,[0,k(g),o]],q=[28,[0,n,[31,a(i[1],0,p)]]],r=a(bh[1][8],[0,jY],d),s=b[3],t=b[2],u=[0,a(h[1][11][5],g,r),t,s],v=a(az[2][6],u,q);return c(j0[1],v)}var
w=a(az[2][6],[0,h[1][11][1],b[2],b[3]],AB),x=[0,h[1][11][1]];function
y(a){return c(j0[1],w)}var
z=L(x,l);return a(j[72][1],z,y)}a(bh[7],ca,Az);function
AC(a){return[0,function(b,a){return c(d[7],0)}]}function
AD(b){var
f=b[1],i=b[2];if(c(e[22][48],f))var
g=c(d[7],0);else
var
j=c(d[3],AE),k=a(d[45],h[1][9],f),g=a(d[12],k,j);return[0,function(e,c){var
b=d_(i);return a(d[12],g,b)}]}function
AF(a){return[0,d[7]]}p(jX[4],ca,AC,AD,AF);function
aS(b,a){return i1(c(h[1][6],b),a)}function
j1(b){switch(b[0]){case
0:return c(d[19],b[1][1]);case
1:return c(d[16],b[1][1]);default:var
e=b[2][1],g=b[3],i=e?c(h[1][9],e[1]):c(d[3],AJ),j=c(d[3],AG),k=function(a){return c(d[3],AH)},l=f(d[39],k,j1,g),m=c(d[3],AI),n=a(d[12],i,m),o=a(d[12],n,l);return a(d[12],o,j)}}function
aT(e,b){var
g=c(d[3],AK);function
h(a){return c(d[3],AL)}var
i=f(d[39],h,j1,b),j=c(d[3],AM),k=a(d[12],j,i),m=a(d[12],k,g),n=c(d[3],e),o=c(d[3],AN),p=c(d[3],AO),q=a(d[12],p,m),r=a(d[12],q,o),s=a(d[12],r,n);return f(l[5],0,0,s)}var
j2=a(i[1],0,AP);function
j3(b,e,d){return aS(b,function(c){if(c)return aT(b,c);var
f=[6,e];return[0,f,function(b){return a(i[1],0,[12,d,b])}]})}aS(AR,function(a){if(a){var
b=a[1];if(0===b[0])if(!a[2]){var
c=[0,[0,b[1][1]]];return[0,c,function(a){return j2}]}}return aT(AQ,a)});aS(AT,function(a){if(a){var
b=a[1];if(0===b[0])if(!a[2]){var
d=[0,c(el[10],b[1][1])];return[0,d,function(a){return j2}]}}return aT(AS,a)});aS(AV,function(a){if(a){var
b=a[2],d=a[1];if(!b){var
g=bz(d),l=g[2],m=[3,g[1]];return[0,m,function(a){return N(0,l,a)}]}var
e=b[1];if(0===e[0])if(!b[2]){var
h=e[1][1],f=bz(d),i=f[2],j=f[1],k=[4,j,[0,c(el[10],h)]];return[0,k,function(a){return N(0,i,a)}]}}return aT(AU,a)});aS(AX,function(a){if(a){var
b=a[2],d=a[1];if(!b){var
g=bz(d),l=g[2],m=[1,g[1]];return[0,m,function(a){return N(0,l,a)}]}var
e=b[1];if(0===e[0])if(!b[2]){var
h=e[1][1],f=bz(d),i=f[2],j=f[1],k=[2,j,[0,c(el[10],h)]];return[0,k,function(a){return N(0,i,a)}]}}return aT(AW,a)});aS(AZ,function(b){if(b)if(!b[2]){var
d=bz(b[1]),e=d[2],f=[5,d[1]];return[0,f,function(b){if(b){var
d=[0,c(e,b[1]),0],f=[4,a(i[1],0,[2,[1,[1,xn]]]),d];return a(i[1],0,f)}return a(i[1],0,[2,[1,[1,xl]]])}]}return aT(AY,b)});aS(A1,function(a){if(a)return aT(A0,a);var
b=0;return[0,b,function(a){return a}]});aS(A3,function(a){if(a)return aT(A2,a);var
b=1;return[0,b,function(a){return a}]});aS(A7,function(a){if(a){var
d=a[1];if(1===d[0])if(!a[2]){var
b=d[1][1],e=b<0?1:0,f=e||(6<b?1:0);if(f)aT(A5,a);var
g=c(bP[22],b),h=[7,q[1],g];return[0,h,function(a){return a}]}return aT(A4,a)}var
i=[7,q[1],A6];return[0,i,function(a){return a}]});aS(A9,function(a){if(a)if(!a[2]){var
b=bz(a[1]),d=b[2],e=b[1];return[0,e,function(a){return bA(c(d,a))}]}return aT(A8,a)});aS(A$,function(b){function
c(a){if(2===a[0]){var
c=a[2][1];if(c)if(!a[3])return c[1]}return aT(A_,b)}var
d=a(e[22][68],c,b);function
f(a){return ci([0,d],a)}return[0,[6,g[16][1]],f]});function
Z(a,d,c){return aS(a,function(b){return b?aT(a,b):[0,[6,d],c]})}function
Ba(a){return aw(ar,a)}Z(Bb,q[3],Ba);Z(Bc,q[4],eu);Z(Bd,q[5],eu);Z(Be,q[6],ev);Z(Bf,q[7],dr);Z(Bg,q[8],gs);Z(Bh,q[9],jq);Z(Bi,q[10],js);Z(Bj,q[11],jt);Z(Bk,q[12],gr);Z(Bl,q[19],jx);Z(Bm,q[14],ew);Z(Bn,q[13],jv);Z(Bo,q[16],jw);Z(Bp,q[15],gu);Z(Bq,q[20],jB);Z(Br,q[21],jC);Z(Bs,q[22],jD);Z(Bt,q[17],jz);Z(Bu,q[18],jA);j3(Bv,g[16][1],dp);j3(Bw,g[16][1],cK);var
gE=[dI,Bx,dF(0)];function
bW(a){if(typeof
a==="number"){if(0===a)throw gE;throw gE}else
switch(a[0]){case
0:return[0,a[1]];case
1:return[1,bW(a[1])];case
2:var
b=a[2],c=bW(a[1]);return[2,c,bW(b)];case
3:return[3,bW(a[1])];case
4:var
d=a[2],e=bW(a[1]);return[4,e,bW(d)];case
5:return[5,bW(a[1])];case
6:return[6,a[1]];case
7:return[7,a[1],a[2]];default:return[8,a[1]]}}function
gF(a){if(a){var
b=a[2],d=a[1];if(b){var
e=b[1];return function(b,a){var
f=[0,c(e,a),b];return c(gF(d),f)}}return function(a,b){return c(gF(d),a)}}return function(b,a){return bj([0,a],b)}}function
j4(a){if(a){var
b=a[1],e=a[2],c=bz(b),f=c[2],g=bW(c[1]),d=j4(e),h=d[2],i=[1,d[1],g],j=0===b[0]?0:[0,f];return[0,i,[0,h,j]]}return By}aS(BA,function(g){try{var
a=j4(c(e[22][9],g)),i=a[1],j=[8,[0,[0,i,c(gF(a[2]),0)],0]],b=j}catch(a){a=v(a);if(a!==gE)throw a;var
h=c(d[3],Bz),b=f(l[5],0,0,h)}return[0,b,function(a){return a}]});var
eJ=[0,w5,xh,xj,wX,xb,wZ,w1,xp,xr];aD(1285,[0,eJ,aR],"Ltac2_plugin__Tac2core");function
gG(a){return ff(function(a){throw[0,m,BB]},a)}function
j5(e){var
a=aP(e),b=a[1];if(0===b){var
c=a[2];if(1===c.length-1)return[0,O(c[1])]}else
if(1===b){var
d=a[2];if(1===d.length-1)return[1,aG(d[1])]}throw[0,m,BC]}var
j6=gG(j5);function
j7(a){switch(a[0]){case
0:if(0===a[1])return 0;break;case
1:var
b=a[1];if(0===b){var
c=a[2];if(1===c.length-1)return[0,aN(H,c[1])]}else
if(1===b){var
d=a[2];if(1===d.length-1){var
e=d[1];return[1,aN(function(a){return fp(j5,H,a)},e)]}}break}throw[0,m,BD]}var
dv=gG(j7),aZ=gG(function(b){var
a=bq(b);if(2===a.length-1){var
c=a[1],d=j7(a[2]);return[0,H(c),d]}throw[0,m,BE]});aD(1286,[0,j6,dv,aZ],"Ltac2_plugin__Tac2extffi");var
gH=j[16];function
dw(b,a){return c9(a,am,b,0)}function
dx(e,d,b){var
f=a(j[3],b,0)[2],g=c(h[1][6],BF);return[0,b,a_(j[15],g,0,d,e,f)[1]]}function
dy(d,c,b,a){return dx(dw(d,c),b,a)}function
cS(b){if(typeof
b==="number")return 0;else{if(0===b[0])return[0,b[1]];var
c=b[1],d=i[1],f=function(b){return a(d,0,b)};return[1,a(e[22][68],f,c)]}}function
bE(a){var
b=a[1];return[0,b,cS(a[2])]}function
bF(d){switch(d[0]){case
0:return a(i[1],0,[0,d[1]]);case
1:var
f=[1,gI(d[1])];return a(i[1],0,f);default:var
b=d[1];if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
c=[0,gJ(b[1])];break;case
1:var
c=[1,a(e[22][68],bF,b[1])];break;case
2:var
g=b[2],h=b[1],j=function(a,b){return dy(o,h,a,b)},k=a(i[1],0,j),c=[2,k,bF(g)];break;default:var
c=[3,b[1]]}return a(i[1],0,[2,c])}}function
gI(a){return typeof
a==="number"?0:0===a[0]?[0,a[1]]:[1,a[1]]}function
gJ(b){if(0===b[0]){var
c=b[1],d=function(b){return a(e[22][68],bF,b)};return[0,a(e[22][68],d,c)]}return[1,a(e[22][68],bF,b[1])]}function
j8(b){return a(e[22][68],bF,b)}function
j9(c,b){return typeof
b==="number"?0===b?0:2:0===b[0]?[0,a(e[22][68],c,b[1])]:[1,a(e[22][68],c,b[1])]}function
bG(a){return j9(function(a){return[0,a]},a)}function
BG(a){var
b=a[3],c=a[1];return[0,[0,bG(a[2]),c],b]}function
aA(b){var
c=bG(b[2]),d=b[1];function
f(b){return a(e[22][68],BG,b)}return[0,a(w[16],f,d),c]}function
j_(c,b){var
d=j8(b);return a(t[41],c,d)}function
j$(g,d,l,b){function
m(b){function
d(a){return c(gH,bE(a))}var
e=dw(aZ,b),f=a(j[72][1],e,d);function
g(a,b){return dx(f,a,b)}return[0,0,a(i[1],0,g)]}var
h=a(e[22][68],m,l);if(b){var
k=b[1],n=k[1],o=a(w[16],bF,k[2]);return a_(t[95],g,d,n,h,o)}return f(t[90],g,d,h)}function
BH(b){var
d=b[1],k=b[4],l=b[3],m=b[2];function
n(b){var
c=gI(b);return a(i[1],0,c)}var
o=a(w[16],n,m);function
p(b){var
c=gJ(b);return a(i[1],0,c)}var
q=a(w[16],p,l),r=a(w[16],aA,k),s=[0,o,q];switch(d[0]){case
0:var
f=d[1],g=function(a){return c(gH,bE(a))},h=a(j[72][1],f,g),e=[0,function(a,b){return dx(h,a,b)}];break;case
1:var
e=[1,a(i[1],0,d[1])];break;default:var
e=[2,d[1]]}return[0,[0,0,e],s,r]}function
gK(g,d,c,b){var
h=a(e[22][68],BH,c),i=[0,h,a(w[16],bE,b)];return f(t[mI],g,d,i)}function
ka(d,c,b){var
e=bE(c),f=a(w[16],bE,b);return p(t[101],d,0,e,f)}function
kb(b){function
d(a){var
b=a[3],c=a[2],d=a[1];return[0,[0,j9(function(a){return a},c),d],b]}var
f=a(e[22][68],d,b);return c(t[150],f)}function
kc(b,a){var
c=bE(a);return f(t[e4],b,0,c)}function
kd(d,c,b,a){var
e=cS(a);return p(t[mL],d,c,b,e)}function
ke(c,b){var
d=cS(b);return a(t[115],c,d)}function
kf(c,b){var
d=cS(b);return a(t[116],c,d)}function
kg(c,b){var
d=[0,cS(b),0];return a(t[117],c,d)}function
kh(c,b){var
d=bE(c),e=a(w[16],bF,b);return a(t[81],d,e)}function
ki(f,d,b){function
g(j){function
g(g,f,b){var
i=c(h[1][11][18],g);function
j(a){return a[2]}var
k=a(e[24][57],j,i);return dx(c9(d,fq(o),o,k),f,b)}var
i=aA(b);return p(t[72],1,f,g,i)}return c(j[67][7],g)}function
kj(g,f,d,b){function
h(b){var
d=b[3],e=b[2],f=b[1];function
g(a){return c(gH,bE(a))}var
h=a(j[72][1],d,g);function
i(a,b){return dx(h,a,b)}return[0,a(w[23],1,f),e,0,i]}var
i=a(e[22][68],h,f),k=aA(d);function
l(a){var
b=dw(am,a);return[0,c(bC[57][34],b),0]}var
m=a(w[16],l,b);return p(dz[11],g,i,k,m)}function
kk(a){var
b=aA(a);return c(t[e6],b)}function
kl(e,d,c,b){var
f=a(w[16],bF,c);return p(t[ah],e,d,f,b)}function
km(b){if(0===b[0]){var
c=b[3],d=b[2],e=a(w[16],bF,b[1]),f=function(a){return dw(am,a)},g=[0,a(w[16],f,c)];return p(t[ah],1,g,e,d)}var
h=b[2],j=[0,a(i[1],0,[1,[0,b[1]]])];return p(t[ah],1,0,j,h)}function
gL(f,e,d,c,b){function
g(b){var
c=b[1],d=gI(b[2]);return[0,c,a(i[1],0,d)]}var
h=a(w[16],g,e),j=aA(b);return a_(t[147],f,h,d,c,j)}function
cT(d){var
a=d[2],b=d[1];if(0===b[0]){var
c=b[1];switch(c[0]){case
0:var
e=[0,[0,c[1]]];return[0,bG(a),e];case
1:var
f=[0,[1,c[1]]];return[0,bG(a),f]}}return[0,bG(a),[1,b]]}function
bk(b){switch(b[0]){case
0:return c(j[16],[0,b[1]]);case
1:return c(j[16],[1,b[1]]);default:var
e=c(d[3],BI),f=c(d[13],0),g=a(bO[44],h[1][10][1],b),i=c(d[13],0),k=c(d[3],BJ),l=a(d[12],k,i),m=a(d[12],l,g),n=a(d[12],m,f),o=a(d[12],n,e);return a(bC[57][5],0,o)}}function
eK(c,b){var
d=aA(b);return a(t[74],c,d)}function
kn(b,d,c){var
e=a(w[16],cT,d),f=aA(c);function
g(c){return a(t[74],[1,[0,b[1],b[2],b[3],b[4],b[5],b[6],c],e],f)}var
h=a(j[20][5][1],bk,b[7]);return a(j[72][1],h,g)}function
ko(b,c){var
d=aA(c);function
e(c){return a(t[74],[2,[0,b[1],b[2],b[3],b[4],b[5],b[6],c]],d)}var
f=a(j[20][5][1],bk,b[7]);return a(j[72][1],f,e)}function
kp(b,c){var
d=aA(c);function
e(c){return a(t[74],[3,[0,b[1],b[2],b[3],b[4],b[5],b[6],c]],d)}var
f=a(j[20][5][1],bk,b[7]);return a(j[72][1],f,e)}function
kq(b,c){var
d=aA(c);function
e(c){return a(t[74],[4,[0,b[1],b[2],b[3],b[4],b[5],b[6],c]],d)}var
f=a(j[20][5][1],bk,b[7]);return a(j[72][1],f,e)}function
kr(d,b){var
e=aA(b);function
f(b){var
d=b[1],e=bG(b[2]);function
f(a){return c(j[16],[0,e,a])}var
g=bk(d);return a(j[72][1],g,f)}function
g(b){return a(t[74],[5,b],e)}var
h=a(j[20][5][1],f,d);return a(j[72][1],h,g)}function
ks(c,b){function
d(a){var
b=a[1];return[0,bG(a[2]),b]}var
f=a(e[22][68],d,c),g=aA(b);return a(t[74],[7,f],g)}function
kt(c,b){var
d=a(w[16],cT,c),e=aA(b);return a(t[74],[9,d],e)}function
ku(c,b){var
d=a(w[16],cT,c),e=aA(b);return a(t[74],[10,d],e)}function
a8(g,e){return aR(function(b,h){var
d=f(a(BK[2],b,g)[1],b,h,e),i=d[2],k=d[1];function
l(a){return c(j[16],i)}var
m=c(j[65][1],k);return a(j[72][1],m,l)})}function
kv(a){return a8(BL,a)}function
kw(a){return a8(0,a)}function
kx(b,d,c){var
e=a(w[16],cT,d);function
f(a){return a8([1,[0,b[1],b[2],b[3],b[4],b[5],b[6],a],e],c)}var
g=a(j[20][5][1],bk,b[7]);return a(j[72][1],g,f)}function
ky(b,c){function
d(a){return a8([2,[0,b[1],b[2],b[3],b[4],b[5],b[6],a]],c)}var
e=a(j[20][5][1],bk,b[7]);return a(j[72][1],e,d)}function
kz(b,c){function
d(a){return a8([3,[0,b[1],b[2],b[3],b[4],b[5],b[6],a]],c)}var
e=a(j[20][5][1],bk,b[7]);return a(j[72][1],e,d)}function
kA(b,c){function
d(a){return a8([4,[0,b[1],b[2],b[3],b[4],b[5],b[6],a]],c)}var
e=a(j[20][5][1],bk,b[7]);return a(j[72][1],e,d)}function
kB(d,b){function
e(b){var
d=b[1],e=bG(b[2]);function
f(a){return c(j[16],[0,e,a])}var
g=bk(d);return a(j[72][1],g,f)}function
f(a){return a8([5,a],b)}var
g=a(j[20][5][1],e,d);return a(j[72][1],g,f)}function
kC(b,a){return a8([6,b],a)}function
kD(c,b){function
d(a){var
b=a[1];return[0,bG(a[2]),b]}return a8([7,a(e[22][68],d,c)],b)}function
kE(c,b){return a8([9,a(w[16],cT,c)],b)}function
kF(c,b){return a8([10,a(w[16],cT,c)],b)}function
gM(e,b,h){function
d(l){if(h){var
k=h[1],d=k[2],m=k[1];switch(d[0]){case
0:var
n=d[1],o=c(j[67][3],l),q=function(e){function
d(d){var
f=d[1],g=cS(d[2]);function
h(d){var
a=p(BM[14],[0,[0,1,1,1-b,1,0,0]],o,d,[0,e,f]);return c(j[16],[0,[0,a[1]],[0,[0,a[2],g]]])}return a(j[72][1],j[54],h)}return a(j[72][1],n,d)},g=a(j[72][1],j[54],q);break;case
1:var
s=[0,0,[1,a(i[1],0,d[1])]],g=c(j[16],s);break;default:var
g=c(j[16],[0,0,[2,d[1]]])}var
r=function(c){var
d=c[1],g=[0,[0,m,c[2]]];if(d){var
h=d[1],i=a(e,b,g);return f(bC[57][38],b,i,h)}return a(e,b,g)};return a(j[72][1],g,r)}return a(e,b,0)}return c(j[67][7],d)}function
kG(c,b){function
d(a){return[0,0,a]}var
e=a(w[16],d,b);return gM(dz[19],c,e)}function
kH(d,c,b){function
e(a){return[0,0,a]}var
f=a(w[16],e,b),g=a(w[16],j8,c);return gM(function(b,a){return p(dz[21],0,g,b,a)},d,f)}function
kI(k,b,j,i){var
c=k?BN:0,d=a(e[22][68],h[1][8],j),g=aA(i);if(b){var
l=dw(am,b[1]);return p(kJ[7],c,l,d,g)}return f(kJ[6],c,d,g)}function
kK(d,c,b){function
g(a){return function(b,c){return dy(o,a,b,c)}}var
i=a(e[22][68],g,c);function
j(b){return a(e[22][68],h[1][8],b)}var
k=a(w[16],j,b);return f(eL[18],[0,d],i,k)}function
kL(f,d,c,b){function
g(a){return function(b,c){return dy(o,a,b,c)}}var
i=a(e[22][68],g,c);function
j(b){return a(e[22][68],h[1][8],b)}var
k=a(w[16],j,b);return p(eL[14],[0,f],d,i,k)}function
kM(d,c,j,b){function
g(b){return a(gN[10],b,0)[2]}function
k(a){return function(b,c){return dy(o,a,b,c)}}var
i=a(e[22][68],k,j);if(b){var
l=a(e[22][68],h[1][8],b[1]),m=g(c);return p(eL[8],[0,d],m,i,l)}var
n=g(c);return f(eL[11],[0,d],n,i)}function
kN(m,f,d,c,b){function
g(a){return function(b,c){return dy(o,a,b,c)}}var
i=a(e[22][68],g,c);function
j(b){return a(e[22][68],h[1][8],b)}var
k=a(w[16],j,b),l=a(gN[10],f,d);return p(gN[5],0,l,i,k)}function
kO(g,f,b){if(b)var
d=0,c=a(e[22][68],h[1][8],b[1]);else
var
d=1,c=[0,kP[1],0];return a_(kP[8],[0,d],0,g,f,c)}function
kQ(e,r,l,k){var
f=k?k[1]:0;function
s(d){return gM(function(o,g){if(g){var
b=g[1][2];switch(b[0]){case
0:var
h=b[1],k=a(i[1],0,BO),l=function(b){function
a(a){return p(gO[1],e,d,f,[1,a])}return c(bC[57][45],a)},n=a(t[81],h,[0,k]);return a(j[72][1],n,l);case
1:return p(gO[1],e,d,f,[1,b[1][1]]);default:return p(gO[1],e,d,f,[0,b[1]])}}throw[0,m,BP]},1,[0,[0,0,r]])}if(l){var
n=l[1];if(2===n[0]){var
b=n[1];if(typeof
b==="number")var
h=1;else
if(0===b[0])var
v=gJ(b[1]),w=[0,a(i[1],0,v)],o=c(j[16],w),g=1,h=0;else
var
h=1;if(h)var
g=0}else
var
g=0;if(!g)var
u=c(d[3],BQ),o=a(bC[57][5],0,u);var
q=o}else
var
q=c(j[16],0);return a(j[72][1],q,s)}function
kR(b){var
d=a(w[16],bE,b);return c(kS[2],d)}aD(1297,[0,j_,j$,gK,ka,kc,kb,kd,ke,kf,kg,kh,ki,kj,kk,kl,km,gL,eK,kn,ko,kp,kq,kr,ks,kt,ku,kv,kw,kx,ky,kz,kA,kB,kC,kD,kE,kF,kG,kH,kI,kK,kL,kM,kN,kO,kQ,kR],"Ltac2_plugin__Tac2tactics");function
aB(a){return ff(function(a){throw[0,m,BR]},a)}function
kT(a){return c(j[16],a)}var
BS=dU(0);function
gP(b,a){return c9(a,am,b,0)}function
kU(a,b){return c9(d1(am,a,b),am,a,0)}function
bl(a){return fw(am,a)}function
kV(b){var
a=a3(aG,b);return a?[0,a[1]]:0}var
eM=aB(kV);function
eN(a){switch(a[0]){case
0:var
b=a[1];if(0===b)return 0;if(1===b)return 1;break;case
1:var
c=a[1];if(0===c){var
d=a[2];if(1===d.length-1)return[0,aN(O,d[1])]}else
if(1===c){var
e=a[2];if(1===e.length-1)return[1,aN(O,e[1])]}break}throw[0,m,BT]}var
gQ=aB(eN);function
kW(b){var
a=bq(b);if(2===a.length-1){var
c=a[1],d=a[2],e=function(d){var
a=bq(d);if(3===a.length-1){var
e=a[1],f=a[2],c=O(a[3]);if(2<c>>>0)throw[0,m,BU];switch(c){case
0:var
b=0;break;case
1:var
b=1;break;default:var
b=2}var
g=eN(f);return[0,aG(e),g,b]}throw[0,m,BV]},f=a3(function(a){return aN(e,a)},c);return[0,f,eN(d)]}throw[0,m,BW]}var
ak=aB(kW),bX=aB(function(b){var
a=bq(b);if(7===a.length-1){var
c=a[1],d=a[2],e=a[3],f=a[4],g=a[5],h=a[6],i=aN(fv,a[7]),j=a2(h),k=a2(g),l=a2(f),n=a2(e),o=a2(d);return[0,a2(c),o,n,l,k,j,i]}throw[0,m,BX]}),cU=be(bp,gQ),kX=be(o,gQ),kY=be(d0,gQ);function
dA(p){var
d=aP(p),f=d[1];if(!(2<f>>>0))switch(f){case
0:var
g=d[2];if(1===g.length-1)return[0,a2(g[1])];break;case
1:var
h=d[2];if(1===h.length-1)return[1,kZ(h[1])];break;default:var
i=d[2];if(1===i.length-1){var
b=i[1];switch(b[0]){case
0:if(0===b[1])var
c=0,a=1;else
var
a=0;break;case
1:var
j=b[1];if(3<j>>>0)var
a=0;else
switch(j){case
0:var
k=b[2];if(1===k.length-1)var
c=[0,k0(k[1])],a=1;else
var
a=0;break;case
1:var
l=b[2];if(1===l.length-1)var
q=l[1],c=[1,aN(function(a){return dA(a)},q)],a=1;else
var
a=0;break;case
2:var
e=b[2];if(2===e.length-1)var
r=e[2],s=d1(am,o,e[1]),c=[2,s,dA(r)],a=1;else
var
a=0;break;default:var
n=b[2];if(1===n.length-1)var
c=[3,a2(n[1])],a=1;else
var
a=0}break;default:var
a=0}if(a)return[2,c];throw[0,m,B0]}}throw[0,m,BY]}function
kZ(a){switch(a[0]){case
0:if(0===a[1])return 0;break;case
1:var
b=a[1];if(0===b){var
c=a[2];if(1===c.length-1)return[0,aG(c[1])]}else
if(1===b){var
d=a[2];if(1===d.length-1)return[1,aG(d[1])]}break}throw[0,m,BZ]}function
k0(e){var
a=aP(e),b=a[1];if(0===b){var
c=a[2];if(1===c.length-1)return[0,aN(gR,c[1])]}else
if(1===b){var
d=a[2];if(1===d.length-1)return[1,gR(d[1])]}throw[0,m,B1]}function
gR(a){return aN(dA,a)}var
dB=aB(dA),k1=aB(gR);function
k2(f){var
a=aP(f),b=a[1];if(!(2<b>>>0))switch(b){case
0:var
c=a[2];if(1===c.length-1)return[0,kU(aZ,c[1])];break;case
1:var
d=a[2];if(1===d.length-1)return[1,aG(d[1])];break;default:var
e=a[2];if(1===e.length-1)return[2,O(e[1])]}throw[0,m,B2]}var
gS=aB(k2),k3=aB(function(b){var
a=bq(b);if(4===a.length-1){var
c=a[2],d=a[3],e=a[4],f=k2(a[1]),g=a3(kZ,c),h=a3(k0,d);return[0,f,g,h,a3(kW,e)]}throw[0,m,B3]}),B5=aB(function(e){var
b=aP(e),d=b[1];if(0===d){var
a=b[2];if(3===a.length-1){var
f=a[1],g=a[2],h=a[3],i=function(a){return d1(am,am,a)},j=a3(dA,f),k=H(g);return[0,j,k,a3(i,h)]}}else
if(1===d){var
c=b[2];if(2===c.length-1){var
l=c[1],n=H(c[2]);return[1,aG(l),n]}}throw[0,m,B4]}),B8=aB(function(i){var
c=bq(i);if(3===c.length-1){var
b=c[2],j=c[3],k=a3(a2,c[1]);switch(b[0]){case
0:var
e=b[1];if(0===e)var
d=0,a=1;else
if(1===e)var
d=1,a=1;else
var
a=0;break;case
1:var
f=b[1];if(0===f){var
g=b[2];if(1===g.length-1)var
d=[0,O(g[1])],a=1;else
var
a=0}else
if(1===f){var
h=b[2];if(1===h.length-1)var
d=[1,O(h[1])],a=1;else
var
a=0}else
var
a=0;break;default:var
a=0}if(a)return[0,k,d,kU(aZ,j)];throw[0,m,B6]}throw[0,m,B7]}),eO=aB(function(b){var
a=O(b);if(2<a>>>0)throw[0,m,B9];switch(a){case
0:return 2;case
1:return 1;default:return 0}}),B$=aB(function(b){var
a=O(b);if(0===a)return 1;if(1===a)return 0;throw[0,m,B_]}),Cb=aB(function(b){var
a=O(b);if(2<a>>>0)throw[0,m,Ca];switch(a){case
0:return 0;case
1:return 1;default:return 2}}),k4=aB(function(a){switch(a[0]){case
0:var
b=a[1];if(0===b)return 0;if(1===b)return 1;break;case
1:var
c=a[1];if(0===c){var
d=a[2];if(1===d.length-1)return[0,aG(d[1])]}else
if(1===c){var
e=a[2];if(1===e.length-1)return[1,aG(e[1])]}break}throw[0,m,Cc]}),Ce=aB(function(b){var
a=bq(b);if(3===a.length-1){var
c=a[1],d=a[2],e=kV(a[3]),f=eN(d);return[0,H(c),f,e]}throw[0,m,Cd]});function
bH(a){return[0,Cf,a]}function
cV(b){var
c=kT(BS);return a(j[72][2],b,c)}function
dC(b,a){var
c=[0,as,function(b){return cV(a)}];return a4(bH(b),c)}function
S(d,b,a){var
e=[0,as,function(d){return cV(c(a,G(b,d)))}];return a4(bH(d),e)}function
ab(e,d,c,b){var
f=[0,[0,as],function(f,e){var
g=G(c,e);return cV(a(b,G(d,f),g))}];return a4(bH(e),f)}function
a9(e,d,c,b,a){var
g=[0,[0,[0,as]],function(h,g,e){var
i=G(b,e),j=G(c,g);return cV(f(a,G(d,h),j,i))}];return a4(bH(e),g)}function
cW(f,e,d,c,b,a){var
g=[0,[0,[0,[0,as]]],function(i,h,g,f){var
j=G(b,f),k=G(c,g),l=G(d,h);return cV(p(a,G(e,i),l,k,j))}];return a4(bH(f),g)}function
k5(g,f,e,d,c,b,a){var
h=[0,[0,[0,[0,[0,as]]]],function(k,j,i,h,g){var
l=G(b,g),m=G(c,h),n=G(d,i),o=G(e,j);return cV(a_(a,G(f,k),o,n,m,l))}];return a4(bH(g),h)}ab(Cg,M,k1,function(b,a){return j_(b,a)});function
Ch(d,c,b,a){return j$(d,c,b,a)}var
Ci=A(be(F,A(dB)));cW(Cj,M,M,z(bl(aZ)),Ci,Ch);function
Ck(c,b,a){return ka(c,b,a)}a9(Cl,M,aZ,A(aZ),Ck);ab(Cm,M,aZ,function(b,a){return kc(b,a)});function
Cn(a){return kb(a)}S(Co,z(Ce),Cn);S(Cp,B5,function(a){return km(a)});function
Cq(d,c,b){function
e(b){function
c(a){return gP(am,a)}return a(w[16],c,b)}return kl(0,a(w[16],e,c),b,d)}var
Cr=A(dB);a9(Cs,o,A(A(bl(am))),Cr,Cq);ab(Cu,eM,o,function(b,a){return a_(t[146],0,b,a,0,Ct[9])});function
Cv(d,c,b){function
e(e){function
f(a){return gL(d,0,a[1],[0,e,a[2]],b)}var
g=gP(be(eM,o),c);return a(j[72][1],g,f)}return a(j[72][1],j[54],e)}a9(Cw,M,bl(be(eM,o)),ak,Cv);function
Cx(i,h,g,f,e){var
b=a(w[23],Cy,f);if(1===b[0]){var
l=b[1],m=function(b){function
c(a){return gL(i,[0,[0,1,l]],h,[0,b,a],e)}var
d=gP(o,g);return a(j[72][1],d,c)};return a(j[72][1],j[54],m)}var
k=c(d[3],Cz);return a(bC[57][5],0,k)}var
CA=A(dB);k5(CB,M,eM,bl(o),CA,ak,Cx);function
CC(c,b,a){return gK(0,c,b,a)}var
CD=A(aZ);a9(CE,M,z(k3),CD,CC);function
CF(c,b,a){return gK(1,c,b,a)}var
CG=A(aZ);a9(CH,M,z(k3),CG,CF);S(CJ,ak,function(a){return eK(CI,a)});S(CK,ak,function(a){return eK(0,a)});function
CL(c,b,a){return kn(c,b,a)}a9(CM,bX,A(cU),ak,CL);ab(CN,bX,ak,function(b,a){return ko(b,a)});ab(CO,bX,ak,function(b,a){return kp(b,a)});ab(CP,bX,ak,function(b,a){return kq(b,a)});function
CQ(b,a){return kr(b,a)}ab(CR,z(kY),ak,CQ);function
CS(b,a){return eK([6,b],a)}ab(CT,z(o),ak,CS);function
CU(b,a){return ks(b,a)}ab(CV,z(kX),ak,CU);function
CW(b,a){return kt(b,a)}ab(CX,A(cU),ak,CW);function
CY(b,a){return ku(b,a)}ab(CZ,A(cU),ak,CY);function
gT(b){function
d(a){var
b=u(a);return c(j[16],b)}return a(j[72][1],b,d)}function
k6(d,b,a){var
e=[0,as,function(d){return gT(c(a,G(b,d)))}];return a4(bH(d),e)}function
bY(e,d,c,b){var
f=[0,[0,as],function(f,e){var
g=G(c,e);return gT(a(b,G(d,f),g))}];return a4(bH(e),f)}k6(C1,o,function(a){return kv(a)});k6(C2,o,function(a){return kw(a)});var
C3=A(cU),C0=[0,[0,[0,as]],function(c,b,a){var
d=G(o,a),e=G(C3,b);return gT(kx(G(bX,c),e,d))}];a4(bH(C4),C0);bY(C5,bX,o,function(b,a){return ky(b,a)});bY(C6,bX,o,function(b,a){return kz(b,a)});bY(C7,bX,o,function(b,a){return kA(b,a)});function
C8(b,a){return kB(b,a)}bY(C9,z(kY),o,C8);function
C_(b,a){return kC(b,a)}bY(C$,z(o),o,C_);function
Da(b,a){return kD(b,a)}bY(Db,z(kX),o,Da);function
Dc(b,a){return kE(b,a)}bY(Dd,A(cU),o,Dc);function
De(b,a){return kF(b,a)}bY(Df,A(cU),o,De);function
Dg(c,b,a){return ki(c,b,a)}var
Dh=fw(fq(o),o);a9(Di,A(bp),Dh,ak,Dg);function
Dj(d,c,b,a){return kj(d,c,b,a)}var
Dk=A(bl(am));cW(Dl,M,z(B8),ak,Dk,Dj);function
Dm(d,c,b,a){return kQ(d,c,b,a)}var
Dn=A(z(F));cW(Do,Cb,gS,A(dB),Dn,Dm);dC(Dp,t[124]);ab(Dq,F,k4,function(c,b){return a(t[82],c,b)});function
Dr(c,b){var
d=a(w[23],1,b);return a(t[18],c,d)}var
Ds=A(k4);ab(Dt,A(F),Ds,Dr);dC(Du,t[42]);S(Dv,o,function(a){return c(t[l5],[0,a])});dC(Dw,c(t[l5],0));S(Dx,o,function(a){return c(t[144],a)});ab(Dy,M,dv,function(b,a){return ke(b,a)});ab(Dz,M,dv,function(b,a){return kf(b,a)});S(DA,j6,function(a){return c(t[31],a)});S(DB,o,function(a){return c(t[43],a)});S(DC,o,function(a){return c(t[44],a)});S(DD,o,function(a){return c(t[45],a)});S(DE,M,function(b){return a(t[110],b,0)});a9(DF,M,I,dv,function(c,b,a){return kd(c,0,b,a)});function
DG(b,a){return kh(b,a)}ab(DH,aZ,A(dB),DG);S(DI,ak,function(a){return kk(a)});ab(DJ,M,dv,function(b,a){return kg(b,a)});function
DK(a){return c(t[83],a)}S(DL,z(be(F,F)),DK);function
DM(a){return c(t[84],a)}S(DN,z(F),DM);dC(DO,j[58]);ab(DP,F,I,function(c,b){return a(t[8],c,b)});S(DQ,F,function(a){return c(t[10],a)});function
DR(a){return c(t[76],a)}S(DS,z(F),DR);function
DT(a){return c(t[79],a)}S(DU,z(F),DT);function
DV(a){return c(t[77],a)}S(DW,z(F),DV);function
DX(b,a){return kG(b,a)}ab(DY,M,A(gS),DX);function
DZ(c,b,a){return kH(c,b,a)}var
D0=A(gS);a9(D1,M,A(k1),D0,DZ);S(D2,o,function(a){return c(kS[1],a)});function
D3(a){return kR(a)}S(D4,A(aZ),D3);function
D5(d,c,b,a){return kI(d,c,b,a)}var
D6=z(F);cW(D7,M,A(bl(am)),D6,ak,D5);function
D8(a){return c(dz[35],a)}S(D9,z(F),D8);function
D_(b){return a(dz[36],0,0)}var
D$=kT(0);dC(Ea,a(j[72][1],D$,D_));function
Eb(c,b,a){return kK(c,b,a)}var
Ec=A(z(F));a9(Ed,eO,z(bl(o)),Ec,Eb);function
Ee(e,d,c,b,a){return kN(e,d,c,b,a)}var
Ef=A(z(F)),Eg=z(bl(o)),Eh=A(I);k5(Ei,eO,A(I),Eh,Eg,Ef,Ee);function
Ej(d,c,b,a){return kL(d,c,b,a)}var
Ek=A(z(F)),El=z(bl(o));cW(Em,eO,A(I),El,Ek,Ej);function
En(d,c,b,a){return kM(d,c,b,a)}var
Eo=A(z(F)),Ep=z(bl(o));cW(Eq,eO,A(I),Ep,Eo,En);function
Er(c,b,a){return kO(c,b,a)}var
Es=A(z(F)),Et=A(I);a9(Eu,A(B$),Et,Es,Er);aD(1299,[0],"Ltac2_plugin__Tac2stdlib");function
k7(a){throw k8[1]}function
bZ(c,b){function
d(c,a){return f(b,c,0,a)?0:k7(0)}return a(g[2][4],c,d)}function
aC(g,e,b,d,a){var
c=f(g,b,d,a);return c?f(e,b,c[1],a):0}function
eP(g,e,c,b,a){var
d=f(g,c,b,a);return d?[0,d[1]]:f(e,c,b,a)}function
k9(c,a,b){return[0,a]}function
aU(f,g,c,d){var
b=a(e[28],c,d);if(typeof
b!=="number")switch(b[0]){case
0:case
2:return a(e[20][34],f,b[1])?[0,a(e[4],c,1)]:0}return 0}function
cj(f,b,d){var
c=a(e[28],b,d);if(typeof
c!=="number"&&2===c[0])return[0,a(e[4],b,1)];return 0}function
k_(g,c,f){var
b=a(e[28],c,f);if(typeof
b!=="number"&&4===b[0]){var
d=b[1];if(!e1(d[2],Ev))if(!e1(d[3],Ew))return[0,a(e[4],c,1)]}return 0}function
Ey(a,b,c){return aU(Ex,a,b,c)}function
Ez(a,b,c){return aC(Ey,cj,a,b,c)}function
eQ(a,b,c){return eP(cj,Ez,a,b,c)}function
gU(b,a){function
c(a,b,c){return aC(cj,gU,a,b,c)}return function(d){return eP(c,k9,b,a,d)}}function
EB(a,b,c){return aU(EA,a,b,c)}function
EC(a,b,c){return eP(eQ,k_,a,b,c)}function
EE(a,b,c){return aU(ED,a,b,c)}function
EF(a,b,c){return aC(EE,EC,a,b,c)}var
k$=bZ(EG,function(a,b,c){return aC(EF,EB,a,b,c)});function
EI(a,b,c){return aU(EH,a,b,c)}function
EK(a,b,c){return aU(EJ,a,b,c)}function
EL(a,b,c){return aC(EK,eQ,a,b,c)}var
la=bZ(EM,function(a,b,c){return aC(EL,EI,a,b,c)});function
EO(a,b,c){return aU(EN,a,b,c)}function
EQ(a,b,c){return aU(EP,a,b,c)}function
ER(a,b,c){return aC(EQ,eQ,a,b,c)}var
gV=bZ(ES,function(a,b,c){return aC(ER,EO,a,b,c)});function
EU(a,b,c){return aU(ET,a,b,c)}function
EW(a,b,c){return aU(EV,a,b,c)}function
EX(a,b,c){return aC(EW,cj,a,b,c)}var
EZ=bZ(EY,function(a,b,c){return aC(EX,EU,a,b,c)});function
lb(h,b,g){var
d=c(k8[13],g),i=a(e[4],d,b),j=a(e[5],i,1);return f(E0[5],h,d,j)?[0,b]:0}function
E2(a,b,c){return aU(E1,a,b,c)}function
E3(a,b,c){return aC(E2,cj,a,b,c)}var
lc=bZ(E4,function(a,b,c){return aC(E3,lb,a,b,c)});function
E6(a,b,c){return aU(E5,a,b,c)}var
ld=bZ(E7,function(a,b,c){return aC(E6,cj,a,b,c)});function
E9(a,b,c){return aU(E8,a,b,c)}var
gW=bZ(E_,function(a,b,c){return aC(gU,E9,a,b,c)}),ac=q[1],aM=c(g[2][1],E$),gX=c(g[2][1],Fa),gY=c(g[2][1],Fb),gZ=c(g[2][1],Fc),g0=c(g[2][1],Fd),g1=c(g[2][1],Fe),g2=c(g[2][1],Ff),g3=Fg[16],le=q[2];function
cX(d,c,b){return a(i[1],[0,c],[12,d,b])}function
lf(b,a){return cX(dp,b,a)}function
lg(b,a){return cX(cK,b,a)}function
lh(b,a){return cX(eq,b,a)}function
li(b,a){return cX(gm,b,a)}function
lj(b,a){return cX(gn,b,a)}function
g4(b){if(bs(b))return a(i[1],b[2],[1,[0,b],0]);if(c(r[32],b)){var
e=[0,[0,c(r[34],b)]];return a(i[1],b[2],e)}var
g=c(d[3],Fh);return f(l[5],b[2],0,g)}var
bm=c(g[2][1],Fi),lk=c(g[2][1],Fj),ll=c(g[2][1],Fk),g5=c(g[2][1],Fl),eR=c(g[2][1],Fm),g6=c(g[2][1],Fn),eS=c(g[2][1],Fo),lm=c(g[2][1],Fp),g7=c(g[2][1],Fq),ln=c(g[2][1],Fr),lo=c(g[2][1],Fs),eT=c(g[2][1],Ft),lp=c(g[2][1],Fu),eU=c(g[2][1],Fv),lq=c(g[2][1],Fw),g8=c(g[2][1],Fx),lr=c(g[2][1],Fy),g9=c(g[2][1],Fz),g_=c(g[2][1],FA),eV=c(g[2][1],FB),g$=c(g[2][1],FC),eW=c(g[2][1],FD),ls=c(g[2][1],FE),lt=c(g[2][1],FF),lu=c(g[2][1],FG),ha=c(g[2][1],FH),hb=c(g[2][1],FI),lv=c(g[2][1],FJ),hc=c(g[2][1],FK),lw=c(g[2][1],FL),FM=0,FN=0,FQ=[0,[0,FP,function(c,b){return a(i[1],[0,b],FO)}],FN],FT=[0,[0,FS,function(c,b){return a(i[1],[0,b],FR)}],FQ];function
FU(a,b){return g4(a)}var
FV=[0,[0,[0,0,[6,g[15][16]]],FU],FT],FZ=[0,[0,FY,0,[0,[0,[0,[0,FX,[6,lk]],FW],function(d,a,c,b){return a}],FV]],FM],F0=0;function
F1(g,e,b){if(bs(e))return a(i[1],[0,b],[1,[0,e],g]);var
h=c(d[3],F2);return f(l[5],[0,b],0,h)}var
F4=[0,[0,[0,[0,0,[6,g[15][16]]],[1,[7,bm,F3]]],F1],F0];function
F5(a,b){return g4(a)}var
F6=[0,[0,[0,0,[6,g[15][16]]],F5],F4],F8=[0,[0,F7,function(d,c,b){return a(i[1],[0,b],[1,[1,[1,eJ[2]]],0])}],F6],Ga=[0,0,[0,[0,F$,F_,[0,[0,[0,[0,[0,0,[6,bm]],F9],[6,bm]],function(d,e,c,b){return a(i[1],[0,b],[1,[1,[1,eJ[3]]],[0,c,[0,d,0]]])}],F8]],FZ]];f(g[19],bm,0,Ga);var
Gb=0,Gc=0,Ge=[0,[0,0,function(b){return a(i[1],[0,b],Gd)}],Gc],Gg=[0,[0,[0,[0,[0,0,[6,bm]],Gf],[6,aM]],function(d,e,c,b){return a(i[1],[0,b],[2,c,d])}],Ge],Gj=[0,[0,[0,[0,[0,0,[6,bm]],Gi],[4,[6,bm],Gh]],function(g,j,f,d){var
b=[0,f,g],h=[1,[1,[0,c(e[22][1],b)]],b];return a(i[1],[0,d],h)}],Gg],Gk=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,bm]],function(a,b){return a}],Gj]],Gb]];f(g[19],lk,0,Gk);var
Gl=0,Gm=0,Go=[0,[0,Gn,function(d,a,c,b){return a}],Gm],Gr=[0,[0,[0,[0,Gq,[6,aM]],Gp],function(g,d,f,c,e,b){return a(i[1],[0,b],[6,c,d])}],Go],Gu=[0,[0,Gt,function(c,b){return a(i[1],[0,b],Gs)}],Gr],Gx=[0,[0,Gw,function(d,c,b){return a(i[1],[0,b],Gv)}],Gu],GC=[0,[0,[0,[0,GB,[4,[7,ac,GA],Gz]],Gy],function(d,b,c,a){return N([0,a],function(a){return a},b)}],Gx],GF=[0,[0,[0,[0,GE,[6,g$]],GD],function(e,c,d,b){return a(i[1],[0,b],[9,c])}],GC],GH=[0,[0,GG,0,[0,[0,[0,0,[6,lm]],function(a,b){return a}],GF]],Gl],GI=0,GK=[0,[0,[0,[0,0,[6,ac]],[1,[7,ac,GJ]]],function(d,c,b){return a(i[1],[0,b],[4,c,d])}],GI];function
GL(f,d,e,c,b){return a(i[1],[0,b],[10,c,[0,d]])}var
GO=[0,[0,[0,[0,GN,[6,g[15][16]]],GM],GL],GK];function
GP(e,h,g,d,f,c,b){return a(i[1],[0,b],[11,c,[0,d],e])}var
GW=[0,[0,GV,GU,[0,[0,[0,[0,[0,[0,GT,[6,g[15][16]]],GS],GR],[7,ac,GQ]],GP],GO]],GH],GX=0,G1=[0,[0,G0,GZ,[0,[0,[0,[0,[0,0,[6,ac]],GY],[6,ac]],function(d,f,c,b){var
e=[4,a(i[1],[0,b],[2,[1,[1,eJ[3]]]]),[0,c,[0,d,0]]];return a(i[1],[0,b],e)}],GX]],GW],G2=0,G5=[0,G4,[0,[0,0,0,[0,[0,G3,function(g,k,f,b){var
d=[0,f,g],h=[2,[1,[0,c(e[22][1],d)]]],j=[4,a(i[1],[0,b],h),d];return a(i[1],[0,b],j)}],G2]],G1]],G6=0,G_=[0,[0,[0,[0,[0,G9,[1,[6,eU]]],G8],[7,ac,G7]],function(d,f,c,e,b){return a(i[1],[0,b],[3,c,d])}],G6],Hd=[0,[0,[0,[0,[0,[0,Hc,[6,eR]],[2,[6,ln],Hb]],Ha],[7,ac,G$]],function(e,g,d,c,f,b){return a(i[1],[0,b],[5,c,d,e])}],G_],Hj=[0,[0,Hi,0,[0,[0,[0,[0,[0,[0,Hh,[7,ac,Hg]],Hf],[6,ll]],He],function(g,d,f,c,e,b){return a(i[1],[0,b],[8,c,d])}],Hd]],G5],Hk=0,Ho=[0,0,[0,[0,Hn,Hm,[0,[0,Hl,function(d,e,c,b){return a(i[1],[0,b],[7,c,d])}],Hk]],Hj]];f(g[19],ac,0,Ho);var
Hp=0,Hq=0,Hr=[0,[0,0,function(a){return 0}],Hq],Hu=[0,[0,[0,Ht,[2,[6,g5],Hs]],function(a,c,b){return a}],Hr],Hw=[0,0,[0,[0,0,0,[0,[0,[0,0,[2,[6,g5],Hv]],function(a,b){return a}],Hu]],Hp]];f(g[19],ll,0,Hw);var
Hx=0,Hy=0,HC=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[7,bm,HB]],HA],[7,ac,Hz]],function(b,d,a,c){return[0,a,b]}],Hy]],Hx]];f(g[19],g5,0,HC);var
HD=0,HE=0,HG=[0,[0,HF,function(b,a){return 1}],HE],HH=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],HG]],HD]];f(g[19],eR,0,HH);var
HI=0,HJ=0,HL=[0,[0,HK,function(b,a){return 1}],HJ],HM=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],HL]],HI]];f(g[19],g6,0,HM);var
HN=0,HO=0;function
HP(a,c,b){return a}f(g[19],eS,0,[0,0,[0,[0,0,0,[0,[0,[0,HQ,[6,g[15][2]]],HP],HO]],HN]]);var
HR=0,HS=0;function
HT(c,b){return a(i[1],[0,b],[0,[0,c]])}var
HU=[0,[0,[0,0,[6,g[15][12]]],HT],HS];function
HV(c,b){return a(i[1],[0,b],[0,[1,c]])}var
HW=[0,[0,[0,0,[6,g[15][13]]],HV],HU];function
HX(b,c){return bs(b)?a(i[1],[0,c],[2,[0,b]]):a(i[1],[0,c],[1,[0,b]])}var
HY=[0,[0,[0,0,[6,g[15][16]]],HX],HW];function
HZ(c,d,b){return ar(a(i[1],[0,b],c))}var
H1=[0,[0,[0,H0,[6,g[15][2]]],HZ],HY],H3=[0,[0,[0,H2,[6,hc]],function(b,c,a){return gt([0,a],b)}],H1];function
H4(b,c,a){return lf(a,b)}var
H6=[0,[0,[0,H5,[6,g[16][1]]],H4],H3];function
H7(f,a,e,d,c,b){return ci(0,a)}var
H_=[0,[0,[0,[0,H9,[6,g[16][3]]],H8],H7],H6];function
H$(f,a,e,d,c,b){return cN(a)}var
Ic=[0,[0,[0,[0,Ib,[6,g[16][3]]],Ia],H$],H_],If=[0,[0,[0,[0,Ie,[6,hc]],Id],function(f,a,e,d,c,b){return ar(a)}],Ic];function
Ig(f,b,e,d,c,a){return lg(a,b)}var
Ij=[0,[0,[0,[0,Ii,[6,g[16][14]]],Ih],Ig],If],Im=[0,[0,[0,[0,Il,[6,lw]],Ik],function(f,b,e,d,c,a){return lh(a,b)}],Ij],Ip=[0,[0,[0,[0,Io,[6,g7]],In],function(f,b,e,d,c,a){return li(a,b)}],Im],Is=[0,0,[0,[0,0,0,[0,[0,[0,[0,Ir,[6,g7]],Iq],function(f,b,e,d,c,a){return lj(a,b)}],Ip]],HR]];f(g[19],lm,0,Is);var
It=0,Iu=0,Iw=[0,[0,[0,[0,[0,[0,0,[6,gW]],[3,[6,eT]]],Iv],[6,g3]],function(b,e,a,d,c){return[0,a,b]}],Iu],Ix=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g3]],function(a,b){return[0,0,a]}],Iw]],It]];f(g[19],g7,0,Ix);var
Iy=0,Iz=0,IC=[0,[0,[0,[0,[0,[0,0,[6,gW]],[3,[6,eT]]],IB],[6,ac]],function(g,m,b,k,j){function
i(b){var
e=b[2],g=b[1],i=bs(a(r[31],e,g));if(i){var
j=c(h[1][9],g),k=c(d[3],IA),m=a(d[12],k,j);return f(l[5],e,0,m)}return i}a(e[22][11],i,b);return[0,b,g]}],Iz],ID=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,ac]],function(a,b){return[0,0,a]}],IC]],Iy]];f(g[19],le,0,ID);var
IE=0,IF=0,IH=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,lo]],IG],[6,ac]],function(c,h,b,e){var
d=b[2],f=b[1],g=d?a(i[1],[0,e],[3,d[1],c]):c;return[0,f,g]}],IF]],IE]];f(g[19],ln,0,IH);var
II=0,IJ=0,IL=[0,0,[0,[0,0,0,[0,[0,[0,0,[1,[6,eU]]],function(a,g){if(a){var
b=a[1],e=b[1];if(0===e[0]){var
h=e[1];if(!a[2])return[0,b,0];if(h)return[0,b,[0,a[2]]]}else
if(!a[2])return[0,b,0]}var
i=c(d[3],IK);return f(l[5],[0,g],0,i)}],IJ]],II]];f(g[19],lo,0,IL);var
IM=0,IN=0,IR=[0,[0,[0,[0,IQ,[7,aM,IP]],IO],function(d,a,c,b){return a}],IN],IS=[0,[0,[0,0,[6,eS]],function(c,b){return a(i[1],[0,b],[0,[0,c]])}],IR],IV=[0,[0,IU,function(c,b){return a(i[1],[0,b],IT)}],IS];function
IW(c,b){return a(i[1],[0,b],[2,[0,c],0])}var
IX=[0,[0,[0,0,[6,g[15][16]]],IW],IV];function
IY(d,f,c,e,b){return a(i[1],[0,b],[2,[0,d],c])}var
I4=[0,[0,I3,0,[0,[0,[0,[0,[0,I2,[2,[7,aM,I1],I0]],IZ],[6,g[15][16]]],IY],IX]],IM],I5=0;function
I6(d,c,b){return a(i[1],[0,b],[2,[0,d],[0,c,0]])}var
I_=[0,[0,I9,I8,[0,[0,[0,I7,[6,g[15][16]]],I6],I5]],I4],I$=0,Je=[0,[0,Jd,0,[0,[0,[0,[0,[0,0,[6,aM]],Jc],[2,[7,aM,Jb],Ja]],function(g,j,f,d){var
b=[0,f,g],h=[2,[1,[0,c(e[22][1],b)]],b];return a(i[1],[0,d],h)}],I$]],I_],Jf=0,Jj=[0,0,[0,[0,Ji,Jh,[0,[0,[0,[0,[0,0,[6,aM]],Jg],[6,aM]],function(d,e,c,b){return a(i[1],[0,b],[1,c,d])}],Jf]],Je]];f(g[19],aM,0,Jj);var
Jk=0,Jl=0;function
Jm(c,b){return a(i[1],[0,b],c)}f(g[19],eT,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][2]]],Jm],Jl]],Jk]]);var
Jn=0,Jo=0,Jq=[0,[0,Jp,function(c,b){return a(i[1],[0,b],0)}],Jo];function
Jr(c,b){return a(i[1],[0,b],[0,c])}f(g[19],lp,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][2]]],Jr],Jq]],Jn]]);var
Js=0,Jt=0,Jv=[0,0,[0,[0,0,0,[0,[0,[0,0,[7,bm,Ju]],function(a,b){return a}],Jt]],Js]];f(g[19],eU,0,Jv);var
Jw=0,Jx=0,Jz=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,0,[6,lp]],[3,[6,eU]]],Jy],[6,ac]],function(d,j,b,g,f){var
h=c(e[22][48],b)?d:a(i[1],[0,f],[3,b,d]);return[0,g,h]}],Jx]],Jw]];f(g[19],lq,0,Jz);var
JA=0,JB=0,JD=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,g6]],[6,eR]],[2,[6,lq],JC]],function(c,b,a,d){return[0,a,b,c]}],JB]],JA]];f(g[19],gX,0,JD);var
JE=0,JF=0;function
JG(b,e,a,d,c){return[4,a,b]}f(g[19],g1,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,JI,[6,g[15][16]]],JH],[6,ac]],JG],JF]],JE]]);var
JJ=0,JK=0,JL=[0,[0,[0,0,[6,aM]],function(a,b){return[0,[0,a]]}],JK],JN=[0,[0,JM,function(d,c,b,a){return 0}],JL],JQ=[0,[0,[0,[0,JP,[6,lr]],JO],function(d,a,c,b){return[1,a]}],JN],JT=[0,0,[0,[0,0,0,[0,[0,[0,[0,JS,[6,g_]],JR],function(d,a,c,b){return[2,a]}],JQ]],JJ]];f(g[19],g8,0,JT);var
JU=0,JV=0,JY=[0,[0,[0,JX,[2,[6,g9],JW]],function(a,c,b){return a}],JV],J0=[0,0,[0,[0,0,0,[0,[0,[0,0,[4,[6,g9],JZ]],function(a,b){return a}],JY]],JU]];f(g[19],lr,0,J0);var
J1=0,J2=0;function
J3(a,b){return[0,a,0]}var
J4=[0,[0,[0,0,[6,g[15][2]]],J3],J2];function
J5(e,b,d,a,c){return[0,a,b]}f(g[19],g9,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,0,[6,g[15][2]]],J8],[4,[6,aM],J7]],J6],J5],J4]],J1]]);var
J9=0,J_=0,Ka=[0,[0,[0,[0,[0,0,[6,eV]],J$],[6,g_]],function(b,d,a,c){return[0,a,b]}],J_],Kc=[0,[0,[0,[0,0,[6,eV]],Kb],function(c,a,b){return[0,a,0]}],Ka],Kd=[0,[0,[0,0,[6,eV]],function(a,b){return[0,a,0]}],Kc],Ke=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Kd]],J9]];f(g[19],g_,0,Ke);var
Kf=0,Kg=0;function
Kh(c,e,b,a,d){return[0,b,a,c]}f(g[19],eV,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,0,[6,g6]],[6,g[15][2]]],Ki],[6,aM]],Kh],Kg]],Kf]]);var
Kj=0,Kk=0,Km=[0,[0,[0,[0,[0,0,[6,eW]],Kl],[6,g$]],function(b,d,a,c){return[0,a,b]}],Kk],Ko=[0,[0,[0,[0,0,[6,eW]],Kn],function(c,a,b){return[0,a,0]}],Km],Kp=[0,[0,[0,0,[6,eW]],function(a,b){return[0,a,0]}],Ko],Kq=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Kp]],Kj]];f(g[19],g$,0,Kq);var
Kr=0,Ks=0;function
Kt(b,d,a,c){return[0,[0,a],b]}f(g[19],eW,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,g[15][16]]],Kv],[7,ac,Ku]],Kt],Ks]],Kr]]);var
Kw=0,Kx=0,Ky=[0,[0,0,function(a){return 0}],Kx],Kz=[0,[0,[0,0,[6,eS]],function(c,b){return[0,a(i[1],[0,b],c),0]}],Ky];function
KA(d,a,c,b){return a}var
KD=0,KF=[0,0,[0,[0,0,0,[0,[0,[0,[0,KE,[2,[8,[0,[0,[1,0,[6,eS]],function(c,b){return a(i[1],[0,b],c)}],KD]],KC]],KB],KA],Kz]],Kw]];f(g[19],ls,0,KF);var
KG=0,KH=0;function
KI(a,c,b,d){return[0,c,a[1],[0,b,a[2]]]}f(g[19],lt,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,ls]],[6,g[15][16]]],[6,lu]],KI],KH]],KG]]);var
KJ=0,KK=0,KM=[0,[0,0,function(a){return KL}],KK],KO=[0,[0,[0,KN,[6,g8]],function(a,c,b){return[0,0,a]}],KM],KQ=[0,0,[0,[0,0,0,[0,[0,[0,KP,[6,g8]],function(a,c,b){return[0,1,a]}],KO]],KJ]];f(g[19],lu,0,KQ);var
KR=0,KS=0,KV=[0,0,[0,[0,0,0,[0,[0,[0,[0,KU,[6,eR]],[2,[6,lt],KT]],function(b,a,d,c){return[1,a,b]}],KS]],KR]];f(g[19],gY,0,KV);var
KW=0,KX=0;function
KY(d,c,i,b,h,a,g,f,e){return[2,a,b,[0,c,d]]}f(g[19],gZ,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,[0,K2,[6,eT]],K1],[7,aM,K0]],KZ],[6,g[15][13]]],[6,g[15][13]]],KY],KX]],KW]]);var
K3=0,K4=0,K6=[0,[0,K5,function(c,b){return a(i[1],[0,b],0)}],K4];function
K7(c,b){return a(i[1],[0,b],[0,c])}f(g[19],ha,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][2]]],K7],K6]],K3]]);var
K8=0,K9=0;function
K_(c,b){return[0,a(i[1],[0,b],c)]}var
K$=[0,[0,[0,0,[6,g[15][13]]],K_],K9];function
La(c,b){return[1,a(i[1],[0,b],c)]}var
Lb=[0,[0,[0,0,[6,g[15][12]]],La],K$],Lc=[0,[0,[0,0,[6,ha]],function(b,a){return[2,a,b,0]}],Lb],Lg=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,0,[6,ha]],Lf],[2,[6,hb],Le]],Ld],function(e,c,d,b,a){return[2,a,b,c]}],Lc]],K8]];f(g[19],hb,0,Lg);var
Lh=0,Li=0,Lj=[0,[0,0,function(a){return 0}],Li];function
Lk(a,c,b){return[0,a]}f(g[19],lv,0,[0,0,[0,[0,0,0,[0,[0,[0,Ll,[6,g[15][12]]],Lk],Lj]],Lh]]);var
Lm=0,Ln=0,Lq=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,Lp,[1,[6,hb]]],[6,lv]],Lo],[6,ac]],function(c,f,b,a,e,d){return[3,a,b,c]}],Ln]],Lm]];f(g[19],g0,0,Lq);var
Lr=0,Ls=0;function
Lt(c,b){return a(i[1],[0,b],c)}f(g[19],hc,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][2]]],Lt],Ls]],Lr]]);var
Lu=0,Lv=0;function
Lw(c,d,b){return a(i[1],[0,b],[1,c])}var
Ly=[0,[0,[0,Lx,[6,g[15][2]]],Lw],Lv];function
Lz(c,b){return a(i[1],[0,b],[0,c])}f(g[19],lw,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][16]]],Lz],Ly]],Lu]]);function
LA(b){var
d=c(e[22][105],b)[1],f=c(e[22][5],b)[1];return a(gf[6],f,d)}var
lx=c(g[2][1],LB),aV=c(g[2][1],LC),cY=c(g[2][1],LD),ck=c(g[2][1],LE),ly=c(g[2][1],LF),lz=c(g[2][1],LG),hd=c(g[2][1],LH),eX=c(g[2][1],LI),he=c(g[2][1],LJ),lA=c(g[2][1],LK),hf=c(g[2][1],LL),lB=c(g[2][1],LM),bI=c(g[2][1],LN),lC=c(g[2][1],LO),eY=c(g[2][1],LP),lD=c(g[2][1],LQ),hg=c(g[2][1],LR),b0=c(g[2][1],LS),hh=c(g[2][1],LT),lE=c(g[2][1],LU),hi=c(g[2][1],LV),dD=c(g[2][1],LW),lF=c(g[2][1],LX),hj=c(g[2][1],LY),lG=c(g[2][1],LZ),hk=c(g[2][1],L0),hl=c(g[2][1],L1),lH=c(g[2][1],L2),lI=c(g[2][1],L3),lJ=c(g[2][1],L4),lK=c(g[2][1],L5),lL=c(g[2][1],L6),hm=c(g[2][1],L7),eZ=c(g[2][1],L8),lM=c(g[2][1],L9),hn=c(g[2][1],L_),ho=c(g[2][1],L$),hp=c(g[2][1],Ma),lN=c(g[2][1],Mb),lO=c(g[2][1],Mc),e0=c(g[2][1],Md),hq=c(g[2][1],Me),lP=c(g[2][1],Mf),lQ=c(g[2][1],Mg),lR=c(g[2][1],Mh),hr=c(g[2][1],Mi),lS=c(g[2][1],Mj),lT=c(g[2][1],Mk),lU=c(g[2][1],Ml),lV=c(g[2][1],Mm),lW=c(g[2][1],Mn),hs=c(g[2][1],Mo),lX=c(g[2][1],Mp),Mq=0,Mr=0;function
Ms(c,d,b){return[1,a(i[1],[0,b],c)]}f(g[19],lx,0,[0,0,[0,[0,0,0,[0,[0,[0,Mt,[6,g[15][2]]],Ms],Mr]],Mq]]);var
Mu=0,Mv=0,Mw=[0,[0,[0,0,[6,cY]],function(a,b){return[0,a]}],Mv];function
Mx(c,d,b){return[1,a(i[1],[0,b],c)]}f(g[19],aV,0,[0,0,[0,[0,0,0,[0,[0,[0,My,[6,g[15][2]]],Mx],Mw]],Mu]]);var
Mz=0,MA=0;function
MB(c,b){return a(i[1],[0,b],c)}f(g[19],cY,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][2]]],MB],MA]],Mz]]);var
MC=0,MD=0;function
ME(c,b){return a(i[1],[0,b],c)}f(g[19],ck,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[15][10]]],ME],MD]],MC]]);var
MF=0,MG=0,MH=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,aV]],function(a,b){return a}],MG]],MF]];f(g[19],q[3],0,MH);var
MI=0,MJ=0,MK=[0,[0,[0,0,[6,lx]],function(a,b){return a}],MJ],ML=[0,[0,[0,0,[6,ck]],function(c,b){return[0,a(i[1],[0,b],[0,c])]}],MK],MM=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,cY]],function(c,b){return[0,a(i[1],[0,b],[1,c])]}],ML]],MI]];f(g[19],ly,0,MM);var
MN=0,MO=0;function
MP(g,d,f,c,e,b){return a(i[1],[0,b],[0,c,d])}f(g[19],lz,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,MS,[6,ly]],MR],[6,g[16][3]]],MQ],MP],MO]],MN]]);var
MT=0,MU=0,MV=[0,[0,[0,[0,0,[6,k$]],[1,[6,lz]]],function(c,d,b){return a(i[1],[0,b],[1,c])}],MU];function
MW(c,b){return a(i[1],[0,b],[0,c])}f(g[19],hd,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[1,[6,g[16][1]]]],MW],MV]],MT]]);var
MX=0,MY=0,MZ=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hd]],function(a,b){return a}],MY]],MX]];f(g[19],q[4],0,MZ);var
M0=0,M1=0,M2=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hg]],function(a,b){return a}],M1]],M0]];f(g[19],q[5],0,M2);var
M3=0,M4=0,M5=[0,0,[0,[0,0,0,[0,[0,[0,0,[3,[6,lB]]],function(c,b){return a(i[1],[0,b],c)}],M4]],M3]];f(g[19],eX,0,M5);var
M6=0,M7=0,M$=[0,[0,[0,[0,M_,[2,[6,eX],M9]],M8],function(e,c,d,b){return a(i[1],[0,b],[0,c])}],M7],Nb=[0,[0,Na,function(d,b){var
c=[1,a(i[1],[0,b],0)];return a(i[1],[0,b],c)}],M$],Ne=[0,[0,[0,[0,Nd,[6,bI]],Nc],function(f,c,e,b){var
d=[1,a(i[1],[0,b],[0,c,0])];return a(i[1],[0,b],d)}],Nb],Nj=[0,[0,[0,[0,[0,[0,Ni,[6,bI]],Nh],[2,[6,bI],Ng]],Nf],function(h,d,g,c,f,b){var
e=[1,a(i[1],[0,b],[0,c,d])];return a(i[1],[0,b],e)}],Ne],No=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,Nn,[6,bI]],Nm],[2,[6,bI],Nl]],Nk],function(j,d,h,c,g,b){function
e(c){if(c){var
d=c[2],f=c[1];if(d)if(d[2]){var
g=[1,e(d)],h=[0,a(i[1],[0,b],g)],j=[2,a(i[1],[0,b],h)],k=[0,f,[0,a(i[1],[0,b],j),0]];return a(i[1],[0,b],k)}}return a(i[1],[0,b],c)}var
f=[1,e([0,c,d])];return a(i[1],[0,b],f)}],Nj]],M6]];f(g[19],he,0,No);var
Np=0,Nq=0,Nt=[0,[0,Ns,function(c,b){return a(i[1],[0,b],Nr)}],Nq],Nw=[0,[0,Nv,function(c,b){return a(i[1],[0,b],Nu)}],Nt],Nz=[0,0,[0,[0,0,0,[0,[0,[0,[0,Ny,[6,eX]],Nx],function(e,c,d,b){return a(i[1],[0,b],[1,c])}],Nw]],Np]];f(g[19],lA,0,Nz);var
NA=0,NB=0,ND=[0,[0,[0,NC,[6,cY]],function(c,d,b){return a(i[1],[0,b],[1,[0,c]])}],NB],NF=[0,[0,[0,NE,[6,cY]],function(c,d,b){return a(i[1],[0,b],[1,[1,c]])}],ND],NH=[0,[0,NG,function(c,b){return a(i[1],[0,b],0)}],NF],NI=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,aV]],function(c,b){return a(i[1],[0,b],[0,c])}],NH]],NA]];f(g[19],hf,0,NI);var
NJ=0,NK=0,NL=[0,[0,[0,0,[6,bI]],function(a,b){return a}],NK],NO=[0,[0,NN,function(c,b){return a(i[1],[0,b],NM)}],NL],NR=[0,0,[0,[0,0,0,[0,[0,NQ,function(c,b){return a(i[1],[0,b],NP)}],NO]],NJ]];f(g[19],lB,0,NR);var
NS=0,NT=0,NU=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lC]],function(a,b){return a}],NT]],NS]];f(g[19],bI,0,NU);var
NV=0,NW=0,NX=[0,[0,[0,0,[6,he]],function(c,b){var
d=[2,a(i[1],[0,b],[0,c])];return a(i[1],[0,b],d)}],NW],NY=[0,[0,[0,0,[6,lA]],function(c,b){return a(i[1],[0,b],[2,c])}],NX],N0=[0,[0,NZ,function(d,b){var
c=[2,a(i[1],[0,b],0)];return a(i[1],[0,b],c)}],NY],N1=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hf]],function(c,b){return a(i[1],[0,b],[1,c])}],N0]],NV]];f(g[19],lC,0,N1);var
N2=0,N3=0,N4=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,eX]],function(a,b){return a}],N3]],N2]];f(g[19],q[7],0,N4);var
N5=0,N6=0,N7=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,bI]],function(a,b){return a}],N6]],N5]];f(g[19],q[6],0,N7);var
N8=0,N9=0,N_=[0,[0,[0,0,[6,ck]],function(a,b){return[0,a]}],N9];function
N$(c,d,b){return[1,a(i[1],[0,b],c)]}f(g[19],eY,0,[0,0,[0,[0,0,0,[0,[0,[0,Oa,[6,g[15][2]]],N$],N_]],N8]]);var
Ob=0,Oc=0,Oe=[0,[0,[0,Od,[6,hf]],function(a,d,c,b){return[0,a]}],Oc],Of=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Oe]],Ob]];f(g[19],lD,0,Of);var
Og=0,Oh=0,Oj=[0,[0,[0,Oi,[6,hd]],function(a,c,b){return a}],Oh],Ok=[0,0,[0,[0,0,0,[0,[0,0,function(b){return a(i[1],[0,b],0)}],Oj]],Og]];f(g[19],hg,0,Ok);var
Ol=0,Om=0;function
On(d,c,b){return a(i[1],[0,b],[0,c,d])}f(g[19],b0,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,g[16][1]]],[6,hg]],On],Om]],Ol]]);var
Oo=0,Op=0,Oq=[0,[0,[0,0,[6,ck]],function(c,b){return a(i[1],[0,b],[2,c])}],Op],Or=[0,[0,[0,0,[6,cY]],function(c,b){return a(i[1],[0,b],[1,c])}],Oq],Os=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,b0]],function(c,b){return a(i[1],[0,b],[0,c])}],Or]],Oo]];f(g[19],hh,0,Os);var
Ot=0,Ou=0,Ov=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hh]],function(a,b){return a}],Ou]],Ot]];f(g[19],q[8],0,Ov);var
Ow=0,Ox=0,Oz=[0,[0,[0,Oy,[6,he]],function(a,c,b){return[0,a]}],Ox],OA=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Oz]],Ow]];f(g[19],lE,0,OA);var
OB=0,OC=0,OD=[0,[0,[0,0,[1,[6,eY]]],function(c,b){return a(i[1],[0,b],[1,c])}],OC],OF=[0,0,[0,[0,0,0,[0,[0,[0,[0,OE,[6,eY]],[3,[6,eY]]],function(d,c,e,b){return a(i[1],[0,b],[0,[0,c,d]])}],OD]],OB]];f(g[19],hi,0,OF);var
OG=0,OH=0,OJ=[0,[0,[0,OI,[6,hi]],function(a,c,b){return a}],OH],OK=[0,0,[0,[0,0,0,[0,[0,0,function(b){return a(i[1],[0,b],0)}],OJ]],OG]];f(g[19],dD,0,OK);var
OL=0,OM=0,ON=[0,[0,[0,0,[6,aV]],function(a,b){return[0,a,0]}],OM],OQ=[0,[0,[0,[0,OP,[6,aV]],OO],function(f,a,e,d,c,b){return[0,a,1]}],ON],OT=[0,0,[0,[0,0,0,[0,[0,[0,[0,OS,[6,aV]],OR],function(f,a,e,d,c,b){return[0,a,2]}],OQ]],OL]];f(g[19],lF,0,OT);var
OU=0,OV=0,OW=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,lF]],[6,dD]],function(b,a,c){return[0,[0,b,a[1]],a[2]]}],OV]],OU]];f(g[19],hj,0,OW);var
OX=0,OY=0,O0=[0,[0,[0,OZ,[6,dD]],function(a,c,b){return[0,0,a]}],OY],O2=[0,[0,[0,O1,[6,hl]],function(a,d,c,b){return[0,0,a]}],O0],O5=[0,[0,[0,[0,[0,0,[4,[6,hj],O4]],O3],[6,hl]],function(b,d,a,c){return[0,[0,a],b]}],O2],O7=[0,0,[0,[0,0,0,[0,[0,[0,0,[4,[6,hj],O6]],function(c,b){return[0,[0,c],a(i[1],[0,b],1)]}],O5]],OX]];f(g[19],lG,0,O7);var
O8=0,O9=0,O$=[0,[0,[0,O_,[6,lG]],function(c,d,b){return a(i[1],[0,b],c)}],O9],Pc=[0,0,[0,[0,0,0,[0,[0,[0,Pb,[6,hi]],function(c,d,b){return a(i[1],[0,b],[0,Pa,c])}],O$]],O8]];f(g[19],hk,0,Pc);var
Pd=0,Pe=0,Pf=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hk]],function(a,b){return a}],Pe]],Pd]];f(g[19],q[12],0,Pf);var
Pg=0,Ph=0,Pj=[0,[0,[0,Pi,[6,dD]],function(a,c,b){return a}],Ph],Pk=[0,0,[0,[0,0,0,[0,[0,0,function(b){return a(i[1],[0,b],1)}],Pj]],Pg]];f(g[19],hl,0,Pk);var
Pl=0,Pm=0,Pn=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,0,[6,hh]],[6,lE]],[6,lD]],[5,[6,hk]]],function(f,e,d,c,b){return a(i[1],[0,b],[0,c,e,d,f])}],Pm]],Pl]];f(g[19],lH,0,Pn);var
Po=0,Pp=0,Pq=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lH]],function(a,b){return a}],Pp]],Po]];f(g[19],q[9],0,Pq);var
Pr=0,Ps=0;function
Pt(c,b){return a(i[1],[0,b],[0,c])}var
Pu=[0,[0,[0,0,[6,g[16][1]]],Pt],Ps];function
Pv(d,e,c,b){return a(i[1],[0,b],[1,c,d])}f(g[19],lI,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,g[16][1]]],Pw],[6,g[16][1]]],Pv],Pu]],Pr]]);var
Px=0,Py=0,Pz=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lI]],function(a,b){return a}],Py]],Px]];f(g[19],q[10],0,Pz);var
PA=0,PB=0,PE=[0,[0,PD,function(c,b){return a(i[1],[0,b],PC)}],PB],PH=[0,[0,PG,function(c,b){return a(i[1],[0,b],PF)}],PE],PI=[0,0,[0,[0,0,0,[0,[0,0,function(b){return a(i[1],[0,b],0)}],PH]],PA]];f(g[19],lJ,0,PI);var
PJ=0,PK=0,PM=[0,[0,[0,PL,[6,b0]],function(c,d,b){return[0,a(i[1],[0,b],1),c]}],PK];function
PN(c,d,b){return[0,a(i[1],[0,b],0),c]}var
PO=[6,b0],PP=0,PR=[0,[0,PQ,function(b,a){return 0}],PP],PT=[0,[0,[0,[0,0,[8,[0,[0,PS,function(b,a){return 0}],PR]]],PO],PN],PM],PV=[0,[0,[0,[0,[0,0,[6,ck]],PU],[6,b0]],function(d,e,c,b){return[0,a(i[1],[0,b],[0,c]),d]}],PT];function
PW(d,e,c,b){return[0,a(i[1],[0,b],[1,c]),d]}var
PX=[6,b0],PY=0,P0=[0,[0,PZ,function(b,a){return 0}],PY],P2=[0,[0,[0,[0,[0,0,[6,ck]],[8,[0,[0,P1,function(b,a){return 0}],P0]]],PX],PW],PV],P3=[0,[0,[0,[0,0,[6,ck]],[6,b0]],function(d,c,b){return[0,a(i[1],[0,b],[0,c]),d]}],P2],P4=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,b0]],function(c,b){var
d=[0,a(i[1],0,1)];return[0,a(i[1],[0,b],d),c]}],P3]],PJ]];f(g[19],lK,0,P4);var
P5=0,P6=0,P7=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,lJ]],[6,lK]],function(b,d,c){return a(i[1],[0,c],[0,d,b[1],b[2]])}],P6]],P5]];f(g[19],lL,0,P7);var
P8=0,P9=0,P_=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lL]],function(a,b){return a}],P9]],P8]];f(g[19],q[11],0,P_);var
P$=0,Qa=0;function
Qb(a,c,b){return a}var
Qf=[0,[0,[0,Qe,[4,[5,[7,q[1],Qd]],Qc]],Qb],Qa],Qg=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],Qf]],P$]];f(g[19],hm,0,Qg);var
Qh=0,Qi=0;function
Qj(a,d,b,c){return[0,[0,[0,b],a[1]],a[2]]}var
Ql=[0,[0,[0,[0,[0,0,[6,q[1]]],Qk],[6,eZ]],Qj],Qi];function
Qm(b,d,a,c){return[0,0,[0,[0,[0,a],b]]]}var
Qo=[0,[0,[0,[0,[0,0,[6,q[1]]],Qn],[6,hm]],Qm],Ql],Qq=[0,[0,[0,Qp,[6,hm]],function(a,c,b){return[0,0,[0,[0,0,a]]]}],Qo];function
Qr(a,b){return[0,[0,[0,a],0],0]}var
Qs=[0,[0,[0,0,[6,q[1]]],Qr],Qq],Qu=[0,[0,[0,Qt,[6,eZ]],function(a,c,b){return[0,[0,0,a[1]],a[2]]}],Qs],Qw=[0,0,[0,[0,0,0,[0,[0,0,function(a){return Qv}],Qu]],Qh]];f(g[19],eZ,0,Qw);var
Qx=0,Qy=0,Qz=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,eZ]],function(c,b){return a(i[1],[0,b],c)}],Qy]],Qx]];f(g[19],q[13],0,Qz);var
QA=0,QB=0,QC=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,dD]],function(a,b){return a}],QB]],QA]];f(g[19],q[14],0,QC);var
QD=0,QE=0,QG=[0,[0,QF,function(c,b){return a(i[1],[0,b],0)}],QE],QI=[0,[0,QH,function(c,b){return a(i[1],[0,b],1)}],QG],QK=[0,[0,QJ,function(c,b){return a(i[1],[0,b],2)}],QI],QM=[0,[0,QL,function(c,b){return a(i[1],[0,b],3)}],QK],QO=[0,[0,QN,function(c,b){return a(i[1],[0,b],4)}],QM],QQ=[0,[0,QP,function(c,b){return a(i[1],[0,b],5)}],QO],QS=[0,0,[0,[0,0,0,[0,[0,[0,QR,[6,hp]],function(a,c,b){return a}],QQ]],QD]];f(g[19],lM,0,QS);var
QT=0,QU=0;function
QV(c,d,b){return[0,a(i[1],[0,b],[1,c])]}var
QX=[0,[0,[0,QW,[6,g[15][2]]],QV],QU];function
QY(c,b){return[0,a(i[1],[0,b],[0,c])]}var
QZ=[0,[0,[0,0,[6,g[15][16]]],QY],QX];function
Q0(c,d,b){return[1,a(i[1],[0,b],c)]}f(g[19],hn,0,[0,0,[0,[0,0,0,[0,[0,[0,Q1,[6,g[15][2]]],Q0],QZ]],QT]]);var
Q2=0,Q3=0,Q4=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hn]],function(a,b){return a}],Q3]],Q2]];f(g[19],q[15],0,Q4);var
Q5=0,Q6=0,Q7=[0,0,[0,[0,0,0,[0,[0,[0,0,[1,[6,hn]]],function(c,b){return a(i[1],[0,b],c)}],Q6]],Q5]];f(g[19],ho,0,Q7);var
Q8=0,Q9=0,Ra=[0,[0,[0,[0,Q$,[6,ho]],Q_],function(f,c,e,d,b){return a(i[1],[0,b],[1,c])}],Q9],Rd=[0,[0,[0,[0,Rc,[6,ho]],Rb],function(e,c,d,b){return a(i[1],[0,b],[0,c])}],Ra],Re=[0,0,[0,[0,0,0,[0,[0,0,function(b){var
c=[1,a(i[1],[0,b],0)];return a(i[1],[0,b],c)}],Rd]],Q8]];f(g[19],hp,0,Re);var
Rf=0,Rg=0,Rh=[0,[0,[0,0,[1,[6,lM]]],function(c,b){return a(i[1],[0,b],c)}],Rg],Ri=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,hp]],function(c,b){var
d=[0,a(i[1],[0,b],5),[0,c,0]],e=[0,a(i[1],[0,b],1),d],f=[0,a(i[1],[0,b],0),e];return a(i[1],[0,b],f)}],Rh]],Rf]];f(g[19],lN,0,Ri);var
Rj=0,Rk=0,Rl=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lN]],function(a,b){return a}],Rk]],Rj]];f(g[19],q[16],0,Rl);var
Rm=0,Rn=0,Rp=[0,[0,Ro,function(c,b){return a(i[1],[0,b],0)}],Rn],Rq=[0,0,[0,[0,0,0,[0,[0,[0,0,[1,[6,aV]]],function(c,b){return a(i[1],[0,b],[0,c])}],Rp]],Rm]];f(g[19],lO,0,Rq);var
Rr=0,Rs=0,Rt=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lO]],function(a,b){return a}],Rs]],Rr]];f(g[19],q[19],0,Rt);var
Ru=0,Rv=0;function
Rw(g,d,f,c,e,b){return a(i[1],[0,b],[1,c,d])}var
RA=[0,[0,[0,[0,[0,[0,Rz,[5,[6,g[15][2]]]],Ry],[6,g[16][14]]],Rx],Rw],Rv];function
RB(c,b){return a(i[1],[0,b],[0,c])}f(g[19],e0,0,[0,0,[0,[0,0,0,[0,[0,[0,0,[6,g[16][14]]],RB],RA]],Ru]]);var
RC=0,RD=0;function
RE(d,e,c,b){return a(i[1],[0,b],[0,c,d])}f(g[19],hq,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,e0]],RF],[6,q[1]]],RE],RD]],RC]]);var
RG=0,RH=0,RJ=[0,[0,[0,0,[2,[6,hq],RI]],function(c,b){return a(i[1],[0,b],c)}],RH],RM=[0,0,[0,[0,0,0,[0,[0,[0,RL,[2,[6,hq],RK]],function(c,d,b){return a(i[1],[0,b],c)}],RJ]],RG]];f(g[19],lP,0,RM);var
RN=0,RO=0,RP=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lP]],function(a,b){return a}],RO]],RN]];f(g[19],q[17],0,RP);var
RQ=0,RR=0;function
RS(b,d,a,c){return[0,a,b]}f(g[19],lQ,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,g[15][3]]],RT],[6,e0]],RS],RR]],RQ]]);var
RU=0,RV=0,R0=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,RZ,[4,[6,lQ],RY]],RX],[6,e0]],RW],function(g,d,f,c,e,b){return a(i[1],[0,b],[0,d,c])}],RV]],RU]];f(g[19],lR,0,R0);var
R1=0,R2=0;function
R3(d,e,c,b){return a(i[1],[0,b],[0,c,d])}f(g[19],hr,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,lR]],R4],[6,q[1]]],R3],R2]],R1]]);var
R5=0,R6=0,R8=[0,[0,[0,0,[2,[6,hr],R7]],function(c,b){return a(i[1],[0,b],c)}],R6],R$=[0,0,[0,[0,0,0,[0,[0,[0,R_,[2,[6,hr],R9]],function(c,d,b){return a(i[1],[0,b],c)}],R8]],R5]];f(g[19],lS,0,R$);var
Sa=0,Sb=0,Sc=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lS]],function(a,b){return a}],Sb]],Sa]];f(g[19],q[18],0,Sc);var
Sd=0,Se=0,Sg=[0,[0,Sf,function(d,c,b){return a(i[1],[0,b],0)}],Se],Si=[0,[0,Sh,function(d,c,b){return a(i[1],[0,b],1)}],Sg],Sk=[0,[0,[0,Sj,[6,aV]],function(c,d,b){return a(i[1],[0,b],[0,c])}],Si],Sm=[0,0,[0,[0,0,0,[0,[0,[0,Sl,[6,aV]],function(c,d,b){return a(i[1],[0,b],[1,c])}],Sk]],Sd]];f(g[19],lT,0,Sm);var
Sn=0,So=0,Sp=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lT]],function(a,b){return a}],So]],Sn]];f(g[19],q[20],0,Sp);var
Sq=0,Sr=0,Ss=[0,[0,0,function(a){return 0}],Sr],Su=[0,0,[0,[0,0,0,[0,[0,[0,St,[6,aV]],function(a,c,b){return[0,a]}],Ss]],Sq]];f(g[19],lU,0,Su);var
Sv=0,Sw=0;function
Sx(h,d,g,c,f,e,b){return a(i[1],[0,b],[0,[0,c],d])}var
SB=[0,[0,[0,[0,[0,[0,[0,[0,0,[6,gV]],SA],[6,aV]],Sz],[6,g[16][3]]],Sy],Sx],Sw];function
SC(d,c,b){return a(i[1],[0,b],[0,d,c])}f(g[19],lV,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,g[16][1]]],[6,lU]],SC],SB]],Sv]]);var
SD=0,SE=0,SF=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lV]],function(a,b){return a}],SE]],SD]];f(g[19],q[21],0,SF);var
SG=0,SH=0,SJ=[0,[0,[0,SI,[6,bI]],function(a,c,b){return[0,a]}],SH],SK=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],SJ]],SG]];f(g[19],lW,0,SK);var
SL=0,SM=0;function
SN(a,c,b){return[0,a]}var
SP=[0,[0,[0,SO,[6,q[1]]],SN],SM],SQ=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],SP]],SL]];f(g[19],hs,0,SQ);var
SR=0,SS=0;function
ST(h,d,g,c,f,e,b){return a(i[1],[0,b],[1,c,d])}var
SX=[0,[0,[0,[0,[0,[0,[0,[0,0,[6,gV]],SW],[6,aV]],SV],[6,g[16][3]]],SU],ST],SS];function
SY(e,l,d,k,c,j,h,b){var
f=[1,a(i[1],[0,b],[0,c])],g=[0,[0,a(i[1],[0,b],f)],d,e];return a(i[1],[0,b],g)}var
S2=[0,[0,[0,[0,[0,[0,[0,[0,[0,0,[6,la]],S1],[6,aV]],S0],[6,g[16][3]]],SZ],[6,hs]],SY],SX];function
S3(e,d,c,b){return a(i[1],[0,b],[0,d,c,e])}f(g[19],lX,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[6,g[16][1]]],[6,lW]],[6,hs]],S3],S2]],SR]]);var
S4=0,S5=0,S6=[0,0,[0,[0,0,0,[0,[0,[0,0,[6,lX]],function(a,b){return a}],S5]],S4]];f(g[19],q[22],0,S6);var
S7=0;function
S8(k,d,j,h,g,b){var
e=c(ao[4],cb),f=[12,0,0,[0,a(ao[7],e,d)]];return a(i[1],[0,b],f)}var
Tb=[0,[0,[0,[0,[0,[0,[0,0,Ta],S$],S_],[6,q[1]]],S9],S8],S7];function
Tc(d,j,h,b){var
e=ju([0,b],a(i[1],[0,b],d)),f=c(ao[4],cb),g=[12,0,0,[0,a(ao[7],f,e)]];return a(i[1],[0,b],g)}var
Te=[0,[0,[0,[0,[0,0,[6,lc]],Td],[6,g[15][2]]],Tc],Tb];function
Tf(d,j,h,b){var
e=a(gf[12],[0,b],d),f=c(ao[4],cc),g=[12,0,0,[0,a(ao[7],f,e)]];return a(i[1],[0,b],g)}var
Th=[0,[0,[0,[0,[0,0,[6,ld]],Tg],[6,g[15][2]]],Tf],Te];function
Ti(a){return f(g[19],g[16][5],0,[0,Tj,[0,[0,0,0,Th],0]])}a(ep[3],jf,Ti);function
lY(a){return c(d[7],0)}function
lZ(a){return c(d[7],0)}var
Tk=0,Tl=[0,[0,[0,0,[6,gX]],function(a,b){return a}],Tk],Tm=[0,[0,[0,0,[6,gY]],function(a,b){return a}],Tl],Tn=[0,[0,[0,0,[6,gZ]],function(a,b){return a}],Tm],To=[0,[0,[0,0,[6,g0]],function(a,b){return a}],Tn],Tp=[1,[0,[0,[0,0,[6,g1]],function(a,b){return a}],To]],Tq=[0,function(b,a){return lY},Tp],l0=a(bJ[3],Tr,Tq),l1=l0[1],Ts=l0[2],Tt=[0,q[1]],Tu=[0,function(b,a){return lZ},Tt],l2=a(bJ[3],Tv,Tu),ht=l2[1],Tw=l2[2];function
l3(a){return 3===a[0]?Tx:bJ[6]}var
Ty=0,Tz=[0,function(a){return bJ[6]}];function
TA(b,a){c(dE[2],a);return[5,function(a){return i8(a,b)}]}var
TD=[0,[0,0,[0,TC,[0,TB,[1,[5,c(ao[16],ht)],0]]],TA,Tz],Ty],TE=[0,function(a){return l3(a)}];function
TF(c,b){var
d=a(dE[1],dE[7],b);return[0,function(a){return i$(d,c)}]}var
TH=[0,[0,0,[0,TG,[1,[5,c(ao[16],l1)],0]],TF,TE],TD];p(bJ[2],TI,0,0,TH);a(TK[5],TJ,g2);var
TL=0,TM=[0,function(b,a){return bJ[7]}];function
TN(d,b,a){c(dE[2],a);return[4,function(a){return jd(a,b,d)}]}var
TP=[1,[5,c(ao[16],TO[26])],0],TQ=[0,[0,0,[1,[5,c(ao[16],ht)],TP],TN,TM],TL];p(bJ[2],TR,0,[0,g2],TQ);var
TS=0,TT=0;function
TU(b,a){c(dE[2],a);return[0,function(a){return jb(b)}]}var
TX=[0,[0,0,[0,TW,[0,TV,[1,[5,c(ao[16],jS[17])],0]]],TU,TT],TS],TY=0,TZ=[0,function(a){return bJ[6]}];p(bJ[2],T0,TZ,TY,TX);aD(1307,[0,k7,bZ,aC,eP,k9,aU,cj,k_,eQ,gU,k$,la,gV,EZ,lb,lc,ld,gW,ac,aM,gX,gY,gZ,g0,g1,g2,g3,le,cX,lf,lg,lh,li,lj,g4,LA,lY,lZ,l1,Ts,ht,Tw,l3],"Ltac2_plugin__G_ltac2");return}
