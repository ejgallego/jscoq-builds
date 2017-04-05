(function(l1){"use strict";var
eD="*",ae=145,eT=" 0\n",bb=108,eY="Z.lt",e2="Z.le",c6="Equation E",c5=" and E",dt=": ",a_=".\n",h=250,eC="Z.succ",eI="omega_plugin",eM="(",eR="Z.pred",eS="not a number",eQ="ZArith",eX=110,eB="+ ",eH="- ",S=124,ex="------------------------\n\n",d=246,eA="Z.sub",dx=119,ew="tag_hypothesis",u=132,aO="omega",eG="Inequation E",du="Z",eP="Extension: cannot occur",m=120,aZ=113,eO=")",eW="N",e1="State",eK=117,eL="with",a$=118,eF="omega'",eV="X%d",eE=" subsumes E",dw=" E",o=109,ap="Omega",eU=" states ",ba=136,a0=248,ez="positive",ds=-18,ey="Equations E",dv="nat",e0="Omega: Can't solve a goal with non-linear products",c4=-16,eN="Z.ge",eJ="Z.gt",a9="Coq",eZ="E%d subsumes E%d.\n",L=l1.jsoo_runtime,ao=L.caml_check_bound,ev=L.caml_equal,aY=L.caml_fresh_oo_id,b=L.caml_new_string,c0=L.caml_notequal,g=L.caml_obj_tag,c3=L.caml_register_global,cH=L.caml_string_notequal,c2=L.caml_trampoline,c1=L.caml_trampoline_return,w=L.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):L.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):L.caml_call_gen(a,[b,c])}function
v(a,b,c,d){return a.length==3?a(b,c,d):L.caml_call_gen(a,[b,c,d])}function
cG(a,b,c,d,e){return a.length==4?a(b,c,d,e):L.caml_call_gen(a,[b,c,d,e])}function
l0(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):L.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}var
t=L.caml_get_global_data(),lZ=[11,b(" + "),[2,0,[12,32,[2,0,[11,b(dw),[4,0,0,0,[11,b(a_),0]]]]]]],ej=[0,[0,2],[0,1,0]],dQ=[0,b(a9),[0,b("Logic"),[0,b("Decidable"),0]]],aW=b(eI),r=t.List,z=t.Printf,n=t.Pervasives,q=t.Util,M=t.Not_found,aq=t.Int,y=t.Hashtbl,dy=t.Assert_failure,I=t.Names,f=t.CamlinternalLazy,e=t.Term,H=t.Bigint,ag=t.CErrors,p=t.Proofview,aT=t.Logic,D=t.Tacmach,C=t.Coqlib,i=t.Tactics,j=t.Tacticals,cX=t.Context,bg=t.Nameops,dd=t.Pp,df=t.Universes,dg=t.Nametab,dh=t.Libnames,cK=t.Goptions,es=t.Stdarg,er=t.Loc,aX=t.Ltac_plugin,et=t.Genarg,dr=t.Mltop,aP=[0,0],fm=[0,[11,b(eG),[4,0,0,0,[11,b(" is divided by "),[2,0,[11,b(" and the constant coefficient is rounded by substracting "),[2,0,[11,b(a_),0]]]]]]],b("Inequation E%d is divided by %s and the constant coefficient is rounded by substracting %s.\n")],fn=[0,[11,b("Constant in equation E"),[4,0,0,0,[11,b(" is not divisible by the pgcd "),[2,0,[11,b(" of its other coefficients.\n"),0]]]]],b("Constant in equation E%d is not divisible by the pgcd %s of its other coefficients.\n")],fo=[0,[12,69,[4,0,0,0,[11,b(" is trivially satisfiable.\n"),0]]],b("E%d is trivially satisfiable.\n")],fp=[0,[11,b(c6),[4,0,0,0,[11,b(" is divided by the pgcd "),[2,0,[11,b(" of its coefficients.\n"),0]]]]],b("Equation E%d is divided by the pgcd %s of its coefficients.\n")],fq=[0,[11,b("We state "),[2,0,[11,b(dw),[4,0,0,0,[11,b(" = "),[2,0,[12,32,[2,0,[11,b(dw),[4,0,0,0,lZ]]]]]]]]]],b("We state %s E%d = %s %s E%d + %s %s E%d.\n")],fr=[0,[11,b("We define a new equation E"),[4,0,0,0,[11,b(dt),0]]],b("We define a new equation E%d: ")],fs=b(" 0"),ft=[0,[11,b("We define E"),[4,0,0,0,[11,b(dt),0]]],b("We define E%d: ")],fu=b(eT),fv=[0,[12,69,[4,0,0,0,[11,b(eE),[4,0,0,0,[11,b(a_),0]]]]],b(eZ)],fw=[0,[12,69,[4,0,0,0,[11,b(eE),[4,0,0,0,[11,b(a_),0]]]]],b(eZ)],fx=[0,[11,b(ey),[4,0,0,0,[11,b(c5),[4,0,0,0,[11,b(" imply a contradiction on their constant factors.\n"),0]]]]],b("Equations E%d and E%d imply a contradiction on their constant factors.\n")],fy=[0,[11,b(ey),[4,0,0,0,[11,b(c5),[4,0,0,0,[11,b(" state that their body is at the same time equal and different\n"),0]]]]],b("Equations E%d and E%d state that their body is at the same time equal and different\n")],fz=[0,[12,69,[4,0,0,0,[11,b(c5),[4,0,0,0,[11,b(" can be merged into E"),[4,0,0,0,[11,b(a_),0]]]]]]],b("E%d and E%d can be merged into E%d.\n")],fA=[0,[11,b(c6),[4,0,0,0,[11,b(eU),[2,0,[11,b(" = 0.\n"),0]]]]],b("Equation E%d states %s = 0.\n")],fB=[0,[11,b(eG),[4,0,0,0,[11,b(" states 0 != 0.\n"),0]]],b("Inequation E%d states 0 != 0.\n")],fC=[0,[11,b(c6),[4,0,0,0,[11,b(eU),[2,0,[11,b(" >= 0.\n"),0]]]]],b("Equation E%d states %s >= 0.\n")],fD=[0,[11,b(c6),[4,0,0,0,[11,b(" is split in E"),[4,0,0,0,[11,b(c5),[4,0,0,0,[11,b("\n\n"),0]]]]]]],b("Equation E%d is split in E%d and E%d\n\n")],fE=[0,[11,b("To ensure a solution in the dark shadow the equation E"),[4,0,0,0,[11,b(" is weakened by "),[2,0,[11,b(a_),0]]]]],b("To ensure a solution in the dark shadow the equation E%d is weakened by %s.\n")],fQ=b("depend"),fU=b("solve"),fS=[0,b("plugins/omega/omega.ml"),602,15],fR=b("no contradiction"),fP=b("disequation in simplify"),fO=b("Product dardk"),fN=[0,0,0,0],fK=b("tl"),fL=b("TL"),fI=b("eliminate_with_in"),fF=[0,[12,88,[4,0,0,0,0]],b(eV)],fk=b(">= 0\n"),fl=b(ex),fh=[0,[12,69,[4,0,0,0,[11,b(dt),0]]],b("E%d: ")],fi=[0,[2,0,[11,b(eT),0]],b("%s 0\n")],fj=b(ex),fe=b("equation"),ff=b("inequation"),fg=b("disequation"),fb=b("="),fc=b(">="),fd=b("!="),e6=b(eH),e9=b(eB),e_=b(""),e7=[0,[2,0,[12,32,0]],b("%s ")],e8=[0,[2,0,[12,32,[2,0,[12,32,0]]]],b("%s %s ")],e$=[0,[11,b(eB),[2,0,[12,32,0]]],b("+ %s ")],fa=[0,[11,b(eH),[2,0,[12,32,0]]],b("- %s ")],e3=b("pgcd_l"),e4=b("Omega.MakeOmegaSolver(I).UNSOLVABLE"),e5=b("Omega.MakeOmegaSolver(I).NO_CONTRADICTION"),fG=b("Omega.MakeOmegaSolver(I).FACTOR1"),fH=b("Omega.MakeOmegaSolver(I).CHOPVAR"),fM=b("Omega.MakeOmegaSolver(I).SOLVED_SYSTEM"),fT=b("Omega.MakeOmegaSolver(I).FULL_SOLUTION"),i3=b(eM),i4=b("+"),i5=b(eO),i6=b("~"),i7=b(eM),i8=b(eD),i9=b(eO),i_=b("?"),i$=b("weight"),ji=[0,2],jj=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jk=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],je=[0,2],jf=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jg=[0,2],jh=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jl=[0,2],jm=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jn=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],jO=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],[0,[0,[0,1],[0,[0,1],[0,[0,2],[0,[0,1],0]]]],0]]]],jP=[0,1],jQ=[0,2],jR=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jS=[0,2],jT=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jU=b(e0),jV=[0,2],jW=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j3=[0,1],j4=[0,2],j5=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],j6=[0,[0,[0,1],[0,[0,1],0]],0],j7=b(e0),j8=[0,2],j9=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],j_=[0,[0,[0,1],0],0],j$=[0,1],ka=[0,2],kb=[0,1],kc=[0,2],kd=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],ke=[0,1],kv=[0,0,0],ks=[0,1],kt=[0,2],ko=[0,1],kp=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kq=[0,1],kr=[0,2],ku=[0,1],kx=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],kw=[0,2],lq=[0,b(a9),[0,b(aO),[0,b(ap),0]]],ll=b("_left"),lm=b("_right"),k8=[0,1],k1=[0,2],k2=[0,1],k3=[0,2],k4=[0,1],k5=[0,2],k6=[0,1],k7=[0,1],k9=[0,[0,3],[0,1,0]],k_=[0,[0,2],[0,1,0]],k$=[0,[0,2],[0,1,0]],la=[0,[0,1],[0,1,0]],lb=[0,[0,2],[0,1,0]],lc=[0,[0,1],[0,1,0]],ld=[0,[0,2],[0,1,0]],le=[0,[0,1],[0,1,0]],lf=[0,[0,2],[0,1,0]],lg=[0,[0,1],[0,1,0]],lh=[0,[0,2],[0,1,0]],li=[0,[0,1],[0,1,0]],kX=[0,0,0],kY=b("Omega can't solve this system"),kW=b(e1),kV=[0,1,0],kB=[0,[0,3],0],kC=[0,[0,2],0],kD=[0,[0,3],0],kE=[0,[0,3],0],kF=[0,[0,1],[0,1,0]],kG=[0,[0,2],[0,1,0]],kH=[0,[0,2],[0,1,0]],kI=[0,[0,[0,1],0],0],kJ=[0,2],kK=[0,1],kL=[0,1],kM=[0,[0,2],[0,1,0]],kO=[0,[0,1],[0,1,0]],kP=[0,[0,3],0],kQ=[0,[0,[0,1],0],0],kR=[0,[0,3],0],kT=[0,[0,2],[0,1,0]],kU=[0,[0,2],[0,1,0]],ky=b("auxiliary"),kz=b("auxiliary_1"),kA=b("auxiliary_2"),kl=b("condense.1"),km=[0,2],kn=[0,0,0],kk=b("reduce_factor.1"),kg=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kh=[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]],ki=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,2],0]],0]],kj=[0,[0,[0,1],0],0],kf=b("shrink.1"),j1=[0,2],j2=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jZ=[0,2],j0=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jX=[0,2],jY=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]]],jB=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]],jC=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jD=[0,2],jE=[0,1],jF=[0,2],jG=[0,2],jH=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jI=[0,2],jJ=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jK=[0,2],jL=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jM=[0,2],jN=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jo=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]]],jp=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jq=[0,2],jr=[0,1],js=[0,2],jt=[0,2],ju=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jv=[0,2],jw=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jx=[0,2],jy=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jz=[0,2],jA=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jc=b("H"),jd=b("P"),ja=b("compile_equation"),i2=b("x"),i1=b("occurrence "),i0=b("abstract_path "),iY=b(eS),iZ=b(eS),iW=b("Omega: Not a quantifier-free goal"),iS=b("not"),iQ=b(eY),iO=b(eN),iN=b(eJ),iM=b(e2),iL=b(eA),iK=b(eR),iJ=b(eC),iF=b(" is not an evaluable constant"),iG=[0,b("Coq_omega")],iE=b("iff"),iD=b("imp_simp"),iC=b("not_not"),iB=b("not_iff"),iA=b("not_imp"),iz=b("not_and"),iy=b("not_or"),ix=b("dec_True"),iw=b("dec_not_not"),iv=b("dec_False"),iu=b("dec_not"),it=b("dec_iff"),is=b("dec_imp"),ir=b("dec_and"),iq=b("dec_or"),ip=b("eq_ind_r"),io=b("not_gt"),im=b("not_ge"),il=b("not_lt"),ik=b("not_le"),ij=b("not_eq"),ii=b("dec_gt"),ih=b("dec_ge"),ig=b("dec_lt"),ie=b("dec_le"),id=b("dec_eq_nat"),ic=b("le_gt_dec"),ib=b("pred_of_minus"),ia=b("O"),h$=b("S"),h_=b(dv),h9=b("Nat.pred"),h8=b("Nat.mul"),h7=b("Nat.add"),h6=b("Nat.sub"),h5=b("gt"),h4=b("ge"),h3=b("lt"),h2=b("le"),h1=b(eY),h0=b(eN),hZ=b(eJ),hY=b(e2),hX=b("Zne"),hW=b("neq"),hV=b("Znot_gt_le"),hU=b("Znot_ge_lt"),hT=b("Znot_lt_ge"),hS=b("Znot_le_gt"),hR=b("not_Zne"),hQ=b("not_Zeq"),hP=b("dec_Zge"),hO=b("dec_Zgt"),hN=b("Z.lt_decidable"),hM=b("Z.le_decidable"),hL=b("dec_Zne"),hK=b("Z.eq_decidable"),hJ=b("intro_Z"),hI=b("new_var"),hH=b("Zle_left"),hG=b("Zgt_left"),hF=b("Zge_left"),hE=b("Zlt_left"),hD=b("Zne_left"),hC=b("Zegal_left"),hB=b("fast_Zopp_involutive"),hA=b("fast_Zopp_eq_mult_neg_1"),hz=b("fast_Zopp_mult_distr_r"),hy=b("fast_Zopp_plus_distr"),hx=b("fast_Zmult_opp_comm"),hw=b("fast_Zmult_plus_distr_l"),hv=b("fast_Zred_factor6"),hu=b("fast_Zred_factor5"),ht=b("fast_Zred_factor4"),hs=b("fast_Zred_factor3"),hr=b("fast_Zred_factor2"),hq=b("fast_Zred_factor1"),hp=b("fast_Zred_factor0"),ho=b("OMEGA20"),hn=b("OMEGA19"),hm=b("OMEGA18"),hl=b("OMEGA17"),hk=b("fast_OMEGA16"),hj=b("fast_OMEGA15"),hi=b("fast_OMEGA14"),hh=b("fast_OMEGA13"),hg=b("fast_OMEGA12"),hf=b("fast_OMEGA11"),he=b("fast_OMEGA10"),hd=b("OMEGA9"),hc=b("OMEGA8"),hb=b("OMEGA7"),ha=b("OMEGA6"),g$=b("OMEGA5"),g_=b("OMEGA4"),g9=b("OMEGA3"),g8=b("OMEGA2"),g7=b("OMEGA1"),g6=b("Zmult_le_approx"),g5=b("fast_Zmult_comm"),g4=b("fast_Zplus_comm"),g3=b("fast_Zplus_permute"),g2=b("fast_Zmult_assoc_reverse"),g1=b("fast_Zplus_assoc"),g0=b("fast_Zplus_assoc_reverse"),gZ=b("inj_eq"),gY=b("inj_neq"),gX=b("Znat.inj_gt"),gW=b("Znat.inj_ge"),gV=b("Znat.inj_lt"),gU=b("Znat.inj_le"),gT=b("Nat2Z.inj_succ"),gS=b("inj_minus2"),gR=b("Nat2Z.inj_sub"),gQ=b("Nat2Z.inj_mul"),gP=b("Nat2Z.inj_add"),gO=b("Z.of_nat"),gN=b(eR),gM=b(eC),gL=b(eA),gK=b("Z.opp"),gJ=b("Z.mul"),gI=b("Z.add"),gH=b("Gt"),gG=b("comparison"),gF=b(du),gE=b("Zneg"),gD=b("Zpos"),gC=b("Z0"),gB=b("xI"),gA=b("xO"),gz=b("xH"),go=b("find_contr"),gn=b(ew),gl=b(ew),gj=[0,1],gi=[0,[12,88,[4,0,0,0,0]],b(eV)],gh=b("WW"),gg=b("Zvar"),gf=b(e1),gd=b(ap),f0=[0,b(ap),[0,b("System"),0]],f1=b("Omega system time displaying flag"),f4=[0,b(ap),[0,b("Action"),0]],f5=b("Omega action display flag"),f8=[0,b(ap),[0,b("OldStyle"),0]],f9=b("Omega old style flag"),ga=[0,b("Stable"),[0,b(ap),0]],gb=b("Omega automatic reset of generated names"),gp=[0,[0,b(a9),[0,b(aO),[0,b("OmegaLemmas"),0]]],0],gt=b(ap),gu=b(ap),gv=[0,[0,b(a9),[0,b(eQ),0]],0],gw=b(ap),gx=[0,[0,b(a9),[0,b(eQ),[0,b("BinInt"),0]]],0],gy=b(ap),lk=b("Coq_omega.Undecidable"),lV=[0,b("plugins/omega/g_omega.ml4"),1,0],lT=[0,[0,[0,b(aO)],[0,[0,b(eL)],[0,[0,b(eD)],0]]],0],lU=b("$l"),lW=[0,b(eL)],lX=[0,b(aO)],lY=b(eF),lN=b(eP),lL=[0,b(dv),[0,b(ez),[0,b(eW),[0,b(du),0]]]],lK=b(eP),lF=b(aO),lG=b(aO),lv=b(eW),lw=b(du),lx=b(dv),ly=b(ez),lA=b("zify_positive"),lB=b("zify_nat"),lC=b("zify_op"),lD=b("zify_N"),lz=b("No Omega knowledge base for type "),lt=[0,b("PreOmega"),[0,b(aO),[0,b(a9),0]]],ls=b(eI),lI=b(aO),lR=b(eF),fJ=t.Failure,k0=t.Reductionops,kN=t.Contradiction,kS=t.Equality,jb=t.Evarutil,iX=t.Globnames,iH=t.Global,iI=t.Tacred,lu=t.String,lP=t.Array,c7=[0,aP,function(f){var
j=f[1],p=f[2];function
ar(b,a){var
d=c(f[2],b,a),e=d||ev(b,a);return e}function
A(b,a){return c(f[2],a,b)}function
P(b,a){var
d=c(f[2],a,b),e=d||ev(b,a);return e}var
h=f[3],l=f[4],i=f[5];function
t(b,a){return c(f[6],b,a)[1]}function
Q(b,a){return c(f[6],b,a)[2]}var
d=f[8],e=f[9],u=c(h,e,e),ad=a(f[7],e);function
m(b){return c(f[2],b,d)?a(f[7],b):b}var
g=f[10],s=f[7];function
as(b,a){return b<a?1:0}function
at(b,a){return a<b?1:0}function
au(b,a){return b<=a?1:0}function
av(b,a){return a<=b?1:0}function
aw(b){a(n[29],b);a(n[32],0);return a(n[46],n[24])}function
E(b,a){a[1]=[0,b,a[1]];return 0}function
ae(f,e){var
b=f,a=e;for(;;){if(c(j,a,d))return b;var
g=Q(b,a),b=a,a=g;continue}}function
af(b){return b?v(r[16],ae,b[1],b[2]):a(n[2],e3)}function
F(b,a){var
g=P(b,d),f=A(a,d);return 0===g?0===f?t(b,a):c(l,t(c(h,b,e),a),e):0===f?c(l,t(c(l,b,e),a),e):t(b,a)}var
k=[a0,e4,aY(0)],ag=[a0,e5,aY(0)];function
G(h,f){var
b=f[2],k=f[1],l=0;function
o(i,b){var
k=c(p,b[1],d)?e6:i?e9:e_;a(n[27],k);var
f=m(b[1]);if(c(j,f,e)){var
l=a(h,b[2]);c(z[2],e7,l)}else{var
o=a(h,b[2]),q=a(g,f);v(z[2],e8,q,o)}return 1}v(r[16],o,l,k);if(A(b,d)){var
q=a(g,b);return c(z[2],e$,q)}var
i=c(p,b,d);if(i){var
s=a(g,m(b));return c(z[2],fa,s)}return i}function
W(a){function
b(b,a){if(15===a[0]){var
d=a[2][2],f=W(a[3][2]),g=W(d);return c(h,c(h,c(h,b,e),g),f)}return c(h,b,e)}return v(r[16],b,d,a)}function
R(a){switch(a){case
0:return fb;case
1:return fc;default:return fd}}function
S(a){switch(a){case
0:return fe;case
1:return ff;default:return fg}}function
x(d,b){function
e(a){var
b=a[4],e=a[3],f=a[2];c(z[2],fh,a[1]);G(d,[0,e,b]);var
g=R(f);return c(z[2],fi,g)}c(r[11],e,b);return a(n[27],fj)}function
ax(d,b){function
e(b){G(d,b);return a(n[27],fk)}c(r[11],e,b);return a(n[27],fl)}function
H(d,p){var
e=p;for(;;){if(e){var
b=e[1],q=e[2];switch(b[0]){case
0:var
r=b[3],s=b[1],t=a(g,b[4]),u=a(g,r);cG(z[2],fm,s[1],u,t);break;case
1:var
w=b[1],x=a(g,b[2]);v(z[2],fn,w[1],x);break;case
2:c(z[2],fo,b[1]);break;case
3:var
y=b[1],A=a(g,b[2]);v(z[2],fp,y[1],A);break;case
4:var
j=b[3],k=j[2],l=b[2],i=l[2],B=j[1],C=l[1],D=b[1],E=k[1],F=S(k[2]),I=a(g,B),J=i[1],K=S(i[2]),L=a(g,C),M=S(i[2]);l0(z[2],fq,M,D,L,K,J,I,F,E);break;case
5:var
f=b[1][1];c(z[2],fr,f[1]);G(d,[0,f[3],f[4]]);var
N=R(f[2]);a(n[27],N);a(n[27],fs);break;case
6:var
h=b[1];c(z[2],ft,h[1]);G(d,[0,h[3],h[4]]);var
O=R(h[2]);a(n[27],O);a(n[27],fu);break;case
7:v(z[2],fv,b[1],b[2]);break;case
8:v(z[2],fw,b[1],b[2]);break;case
9:v(z[2],fx,b[1][1],b[2][1]);break;case
10:v(z[2],fy,b[1][1],b[2][1]);break;case
11:cG(z[2],fz,b[2][1],b[3],b[1]);break;case
12:var
P=b[1],Q=a(g,b[2]);v(z[2],fA,P,Q);break;case
13:c(z[2],fB,b[1]);break;case
14:var
T=b[1],U=a(g,b[2]);v(z[2],fC,T,U);break;case
15:var
m=b[3],o=b[2],V=m[2],W=o[2];cG(z[2],fD,b[1][1],o[1],m[1]);H(d,W);a(n[32],0);H(d,V);a(n[32],0);break;default:var
X=b[1],Y=a(g,b[2]);v(z[2],fE,X,Y)}var
e=q;continue}return a(n[46],n[24])}}function
ah(a){return c(z[4],fF,a)}var
X=[0,0];function
T(a){X[1]=0;return 0}function
I(a){return X[1]}function
b(a){if(aP[1])H(ah,[0,a,0]);return E(a,X)}function
ay(b,a){return a[2]-b[2]|0}var
ai=a(r[41],ay);function
az(b){var
c=b[2],d=c[2],e=b[1];return[0,e,[0,a(ai,c[1]),d]]}function
B(i){function
e(k){var
b=k;for(;;){if(b){var
f=b[2],g=b[1],h=a(i,g[1]);if(c(j,h,d)){var
b=f;continue}var
l=e(f);return[0,[0,h,g[2]],l]}return 0}}return e}function
C(c,b){var
d=a(c,b[4]),e=b[3],f=a(B(c),e);return[0,b[1],b[2],f,d]}function
aA(b){return a(s,b)}function
J(a){return C(aA,a)}function
K(m,l){var
b=m,a=l;for(;;){if(b){if(a){var
g=a[2],f=a[1],i=b[2],e=b[1];if(e[2]===f[2]){var
k=c(h,e[1],f[1]);if(c(j,k,d)){var
b=i,a=g;continue}var
n=K(i,g);return[0,[0,k,e[2]],n]}return f[2]<e[2]?[0,e,K(i,a)]:[0,f,K(b,g)]}return b}return a}}function
Y(e,b,d){var
f=c(h,b[4],d[4]),g=K(b[3],d[3]),i=b[2];return[0,a(e,0),i,g,f]}var
Z=[a0,fG,aY(0)];function
_(a){if(a){var
d=a[2],b=a[1];if(c(j,m(b[1]),e))return[0,b,d];var
f=_(d);return[0,f[1],[0,b,f[2]]]}throw Z}var
U=[a0,fH,aY(0)];function
L(c,a){if(a){var
d=a[2],b=a[1];if(b[2]===c)return[0,b,d];var
e=L(c,d);return[0,e[1],[0,b,e[2]]]}throw U}function
o(f){var
g=f[4],p=f[3],n=f[2],o=f[1];if(0===p)switch(n){case
0:if(c(j,g,d))return 0;b([12,o,g]);throw k;case
1:if(P(g,d))return 0;b([14,o,g]);throw k;default:if(c0(g,d))return 0;b([13,o]);throw k}function
w(a){return m(a[1])}var
h=af(c(r[13],w,p));if(0===n)if(c0(Q(g,h),d)){b([1,f,h]);throw k}if(2===n)if(c0(Q(g,h),d)){b([2,f[1]]);return 0}if(c0(h,e)){var
s=F(g,h),x=c(l,g,c(i,s,h)),u=[0,o,n,a(B(function(a){return t(a,h)}),p),s];if(0===n)var
q=0;else
if(2===n)var
q=0;else
var
v=[0,f,u,h,x],q=1;if(!q)var
v=[3,f,h];b(v);return[0,u,0]}return[0,f,0]}function
D(o,g,f,d){var
h=g[1],p=d[3],q=g[2];try{var
k=L(q,p)[1],l=c(j,h,e)?a(s,k[1]):c(j,h,ad)?k[1]:a(n[2],fI),m=Y(o,d,C(function(a){return c(i,a,l)},f));b([4,m[1],[0,e,d],[0,l,f]]);return m}catch(a){a=w(a);if(a===U)return d;throw a}}function
$(b,a){var
d=c(i,u,a);return c(l,b,c(i,a,F(c(h,c(i,u,b),a),d)))}function
aj(k,f,J,I){var
g=k[1],l=f[3],K=k[3],E=a(k[2],0);try{var
ab=a(r[4],l),ac=a(r[3],l)[2],ad=[0,m(a(r[3],l)[1]),ac],ae=function(b,a){var
c=b[1],d=b[2];if(A(c,m(a[1]))){var
e=a[2];return[0,m(a[1]),e]}return[0,c,d]},af=v(r[16],ae,ad,ab),p=af}catch(b){b=w(b);if(b[1]===fJ)if(cH(b[2],fK))var
z=0;else{x(K,[0,f,0]);var
p=a(n[2],fL),z=1}else
var
z=0;if(!z)throw b}var
M=p[2],d=c(h,p[1],e),N=$(f[4],d),O=f[3],P=a(B(function(a){return $(a,d)}),O),Q=[0,[0,a(s,d),E],P],G=[0,a(g,0),0,Q,N],R=c(i,u,d),S=a(s,F(c(h,c(i,u,f[4]),d),R)),T=f[3],U=a(B(function(b){var
e=c(i,u,d);return a(s,F(c(h,c(i,u,b),d),e))}),T);b([5,[0,G,[0,a(g,0),0,U,S],f,d,E]]);var
V=o(G),j=a(r[3],V),y=L(M,j[3])[1];function
W(a){return o(D(g,y,j,a))}var
X=c(q[17][aZ],W,J);function
Y(a){return o(D(g,y,j,a))}var
Z=c(q[17][aZ],Y,I),H=D(g,y,j,f),_=C(function(a){return t(a,d)},H);b([3,H,d]);var
aa=o(_);return[0,a(r[3],aa),X,Z]}function
ak(d,i){var
b=i;for(;;){var
f=b[3],e=b[2],a=b[1],g=d[1],j=d[3];if(aP[1])x(j,[0,a,e]);try{var
h=_(a[3])[1],k=function(b,c,d){return function(a){return o(D(c,d,b,a))}}(a,g,h),l=c(q[17][aZ],k,f),m=function(b,c,d){return function(a){return o(D(c,d,b,a))}}(a,g,h),n=[0,c(q[17][aZ],m,e),l];return n}catch(c){c=w(c);if(c===Z){var
b=aj(d,a,e,f);continue}throw c}}}function
aa(l,p){var
f=p;for(;;){var
g=f[2],h=f[1],q=l[3],n=function(a){if(a){var
d=a[2],b=a[1],g=b[3],h=function(a){return c(j,m(a[1]),e)};if(c(r[24],h,g))return[0,b,d];var
f=n(d);return[0,f[1],[0,b,f[2]]]}throw M};if(h){var
s=h[2],t=h[1];try{var
o=n(h),u=o[2],v=o[1],a=v,i=u}catch(b){b=w(b);if(b!==M)throw b;var
a=t,i=s}if(0===a[3]){if(c(j,a[4],d)){b([2,a[1]]);var
f=[0,i,g];continue}b([12,a[1],a[4]]);throw k}var
f=ak(l,[0,a,i,g]);continue}if(aP[1])x(q,g);return g}}function
N(n,e){function
x(a){var
b=a[3];if(b)if(c(p,b[1][1],d))return[0,J(a),0];return[0,a,1]}var
f=c(y[1],0,7);function
i(z){var
m=x(z),n=m[2],a=m[1],g=a[3];if(0===g){if(c(p,a[4],d)){b([14,a[1],a[4]]);throw k}return b([2,a[1]])}try{var
o=c(y[6],f,g),i=o[2],j=o[1];if(1===n)if(j)var
l=j[1],C=c(p,l[4],a[4])?(b([7,l[1],a[1]]),l):(b([7,a[1],l[1]]),a),e=[0,[0,C],i];else
var
e=[0,[0,a],i];else
if(i){var
h=i[1];if(A(h[4],a[4]))b([8,h[1],a[1]]);else
b([8,a[1],h[1]]);var
E=A(h[4],a[4])?h:a,e=[0,j,[0,E]]}else
var
e=[0,j,[0,a]];var
q=e[1];if(q){var
r=e[2];if(r){var
s=r[1],t=q[1];if(c(p,t[4],s[4])){b([9,t,J(s)]);throw k}var
u=1}else
var
u=0}else
var
u=0;c(y[9],f,g);var
D=v(y[5],f,g,e);return D}catch(b){b=w(b);if(b===M){var
B=1===n?[0,[0,a],0]:[0,0,[0,a]];return v(y[5],f,g,B)}throw b}}c(r[11],i,e);var
h=[0,0],g=[0,0];function
l(o,f){var
d=f[1];if(d){var
i=f[2];if(i){var
k=i[1],e=d[1];if(c(j,e[4],k[4])){var
l=a(n,0);b([11,l,e,k[1]]);return E([0,l,0,e[3],e[4]],h)}}}var
m=f[2];if(d)E(d[1],g);return m?E(J(m[1]),g):0}c(y[11],l,f);return[0,h[1],g[1]]}var
V=[a0,fM,aY(0)];function
al(g){var
b=c(y[1],0,7);function
h(e,d){try{var
a=c(y[6],b,e),g=m(d);a[1]=c(n[5],a[1],g);var
h=0;return h}catch(a){a=w(a);if(a===M){var
f=[0,m(d)];return v(y[5],b,e,f)}throw a}}function
i(a){var
b=a[3];function
d(a){return h(a[2],a[1])}return c(r[11],d,b)}c(r[11],i,g);var
e=[0,d],a=[0,-1],f=[0,0];function
j(h,g){var
b=g[1];f[1]++;var
i=c(p,b,e[1]),d=i||(-1===a[1]?1:0),j=d?(a[1]=h,e[1]=b,0):d;return j}c(y[11],j,b);if(f[1]<1)throw V;return a[1]}function
am(i,b){function
c(c,b){var
e=c[3],f=c[2],g=c[1];try{var
h=L(i,b[3])[1],j=P(h[1],d)?[0,g,[0,[0,h[1],b],f],e]:[0,g,f,[0,[0,a(s,h[1]),b],e]];return j}catch(a){a=w(a);if(a===U)return[0,[0,b,g],f,e];throw a}}return v(r[16],c,fN,b)}function
an(u,t,d,g){var
f=0;function
h(h,d){var
j=d[2],f=d[1];function
k(m,k){var
p=k[2],g=k[1],v=C(function(a){return c(i,a,f)},p),q=Y(u,C(function(a){return c(i,a,g)},j),v);b([4,q[1],[0,g,j],[0,f,p]]);var
h=o(q);if(h){var
d=h[1];if(h[2])return a(n[2],fO);if(t){var
w=c(l,g,e),r=c(i,c(l,f,e),w);b([16,d[1],r]);var
x=c(l,d[4],r),s=[0,d[1],1,d[3],x]}else
var
s=d;return[0,s,m]}return m}return v(r[16],k,h,g)}return v(r[16],h,f,d)}function
ab(d,f,b){var
g=d[3],h=d[1],a=am(al(b),b),i=a[1],j=an(h,f,a[2],a[3]),e=c(n[22],i,j);if(aP[1])x(g,e);return e}function
ao(d,l,e){var
h=d[1],m=d[3];function
p(a){return 2===a[2]?1:0}if(c(r[24],p,e))a(n[2],fP);T(0);function
s(a){return b([6,a])}c(r[11],s,e);var
t=c(q[17][aZ],o,e);function
u(a){return 0===a[2]?1:0}var
i=c(r[32],u,t),v=i[1],j=N(h,i[2]),y=j[2],z=[0,c(n[22],v,j[1]),y];function
g(b,c){var
a=aa(d,c);return b<50?f(b+1|0,a):c1(f,[0,a])}function
f(e,f){var
a=N(h,f),b=a[2],c=a[1];if(0===c)return b;var
d=[0,c,b];return e<50?g(e+1|0,d):c1(g,[0,d])}function
A(a){return c2(g(0,a))}function
B(a){return c2(f(0,a))}function
k(b){try{var
a=k(B(ab(d,l,b)));return a}catch(a){a=w(a);if(a===V){if(aP[1])x(m,b);return b}throw a}}return k(A(z))}function
O(k,j,i){var
f=k,d=j,e=i;for(;;){if(e){var
g=e[2],b=e[1];switch(b[0]){case
0:if(c(aq[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
1:var
f=[0,b[1][1],f],d=[0,b,d],e=g;continue;case
2:var
e=g;continue;case
3:if(c(aq[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
4:var
l=b[3][2],m=b[2][2];if(c(aq[4][1],b[1],f)){var
f=[0,m[1],[0,l[1],f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
5:var
h=b[1],o=h[3];if(c(aq[4][1],h[1][1],f)){var
f=[0,o[1],f],d=[0,b,d],e=g;continue}var
e=g;continue;case
6:if(c(aq[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
7:var
e=g;continue;case
8:var
e=g;continue;case
9:var
f=[0,b[1][1],[0,b[2][1],f]],d=[0,b,d],e=g;continue;case
10:var
f=[0,b[1][1],[0,b[2][1],f]],d=[0,b,d],e=g;continue;case
11:var
p=b[3],q=b[2];if(c(aq[4][1],b[1],f)){var
f=[0,q[1],[0,p,f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
12:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
13:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
14:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
15:return a(n[2],fQ);default:if(c(aq[4][1],b[1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue}}return[0,f,d]}}function
aB(b,c){var
d=b[3],e=b[1];try{ao(e,0,c);var
f=a(n[2],fR);return f}catch(a){a=w(a);if(a===k)return H(d,O(0,0,I(0))[2]);throw a}}function
ap(a){var
g=a[2],h=a[1];function
i(a){return 2===a[2]?1:0}var
j=c(r[32],i,g)[1];function
e(a){var
b=a[3];if(b)if(c(p,b[1][1],d))return[0,J(a),0];return[0,a,1]}var
f=c(y[1],0,7);function
l(a){var
b=e(a),c=b[1];return v(y[5],f,[0,c[3],c[4]],[0,b[2],a])}c(r[11],l,j);function
m(a){if(0===a[2]){var
d=e(a),g=d[1],i=d[2],j=g[4],l=g[3];try{var
h=c(y[6],f,[0,l,j]);b([10,a,h[2],i===h[1]?1:0]);throw k}catch(a){a=w(a);if(a===M)return 0;throw a}}throw[0,dy,fS]}return c(r[11],m,h)}var
ac=[a0,fT,aY(0)];return[0,j,p,ar,A,P,h,l,i,t,Q,d,e,u,ad,m,g,s,as,at,au,av,aw,E,ae,af,F,k,ag,G,W,R,S,x,ax,g,H,ah,b,I,T,ai,az,B,C,J,K,Y,Z,_,U,L,o,D,$,aj,ak,aa,N,V,al,am,an,ab,ao,O,aB,ap,ac,function(d,j){var
f=d[1],D=d[3];T(0);function
E(a){return b([6,a])}c(r[11],E,j);function
i(c,a){ap(a);var
b=aa(d,a);return c<50?h(c+1|0,b):c1(h,[0,b])}function
h(j,k){function
l(a){return 2===a[2]?1:0}var
a=c(r[32],l,k),b=a[1],d=N(f,a[2]),e=d[2],g=d[1];if(0===g)return c(n[22],b,e);var
h=[0,g,c(n[22],b,e)];return j<50?i(j+1|0,h):c1(i,[0,h])}function
F(a){return c2(i(0,a))}function
G(a){return c2(h(0,a))}function
m(b){try{var
a=m(G(ab(d,0,b)));return a}catch(a){a=w(a);if(a===V){if(aP[1])x(D,b);return b}throw a}}function
H(k){var
d=k;for(;;){var
g=d[1];if(g){var
j=d[2],b=g[1],m=d[3],o=g[2],h=a(f,0),i=a(f,0),p=c(l,b[4],e),q=[0,h,1,b[3],p],t=c(l,a(s,b[4]),e),u=b[3],v=[0,i,1,a(B(s),u),t],w=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,0],a[1]],[0,d,a[2]]]}}(b,i,v),x=c(r[13],w,j),y=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,1],a[1]],[0,d,a[2]]]}}(b,h,q),z=c(r[13],y,j),A=c(n[22],z,x),d=[0,o,A,[0,[0,b[1],[0,b,h,i]],m]];continue}return[0,d[2],d[3]]}}try{var
J=c(q[17][aZ],o,j),K=function(a){return 0===a[2]?1:0},p=c(r[32],K,J),L=p[2],P=p[1],Q=function(a){return 2===a[2]?1:0},t=c(r[32],Q,L),R=t[1],u=N(f,t[2]),S=u[1],U=c(n[22],u[2],R),W=F([0,c(n[22],P,S),U]),X=function(a){return 2===a[2]?1:0},z=c(r[32],X,W),Y=z[2],Z=z[1],_=I(0),A=H([0,Z,[0,[0,0,Y],0],0]),$=A[2],ad=A[1],ae=function(a){var
b=a[1],f=a[2];T(0);try{m(f);throw ag}catch(a){a=w(a);if(a===k){var
d=O(0,0,I(0)),e=d[1],g=d[2],h=function(a){return c(aq[4][1],a[2],e)},i=c(r[32],h,b)[1],j=function(a){return a[1]};return[0,c(r[13],j,i),e,b,g]}throw a}},af=c(r[13],ae,ad),ah=function(e){var
b=c(y[1],0,7),a=[0,-1],d=[0,0];function
f(d){try{c(y[6],b,d)[1]++;var
a=0;return a}catch(a){a=w(a);if(a===M)return v(y[5],b,d,[0,1]);throw a}}function
g(a){var
b=a[1];if(b)return c(r[11],f,b);throw[0,ac,a[4],a[2]]}c(r[11],g,e);function
h(e,b){var
c=d[1]<b[1]?1:0,f=c?(a[1]=e,d[1]=b[1],0):c;return f}c(y[11],h,b);return a[1]},g=function(e){try{var
d=ah(e),l=function(g){var
b=g[3];for(;;){if(b){var
c=b[1],e=b[2],f=c[3];if(d===c[1])return f;var
b=e;continue}return a(n[2],fU)}},f=c(r[32],l,e),m=f[2],o=f[1],h=function(a){var
b=a[4],c=a[3],e=a[2],f=a[1];function
g(b,a){return b===a?1:0}return[0,v(q[17][87],g,d,f),e,c,b]},p=c(r[13],h,o),s=c(r[13],h,m),i=g(p),t=i[2],u=i[1],j=g(s),x=j[2],y=j[1],b=c(aq[4][2],d,$),k=b[1],z=b[3],A=b[2],B=function(b,a){return b===a?1:0},C=v(q[17][53],B,t,x),D=[0,[0,[15,k,[0,A,u],[0,z,y]],0],[0,k[1],C]];return D}catch(a){a=w(a);if(a[1]===ac)return[0,a[2],a[3]];throw a}},C=g(af),ai=O(C[2],C[1],_)[2];return ai}catch(a){a=w(a);if(a===k)return O(0,0,I(0))[2];throw a}}]}];c3(408,c7,"Omega_plugin.Omega");var
l=a(c7[2],[0,H[17],H[16],H[12],H[13],H[14],H[15],H[22],H[5],H[6],H[2]]),fV=0;function
aQ(b){var
d=[0,function(d){var
e=c(D[48][2],b,d);return a(i[99],e)}];return a(p[63][9],d)}function
bc(d,b){var
e=c(D[14],b,d),f=a(i[85],e);return c(p[67][8],f,b)}var
cI=[0,0],bd=[0,0],be=[0,0],cJ=[0,1];function
fW(d,c,b){return a(c,b)}var
fX=[0,0];function
fY(a,b){return a[1]}function
bf(b,a){b[1]=a;return 0}function
fZ(a){return bf(cI,a)}var
f2=[0,0,0,f1,f0,function(a){return cI[1]},fZ];c(cK[4],0,f2);function
f3(a){return bf(bd,a)}var
f6=[0,0,0,f5,f4,function(a){return bd[1]},f3];c(cK[4],0,f6);function
f7(a){return bf(be,a)}var
f_=[0,0,0,f9,f8,function(a){return be[1]},f7];c(cK[4],0,f_);function
f$(a){return bf(cJ,a)}var
gc=[0,1,1,gb,ga,function(a){return cJ[1]},f$];c(cK[4],0,gc);var
c8=[0,0];function
dz(d){var
a=c8[1];function
b(a){a[1][1]=a[2];return 0}return c(q[17][11],b,a)}function
aR(a){var
b=[0,a];c8[1]=[0,[0,b,a],c8[1]];return b}var
dA=aR(0);function
ai(e){var
b=a(n[20],dA[1]),d=c(n[16],gd,b);dA[1]++;return a(I[1][6],d)}var
dB=aR(0);function
ge(b){var
a=c(bg[3],gf,[0,dB[1]]);dB[1]++;return a}var
dC=aR(0);function
dD(e){var
b=a(n[20],dC[1]),d=c(n[16],gg,b);dC[1]++;return a(I[1][6],d)}var
dE=aR(0);function
bh(a){dE[1]++;return dE[1]}var
dF=aR(1e3);function
c9(a){dF[1]++;return dF[1]}var
dG=aR(0);function
dH(a){dG[1]++;return c(bg[3],gh,[0,dG[1]])}function
a1(a){return c(z[4],gi,a)}var
c_=[0,0],cL=c(y[1],0,7),c$=c(y[1],0,7);function
dI(b){c_[1]=0;return a(y[2],cL)}function
da(b){try{var
a=c(y[6],c$,b);return a}catch(a){a=w(a);if(a===M){var
d=dH(0);v(y[5],cL,d,b);v(y[5],c$,b,d);return d}throw a}}function
cM(b){try{var
a=c(y[6],cL,b);return a}catch(a){a=w(a);if(a===M){var
d=c_[1];v(y[5],cL,b,d);v(y[5],c$,d,b);c_[1]++;return d}throw a}}var
U=j[7];function
dJ(a){return cG(i[eX],0,gj,1,[0,[0,a,0]])}function
x(b){return a(i[148],b)}function
gk(b){return a(i[99],b)}function
db(b){return a(D[45],b)}function
R(b){var
c=g(b),e=0,j=h===c?b[1]:d===c?a(f[2],b):b;return a(i[67],[0,[0,0,j],e])}function
dK(c){return function(d){var
a=d;for(;;){if(a){var
b=a[1],e=a[2],f=b[1];if(c===b[2])return f;var
a=e;continue}throw M}}}var
bi=[0,0];function
dL(a){bi[1]=0;return 0}function
N(b){try{var
c=bi[1],d=a(dK(b),c);return d}catch(b){b=w(b);if(b===M)return a(n[2],gl);throw b}}function
gm(b){try{var
d=c(I[1][13][3],b,bi[1]);return d}catch(b){b=w(b);if(b===M)return a(n[2],gn);throw b}}function
aI(b,a){bi[1]=[0,[0,b,a],bi[1]];return 0}var
bj=[0,0];function
dM(a){return bj[1]}function
dc(a){bj[1]=0;return 0}function
dN(b){try{var
c=v(q[17][dx],e[135],b,bj[1]);return c}catch(b){b=w(b);if(b===M)return a(n[2],go);throw b}}function
dO(d,c,b,a){bj[1]=[0,[0,d,[0,c,b,a]],bj[1]];return 0}function
dP(b){var
a=cJ[1];return a?(dz(0),dI(0),dL(0),dc(0)):a}var
gq=c(q[18],C[9],gp),gr=c(q[18],[0,dQ,0],gq),gs=c(q[18],C[8],gr),dR=c(q[18],C[10],gs),ab=c(C[6],gt,C[10]),k=c(C[6],gu,dR),af=c(C[6],gw,gv),T=c(C[6],gy,gx),ar=[d,function(b){return a(k,gz)}],as=[d,function(b){return a(k,gA)}],at=[d,function(b){return a(k,gB)}],V=[d,function(b){return a(k,gC)}],W=[d,function(b){return a(k,gD)}],X=[d,function(b){return a(k,gE)}],A=[d,function(b){return a(k,gF)}],au=[d,function(b){return a(k,gG)}],av=[d,function(b){return a(k,gH)}],B=[d,function(b){return a(T,gI)}],O=[d,function(b){return a(T,gJ)}],F=[d,function(b){return a(T,gK)}],aj=[d,function(b){return a(T,gL)}],ak=[d,function(b){return a(T,gM)}],a2=[d,function(b){return a(T,gN)}],aw=[d,function(b){return a(T,gO)}],bk=[d,function(b){return a(af,gP)}],bl=[d,function(b){return a(af,gQ)}],bm=[d,function(b){return a(af,gR)}],bn=[d,function(b){return a(k,gS)}],bo=[d,function(b){return a(af,gT)}],bp=[d,function(b){return a(af,gU)}],bq=[d,function(b){return a(af,gV)}],br=[d,function(b){return a(af,gW)}],bs=[d,function(b){return a(af,gX)}],bt=[d,function(b){return a(af,gY)}],bu=[d,function(b){return a(af,gZ)}],G=[d,function(b){return a(k,g0)}],bv=[d,function(b){return a(k,g1)}],bw=[d,function(b){return a(k,g2)}],ax=[d,function(b){return a(k,g3)}],ay=[d,function(b){return a(k,g4)}],bx=[d,function(b){return a(k,g5)}],by=[d,function(b){return a(k,g6)}],bz=[d,function(b){return a(k,g7)}],bA=[d,function(b){return a(k,g8)}],bB=[d,function(b){return a(k,g9)}],bC=[d,function(b){return a(k,g_)}],bD=[d,function(b){return a(k,g$)}],bE=[d,function(b){return a(k,ha)}],bF=[d,function(b){return a(k,hb)}],bG=[d,function(b){return a(k,hc)}],bH=[d,function(b){return a(k,hd)}],bI=[d,function(b){return a(k,he)}],Y=[d,function(b){return a(k,hf)}],P=[d,function(b){return a(k,hg)}],bJ=[d,function(b){return a(k,hh)}],bK=[d,function(b){return a(k,hi)}],bL=[d,function(b){return a(k,hj)}],bM=[d,function(b){return a(k,hk)}],bN=[d,function(b){return a(k,hl)}],bO=[d,function(b){return a(k,hm)}],bP=[d,function(b){return a(k,hn)}],bQ=[d,function(b){return a(k,ho)}],bR=[d,function(b){return a(k,hp)}],bS=[d,function(b){return a(k,hq)}],bT=[d,function(b){return a(k,hr)}],bU=[d,function(b){return a(k,hs)}],bV=[d,function(b){return a(k,ht)}],Z=[d,function(b){return a(k,hu)}],bW=[d,function(b){return a(k,hv)}],bX=[d,function(b){return a(k,hw)}],bY=[d,function(b){return a(k,hx)}],bZ=[d,function(b){return a(k,hy)}],b0=[d,function(b){return a(k,hz)}],_=[d,function(b){return a(k,hA)}],b1=[d,function(b){return a(k,hB)}],b2=[d,function(b){return a(k,hC)}],b3=[d,function(b){return a(k,hD)}],b4=[d,function(b){return a(k,hE)}],b5=[d,function(b){return a(k,hF)}],b6=[d,function(b){return a(k,hG)}],b7=[d,function(b){return a(k,hH)}],b8=[d,function(b){return a(k,hI)}],b9=[d,function(b){return a(k,hJ)}],b_=[d,function(b){return a(T,hK)}],dS=[d,function(b){return a(k,hL)}],dT=[d,function(b){return a(T,hM)}],dU=[d,function(b){return a(T,hN)}],dV=[d,function(b){return a(k,hO)}],dW=[d,function(b){return a(k,hP)}],b$=[d,function(b){return a(k,hQ)}],dX=[d,function(b){return a(k,hR)}],dY=[d,function(b){return a(k,hS)}],dZ=[d,function(b){return a(k,hT)}],d0=[d,function(b){return a(k,hU)}],d1=[d,function(b){return a(k,hV)}],az=[d,function(b){return a(k,hW)}],aA=[d,function(b){return a(k,hX)}],al=[d,function(b){return a(T,hY)}],am=[d,function(b){return a(T,hZ)}],a3=[d,function(b){return a(T,h0)}],a4=[d,function(b){return a(T,h1)}],aB=[d,function(b){return a(ab,h2)}],ca=[d,function(b){return a(ab,h3)}],cb=[d,function(b){return a(ab,h4)}],aC=[d,function(b){return a(ab,h5)}],aD=[d,function(b){return a(ab,h6)}],cc=[d,function(b){return a(ab,h7)}],cd=[d,function(b){return a(ab,h8)}],ce=[d,function(b){return a(ab,h9)}],$=[d,function(b){return a(ab,h_)}],aE=[d,function(b){return a(ab,h$)}],aF=[d,function(b){return a(ab,ia)}],cf=[d,function(b){return a(k,ib)}],cg=[d,function(b){return a(k,ic)}],ch=[d,function(b){return a(k,id)}],d2=[d,function(b){return a(k,ie)}],d3=[d,function(b){return a(k,ig)}],d4=[d,function(b){return a(k,ih)}],d5=[d,function(b){return a(k,ii)}],ci=[d,function(b){return a(k,ij)}],d6=[d,function(b){return a(k,ik)}],d7=[d,function(b){return a(k,il)}],d8=[d,function(b){return a(k,im)}],d9=[d,function(b){return a(k,io)}],cj=[d,function(b){return a(k,ip)}],ck=[d,function(b){return a(k,iq)}],cl=[d,function(b){return a(k,ir)}],cm=[d,function(b){return a(k,is)}],cn=[d,function(b){return a(k,it)}],co=[d,function(b){return a(k,iu)}],cp=[d,function(b){return a(k,iv)}],cq=[d,function(b){return a(k,iw)}],cr=[d,function(b){return a(k,ix)}],cs=[d,function(b){return a(k,iy)}],ct=[d,function(b){return a(k,iz)}],cu=[d,function(b){return a(k,iA)}],cv=[d,function(b){return a(k,iB)}],cw=[d,function(b){return a(k,iC)}],cx=[d,function(b){return a(k,iD)}],cy=[d,function(b){return a(k,iE)}];function
aG(l,b){var
i=g(b),m=h===i?b[1]:d===i?a(f[2],b):b,j=a(e[ba],m);if(10===j[0]){var
k=j[1][1],q=a(iH[2],0);if(c(iI[2],q,[1,k]))return[1,k]}var
o=c(n[16],l,iF),p=a(dd[3],o);return v(ag[3],0,iG,p)}var
d_=[d,function(a){return aG(iJ,ak)}],d$=[d,function(a){return aG(iK,a2)}],ea=[d,function(a){return aG(iL,aj)}],de=[d,function(a){return aG(iM,al)}],aJ=[d,function(a){return aG(iN,am)}],iP=[d,function(a){return aG(iO,a3)}],iR=[d,function(a){return aG(iQ,a4)}],cN=[d,function(b){return aG(iS,[d,function(b){return a(C[53],0)}])}];function
iT(b){var
c=a(I[1][6],b);return a(e[o],c)}function
ah(i,c){var
b=g(B),j=[0,i,c],k=h===b?B[1]:d===b?a(f[2],B):B;return a(e[m],[0,k,j])}function
cO(i,c){var
b=g(O),j=[0,i,c],k=h===b?O[1]:d===b?a(f[2],O):O;return a(e[m],[0,k,j])}function
eb(i,c){var
b=g(aj),j=[0,i,c],k=h===b?aj[1]:d===b?a(f[2],aj):aj;return a(e[m],[0,k,j])}function
cz(i,c){var
b=g(A),j=h===b?A[1]:d===b?a(f[2],A):A,k=a(C[41],0),l=[0,a(df[46],k),[0,j,i,c]];return a(e[m],l)}function
iU(i,c){var
b=g(al),j=[0,i,c],k=h===b?al[1]:d===b?a(f[2],al):al;return a(e[m],[0,k,j])}function
aK(i,c){var
b=g(am),j=[0,i,c],k=h===b?am[1]:d===b?a(f[2],am):am;return a(e[m],[0,k,j])}function
aL(c){var
b=g(F),i=[0,c],j=h===b?F[1]:d===b?a(f[2],F):F;return a(e[m],[0,j,i])}function
cA(c,b){var
d=[0,a(C[54],0),[0,c,b]];return a(e[m],d)}function
cP(c,b){var
d=[0,a(C[59],0),[0,c,b]];return a(e[m],d)}function
aH(b){var
c=[0,a(C[53],0),[0,b]];return a(e[m],c)}function
iV(i,c){var
b=g(au),j=h===b?au[1]:d===b?a(f[2],au):au,k=a(C[41],0),l=[0,a(df[46],k),[0,j,i,c]];return a(e[m],l)}function
aM(c){var
b=g(aw),i=[0,c],j=h===b?aw[1]:d===b?a(f[2],aw):aw;return a(e[m],[0,j,i])}function
J(b){function
i(b){if(c(l[1],b,l[12])){var
j=g(ar);return h===j?ar[1]:d===j?a(f[2],ar):ar}var
p=[0,i(c(l[9],b,l[13]))],q=l[11],r=c(l[10],b,l[13]);if(c(l[1],r,q))var
k=g(as),s=h===k?as[1]:d===k?a(f[2],as):as,n=s;else
var
o=g(at),t=h===o?at[1]:d===o?a(f[2],at):at,n=t;return a(e[m],[0,n,p])}if(c(l[1],b,l[11])){var
j=g(V);return h===j?V[1]:d===j?a(f[2],V):V}var
p=[0,i(a(l[15],b))];if(c(l[4],b,l[11]))var
k=g(W),q=h===k?W[1]:d===k?a(f[2],W):W,n=q;else
var
o=g(X),r=h===o?X[1]:d===o?a(f[2],X):X,n=r;return a(e[m],[0,n,p])}function
aS(z){var
k=a(e[39],z),b=k[2],i=k[1],j=a(e[ba],i);if(b){var
l=b[2];if(l){var
m=l[2];if(m){if(!m[2]){var
E=a(C[41],0);if(c(iX[11],E,i))return[1,16,b]}}else{var
n=g(az),F=h===n?az[1]:d===n?a(f[2],az):az;if(c(e[u],i,F))return[1,17,b];var
o=g(aA),G=h===o?aA[1]:d===o?a(f[2],aA):aA;if(c(e[u],i,G))return[1,18,b];var
p=g(al),H=h===p?al[1]:d===p?a(f[2],al):al;if(c(e[u],i,H))return[1,19,b];var
q=g(a4),I=h===q?a4[1]:d===q?a(f[2],a4):a4;if(c(e[u],i,I))return[1,20,b];var
r=g(a3),J=h===r?a3[1]:d===r?a(f[2],a3):a3;if(c(e[u],i,J))return[1,21,b];var
s=g(am),K=h===s?am[1]:d===s?a(f[2],am):am;if(c(e[u],i,K))return[1,22,b];var
L=a(C[54],0);if(c(e[u],i,L))return[1,25,b];var
M=a(C[59],0);if(c(e[u],i,M))return[1,26,b];var
t=g(cy),N=h===t?cy[1]:d===t?a(f[2],cy):cy;if(c(e[u],i,N))return[1,30,b];var
v=g(aB),O=h===v?aB[1]:d===v?a(f[2],aB):aB;if(c(e[u],i,O))return[1,31,b];var
w=g(ca),P=h===w?ca[1]:d===w?a(f[2],ca):ca;if(c(e[u],i,P))return[1,32,b];var
x=g(cb),Q=h===x?cb[1]:d===x?a(f[2],cb):cb;if(c(e[u],i,Q))return[1,33,b];var
y=g(aC),R=h===y?aC[1]:d===y?a(f[2],aC):aC;if(c(e[u],i,R))return[1,34,b]}}else{var
S=a(C[53],0);if(c(e[u],i,S))return[1,29,b]}}else{var
T=a(C[50],0);if(c(e[u],i,T))return[1,27,b];var
U=a(C[51],0);if(c(e[u],i,U))return[1,28,b]}switch(j[0]){case
1:if(!b)return[0,j[1]];break;case
6:if(j[1]){if(!b)return a(ag[7],iW)}else
if(!b)return[2,j[2],j[3]];break;case
10:var
A=a(dg[36],[1,j[1][1]]);return[1,[0,a(dh[22],A)],b];case
11:var
B=a(dg[36],[2,j[1][1]]);return[1,[0,a(dh[22],B)],b];case
12:var
D=a(dg[36],[3,j[1][1]]);return[1,[0,a(dh[22],D)],b]}return 0}function
cB(m){var
j=a(e[39],m),b=j[2],i=j[1];a(e[ba],i);if(!b){var
k=g(A),n=h===k?A[1]:d===k?a(f[2],A):A;if(c(e[u],i,n))return[1,23,b];var
l=g($),o=h===l?$[1]:d===l?a(f[2],$):$;if(c(e[u],i,o))return[1,24,b]}return 0}function
cC(E){var
j=a(e[39],E),b=j[2],i=j[1],k=a(e[ba],i);if(b){var
l=b[2];if(l){if(!l[2]){var
m=g(B),G=h===m?B[1]:d===m?a(f[2],B):B;if(c(e[u],i,G))return[1,0,b];var
n=g(O),H=h===n?O[1]:d===n?a(f[2],O):O;if(c(e[u],i,H))return[1,1,b];var
o=g(aj),I=h===o?aj[1]:d===o?a(f[2],aj):aj;if(c(e[u],i,I))return[1,2,b];var
p=g(cc),J=h===p?cc[1]:d===p?a(f[2],cc):cc;if(c(e[u],i,J))return[1,6,b];var
q=g(cd),K=h===q?cd[1]:d===q?a(f[2],cd):cd;if(c(e[u],i,K))return[1,7,b];var
r=g(aD),L=h===r?aD[1]:d===r?a(f[2],aD):aD;if(c(e[u],i,L))return[1,8,b]}}else{var
s=g(ak),M=h===s?ak[1]:d===s?a(f[2],ak):ak;if(c(e[u],i,M))return[1,3,b];var
t=g(a2),N=h===t?a2[1]:d===t?a(f[2],a2):a2;if(c(e[u],i,N))return[1,5,b];var
v=g(F),P=h===v?F[1]:d===v?a(f[2],F):F;if(c(e[u],i,P))return[1,4,b];var
w=g(ce),Q=h===w?ce[1]:d===w?a(f[2],ce):ce;if(c(e[u],i,Q))return[1,9,b];var
x=g(aE),R=h===x?aE[1]:d===x?a(f[2],aE):aE;if(c(e[u],i,R))return[1,10,b];var
y=g(W),S=h===y?W[1]:d===y?a(f[2],W):W;if(c(e[u],i,S))return[1,13,b];var
z=g(X),T=h===z?X[1]:d===z?a(f[2],X):X;if(c(e[u],i,T))return[1,12,b];var
A=g(aw),U=h===A?aw[1]:d===A?a(f[2],aw):aw;if(c(e[u],i,U))return[1,15,b]}}else{var
C=g(aF),Y=h===C?aF[1]:d===C?a(f[2],aF):aF;if(c(e[u],i,Y))return[1,11,b];var
D=g(V),Z=h===D?V[1]:d===D?a(f[2],V):V;if(c(e[u],i,Z))return[1,14,b];if(1===k[0])return[0,k[1]]}return 0}function
ec(r){function
b(r){var
k=a(e[39],r),i=k[2],j=k[1];if(i){if(!i[2]){var
m=i[1],o=g(at),s=h===o?at[1]:d===o?a(f[2],at):at;if(c(e[u],j,s)){var
t=b(m),v=c(l[8],l[13],t);return c(l[6],l[12],v)}var
p=g(as),w=h===p?as[1]:d===p?a(f[2],as):as;if(c(e[u],j,w)){var
x=b(m);return c(l[8],l[13],x)}}}else{var
q=g(ar),y=h===q?ar[1]:d===q?a(f[2],ar):ar;if(c(e[u],j,y))return l[12]}return a(n[2],iY)}var
k=a(e[39],r),i=k[2],j=k[1];if(i){if(!i[2]){var
m=i[1],o=g(W),s=h===o?W[1]:d===o?a(f[2],W):W;if(c(e[u],j,s))return b(m);var
p=g(X),t=h===p?X[1]:d===p?a(f[2],X):X;if(c(e[u],j,t)){var
v=b(m);return a(l[17],v)}}}else{var
q=g(V),w=h===q?V[1]:d===q?a(f[2],V):V;if(c(e[u],j,w))return l[11]}return a(n[2],iZ)}function
di(C,f,b){function
d(f,h,v){var
b=a(e[ba],v);if(5===b[0]){var
an=b[3],ap=b[2],aq=[0,d(f,h,b[1]),ap,an];return a(e[116],aq)}if(h){var
i=h[1];if(typeof
i==="number")switch(i){case
0:var
k=h[2];switch(b[0]){case
6:var
G=b[2],H=b[1],I=[0,H,G,d(f+1|0,k,b[3])];return a(e[eK],I);case
7:var
J=b[2],K=b[1],L=[0,K,J,d(f+1|0,k,b[3])];return a(e[a$],L);case
8:var
M=b[3],N=b[2],O=b[1],P=[0,O,N,M,d(f+1|0,k,b[4])];return a(e[dx],P);case
14:var
g=0;break;default:var
g=1}break;case
1:var
p=h[2];switch(b[0]){case
6:var
T=b[3],U=b[1],V=[0,U,d(f,p,b[2]),T];return a(e[eK],V);case
7:var
W=b[3],X=b[1],Y=[0,X,d(f,p,b[2]),W];return a(e[a$],Y);case
8:var
Z=b[4],_=b[2],$=b[1],aa=[0,$,_,d(f,p,b[3]),Z];return a(e[dx],aa);case
14:var
g=0;break;default:var
g=1}break;case
2:var
ab=h[2];switch(b[0]){case
9:var
ac=b[2],ad=[0,d(f,ab,b[1]),ac];return a(e[61],ad);case
14:var
g=0;break;default:var
g=1}break;default:var
ae=h[2];switch(b[0]){case
9:var
af=b[1],r=a(q[19][8],b[2]);r[1]=d(f,ae,ao(r,0)[1]);return a(e[m],[0,af,r]);case
14:var
g=0;break;default:var
g=1}}else
if(0===i[0]){var
z=i[1],ag=h[2];switch(b[0]){case
9:var
ah=b[1],s=a(q[19][8],b[2]),A=z-1|0,B=z-1|0,ai=d(f,ag,ao(s,A)[A+1]);ao(s,B)[B+1]=ai;return a(e[m],[0,ah,s]);case
14:var
g=0;break;default:var
g=1}}else{var
t=i[1],aj=h[2];switch(b[0]){case
13:var
ak=b[3],al=b[2],am=b[1],u=a(q[19][8],b[4]);u[t+1]=d(f,aj,ao(u,t)[t+1]);return a(e[129],[0,am,al,ak,u]);case
14:var
g=0;break;default:var
g=1}}if(g){var
D=a(q[17][1],h),E=a(n[20],D),F=c(n[16],i0,E);return a(n[2],F)}var
w=b[1],l=w[2],o=l[3],x=w[1],j=x[2],Q=l[2],R=l[1],y=a(q[19][8],o),S=d(f+(o.length-1)|0,h,ao(o,j)[j+1]);ao(y,j)[j+1]=S;return a(e[130],[0,x,[0,R,Q,y]])}return c(C,f,v)}return d(1,f,b)}function
ed(r,p){var
b=r,f=p;for(;;){var
d=a(e[ba],f);if(5===d[0]){var
f=d[1];continue}if(b){var
h=b[1];if(typeof
h==="number")switch(h){case
0:var
i=b[2];switch(d[0]){case
6:var
b=i,f=d[3];continue;case
7:var
b=i,f=d[3];continue;case
8:var
b=i,f=d[4];continue;case
14:var
g=0;break;default:var
g=1}break;case
1:var
j=b[2];switch(d[0]){case
6:var
b=j,f=d[2];continue;case
7:var
b=j,f=d[2];continue;case
8:var
b=j,f=d[3];continue;case
14:var
g=0;break;default:var
g=1}break;case
2:var
v=b[2];switch(d[0]){case
9:var
b=v,f=d[1];continue;case
14:var
g=0;break;default:var
g=1}break;default:var
w=b[2];switch(d[0]){case
9:var
b=w,f=ao(d[2],0)[1];continue;case
14:var
g=0;break;default:var
g=1}}else
if(0===h[0]){var
x=b[2],y=h[1];switch(d[0]){case
9:var
m=y-1|0,b=x,f=ao(d[2],m)[m+1];continue;case
14:var
g=0;break;default:var
g=1}}else{var
o=h[1],z=b[2];switch(d[0]){case
13:var
b=z,f=ao(d[4],o)[o+1];continue;case
14:var
g=0;break;default:var
g=1}}if(g){var
s=a(q[17][1],b),t=a(n[20],s),u=c(n[16],i1,t);return a(n[2],u)}var
k=d[1],l=k[1][2],f=ao(k[2][3],l)[l+1];continue}return f}}function
dj(f,d,c){var
b=[0,a(e[bb],0)],g=di(function(d,c){b[1]=c;return a(e[bb],d)},d,c),h=b[1],i=[0,[0,a(I[1][6],i2)],f,g];return[0,a(e[a$],i),h]}function
K(h){return function(b){var
d=a(D[7],b),e=a(q[17][6],h),f=di(function(d,a){return c(D[29],b,a)},e,d),g=c(i[5],f,2);return c(p[67][8],g,b)}}function
aN(c){var
b=c;for(;;)switch(b[0]){case
0:var
d=b[2],e=b[1];a(n[27],i3);aN(e);a(n[27],i4);aN(d);return a(n[27],i5);case
1:var
f=b[1];a(n[27],i6);var
b=f;continue;case
2:var
g=b[2],h=b[1];a(n[27],i7);aN(h);a(n[27],i8);aN(g);return a(n[27],i9);case
3:var
i=a(I[1][8],b[1]);return a(n[27],i);case
4:var
j=a(l[16],b[1]);return a(n[27],j);default:return a(n[27],i_)}}function
aa(c){var
b=c;for(;;)switch(b[0]){case
0:return a(n[2],i$);case
1:var
b=b[1];continue;case
2:var
b=b[1];continue;case
3:return cM(b[1]);case
4:return-1;default:return-1}}function
E(b){switch(b[0]){case
0:var
k=b[1],l=E(b[2]),c=g(B),n=[0,E(k),l],p=h===c?B[1]:d===c?a(f[2],B):B;return a(e[m],[0,p,n]);case
1:var
i=g(F),q=[0,E(b[1])],r=h===i?F[1]:d===i?a(f[2],F):F;return a(e[m],[0,r,q]);case
2:var
s=b[1],t=E(b[2]),j=g(O),u=[0,E(s),t],v=h===j?O[1]:d===j?a(f[2],O):O;return a(e[m],[0,v,u]);case
3:return a(e[o],b[1]);case
4:return J(b[1]);default:return b[1]}}function
ee(i,h){var
n=0;return function(o){var
c=n,b=o;for(;;){switch(b[0]){case
0:var
d=b[1];if(2===d[0]){var
e=d[1];if(3===e[0]){var
f=d[2];if(4===f[0]){var
k=b[2],l=f[1],c=[0,[0,l,cM(e[1])],c],b=k;continue}}}break;case
4:var
m=b[1],g=bh(0);aI(i,g);return[0,g,h,a(q[17][6],c),m]}var
j=a(dd[3],ja);return v(ag[3],0,0,j)}}}function
Q(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1],g=c(a[2]);return[0,[2,[3,da(e)],[4,f]],g]}return[4,b[4]]}return c(b[3])}function
cQ(c){var
b=a(jb[1],0);return a(e[eX],b)}function
dk(b,o,j,n,i){var
p=a(D[7],i),k=dj(b,a(q[17][6],o),p),r=k[2],s=[0,k[1],0],t=a(e[bb],1),l=g(cj),u=[0,b,j,a(e[bb],2),t,r,n],v=h===l?cj[1]:d===l?a(f[2],cj):cj,w=a(e[m],[0,v,u]),x=[0,a(e[bb],1),[0,j,0]],y=a(e[59],x),z=[0,[0,a(I[1][6],jc)],y,w],A=a(e[a$],z),B=c(e[49],b,e[aZ]),C=[0,[0,a(I[1][6],jd)],B,A],E=[0,a(e[a$],C),s],F=a(e[59],E),G=[0,F,[0,cQ(0),0]];return a(db(a(e[59],G)),i)}function
cR(j,i,e,c){var
b=g(A),k=h===b?A[1]:d===b?a(f[2],A):A;return dk(k,j,i,e,c)}function
ef(j,i,e,c){var
b=g($),k=h===b?$[1]:d===b?a(f[2],$):$;return dk(k,j,i,e,c)}function
a5(d,c,b){var
f=a(e[59],[0,b[1],b[2]]);return function(a){return cR(d,c,f,a)}}function
eg(d,c,b){var
f=a(e[59],[0,b[1],b[2]]);return function(a){return ef(d,c,f,a)}}function
s(m,l,k,b){var
n=a(D[7],b),i=g(A),o=a(q[17][6],m),p=h===i?A[1]:d===i?a(f[2],A):A,j=dj(p,o,n),r=j[2],s=j[1];function
t(a){return ed(a,r)}var
u=c(q[17][12],t,l),v=[0,k,c(q[18],u,[0,s,0])],w=a(e[59],v),x=[0,w,[0,cQ(0),0]];return a(db(a(e[59],x)),b)}function
a6(i,m){var
b=m[2],e=m[1];switch(e[0]){case
0:var
n=e[2],j=e[1];if(0===b[0]){var
o=b[1],B=b[2],C=aa(o),D=aa(j);if(c(l[19],D,C)){var
p=a6([0,je,i],[0,n,b]),q=g(G),E=p[1],F=[0,j,p[2]],I=h===q?G[1]:d===q?a(f[2],G):G;return[0,[0,function(a){return s(i,jf,I,a)},E],F]}var
r=a6([0,jg,i],[0,e,B]),t=g(ax),J=r[1],L=[0,o,r[2]],M=h===t?ax[1]:d===t?a(f[2],ax):ax;return[0,[0,function(a){return s(i,jh,M,a)},J],L]}var
N=aa(b),O=aa(j);if(c(l[19],O,N)){var
u=a6([0,ji,i],[0,n,b]),v=g(G),P=u[1],Q=[0,j,u[2]],R=h===v?G[1]:d===v?a(f[2],G):G;return[0,[0,function(a){return s(i,jj,R,a)},P],Q]}var
w=g(ay),S=[0,b,e],T=0,U=h===w?ay[1]:d===w?a(f[2],ay):ay;return[0,[0,function(a){return s(i,jk,U,a)},T],S];case
4:var
af=e[1];switch(b[0]){case
0:var
k=0;break;case
4:var
ag=[4,c(H[12],af,b[1])];return[0,[0,K(i),0],ag];default:var
k=1}break;default:var
k=0}if(!k)if(0===b[0]){var
x=b[1],V=b[2],W=aa(e),X=aa(x);if(c(l[19],X,W)){var
y=a6([0,jl,i],[0,e,V]),z=g(ax),Y=y[1],Z=[0,x,y[2]],_=h===z?ax[1]:d===z?a(f[2],ax):ax;return[0,[0,function(a){return s(i,jm,_,a)},Y],Z]}return[0,0,[0,e,b]]}var
$=aa(b),ab=aa(e);if(c(l[18],ab,$)){var
A=g(ay),ac=[0,b,e],ad=0,ae=h===A?ay[1]:d===A?a(f[2],ay):ay;return[0,[0,function(a){return s(i,jn,ae,a)},ad],ac]}return[0,0,[0,e,b]]}function
eh(o,D,i,C,b){function
e(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],E=p[1];if(j){var
n=j[2],r=j[1],t=r[2],F=r[1];if(q===t){var
u=g(bI),G=h===u?bI[1]:d===u?a(f[2],bI):bI,v=function(a){return s(b,jo,G,a)},I=l[11],J=c(H[14],C,F),L=c(H[14],D,E),M=c(H[12],L,J);if(c(l[1],M,I)){var
w=g(Z),N=h===w?Z[1]:d===w?a(f[2],Z):Z,O=function(a){return s(b,jp,N,a)},Q=[0,O,e(b,[0,k,n])];return[0,v,[0,K([0,jr,[0,jq,b]]),Q]]}return[0,v,e([0,js,b],[0,k,n])]}if(c(l[19],q,t)){var
x=g(Y),R=e([0,jt,b],[0,k,j]),S=h===x?Y[1]:d===x?a(f[2],Y):Y;return[0,function(a){return s(b,ju,S,a)},R]}var
y=g(P),T=e([0,jv,b],[0,i,n]),U=h===y?P[1]:d===y?a(f[2],P):P;return[0,function(a){return s(b,jw,U,a)},T]}var
z=g(Y),V=e([0,jx,b],[0,k,0]),W=h===z?Y[1]:d===z?a(f[2],Y):Y;return[0,function(a){return s(b,jy,W,a)},V]}var
A=m[2];if(A){var
B=g(P),X=e([0,jz,b],[0,0,A[2]]),_=h===B?P[1]:d===B?a(f[2],P):P;return[0,function(a){return s(b,jA,_,a)},X]}return[0,K(o),0]}return e(o,[0,i,b])}function
cS(o,i,C,b){function
e(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],D=p[1];if(j){var
n=j[2],r=j[1],t=r[2],E=r[1];if(q===t){var
u=g(bL),F=h===u?bL[1]:d===u?a(f[2],bL):bL,v=function(a){return s(b,jB,F,a)},I=l[11],J=c(H[14],C,E),L=c(H[12],D,J);if(c(l[1],L,I)){var
w=g(Z),M=h===w?Z[1]:d===w?a(f[2],Z):Z,N=function(a){return s(b,jC,M,a)},O=[0,N,e(b,[0,k,n])];return[0,v,[0,K([0,jE,[0,jD,b]]),O]]}return[0,v,e([0,jF,b],[0,k,n])]}if(c(l[19],q,t)){var
x=g(G),Q=e([0,jG,b],[0,k,j]),R=h===x?G[1]:d===x?a(f[2],G):G;return[0,function(a){return s(b,jH,R,a)},Q]}var
y=g(P),S=e([0,jI,b],[0,i,n]),T=h===y?P[1]:d===y?a(f[2],P):P;return[0,function(a){return s(b,jJ,T,a)},S]}var
z=g(G),U=e([0,jK,b],[0,k,0]),V=h===z?G[1]:d===z?a(f[2],G):G;return[0,function(a){return s(b,jL,V,a)},U]}var
A=m[2];if(A){var
B=g(P),W=e([0,jM,b],[0,0,A[2]]),X=h===B?P[1]:d===B?a(f[2],P):P;return[0,function(a){return s(b,jN,X,a)},W]}return[0,K(o),0]}return e(o,[0,i,b])}function
dl(e,b){if(b){var
m=b[2];if(c(l[4],b[1][1],l[11]))var
i=g(bJ),n=h===i?bJ[1]:d===i?a(f[2],bJ):bJ,j=n;else
var
k=g(bK),p=h===k?bK[1]:d===k?a(f[2],bK):bK,j=p;var
o=function(a){return s(e,jO,j,a)};return[0,o,dl(e,m)]}return[0,K(e),0]}function
cD(i,j,b){switch(b[0]){case
0:var
v=b[2],k=cD([0,jP,i],j,b[1]),w=k[2],x=k[1],n=cD([0,jQ,i],j,v),y=[0,w,n[2]],o=g(bX),z=c(q[18],x,n[1]),A=h===o?bX[1]:d===o?a(f[2],bX):bX;return[0,[0,function(a){return s(i,jR,A,a)},z],y];case
1:var
B=b[1],C=[2,B,[4,a(l[17],j)]],p=g(bY),D=[0,K([0,jS,i]),0],E=h===p?bY[1]:d===p?a(f[2],bY):bY;return[0,[0,function(a){return s(i,jT,E,a)},D],C];case
2:var
r=b[2],F=b[1];if(4===r[0]){var
G=[2,F,[4,c(l[8],j,r[1])]],t=g(bw),H=[0,K([0,jV,i]),0],I=h===t?bw[1]:d===t?a(f[2],bw):bw;return[0,[0,function(a){return s(i,jW,I,a)},H],G]}return a(ag[7],jU);case
3:return[0,0,[2,b,[4,j]]];case
4:var
L=[4,c(l[8],j,b[1])];return[0,[0,K(i),0],L];default:var
M=b[1],u=g(O),N=[0,J(j),M],P=h===u?O[1]:d===u?a(f[2],O):O;return[0,0,[5,a(e[m],[0,P,N])]]}}function
cT(b){function
c(i,e){if(e){var
j=g(bM),k=c([0,jX,i],e[2]),l=h===j?bM[1]:d===j?a(f[2],bM):bM;return[0,function(a){return s(i,jY,l,a)},k]}return[0,K(b),0]}return function(a){return c(b,a)}}function
ei(b){function
c(i,e){if(e){var
j=g(G),k=c([0,jZ,i],e[2]),l=h===j?G[1]:d===j?a(f[2],G):G;return[0,function(a){return s(i,j0,l,a)},k]}return[0,K(b),0]}return function(a){return c(b,a)}}function
cU(b){function
c(i,e){if(e){var
j=g(Y),k=c([0,j1,i],e[2]),l=h===j?Y[1]:d===j?a(f[2],Y):Y;return[0,function(a){return s(i,j2,l,a)},k]}return[0,K(b),0]}return function(a){return c(b,a)}}function
cV(i,b){switch(b[0]){case
0:var
v=b[2],j=cV([0,j3,i],b[1]),w=j[2],x=j[1],k=cV([0,j4,i],v),y=[0,w,k[2]],n=g(bZ),z=c(q[18],x,k[1]),A=h===n?bZ[1]:d===n?a(f[2],bZ):bZ;return[0,[0,function(a){return s(i,j5,A,a)},z],y];case
1:var
o=g(b1),B=b[1],C=0,D=h===o?b1[1]:d===o?a(f[2],b1):b1;return[0,[0,function(a){return s(i,j6,D,a)},C],B];case
2:var
p=b[2],E=b[1];if(4===p[0]){var
G=[2,E,[4,a(l[17],p[1])]],r=g(b0),H=[0,K([0,j8,i]),0],I=h===r?b0[1]:d===r?a(f[2],b0):b0;return[0,[0,function(a){return s(i,j9,I,a)},H],G]}return a(ag[7],j7);case
3:var
t=g(_),J=[2,b,[4,l[14]]],L=0,M=h===t?_[1]:d===t?a(f[2],_):_;return[0,[0,function(a){return s(i,j_,M,a)},L],J];case
4:var
N=[4,a(l[17],b[1])];return[0,[0,K(i),0],N];default:var
u=g(F),O=[0,b[1]],P=h===u?F[1]:d===u?a(f[2],F):F;return[0,0,[5,a(e[m],[0,P,O])]]}}function
an(j,r){function
t(h,d){try{var
b=dN(d),g=b[1],q=a(e[o],b[2]),n=[3,g],p=0,r=a(e[o],g),s=[0,[0,function(a){return cR(j,r,q,a)},p],n];return s}catch(b){b=w(b);if(a(ag[21],b)){var
c=dD(0),f=ai(0);dO(d,c,f,h);var
l=a(e[o],f),i=[3,c],k=0,m=a(e[o],c);return[0,[0,function(a){return cR(j,m,l,a)},k],i]}throw b}}try{var
k=cC(r);if(typeof
k==="number")var
i=0;else
switch(k[0]){case
0:var
u=[0,0,[3,k[1]]],i=1;break;case
1:var
v=k[1];if(typeof
v==="number")if(16<=v)var
i=0;else{switch(v){case
0:var
x=k[2];if(x){var
y=x[2];if(y)if(y[2])var
i=0,b=0;else
var
af=y[1],N=an([0,j$,j],x[1]),ah=N[2],aj=N[1],O=an([0,ka,j],af),ak=O[1],P=a6(j,[0,ah,O[2]]),al=P[2],am=c(q[18],ak,P[1]),n=[0,c(q[18],aj,am),al],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
z=k[2];if(z){var
A=z[2];if(A)if(A[2])var
i=0,b=0;else{var
ao=A[1],Q=an([0,kb,j],z[1]),C=Q[2],S=Q[1],T=an([0,kc,j],ao),D=T[2],U=T[1];if(4===D[0])var
X=cD(j,D[1],C),au=X[2],av=c(q[18],U,X[1]),E=[0,c(q[18],S,av),au];else
if(4===C[0])var
V=g(bx),ap=C[1],aq=h===V?bx[1]:d===V?a(f[2],bx):bx,ar=function(a){return s(j,kd,aq,a)},W=cD(j,ap,D),as=W[2],at=c(q[18],U,[0,ar,W[1]]),E=[0,c(q[18],S,at),as];else
var
E=t(0,r);var
n=E,b=1}else
var
i=0,b=0}else
var
i=0,b=0;break;case
2:var
G=k[2];if(G){var
H=G[2];if(H)if(H[2])var
i=0,b=0;else
var
Y=g(F),aw=G[1],ax=[0,H[1]],ay=h===Y?F[1]:d===Y?a(f[2],F):F,Z=g(B),az=[0,aw,a(e[m],[0,ay,ax])],aA=h===Z?B[1]:d===Z?a(f[2],B):B,_=an(j,a(e[m],[0,aA,az])),aB=_[2],aC=_[1],aD=R(ea),n=[0,[0,a(p[67][8],aD),aC],aB],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
3:var
I=k[2];if(I)if(I[2])var
i=0,b=0;else
var
aE=I[1],$=g(B),aF=[0,aE,J(l[12])],aG=h===$?B[1]:d===$?a(f[2],B):B,aa=an(j,a(e[m],[0,aG,aF])),aH=aa[2],aI=aa[1],aJ=R(d_),n=[0,[0,a(p[67][8],aJ),aI],aH],b=1;else
var
i=0,b=0;break;case
4:var
K=k[2];if(K)if(K[2])var
i=0,b=0;else
var
ab=an([0,ke,j],K[1]),aK=ab[1],ac=cV(j,ab[2]),aL=ac[2],n=[0,c(q[18],aK,ac[1]),aL],b=1;else
var
i=0,b=0;break;case
5:var
L=k[2];if(L)if(L[2])var
i=0,b=0;else
var
aM=L[1],ad=g(B),aN=[0,aM,J(l[14])],aO=h===ad?B[1]:d===ad?a(f[2],B):B,ae=an(j,a(e[m],[0,aO,aN])),aP=ae[2],aQ=ae[1],aR=R(d$),n=[0,[0,a(p[67][8],aR),aQ],aP],b=1;else
var
i=0,b=0;break;case
15:var
M=k[2];if(M)if(M[2])var
i=0,b=0;else
var
n=t(1,M[1]),b=1;else
var
i=0,b=0;break;case
12:case
13:case
14:try{var
aS=[0,0,[4,ec(r)]],n=aS,b=1}catch(c){c=w(c);if(!a(ag[21],c))throw c;var
n=t(0,r),b=1}break;default:var
i=0,b=0}if(b)var
u=n,i=1}else
var
i=0;break;default:var
i=0}if(!i)var
u=t(0,r);return u}catch(b){b=w(b);if(a(aT[4],b))return t(0,r);throw b}}function
dm(e,c,b){switch(c[0]){case
2:var
i=c[1],q=c[2];switch(b[0]){case
2:if(3===i[0]){var
j=g(bV),r=[2,[3,i[1]],[0,c[2],b[2]]],t=h===j?bV[1]:d===j?a(f[2],bV):bV;return[0,function(a){return s(e,kg,t,a)},r]}break;case
3:var
k=g(bU),u=[2,[3,b[1]],[0,q,[4,l[12]]]],v=h===k?bU[1]:d===k?a(f[2],bU):bU;return[0,function(a){return s(e,kh,v,a)},u]}break;case
3:var
m=c[1];switch(b[0]){case
2:var
o=g(bT),w=[2,[3,m],[0,b[2],[4,l[12]]]],x=h===o?bT[1]:d===o?a(f[2],bT):bT;return[0,function(a){return s(e,ki,x,a)},w];case
3:var
p=g(bS),y=[2,[3,m],[4,l[13]]],z=h===p?bS[1]:d===p?a(f[2],bS):bS;return[0,function(a){return s(e,kj,z,a)},y]}break}aN(c);a(n[32],0);aN(b);a(n[32],0);a(n[46],n[24]);return a(ag[7],kf)}function
cE(i,b){switch(b[0]){case
2:var
j=b[1];if(3===j[0]){var
k=b[2],n=j[1];if(4===k[0])return[0,0,b];var
e=function(b){switch(b[0]){case
0:var
d=b[1],f=e(b[2]),g=e(d);return c(H[12],g,f);case
4:return b[1];default:return a(ag[7],kl)}},o=[2,[3,n],[4,e(k)]];return[0,[0,K([0,km,i]),0],o]}break;case
3:var
m=g(bR),p=[2,[3,b[1]],[4,l[12]]],q=0,r=h===m?bR[1]:d===m?a(f[2],bR):bR;return[0,[0,function(a){return s(i,kn,r,a)},q],p]}aN(b);return a(ag[7],kk)}function
a7(b,j){switch(j[0]){case
0:var
e=j[2],i=j[1];switch(e[0]){case
0:var
k=e[1],A=e[2],B=aa(k);if(aa(i)===B){var
m=dm([0,ko,b],i,k),n=g(bv),C=m[2],D=m[1],E=h===n?bv[1]:d===n?a(f[2],bv):bv,F=function(a){return s(b,kp,E,a)},o=a7(b,[0,C,A]);return[0,[0,F,[0,D,o[1]]],o[2]]}var
p=cE([0,kq,b],i),G=p[2],H=p[1],r=a7([0,kr,b],e),I=[0,G,r[2]];return[0,c(q[18],H,r[1]),I];case
4:var
O=e[1],x=cE([0,ku,b],i);return[0,x[1],[0,x[2],[4,O]]];default:var
J=aa(e);if(aa(i)===J){var
t=dm(b,i,e),K=t[1],u=a7(b,t[2]);return[0,[0,K,u[1]],u[2]]}var
v=cE([0,ks,b],i),L=v[2],M=v[1],w=a7([0,kt,b],e),N=[0,L,w[2]];return[0,c(q[18],M,w[1]),N]}case
4:return[0,0,j];default:var
y=cE(b,j),z=g(bW),P=y[1],Q=[0,y[2],[4,l[11]]],R=h===z?bW[1]:d===z?a(f[2],bW):bW,S=[0,function(a){return s(b,kv,R,a)},0];return[0,c(q[18],P,S),Q]}}function
cW(i,b){if(0===b[0]){var
e=b[1];if(2===e[0])if(3===e[1][0]){var
k=e[2];if(4===k[0]){var
o=b[2];if(c(l[1],k[1],l[11])){var
m=g(Z),p=h===m?Z[1]:d===m?a(f[2],Z):Z,q=function(a){return s(i,kx,p,a)},n=cW(i,o);return[0,[0,q,n[1]],n[2]]}}}var
j=cW([0,kw,i],b[2]);return[0,j[1],[0,e,j[2]]]}return[0,0,b]}function
dn(bp){var
k=a(I[1][6],ky),n=a(I[1][6],kz),q=a(I[1][6],kA),z=J(l[11]);function
r(bq){var
t=bq;for(;;){if(t){var
b=t[1];switch(b[0]){case
0:var
am=b[2],an=b[1],br=t[2],bs=b[4],bt=b[3],u=N(an[1]),ao=E(Q(an)),ap=E(Q(am)),F=J(bt),W=J(bs),aq=ah(cO(ap,F),W),bu=cz(ao,aq),bv=am[3],bw=a(cU(kB),bv),bx=i[S],bI=a(U,bw),bJ=a(p[67][1],bI),bK=[0,c(j[70][3],bJ,bx),0],bL=[0,i[60],[0,i[S],0]],bM=[0,R(aJ),bL],bR=[0,a(j[70][20],bM),0],bS=[0,i[60],[0,i[S],0]],bT=[0,R(aJ),bS],bU=[0,a(j[70][20],bT),0],bV=[0,r(br),0],bW=[0,a(i[25],[0,u,0]),bV],bX=[0,a(i[74],[0,n,[0,q,[0,u,0]]]),bW],bZ=a(e[o],u),b0=a(e[o],q),ar=g(by),bY=0,b1=[0,F,ap,W,a(e[o],n),b0,bZ],b2=h===ar?by[1]:d===ar?a(f[2],by):by,b3=[0,x([0,a(e[m],[0,b2,b1]),bY]),bX],b4=[0,a(i[25],[0,n,[0,q,0]]),b3],b5=[0,a(j[70][20],b4),bU],b6=aK(F,z),b7=a(i[ae],b6),b8=[0,c(j[70][19],b7,b5),bR],b9=aK(F,W),b_=[0,a(i[ae],b9),0],b$=[0,a(i[25],[0,u,0]),b_],ca=[0,a(i[74],[0,k,[0,u,0]]),b$],cc=a(e[o],u),as=g(bz),cb=0,cd=[0,ao,aq,a(e[o],k),cc],ce=h===as?bz[1]:d===as?a(f[2],bz):bz,cf=[0,x([0,a(e[m],[0,ce,cd]),cb]),ca],cg=[0,a(i[25],[0,k,0]),cf],ch=a(j[70][20],cg),ci=[0,c(j[70][19],ch,b8),bK],cj=a(i[ae],bu);return c(j[70][19],cj,ci);case
1:var
G=b[2],K=b[1],at=c(l[26],K[4],G),ck=c(H[14],at,G),cl=c(H[13],K[4],ck),cm=K[3],cn=function(a){return c(l[9],a,G)},co=c(l[43],cn,cm),aw=[0,K[1],0,co,at],cp=E(Q(aw)),ax=J(G),X=J(cl),cq=aw[3],cr=a(cU(kC),cq),cs=[0,i[60],[0,i[S],0]],ct=[0,R(aJ),cs],cu=[0,a(j[70][20],ct),0],cv=[0,i[60],[0,i[S],0]],cw=[0,R(aJ),cv],cx=[0,a(j[70][20],cw),0],cy=[0,i[41],0],cA=a(U,cr),cB=[0,a(p[67][1],cA),cy],cC=function(a){return bc(k,a)},cD=[0,a(p[67][1],cC),cB],cE=[0,a(i[25],[0,k,0]),cD],cF=[0,R(cN),cE],cG=[0,a(i[74],[0,n,[0,q,0]]),cF],cI=a(e[o],q),ay=g(bC),cH=0,cJ=[0,X,ax,cp,a(e[o],n),cI],cK=h===ay?bC[1]:d===ay?a(f[2],bC):bC,cL=[0,x([0,a(e[m],[0,cK,cJ]),cH]),cG],cM=[0,a(i[25],[0,q,[0,n,0]]),cL],cP=[0,a(j[70][20],cM),cx],cQ=aK(ax,X),cR=a(i[ae],cQ),cV=[0,c(j[70][19],cR,cP),cu],cW=aK(X,z),cX=a(i[ae],cW);return c(j[70][19],cX,cV);case
3:var
az=t[2],aA=b[2],L=b[1],v=N(L[1]),cY=function(a){return c(l[9],a,aA)},Y=c(l[44],cY,L),Z=E(Q(L)),$=E(Q(Y)),O=J(aA),aB=cz(Z,cO($,O));if(2===L[2]){var
cZ=Y[3],c0=a(cT(kD),cZ),c1=i[S],c2=a(U,c0),c3=a(p[67][1],c2),c4=[0,c(j[70][3],c3,c1),0],c5=[0,r(az),0],c6=[0,a(i[25],[0,v,0]),c5],c7=[0,a(i[74],[0,n,[0,v,0]]),c6],c9=a(e[o],v),aC=g(bO),c8=0,c_=[0,Z,$,O,a(e[o],n),c9],c$=h===aC?bO[1]:d===aC?a(f[2],bO):bO,db=[0,x([0,a(e[m],[0,c$,c_]),c8]),c7],dc=[0,a(i[25],[0,n,0]),db],dd=[0,a(j[70][20],dc),c4],dg=a(i[ae],aB);return c(j[70][19],dg,dd)}var
dh=Y[3],di=a(cT(kE),dh),dj=i[S],dk=a(U,di),dm=a(p[67][1],dk),dn=[0,c(j[70][3],dm,dj),0],dp=[0,i[60],[0,i[S],0]],dq=[0,R(aJ),dp],dr=[0,a(j[70][20],dq),0],ds=[0,r(az),0],dt=[0,a(i[25],[0,v,0]),ds],du=[0,a(i[74],[0,n,[0,q,[0,v,0]]]),dt],dw=a(e[o],v),dx=a(e[o],n),aD=g(bB),dv=0,dy=[0,Z,$,O,a(e[o],q),dx,dw],dz=h===aD?bB[1]:d===aD?a(f[2],bB):bB,dA=[0,x([0,a(e[m],[0,dz,dy]),dv]),du],dB=[0,a(i[25],[0,q,[0,n,0]]),dA],dC=[0,a(j[70][20],dB),dr],dD=aK(O,z),dE=a(i[ae],dD),dF=[0,c(j[70][19],dE,dC),dn],dG=a(i[ae],aB);return c(j[70][19],dG,dF);case
4:var
aE=t[2],aF=b[3],B=aF[2],P=aF[1],aG=b[2],y=aG[2],aa=aG[1],dH=b[1],ab=ai(0);aI(ab,dH);var
aH=N(y[1]),aM=N(B[1]),aN=E(Q(y)),aO=E(Q(B));if(c(l[1],aa,l[12]))if(0===B[2]){switch(y[2]){case
0:var
aP=g(bD),dI=h===aP?bD[1]:d===aP?a(f[2],bD):bD,ac=dI;break;case
1:var
aR=g(bE),dU=h===aR?bE[1]:d===aR?a(f[2],bE):bE,ac=dU;break;default:var
aS=g(bQ),dV=h===aS?bQ[1]:d===aS?a(f[2],bQ):bQ,ac=dV}var
dK=J(P),dL=2===y[2]?kF:kG,dM=cS(dL,y[3],P,B[3]),dN=[0,r(aE),0],dO=[0,a(i[25],[0,ab,0]),dN],dP=a(U,dM),dQ=[0,a(p[67][1],dP),dO],dR=a(e[o],aM),dS=[0,ac,[0,aN,aO,dK,a(e[o],aH),dR]],dT=[0,x([0,a(e[m],dS),0]),dQ];return a(j[70][20],dT)}var
aT=J(aa),aU=J(P),dW=eh(kH,aa,y[3],P,B[3]),dX=[0,i[60],[0,i[S],0]],dY=[0,R(aJ),dX],dZ=[0,a(j[70][20],dY),0],d0=[0,i[60],[0,i[S],0]],d1=[0,R(aJ),d0],d2=[0,a(j[70][20],d1),0],d3=[0,r(aE),0],d4=[0,a(i[25],[0,ab,0]),d3],d5=a(U,dW),d6=[0,a(p[67][1],d5),d4],d7=[0,a(i[74],[0,n,[0,q,0]]),d6],d9=a(e[o],aM),d_=a(e[o],aH),d$=a(e[o],q),aV=g(bF),d8=0,ea=[0,aN,aO,aT,aU,a(e[o],n),d$,d_,d9],eb=h===aV?bF[1]:d===aV?a(f[2],bF):bF,ec=[0,x([0,a(e[m],[0,eb,ea]),d8]),d7],ed=[0,a(i[25],[0,q,[0,n,0]]),ec],ee=[0,a(j[70][20],ed),d2],ef=aK(aU,z),eg=a(i[ae],ef),ek=[0,c(j[70][19],eg,ee),dZ],el=aK(aT,z),em=a(i[ae],el);return c(j[70][19],em,ek);case
5:var
D=b[1],aW=D[5],aX=D[4],ad=D[3],aY=D[2],en=t[2],eo=D[1],aZ=ai(0),ep=N(ad[1]);aI(aZ,eo[1]);var
af=E(Q(aY)),eq=E(Q(ad)),ag=da(aW),a0=g(A),er=cz(a(e[bb],1),af),es=h===a0?A[1]:d===a0?a(f[2],A):A,a1=g(A),et=a(e[a$],[0,[0,ag],es,er]),eu=h===a1?A[1]:d===a1?a(f[2],A):A,ev=[0,a(C[60],0),[0,eu,et]],ew=a(e[m],ev),ex=J(aX),a2=g(_),ey=cS(ej,ad[3],aX,[0,[0,l[14],aW],aY[3]]),ez=h===a2?_[1]:d===a2?a(f[2],_):_,eA=[0,kL,[0,kK,[0,kJ,ej]]],eB=[0,function(a){return s(eA,kI,ez,a)},ey],eC=i[S],eD=dJ(af),eE=[0,c(j[70][3],eD,eC),0],eF=[0,r(en),0],eG=[0,a(i[25],[0,aZ,0]),eF],eH=[0,a(i[74],[0,k,0]),eG],eI=a(U,eB),eJ=[0,a(p[67][1],eI),eH],eL=a(e[o],k),eM=a(e[o],ep),a3=g(bH),eK=0,eN=[0,a(e[o],ag),eq,af,ex,eM,eL],eO=h===a3?bH[1]:d===a3?a(f[2],bH):bH,eP=[0,x([0,a(e[m],[0,eO,eN]),eK]),eJ],eQ=[0,a(i[25],[0,ag,[0,k,0]]),eP],eR=[0,a(i[74],[0,k,0]),eQ],eS=[0,aQ(k),eR],eT=[0,a(i[25],[0,k,0]),eS],eU=[0,a(j[70][20],eT),eE],eV=a(i[ae],ew);return c(j[70][19],eV,eU);case
6:var
a4=t[2],eW=b[1];try{var
eX=r(a4),eY=N(eW[1]),eZ=c(I[1][13][3],eY,bp),e0=c(j[70][3],eZ,eX);return e0}catch(a){a=w(a);if(a===M){var
t=a4;continue}throw a}case
9:var
a5=b[2],aj=b[1],e1=Q(aj),e2=Q(a5),a6=g(av),e3=dl(kM,aj[3]),e4=h===a6?av[1]:d===a6?a(f[2],av):av,a7=g(av),e5=h===a7?av[1]:d===a7?a(f[2],av):av,a8=g(au),e6=h===a8?au[1]:d===a8?a(f[2],au):au,e7=a(C[41],0),e8=[0,a(df[46],e7),[0,e6,e5,e4]],e9=a(e[m],e8),e_=[0,i[41],[0,i[S],0]],e$=[0,a(kN[1],e9),0],fa=[0,i[60],[0,i[16],e$]],fb=[0,R(de),fa],fc=a(j[70][20],fb),fd=c(j[70][19],fc,e_),fe=N(a5[1]),ff=a(e[o],fe),fg=N(aj[1]),fh=a(e[o],fg),fi=E(e2),a9=g(bA),fj=[0,E(e1),fi,fh,ff],fk=h===a9?bA[1]:d===a9?a(f[2],bA):bA,fl=a(e[m],[0,fk,fj]),fm=a(U,e3),fn=x([0,fl,0]),fo=a(p[67][8],fn),fp=c(j[5],fo,fm),fq=a(p[67][1],fp);return c(p[15],fq,fd);case
10:var
ak=b[2],al=b[1],fr=b[3],fs=Q(ak),ft=Q(al),fu=N(ak[1]),fv=N(al[1]),a_=fr?l[14]:l[12],fw=cS(kO,ak[3],a_,al[3]),fx=[0,i[S],0],fy=function(a){return bc(k,a)},fz=[0,a(p[67][1],fy),fx],fA=[0,a(i[25],[0,k,0]),fz],fB=a(U,fw),fC=[0,a(p[67][1],fB),fA],fE=a(e[o],fv),fF=a(e[o],fu),fG=J(a_),fH=E(ft),ba=g(bN),fD=0,fI=[0,E(fs),fH,fG,fF,fE],fJ=h===ba?bN[1]:d===ba?a(f[2],bN):bN,fK=[0,x([0,a(e[m],[0,fJ,fI]),fD]),fC];return a(j[70][20],fK);case
11:var
T=b[2],fL=t[2],fM=b[3],fN=b[1],bd=ai(0);aI(bd,fN);var
be=N(T[1]),bf=N(fM),bg=E(Q(T)),bh=E(Q(a(l[45],T))),fO=T[3],bi=g(_),fP=a(cT(kP),fO),fQ=h===bi?_[1]:d===bi?a(f[2],_):_,fR=[0,function(a){return s(kR,kQ,fQ,a)},fP],fS=i[S],fT=a(U,fR),fU=a(p[67][1],fT),fV=[0,c(j[70][3],fU,fS),0],fW=[0,r(fL),0],fX=[0,a(i[25],[0,bd,0]),fW],fY=[0,a(i[74],[0,be,[0,bf,[0,k,0]]]),fX],f0=a(e[o],k),f1=a(e[o],bf),bj=g(bG),fZ=0,f2=[0,bg,bh,a(e[o],be),f1,f0],f3=h===bj?bG[1]:d===bj?a(f[2],bG):bG,f4=[0,x([0,a(e[m],[0,f3,f2]),fZ]),fY],f5=[0,a(i[25],[0,k,0]),f4],f6=[0,a(j[70][20],f5),fV],f7=cz(bg,aL(bh)),f8=a(i[ae],f7);return c(j[70][19],f8,f6);case
12:var
f9=kS[15],f_=N(b[1]),f$=x([0,a(e[o],f_),0]);return c(j[70][3],f$,f9);case
13:var
ga=i[S],gb=N(b[1]),gc=function(a){return bc(gb,a)},gd=a(p[67][1],gc);return c(j[70][3],gd,ga);case
14:var
ge=b[1],gf=[0,i[S],0],gg=function(a){return bc(k,a)},gh=[0,a(p[67][1],gg),gf],gi=[0,a(i[25],[0,k,0]),gh],gj=[0,R(cN),gi],gk=[0,i[60],gj],gl=[0,R(de),gk],gm=N(ge),gn=[0,x([0,a(e[o],gm),0]),gl];return a(j[70][20],gn);case
15:var
bk=b[3],bl=b[2],V=b[1],go=bk[2],gp=bk[1],gq=bl[2],gr=bl[1],bm=ai(0),bn=ai(0);aI(bm,gr);aI(bn,gp);var
gs=N(V[1]),gt=V[3],gu=a(ei(kT),gt),gv=V[3],gw=a(cU(kU),gv),gx=E(Q(V)),gy=[0,r(go),0],gz=[0,a(i[25],[0,bn,0]),gy],gA=a(U,gw),gB=[0,a(p[67][1],gA),gz],gC=[0,a(j[70][20],gB),0],gD=[0,r(gq),0],gE=[0,a(i[25],[0,bm,0]),gD],gF=a(U,gu),gG=[0,a(p[67][1],gF),gE],gH=[0,a(j[70][20],gG),gC],bo=g(bP),gI=[0,gx,[0,a(e[o],gs),0]],gJ=h===bo?bP[1]:d===bo?a(f[2],bP):bP,gK=a(e[59],[0,gJ,gI]),gL=a(i[99],gK);return c(j[70][19],gL,gH)}}return a(p[13],0)}}return r}function
ek(a,f){var
b=an(a,f),g=b[1],d=a7(a,b[2]),h=d[1],e=cW(a,d[2]),i=e[2],j=c(q[18],h,e[1]);return[0,c(q[18],g,j),i]}function
aU(f,v,u,t,s,r,n,d){var
g=d[2],h=d[1],k=ek([0,[0,t],kV],s),l=k[1],w=k[2],y=a(i[74],[0,f,0]),z=a(p[67][8],y),A=a(j[21],z),B=[0,u,[0,r,n,a(e[o],f)]],C=x([0,a(e[m],B),0]),D=a(p[67][8],C),E=c(j[5],D,A);if(a(q[17][47],l))return[0,h,g];var
b=ai(0),F=[0,a(ee(b,v),w),g],G=[0,a(i[25],[0,b,0]),0],H=a(U,l),I=[0,a(p[67][1],H),G],J=[0,a(p[67][1],E),I];return[0,[0,[0,b,a(j[70][20],J)],h],F]}function
el(ab,i,F){var
k=F[1],ac=F[2];if(L.caml_string_equal(a(bg[5],k),kW))return i;try{var
j=aS(ac);if(typeof
j==="number")var
e=0;else
if(1===j[0]){var
p=j[1];if(typeof
p==="number")if(16<=p){switch(p+c4|0){case
0:var
q=j[2];if(q){var
r=q[2];if(r){var
s=r[2];if(s)if(s[2])var
e=0,b=0;else{var
H=s[1],I=r[1],o=cB(c(D[29],ab,q[1]));if(typeof
o==="number")var
n=0;else
if(1===o[0]){var
N=o[1];if(typeof
N==="number")if(23===N)if(o[2])var
n=0;else
var
K=1,n=1;else
var
n=0;else
var
n=0}else
var
n=0;if(!n)var
K=0;if(K)var
M=g(b2),ad=ah(I,aL(H)),ae=2,af=h===M?b2[1]:d===M?a(f[2],b2):b2,m=aU(k,0,af,ae,ad,I,H,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0}else
var
e=0,b=0}else
var
e=0,b=0;break;case
2:var
t=j[2];if(t){var
u=t[2];if(u)if(u[2])var
e=0,b=0;else
var
O=u[1],P=t[1],Q=g(b3),ag=ah(P,aL(O)),ai=1,aj=h===Q?b3[1]:d===Q?a(f[2],b3):b3,m=aU(k,2,aj,ai,ag,P,O,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
3:var
v=j[2];if(v){var
x=v[2];if(x)if(x[2])var
e=0,b=0;else
var
R=x[1],S=v[1],T=g(b7),ak=ah(R,aL(S)),al=2,am=h===T?b7[1]:d===T?a(f[2],b7):b7,m=aU(k,1,am,al,ak,S,R,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
4:var
y=j[2];if(y){var
z=y[2];if(z)if(z[2])var
e=0,b=0;else
var
U=z[1],V=y[1],an=aL(V),W=g(b4),ao=ah(ah(U,J(l[14])),an),ap=2,aq=h===W?b4[1]:d===W?a(f[2],b4):b4,m=aU(k,1,aq,ap,ao,V,U,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
5:var
A=j[2];if(A){var
B=A[2];if(B)if(B[2])var
e=0,b=0;else
var
X=B[1],Y=A[1],Z=g(b5),ar=ah(Y,aL(X)),as=2,at=h===Z?b5[1]:d===Z?a(f[2],b5):b5,m=aU(k,1,at,as,ar,Y,X,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
6:var
C=j[2];if(C){var
E=C[2];if(E)if(E[2])var
e=0,b=0;else
var
_=E[1],$=C[1],au=aL(_),aa=g(b6),av=ah(ah($,J(l[14])),au),aw=2,ax=h===aa?b6[1]:d===aa?a(f[2],b6):b6,m=aU(k,1,ax,aw,av,$,_,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;default:var
e=0,b=0}if(b)var
G=m,e=1}else
var
e=0;else
var
e=0}else
var
e=0;if(!e)var
G=i;return G}catch(b){b=w(b);if(a(aT[4],b))return i;throw b}}function
aV(b){var
d=a(i[22],b),e=a(i[74],[0,b,0]),f=a(j[70][22],e);return c(j[70][3],f,d)}var
kZ=[0,function(k){dc(0);var
u=a(D[48][13],k),x=c(D[48][3],el,k),m=v(q[17][15],x,kX,u),n=m[1],y=m[2],z=dM(0),A=[0,a(p[13],0),0];function
B(n,m){var
c=m[2],o=c[2],k=c[1],p=m[1],q=n[2],r=n[1];if(c[3]){var
b=ai(0),s=bh(0);aI(b,s);var
v=l[11],w=cM(k),x=[0,[0,s,1,[0,[0,l[12],w],0],v],q],y=[0,a(i[25],[0,o,[0,b,0]]),[0,r,0]],z=[0,a(i[74],[0,b,0]),y],A=[0,aQ(b),z],t=g(b9),B=[0,a(i[25],[0,k,[0,b,0]]),A],C=[0,p,0],D=h===t?b9[1]:d===t?a(f[2],b9):b9,E=a(e[59],[0,D,C]),F=[0,a(i[99],E),B];return[0,a(j[70][20],F),x]}var
u=g(b8),G=[0,a(i[25],[0,k,[0,o,0]]),[0,r,0]],H=[0,p,0],I=h===u?b8[1]:d===u?a(f[2],b8):b8,J=a(e[59],[0,I,H]),K=[0,a(i[99],J),G];return[0,a(j[70][20],K),q]}var
o=v(q[17][15],B,A,z),r=o[1],b=c(q[18],y,o[2]);if(cI[1])c(l[33],a1,b);if(be[1])try{v(l[64],[0,bh,c9,a1],0,b);var
F=a(p[13],0);return F}catch(b){b=w(b);if(b===l[27]){var
C=a(l[39],0),s=v(l[65],0,0,C)[2];if(bd[1])c(l[36],a1,s);var
E=a(dn(n),s);return c(j[70][3],r,E)}throw b}try{var
t=c(l[69],[0,bh,c9,a1],b);if(bd[1])c(l[36],a1,t);var
H=a(dn(n),t),I=c(j[70][3],r,H);return I}catch(b){b=w(b);if(b===l[28]){var
G=a(dd[3],kY);return c(j[70][5],0,G)}throw b}}],em=a(p[63][9],kZ),lj=[0,function(b){function
n(a){return c(k0[77],0,a)}var
af=c(D[48][1],n,b);function
k(q,N){try{var
s=cC(N);if(typeof
s==="number")var
n=0;else
if(1===s[0]){var
y=s[1];if(typeof
y==="number")if(12<=y)var
n=0;else{switch(y){case
6:var
z=s[2];if(z){var
A=z[2];if(A)if(A[2])var
n=0,b=0;else
var
B=A[1],C=z[1],ad=[0,k([0,k1,q],B),0],P=g(bk),ae=[0,k([0,k2,q],C),ad],af=[0,C,[0,B,0]],ag=h===P?bk[1]:d===P?a(f[2],bk):bk,aj=aM(B),al=a5(q,ah(aM(C),aj),[0,ag,af]),am=[0,a(p[67][1],al),ae],v=a(j[70][20],am),b=1;else
var
n=0,b=0}else
var
n=0,b=0;break;case
7:var
D=s[2];if(D){var
E=D[2];if(E)if(E[2])var
n=0,b=0;else
var
F=E[1],G=D[1],an=[0,k([0,k3,q],F),0],Q=g(bl),ao=[0,k([0,k4,q],G),an],ap=[0,G,[0,F,0]],aq=h===Q?bl[1]:d===Q?a(f[2],bl):bl,ar=aM(F),as=a5(q,cO(aM(G),ar),[0,aq,ap]),at=[0,a(p[67][1],as),ao],v=a(j[70][20],at),b=1;else
var
n=0,b=0}else
var
n=0,b=0;break;case
8:var
H=s[2];if(H){var
I=H[2];if(I)if(I[2])var
n=0,b=0;else
var
t=I[1],u=H[1],x=ai(0),R=g(aC),au=0,av=0,aw=[0,t,u],ax=h===R?aC[1]:d===R?a(f[2],aC):aC,ay=r([0,[0,x,a(e[m],[0,ax,aw])],av]),S=g(bn),az=[0,u,[0,t,[0,a(e[o],x),0]]],aA=h===S?bn[1]:d===S?a(f[2],bn):bn,aG=a5(q,J(l[11]),[0,aA,az]),aH=a(p[67][1],aG),aI=[0,c(j[70][3],aH,ay),au],aJ=[0,k([0,k5,q],t),0],T=g(aB),aK=[0,k([0,k6,q],u),aJ],aL=0,aN=[0,t,u],aO=h===T?aB[1]:d===T?a(f[2],aB):aB,aP=[0,r([0,[0,x,a(e[m],[0,aO,aN])],aL]),aK],U=g(bm),aQ=[0,u,[0,t,[0,a(e[o],x),0]]],aR=h===U?bm[1]:d===U?a(f[2],bm):bm,aS=aM(t),aU=a5(q,eb(aM(u),aS),[0,aR,aQ]),aV=[0,a(p[67][1],aU),aP],aW=[0,a(j[70][20],aV),aI],V=g(cg),aX=a(i[25],[0,x,0]),aY=[0,t,[0,u,0]],aZ=h===V?cg[1]:d===V?a(f[2],cg):cg,a0=a(e[59],[0,aZ,aY]),a1=a(i[99],a0),a2=c(j[70][3],a1,aX),v=c(j[70][19],a2,aW),b=1;else
var
n=0,b=0}else
var
n=0,b=0;break;case
9:var
L=s[2];if(L)if(L[2])var
n=0,b=0;else
var
W=L[1],X=g(aF),a3=h===X?aF[1]:d===X?a(f[2],aF):aF,Y=g(aE),a4=[0,a3],a6=h===Y?aE[1]:d===Y?a(f[2],aE):aE,Z=g(aD),a7=[0,W,a(e[m],[0,a6,a4])],a8=h===Z?aD[1]:d===Z?a(f[2],aD):aD,_=a(e[m],[0,a8,a7]),$=g(cf),a9=k(q,_),a_=[0,W,0],a$=h===$?cf[1]:d===$?a(f[2],cf):cf,ba=eg([0,k7,q],_,[0,a$,a_]),bb=a(p[67][1],ba),v=c(j[70][3],bb,a9),b=1;else
var
n=0,b=0;break;case
10:var
M=s[2];if(M)if(M[2])var
n=0,b=0;else{var
bc=M[1],aa=function(i){try{var
d=cC(i);if(typeof
d==="number")var
b=0;else
if(1===d[0]){var
e=d[1];if(typeof
e==="number"){if(10===e){var
f=d[2];if(f)if(f[2])var
b=0,c=0;else
var
h=aa(f[1]),c=1;else
var
b=0,c=0}else
if(11===e)if(d[2])var
b=0,c=0;else
var
h=1,c=1;else
var
b=0,c=0;if(c)var
g=h,b=1}else
var
b=0}else
var
b=0;if(!b)var
g=0;return g}catch(b){b=w(b);if(a(aT[4],b))return 0;throw b}},ab=function(i,n){try{var
l=cC(n);if(typeof
l==="number")var
b=0;else
if(1===l[0]){var
s=l[1];if(typeof
s==="number")if(10===s){var
o=l[2];if(o)if(o[2])var
b=0;else
var
q=o[1],t=g(bo),v=ab([0,k8,i],q),x=[0,q,0],y=h===t?bo[1]:d===t?a(f[2],bo):bo,u=g(ak),z=[0,y,x],A=[0,aM(q)],B=h===u?ak[1]:d===u?a(f[2],ak):ak,C=a5(i,a(e[m],[0,B,A]),z),D=a(p[67][1],C),r=c(j[70][3],D,v),b=1;else
var
b=0}else
var
b=0;else
var
b=0}else
var
b=0;if(!b)var
r=k(i,n);return r}catch(b){b=w(b);if(a(aT[4],b))return k(i,n);throw b}};if(aa(bc))var
bd=K(q),ac=a(p[67][1],bd);else
var
ac=ab(q,N);var
v=ac,b=1}else
var
n=0,b=0;break;case
11:if(s[2])var
n=0,b=0;else
var
be=K(q),v=a(p[67][1],be),b=1;break;default:var
n=0,b=0}if(b)var
O=v,n=1}else
var
n=0}else
var
n=0;if(!n)var
O=a(p[13],0);return O}catch(b){b=w(b);if(a(aT[4],b))return a(p[13],0);throw b}}function
r(ag){var
t=ag;for(;;){if(t){var
n=t[2],K=t[1],l=K[1],ah=K[2];try{var
q=aS(ah);if(typeof
q==="number")var
i=0;else
if(1===q[0]){var
u=q[1];if(typeof
u==="number")if(16<=u){switch(u+c4|0){case
0:var
v=q[2];if(v){var
y=v[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else{var
M=z[1],N=y[1],O=g($),ai=v[1],aj=h===O?$[1]:d===O?a(f[2],$):$;if(c(af,ai,aj))var
ak=[0,r(n),0],al=[0,aV(l),ak],am=[0,k(k9,M),al],an=[0,k(k_,N),am],P=g(bu),ao=0,ap=[0,N,M,a(e[o],l)],aq=h===P?bu[1]:d===P?a(f[2],bu):bu,ar=[0,x([0,a(e[m],[0,aq,ap]),ao]),an],Q=a(j[70][20],ar);else
var
Q=r(n);var
s=Q,b=1}else
var
i=0,b=0}else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
A=q[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else
var
R=B[1],S=A[1],as=[0,r(n),0],at=[0,aV(l),as],au=[0,k(k$,R),at],av=[0,k(la,S),au],T=g(bt),aw=0,ax=[0,S,R,a(e[o],l)],ay=h===T?bt[1]:d===T?a(f[2],bt):bt,az=[0,x([0,a(e[m],[0,ay,ax]),aw]),av],s=a(j[70][20],az),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
15:var
C=q[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
U=D[1],V=C[1],aA=[0,r(n),0],aB=[0,aV(l),aA],aC=[0,k(lb,U),aB],aD=[0,k(lc,V),aC],W=g(bp),aE=0,aF=[0,V,U,a(e[o],l)],aG=h===W?bp[1]:d===W?a(f[2],bp):bp,aH=[0,x([0,a(e[m],[0,aG,aF]),aE]),aD],s=a(j[70][20],aH),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
16:var
E=q[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
X=F[1],Y=E[1],aI=[0,r(n),0],aJ=[0,aV(l),aI],aK=[0,k(ld,X),aJ],aL=[0,k(le,Y),aK],Z=g(bq),aM=0,aN=[0,Y,X,a(e[o],l)],aO=h===Z?bq[1]:d===Z?a(f[2],bq):bq,aP=[0,x([0,a(e[m],[0,aO,aN]),aM]),aL],s=a(j[70][20],aP),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
17:var
G=q[2];if(G){var
H=G[2];if(H)if(H[2])var
i=0,b=0;else
var
_=H[1],aa=G[1],aQ=[0,r(n),0],aR=[0,aV(l),aQ],aU=[0,k(lf,_),aR],aW=[0,k(lg,aa),aU],ab=g(br),aX=0,aY=[0,aa,_,a(e[o],l)],aZ=h===ab?br[1]:d===ab?a(f[2],br):br,a0=[0,x([0,a(e[m],[0,aZ,aY]),aX]),aW],s=a(j[70][20],a0),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
18:var
I=q[2];if(I){var
J=I[2];if(J)if(J[2])var
i=0,b=0;else
var
ac=J[1],ad=I[1],a1=[0,r(n),0],a2=[0,aV(l),a1],a3=[0,k(lh,ac),a2],a4=[0,k(li,ad),a3],ae=g(bs),a5=0,a6=[0,ad,ac,a(e[o],l)],a7=h===ae?bs[1]:d===ae?a(f[2],bs):bs,a8=[0,x([0,a(e[m],[0,a7,a6]),a5]),a4],s=a(j[70][20],a8),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;default:var
i=0,b=0}if(b)var
L=s,i=1}else
var
i=0;else
var
i=0}else
var
i=0;if(!i)var
L=r(n);return L}catch(b){b=w(b);if(a(aT[4],b)){var
t=n;continue}throw b}}return a(p[13],0)}}var
s=a(D[48][13],b);return r(a(q[17][6],s))}],en=a(p[63][9],lj);function
eo(a){if(typeof
a==="number")if(18<=a)switch(a+ds|0){case
0:return dS;case
1:return dT;case
2:return dU;case
3:return dW;case
4:return dV;case
13:return d2;case
14:return d3;case
15:return d4;case
16:return d5}throw M}function
ep(a){if(typeof
a==="number")if(18<=a)switch(a+ds|0){case
0:return dX;case
1:return dY;case
2:return dZ;case
3:return d0;case
4:return d1;case
13:return d6;case
14:return d7;case
15:return d8;case
16:return d9}throw M}var
a8=[a0,lk,aY(0)];function
ac(j,Z){var
i=aS(Z);if(typeof
i!=="number")switch(i[0]){case
1:var
l=i[1];if(typeof
l==="number")if(16<=l)switch(l+c4|0){case
0:var
o=i[2];if(o){var
p=o[2];if(p){var
q=p[2];if(q){if(!q[2]){var
C=q[1],E=p[1],k=cB(c(D[29],j,o[1]));if(typeof
k!=="number"&&1===k[0]){var
r=k[1];if(typeof
r==="number")if(23===r){if(!k[2]){var
F=g(b_),_=[0,E,C],$=h===F?b_[1]:d===F?a(f[2],b_):b_;return a(e[m],[0,$,_])}}else
if(24===r)if(!k[2]){var
G=g(ch),aa=[0,E,C],ab=h===G?ch[1]:d===G?a(f[2],ch):ch;return a(e[m],[0,ab,aa])}}throw a8}var
b=1}else
var
b=0}else
var
b=1}else
var
b=1;break;case
9:var
u=i[2];if(u){var
v=u[2];if(v){if(!v[2]){var
I=v[1],J=u[1],ai=ac(j,I),K=g(cl),aj=[0,J,I,ac(j,J),ai],ak=h===K?cl[1]:d===K?a(f[2],cl):cl;return a(e[m],[0,ak,aj])}var
b=1}else
var
b=1}else
var
b=1;break;case
10:var
x=i[2];if(x){var
y=x[2];if(y){if(!y[2]){var
L=y[1],N=x[1],al=ac(j,L),O=g(ck),am=[0,N,L,ac(j,N),al],an=h===O?ck[1]:d===O?a(f[2],ck):ck;return a(e[m],[0,an,am])}var
b=1}else
var
b=1}else
var
b=1;break;case
11:if(!i[2]){var
P=g(cp);return h===P?cp[1]:d===P?a(f[2],cp):cp}var
b=0;break;case
12:if(!i[2]){var
Q=g(cr);return h===Q?cr[1]:d===Q?a(f[2],cr):cr}var
b=0;break;case
13:var
z=i[2];if(z){if(!z[2]){var
R=z[1],S=g(co),ao=[0,R,ac(j,R)],ap=h===S?co[1]:d===S?a(f[2],co):co;return a(e[m],[0,ap,ao])}var
b=0}else
var
b=1;break;case
14:var
A=i[2];if(A){var
B=A[2];if(B){if(!B[2]){var
T=B[1],U=A[1],aq=ac(j,T),V=g(cn),ar=[0,U,T,ac(j,U),aq],as=h===V?cn[1]:d===V?a(f[2],cn):cn;return a(e[m],[0,as,ar])}var
b=1}else
var
b=1}else
var
b=1;break;default:var
b=0}else
var
b=0;else
var
b=0;if(!b){var
s=i[2];if(s){var
t=s[2];if(t)if(!t[2]){var
ad=t[1],ae=s[1];try{var
n=eo(l),H=g(n),af=[0,ae,ad],ag=h===H?n[1]:d===H?a(f[2],n):n,ah=a(e[m],[0,ag,af]);return ah}catch(a){a=w(a);if(a===M)throw a8;throw a}}}}break;case
2:var
W=i[2],X=i[1],at=ac(j,W),Y=g(cm),au=[0,X,W,ac(j,X),at],av=h===Y?cm[1]:d===Y?a(f[2],cm):cm;return a(e[m],[0,av,au])}throw a8}function
ad(b,e){var
d=[0,function(f){var
g=c(i[14],0,b),d=c(D[48][3],g,f),h=a(e,d),k=c(i[2],0,d);return c(j[70][3],k,h)}],f=a(p[63][9],d),g=a(i[74],[0,b,0]),h=a(j[70][22],g);return c(j[70][3],h,f)}function
dp(b,g){var
d=[0,function(d){var
h=c(bg[7],b,ll),k=c(i[14],0,h),e=c(D[48][3],k,d),l=c(bg[7],b,lm),m=c(i[14],0,l),f=c(D[48][3],m,d),n=[0,c(g,e,f),0],o=[0,c(i[2],0,f),n],p=[0,c(i[2],0,e),o];return a(j[70][20],p)}],e=a(p[63][9],d),f=a(i[74],[0,b,0]),h=a(j[70][22],f);return c(j[70][3],h,e)}var
ln=[0,function(b){var
a1=a(D[48][7],b),v=c(D[48][3],ac,b),at=c(D[48][3],D[29],b);function
l(a2){var
z=a2;for(;;){if(z){var
k=z[2],E=z[1],b=a(cX[2][1][1],E);try{var
t=aS(a(cX[2][1][3],E));if(typeof
t==="number")var
n=0;else
switch(t[0]){case
1:var
K=t[1];if(typeof
K==="number")if(18<=K){switch(K+ds|0){case
7:var
L=t[2];if(L){var
N=L[2];if(N)if(N[2])var
n=0,p=0;else
var
a3=N[1],a4=L[1],a5=dp(b,function(c,d,e){return function(b,a){return l([0,[0,b,e],[0,[0,a,d],c]])}}(k,a3,a4)),a6=aQ(b),A=c(j[70][3],a6,a5),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
8:var
O=t[2];if(O){var
P=O[2];if(P)if(P[2])var
n=0,p=0;else
var
a7=P[1],a9=O[1],a_=0,a$=[0,ad(b,function(b,c){return function(a){return l([0,[0,a,c],b])}}(k,a7)),a_],ba=[0,ad(b,function(b,c){return function(a){return l([0,[0,a,c],b])}}(k,a9)),a$],bb=aQ(b),A=c(j[70][19],bb,ba),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
9:if(t[2])var
n=0,p=0;else
var
A=aQ(b),p=1;break;case
11:var
Q=t[2];if(Q)if(Q[2])var
n=0,p=0;else{var
u=aS(Q[1]);if(typeof
u==="number")var
r=0;else
switch(u[0]){case
1:var
F=u[1];if(typeof
F==="number")if(16<=F){switch(F+c4|0){case
0:var
S=u[2];if(S){var
T=S[2];if(T){var
U=T[2];if(U)if(U[2])var
r=0,s=0,q=0;else{var
G=U[1],H=T[1],ax=S[1];if(be[1]){var
V=cB(a(at,ax));if(typeof
V==="number")var
C=0;else
if(1===V[0]){var
W=V[1];if(typeof
W==="number"){if(23===W)var
bc=0,bd=[0,ad(b,function(a){return function(b){return l(a)}}(k)),bc],aD=g(b$),bf=[0,H,G,a(e[o],b)],bg=h===aD?b$[1]:d===aD?a(f[2],b$):b$,bh=a(e[m],[0,bg,bf]),bi=[0,a(i[99],bh),bd],aE=a(j[70][20],bi),aq=1;else
if(24===W)var
bj=0,bk=[0,ad(b,function(a){return function(b){return l(a)}}(k)),bj],aF=g(ci),bl=[0,H,G,a(e[o],b)],bm=h===aF?ci[1]:d===aF?a(f[2],ci):ci,bn=a(e[m],[0,bm,bl]),bo=[0,a(i[99],bn),bk],aE=a(j[70][20],bo),aq=1;else
var
C=0,aq=0;if(aq)var
ay=aE,C=1}else
var
C=0}else
var
C=0;if(!C)var
ay=l(k);var
aB=ay}else{var
X=cB(a(at,ax));if(typeof
X==="number")var
D=0;else
if(1===X[0]){var
Y=X[1];if(typeof
Y==="number"){if(23===Y)var
aI=g(aA),bp=l(k),bq=[0,H,G],br=h===aI?aA[1]:d===aI?a(f[2],aA):aA,bs=a(e[m],[0,br,bq]),bt=c(cX[2][1][5],bs,E),bu=a(i[6],bt),aJ=c(j[70][3],bu,bp),ar=1;else
if(24===Y)var
aK=g(az),bv=l(k),bw=[0,H,G],bx=h===aK?az[1]:d===aK?a(f[2],az):az,by=a(e[m],[0,bx,bw]),bz=c(cX[2][1][5],by,E),bA=a(i[6],bz),aJ=c(j[70][3],bA,bv),ar=1;else
var
D=0,ar=0;if(ar)var
aG=aJ,D=1}else
var
D=0}else
var
D=0;if(!D)var
aG=l(k);var
aB=aG}var
B=aB,q=1}else
var
s=1,q=0}else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
9:var
$=u[2];if($){var
aa=$[2];if(aa)if(aa[2])var
r=0,s=0,q=0;else
var
aN=aa[1],ab=$[1],bK=a(v,ab),bL=0,bM=[0,ad(b,function(c,d,e){return function(a){var
b=aH(d);return l([0,[0,a,cP(aH(e),b)],c])}}(k,aN,ab)),bL],aO=g(ct),bN=0,bO=[0,ab,aN,bK,a(e[o],b)],bP=h===aO?ct[1]:d===aO?a(f[2],ct):ct,bQ=[0,x([0,a(e[m],[0,bP,bO]),bN]),bM],B=a(j[70][20],bQ),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
10:var
ac=u[2];if(ac){var
ae=ac[2];if(ae)if(ae[2])var
r=0,s=0,q=0;else
var
aP=ae[1],aR=ac[1],bR=0,bS=[0,ad(b,function(c,d,e){return function(a){var
b=aH(d);return l([0,[0,a,cA(aH(e),b)],c])}}(k,aP,aR)),bR],aU=g(cs),bT=0,bU=[0,aR,aP,a(e[o],b)],bV=h===aU?cs[1]:d===aU?a(f[2],cs):cs,bW=[0,x([0,a(e[m],[0,bV,bU]),bT]),bS],B=a(j[70][20],bW),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
13:var
af=u[2];if(af)if(af[2])var
s=1,q=0;else
var
ag=af[1],bX=a(v,ag),bY=0,bZ=[0,ad(b,function(b,c){return function(a){return l([0,[0,a,c],b])}}(k,ag)),bY],aV=g(cw),b0=0,b1=[0,ag,bX,a(e[o],b)],b2=h===aV?cw[1]:d===aV?a(f[2],cw):cw,b3=[0,x([0,a(e[m],[0,b2,b1]),b0]),bZ],B=a(j[70][20],b3),q=1;else
var
r=0,s=0,q=0;break;case
14:var
ah=u[2];if(ah){var
ai=ah[2];if(ai)if(ai[2])var
r=0,s=0,q=0;else
var
aj=ai[1],ak=ah[1],b4=a(v,ak),b5=a(v,aj),b6=0,b7=[0,ad(b,function(e,a,b){return function(c){var
d=cA(aH(b),a);return l([0,[0,c,cP(cA(b,aH(a)),d)],e])}}(k,aj,ak)),b6],aW=g(cv),b8=0,b9=[0,ak,aj,b4,b5,a(e[o],b)],b_=h===aW?cv[1]:d===aW?a(f[2],cv):cv,ca=[0,x([0,a(e[m],[0,b_,b9]),b8]),b7],B=a(j[70][20],ca),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;default:var
s=1,q=0}if(q)var
aC=B,s=2}else
var
s=1;else
var
s=1;switch(s){case
0:var
y=0;break;case
1:var
Z=u[2];if(Z){var
_=Z[2];if(_)if(_[2])var
r=0,y=0;else{var
bB=_[1],bC=Z[1];try{var
I=ep(F),bD=0,bE=[0,ad(b,function(a){return function(b){return l(a)}}(k)),bD],aM=g(I),bF=0,bG=[0,bC,bB,a(e[o],b)],bH=h===aM?I[1]:d===aM?a(f[2],I):I,bI=[0,x([0,a(e[m],[0,bH,bG]),bF]),bE],bJ=a(j[70][20],bI),aL=bJ}catch(a){a=w(a);if(a!==M)throw a;var
aL=l(k)}var
aC=aL,y=1}else
var
r=0,y=0}else
var
r=0,y=0;break;default:var
y=1}if(y)var
R=aC,r=1;break;case
2:var
aX=u[2],al=u[1],cb=a(v,al),cc=0,cd=[0,ad(b,function(b,c,d){return function(a){return l([0,[0,a,cA(d,aH(c))],b])}}(k,aX,al)),cc],aY=g(cu),ce=0,cf=[0,al,aX,cb,a(e[o],b)],cg=h===aY?cu[1]:d===aY?a(f[2],cu):cu,ch=[0,x([0,a(e[m],[0,cg,cf]),ce]),cd],R=a(j[70][20],ch),r=1;break;default:var
r=0}if(!r)var
R=l(k);var
A=R,p=1}else
var
n=0,p=0;break;case
12:var
am=t[2];if(am){var
an=am[2];if(an)if(an[2])var
n=0,p=0;else
var
cj=an[1],ck=am[1],cl=dp(b,function(h,a,b){return function(f,d){var
g=[0,[0,d,c(e[49],a,b)],h];return l([0,[0,f,c(e[49],b,a)],g])}}(k,cj,ck)),cm=aQ(b),A=c(j[70][3],cm,cl),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
0:case
1:case
2:case
3:case
4:var
au=t[2];if(au){var
av=au[2];if(av)if(av[2])var
n=0,p=0;else
var
aw=l(k),p=2;else
var
n=0,p=0}else
var
n=0,p=0;break;default:var
n=0,p=0}switch(p){case
0:var
as=0;break;case
1:var
aw=A,as=1;break;default:var
as=1}if(as)var
J=aw,n=1}else
var
n=0;else
var
n=0;break;case
2:var
ao=t[2],ap=t[1],cn=a(a1,ao);if(a(e[22],cn))var
co=a(v,ap),cp=0,cq=[0,ad(b,function(b,c,d){return function(a){return l([0,[0,a,cP(aH(d),c)],b])}}(k,ao,ap)),cp],aZ=g(cx),cr=0,cy=[0,ap,ao,co,a(e[o],b)],cz=h===aZ?cx[1]:d===aZ?a(f[2],cx):cx,cC=[0,x([0,a(e[m],[0,cz,cy]),cr]),cq],a0=a(j[70][20],cC);else
var
a0=l(k);var
J=a0,n=1;break;default:var
n=0}if(!n)var
J=l(k);return J}catch(b){b=w(b);if(b===a8){var
z=k;continue}if(a(aT[4],b)){var
z=k;continue}throw b}}return c(j[70][3],en,em)}}return l(a(p[63][4],b))}],cY=a(p[63][9],ln),lo=[0,function(b){var
k=a(p[63][3],b),u=c(D[48][3],ac,b);function
l(k){var
b=aS(k);if(typeof
b!=="number")switch(b[0]){case
1:var
r=b[1];if(typeof
r==="number"){var
s=r-27|0;if(!(2<s>>>0))switch(s){case
0:if(!b[2])return cY;break;case
1:break;default:var
t=b[2];if(t)if(!t[2]){var
H=i[16],I=R(cN),J=c(j[70][3],I,H);return c(j[70][3],J,cY)}}}break;case
2:var
K=l(b[2]);return c(j[70][3],i[16],K)}try{var
x=a(u,k),y=i[16],q=g(cq),z=[0,k,x,cQ(0)],A=h===q?cq[1]:d===q?a(f[2],cq):cq,B=a(e[m],[0,A,z]),E=a(D[45],B),F=a(p[67][1],E),G=c(j[70][3],F,y),n=G}catch(b){b=w(b);if(b!==a8)throw b;var
v=a(C[50],0),n=a(i[o],v)}return c(j[70][3],n,cY)}return l(k)}],eq=a(p[63][9],lo);function
lp(b){a(C[11],lq);dP(0);return eq}var
lr=a(p[13],0),dq=[0,fV,l,aQ,bc,fW,fX,cI,bd,be,cJ,fY,bf,aR,dz,ai,ge,dD,bh,c9,dH,a1,cM,da,dI,U,dJ,x,gk,db,R,dK,aI,gm,N,dL,dO,dN,dc,dM,dP,dQ,dR,ab,k,af,T,ar,as,at,V,W,X,A,au,av,B,O,F,aj,ak,a2,aw,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,G,bv,bw,ax,ay,bx,by,bz,bA,bB,bC,bD,bE,bF,bG,bH,bI,Y,P,bJ,bK,bL,bM,bN,bO,bP,bQ,bR,bS,bT,bU,bV,Z,bW,bX,bY,bZ,b0,_,b1,b2,b3,b4,b5,b6,b7,b8,b9,b_,dS,dT,dU,dV,dW,b$,dX,dY,dZ,d0,d1,az,aA,al,am,a3,a4,aB,ca,cb,aC,aD,cc,cd,ce,$,aE,aF,cf,cg,ch,d2,d3,d4,d5,ci,d6,d7,d8,d9,cj,ck,cl,cm,cn,co,cp,cq,cr,cs,ct,cu,cv,cw,cx,cy,aG,d_,d$,ea,de,aJ,iP,iR,cN,iT,ah,cO,eb,cz,iU,aK,aL,cA,cP,aH,iV,aM,J,aS,cB,cC,ec,di,ed,dj,K,aN,aa,E,ee,Q,cQ,dk,cR,ef,a5,eg,s,a6,eh,cS,dl,cD,cT,ei,cU,cV,an,dm,cE,a7,cW,dn,ek,aU,el,aV,em,en,eo,ep,a8,ac,ad,dp,cY,eq,c(p[68][1],lr,lp)];c3(434,dq,"Omega_plugin.Coq_omega");a(dr[12],ls);function
cF(b){var
d=c(r[13],I[1][6],lt),e=a(I[5][4],d),f=a(I[6][4],b),g=c(I[13][2],[0,e],f),h=a(aX[6][6],g);return a(aX[12][17],h)}function
cZ(b){var
d=c(q[17][96],lu[29],b);function
e(b){if(cH(b,lv)){if(cH(b,lw)){if(cH(b,lx)){if(cH(b,ly)){var
d=c(n[16],lz,b);return a(ag[7],d)}return cF(lA)}return cF(lB)}return cF(lC)}return cF(lD)}var
f=c(r[13],e,d),g=dq[251],h=a(j[70][20],f),i=a(j[70][29],h);return c(j[70][3],i,g)}function
lE(d){var
b=[28,[0,0,[31,er[4],[0,[0,aW,lF],0],0]]],c=a(I[1][6],lG);return cG(aX[6][4],1,0,c,b)}var
lH=[0,function(b,a){return cZ(0)}];v(aX[6][9],0,[0,aW,lI],lH);c(dr[19],lE,aW);var
lJ=0,lM=[0,function(b){return b?a(n[2],lK):function(a){return cZ(lL)}},lJ],lO=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(et[17],es[9]),f=a(et[6],e),g=c(aX[12][2][7],f,d);return function(a){return cZ(c(r[13],I[1][8],g))}}return a(n[2],lN)},lM],lQ=a(lP[12],lO);v(aX[6][9],0,[0,aW,lR],lQ);function
lS(e){var
d=a(I[1][7],lU),b=es[9],c=0;if(0===b[0])return v(aX[9][4],[0,aW,lY],0,[0,[0,lX,[0,lW,[0,[1,er[4],[0,[5,[0,b[1]]]],d],c]]],lT]);throw[0,dy,lV]}c(dr[19],lS,aW);var
eu=[0,aW,cF,cZ];c3(442,eu,"Omega_plugin.G_omega");c3(443,[0,c7,dq,eu],"Omega_plugin");return});
