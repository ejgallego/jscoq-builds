function(ma){"use strict";var
eJ="not",eK="*",eW=" 0\n",e0="Z.lt",e5="Z.le",dc="Equation E",db=" and E",dA=": ",bc=".\n",g=250,eI="Z.succ",eQ="(",eU="Z.pred",eV="not a number",eT="ZArith",eH="+ ",eN="- ",eD="------------------------\n\n",c=246,eG="Z.sub",eC="tag_hypothesis",aY="omega",eM="Inequation E",dB="Z",eS=")",dC="plugins/omega/coq_omega.ml",eZ="N",e4="State",N=122,eP="with",ab=143,eY="X%d",eL=" subsumes E",dE=" E",af="Omega",eX=" states ",aZ=248,eF="positive",dz=-18,eE="Equations E",dD="nat",e3="Omega: Can't solve a goal with non-linear products",da=-16,eR="Z.ge",e2=107,eO="Z.gt",bb="Coq",e1="E%d subsumes E%d.\n",D=ma.jsoo_runtime,ao=D.caml_check_bound,eA=D.caml_equal,aX=D.caml_fresh_oo_id,b=D.caml_new_string,c7=D.caml_notequal,f=D.caml_obj_tag,c_=D.caml_register_global,c$=D.caml_string_notequal,c9=D.caml_trampoline,c8=D.caml_trampoline_return,t=D.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):D.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):D.caml_call_gen(a,[b,c])}function
n(a,b,c,d){return a.length==3?a(b,c,d):D.caml_call_gen(a,[b,c,d])}function
c6(a,b,c,d,e){return a.length==4?a(b,c,d,e):D.caml_call_gen(a,[b,c,d,e])}function
eB(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):D.caml_call_gen(a,[b,c,d,e,f])}function
l_(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):D.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
l$(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):D.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}var
s=D.caml_get_global_data(),l9=[11,b(" + "),[2,0,[12,32,[2,0,[11,b(dE),[4,0,0,0,[11,b(bc),0]]]]]]],er=[0,[0,2],[0,1,0]],dW=[0,b(bb),[0,b("Logic"),[0,b("Decidable"),0]]],c3=b("omega_plugin"),p=s.Stdlib__list,w=s.Stdlib__printf,m=s.Stdlib,o=s.Util,I=s.Not_found,ap=s.Int,v=s.Stdlib__hashtbl,cJ=s.Assert_failure,A=s.Names,e=s.CamlinternalLazy,h=s.EConstr,E=s.Bigint,am=s.Pp,ad=s.CErrors,aQ=s.Logic,a3=s.Coqlib,i=s.Tactics,j=s.Tacticals,q=s.Proofview,c1=s.Context,a1=s.Nameops,H=s.Tacmach,dp=s.Reductionops,em=s.Refine,dm=s.Nametab,dn=s.Libnames,dj=s.Tacred,bg=s.Goptions,c4=s.Ltac_plugin,aR=[0,0],fp=[0,[11,b(eM),[4,0,0,0,[11,b(" is divided by "),[2,0,[11,b(" and the constant coefficient is rounded by subtracting "),[2,0,[11,b(bc),0]]]]]]],b("Inequation E%d is divided by %s and the constant coefficient is rounded by subtracting %s.\n")],fq=[0,[11,b("Constant in equation E"),[4,0,0,0,[11,b(" is not divisible by the pgcd "),[2,0,[11,b(" of its other coefficients.\n"),0]]]]],b("Constant in equation E%d is not divisible by the pgcd %s of its other coefficients.\n")],fr=[0,[12,69,[4,0,0,0,[11,b(" is trivially satisfiable.\n"),0]]],b("E%d is trivially satisfiable.\n")],fs=[0,[11,b(dc),[4,0,0,0,[11,b(" is divided by the pgcd "),[2,0,[11,b(" of its coefficients.\n"),0]]]]],b("Equation E%d is divided by the pgcd %s of its coefficients.\n")],ft=[0,[11,b("We state "),[2,0,[11,b(dE),[4,0,0,0,[11,b(" = "),[2,0,[12,32,[2,0,[11,b(dE),[4,0,0,0,l9]]]]]]]]]],b("We state %s E%d = %s %s E%d + %s %s E%d.\n")],fu=[0,[11,b("We define a new equation E"),[4,0,0,0,[11,b(dA),0]]],b("We define a new equation E%d: ")],fv=b(" 0"),fw=[0,[11,b("We define E"),[4,0,0,0,[11,b(dA),0]]],b("We define E%d: ")],fx=b(eW),fy=[0,[12,69,[4,0,0,0,[11,b(eL),[4,0,0,0,[11,b(bc),0]]]]],b(e1)],fz=[0,[12,69,[4,0,0,0,[11,b(eL),[4,0,0,0,[11,b(bc),0]]]]],b(e1)],fA=[0,[11,b(eE),[4,0,0,0,[11,b(db),[4,0,0,0,[11,b(" imply a contradiction on their constant factors.\n"),0]]]]],b("Equations E%d and E%d imply a contradiction on their constant factors.\n")],fB=[0,[11,b(eE),[4,0,0,0,[11,b(db),[4,0,0,0,[11,b(" state that their body is at the same time equal and different\n"),0]]]]],b("Equations E%d and E%d state that their body is at the same time equal and different\n")],fC=[0,[12,69,[4,0,0,0,[11,b(db),[4,0,0,0,[11,b(" can be merged into E"),[4,0,0,0,[11,b(bc),0]]]]]]],b("E%d and E%d can be merged into E%d.\n")],fD=[0,[11,b(dc),[4,0,0,0,[11,b(eX),[2,0,[11,b(" = 0.\n"),0]]]]],b("Equation E%d states %s = 0.\n")],fE=[0,[11,b(eM),[4,0,0,0,[11,b(" states 0 != 0.\n"),0]]],b("Inequation E%d states 0 != 0.\n")],fF=[0,[11,b(dc),[4,0,0,0,[11,b(eX),[2,0,[11,b(" >= 0.\n"),0]]]]],b("Equation E%d states %s >= 0.\n")],fG=[0,[11,b(dc),[4,0,0,0,[11,b(" is split in E"),[4,0,0,0,[11,b(db),[4,0,0,0,[11,b("\n\n"),0]]]]]]],b("Equation E%d is split in E%d and E%d\n\n")],fH=[0,[11,b("To ensure a solution in the dark shadow the equation E"),[4,0,0,0,[11,b(" is weakened by "),[2,0,[11,b(bc),0]]]]],b("To ensure a solution in the dark shadow the equation E%d is weakened by %s.\n")],fR=b("depend"),fU=b("solve"),fS=[0,b("plugins/omega/omega.ml"),602,15],fQ=b("disequation in simplify"),fP=b("Product dardk"),fO=[0,0,0,0],fM=b("TL"),fL=b("eliminate_with_in"),fI=[0,[12,88,[4,0,0,0,0]],b(eY)],fn=b(">= 0\n"),fo=b(eD),fk=[0,[12,69,[4,0,0,0,[11,b(dA),0]]],b("E%d: ")],fl=[0,[2,0,[11,b(eW),0]],b("%s 0\n")],fm=b(eD),fh=b("equation"),fi=b("inequation"),fj=b("disequation"),fe=b("="),ff=b(">="),fg=b("!="),e9=b(eN),fa=b(eH),fb=b(""),e_=[0,[2,0,[12,32,0]],b("%s ")],e$=[0,[2,0,[12,32,[2,0,[12,32,0]]]],b("%s %s ")],fc=[0,[11,b(eH),[2,0,[12,32,0]]],b("+ %s ")],fd=[0,[11,b(eN),[2,0,[12,32,0]]],b("- %s ")],e6=b("pgcd_l"),e7=b("Omega.MakeOmegaSolver(I).UNSOLVABLE"),e8=b("Omega.MakeOmegaSolver(I).NO_CONTRADICTION"),fJ=b("Omega.MakeOmegaSolver(I).FACTOR1"),fK=b("Omega.MakeOmegaSolver(I).CHOPVAR"),fN=b("Omega.MakeOmegaSolver(I).SOLVED_SYSTEM"),fT=b("Omega.MakeOmegaSolver(I).FULL_SOLUTION"),jc=b(eQ),jd=b("+"),je=b(eS),jf=b("~"),jg=b(eQ),jh=b(eK),ji=b(eS),jj=b("?"),jk=b("weight"),jw=[0,2],jx=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jy=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],js=[0,2],jt=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],ju=[0,2],jv=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jz=[0,2],jA=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jB=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],j2=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],[0,[0,[0,1],[0,[0,1],[0,[0,2],[0,[0,1],0]]]],0]]]],j3=[0,1],j4=[0,2],j5=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j6=[0,2],j7=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],j8=b(e3),j9=[0,2],j_=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],kf=[0,1],kg=[0,2],kh=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],ki=[0,[0,[0,1],[0,[0,1],0]],0],kj=b(e3),kk=[0,2],kl=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],km=[0,[0,[0,1],0],0],kn=[0,1],ko=[0,2],kp=[0,1],kq=[0,2],kr=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],ks=[0,1],kJ=[0,0,0],kG=[0,1],kH=[0,2],kC=[0,1],kD=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kE=[0,1],kF=[0,2],kI=[0,1],kL=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],kK=[0,2],lF=[0,b(bb),[0,b(aY),[0,b(af),0]]],lC=b("_eqn"),ly=b("_left"),lz=b("_right"),lk=[0,1],ld=[0,2],le=[0,1],lf=[0,2],lg=[0,1],lh=[0,2],li=[0,1],lj=[0,1],ll=[0,[0,3],[0,1,0]],lm=[0,[0,2],[0,1,0]],ln=[0,[0,2],[0,1,0]],lo=[0,[0,1],[0,1,0]],lp=[0,[0,2],[0,1,0]],lq=[0,[0,1],[0,1,0]],lr=[0,[0,2],[0,1,0]],ls=[0,[0,1],[0,1,0]],lt=[0,[0,2],[0,1,0]],lu=[0,[0,1],[0,1,0]],lv=[0,[0,2],[0,1,0]],lw=[0,[0,1],[0,1,0]],la=[0,0,0],lb=b("Omega can't solve this system"),k_=b(e4),k9=[0,1,0],kP=[0,[0,3],0],kQ=[0,[0,2],0],kR=[0,[0,3],0],kS=[0,[0,3],0],kT=[0,[0,1],[0,1,0]],kU=[0,[0,2],[0,1,0]],kV=[0,[0,2],[0,1,0]],kW=[0,[0,[0,1],0],0],kX=[0,2],kY=[0,1],kZ=[0,1],k0=[0,[0,2],[0,1,0]],k2=[0,[0,1],[0,1,0]],k3=[0,[0,3],0],k4=[0,[0,[0,1],0],0],k5=[0,[0,3],0],k7=[0,[0,2],[0,1,0]],k8=[0,[0,2],[0,1,0]],kM=b("auxiliary"),kN=b("auxiliary_1"),kO=b("auxiliary_2"),kz=b("condense.1"),kA=[0,2],kB=[0,0,0],ky=b("reduce_factor.1"),ku=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kv=[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]],kw=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,2],0]],0]],kx=[0,[0,[0,1],0],0],kt=b("shrink.1"),kd=[0,2],ke=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],kb=[0,2],kc=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j$=[0,2],ka=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]]],jP=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]],jQ=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jR=[0,1],jS=[0,2],jT=[0,2],jU=[0,2],jV=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jW=[0,2],jX=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jY=[0,2],jZ=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j0=[0,2],j1=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jC=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]]],jD=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jE=[0,1],jF=[0,2],jG=[0,2],jH=[0,2],jI=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jJ=[0,2],jK=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jL=[0,2],jM=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jN=[0,2],jO=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jr=[0,b(dC),743,17],jq=[0,b(dC),744,13],jp=[0,b(dC),703,9],jn=b("H"),jo=b("P"),jl=b("compile_equation."),jb=b("x"),ja=b("occurrence "),i$=b("abstract_path "),i9=b(eV),i_=b(eV),i8=b("Omega: Not a quantifier-free goal"),i4=b(eJ),i2=b(e0),i0=b(eR),iZ=b(eO),iY=b(e5),iX=b(eG),iW=b(eU),iV=b(eI),iT=b(" is not an evaluable constant."),iU=[0,b("Coq_omega")],iQ=b("True"),iP=b("False"),iO=b("ex"),iN=b("eq"),iM=b("or"),iL=b("and"),iK=b(eJ),iJ=b("iff"),iI=b("imp_simp"),iH=b("not_not"),iG=b("not_iff"),iF=b("not_imp"),iE=b("not_and"),iD=b("not_or"),iC=b("dec_True"),iB=b("dec_not_not"),iA=b("dec_False"),iz=b("dec_not"),iy=b("dec_iff"),ix=b("dec_imp"),iw=b("dec_and"),iv=b("dec_or"),iu=b("eq_ind_r"),it=b("not_gt"),is=b("not_ge"),ir=b("not_lt"),iq=b("not_le"),ip=b("not_eq"),io=b("dec_gt"),im=b("dec_ge"),il=b("dec_lt"),ik=b("dec_le"),ij=b("dec_eq_nat"),ii=b("le_gt_dec"),ih=b("pred_of_minus"),ig=b("O"),ie=b("S"),id=b(dD),ic=b("Nat.pred"),ib=b("Nat.mul"),ia=b("Nat.add"),h$=b("Nat.sub"),h_=b("gt"),h9=b("ge"),h8=b("lt"),h7=b("le"),h6=b(e0),h5=b(eR),h4=b(eO),h3=b(e5),h2=b("Zne"),h1=b("neq"),h0=b("Znot_gt_le"),hZ=b("Znot_ge_lt"),hY=b("Znot_lt_ge"),hX=b("Znot_le_gt"),hW=b("not_Zne"),hV=b("not_Zeq"),hU=b("dec_Zge"),hT=b("dec_Zgt"),hS=b("Z.lt_decidable"),hR=b("Z.le_decidable"),hQ=b("dec_Zne"),hP=b("Z.eq_decidable"),hO=b("intro_Z"),hN=b("new_var"),hM=b("Zle_left"),hL=b("Zgt_left"),hK=b("Zge_left"),hJ=b("Zlt_left"),hI=b("Zne_left"),hH=b("Zegal_left"),hG=b("fast_Zopp_involutive"),hF=b("fast_Zopp_eq_mult_neg_1"),hE=b("fast_Zopp_mult_distr_r"),hD=b("fast_Zopp_plus_distr"),hC=b("fast_Zmult_opp_comm"),hB=b("fast_Zmult_plus_distr_l"),hA=b("fast_Zred_factor6"),hz=b("fast_Zred_factor5"),hy=b("fast_Zred_factor4"),hx=b("fast_Zred_factor3"),hw=b("fast_Zred_factor2"),hv=b("fast_Zred_factor1"),hu=b("fast_Zred_factor0"),ht=b("OMEGA20"),hs=b("OMEGA19"),hr=b("OMEGA18"),hq=b("OMEGA17"),hp=b("fast_OMEGA16"),ho=b("fast_OMEGA15"),hn=b("fast_OMEGA14"),hm=b("fast_OMEGA13"),hl=b("fast_OMEGA12"),hk=b("fast_OMEGA11"),hj=b("fast_OMEGA10"),hi=b("OMEGA9"),hh=b("OMEGA8"),hg=b("OMEGA7"),hf=b("OMEGA6"),he=b("OMEGA5"),hd=b("OMEGA4"),hc=b("OMEGA3"),hb=b("OMEGA2"),ha=b("OMEGA1"),g$=b("Zmult_le_approx"),g_=b("fast_Zmult_comm"),g9=b("fast_Zplus_comm"),g8=b("fast_Zplus_permute"),g7=b("fast_Zmult_assoc_reverse"),g6=b("fast_Zplus_assoc"),g5=b("fast_Zplus_assoc_reverse"),g4=b("inj_eq"),g3=b("inj_neq"),g2=b("Znat.inj_gt"),g1=b("Znat.inj_ge"),g0=b("Znat.inj_lt"),gZ=b("Znat.inj_le"),gY=b("Nat2Z.inj_succ"),gX=b("inj_minus2"),gW=b("Nat2Z.inj_sub"),gV=b("Nat2Z.inj_mul"),gU=b("Nat2Z.inj_add"),gT=b("Z.of_nat"),gS=b(eU),gR=b(eI),gQ=b(eG),gP=b("Z.opp"),gO=b("Z.mul"),gN=b("Z.add"),gM=b("Gt"),gL=b("comparison"),gK=b(dB),gJ=b("Zneg"),gI=b("Zpos"),gH=b("Z0"),gG=b("xI"),gF=b("xO"),gE=b("xH"),gr=b("find_contr"),gq=b(eC),go=b(eC),gm=[0,1],gl=[0,[12,88,[4,0,0,0,0]],b(eY)],gk=b("WW"),gj=b("Zvar"),gi=b(e4),gg=b(af),fZ=[0,b(af),[0,b("System"),0]],f0=b("Omega system time displaying flag"),f3=[0,b(af),[0,b("Action"),0]],f4=b("Omega action display flag"),f7=[0,b(af),[0,b("OldStyle"),0]],f8=b("Omega old style flag"),f$=[0,b("Stable"),[0,b(af),0]],ga=b("Omega automatic reset of generated names"),gd=[0,b(af),[0,b("UseLocalDefs"),0]],ge=b("Omega takes advantage of context variables with body"),gs=[0,[0,b(bb),[0,b(aY),[0,b("OmegaLemmas"),0]]],0],gy=b(af),gz=b(af),gA=[0,[0,b(bb),[0,b(eT),0]],0],gB=b(af),gC=[0,[0,b(bb),[0,b(eT),[0,b("BinInt"),0]]],0],gD=b(af),lx=b("Coq_omega.Undecidable"),lY=[0,b(dD),[0,b(eF),[0,b(eZ),[0,b(dB),0]]]],lK=b(eZ),lL=b(dB),lM=b(dD),lN=b(eF),lP=b("zify_positive"),lQ=b("zify_nat"),lR=b("zify_op"),lS=b("zify_N"),lO=b("No Omega knowledge base for type "),lI=[0,b("PreOmega"),[0,b(aY),[0,b(bb),0]]],lU=[0,b(aY),0],lW=b(aY),lZ=[0,b(aY),[0,b(eP),[0,b(eK),0]]],l2=b("l"),l5=b(eP),l6=b(aY),l8=b("omega'"),lB=s.Termops,k1=s.Contradiction,k6=s.Equality,jm=s.Evarutil,iR=s.Global,iS=s.Evd,gw=s.UnivGen,lJ=s.Stdlib__string,lH=s.Mltop,l3=s.Stdarg,l4=s.Genarg,dd=[0,aR,function(f){var
j=f[1],s=f[2];function
aq(b,a){var
c=d(f[2],b,a),e=c||eA(b,a);return e}function
A(b,a){return d(f[2],a,b)}function
M(b,a){var
c=d(f[2],a,b),e=c||eA(b,a);return e}var
h=f[3],k=f[4],i=f[5];function
x(b,a){return d(f[6],b,a)[1]}function
N(b,a){return d(f[6],b,a)[2]}var
c=f[8],e=f[9],y=d(h,e,e),ad=a(f[7],e);function
l(b){return d(f[2],b,c)?a(f[7],b):b}var
g=f[10],u=f[7];function
ar(b,a){return b<a?1:0}function
as(b,a){return a<b?1:0}function
at(b,a){return b<=a?1:0}function
au(b,a){return a<=b?1:0}function
av(b){a(m[33],b);a(m[36],0);return a(m[52],m[28])}function
E(b,a){a[1]=[0,b,a[1]];return 0}function
ae(f,e){var
b=f,a=e;for(;;){if(d(j,a,c))return b;var
g=N(b,a),b=a,a=g;continue}}function
af(b){return b?n(p[20],ae,b[1],b[2]):a(m[3],e6)}function
F(b,a){var
g=M(b,c),f=A(a,c);return 0===g?0===f?x(b,a):d(k,x(d(h,b,e),a),e):0===f?d(k,x(d(k,b,e),a),e):x(b,a)}var
q=[aZ,e7,aX(0)],ag=[aZ,e8,aX(0)];function
G(h,f){var
b=f[2],k=f[1],o=0;function
q(i,b){var
k=d(s,b[1],c)?e9:i?fa:fb;a(m[31],k);var
f=l(b[1]);if(d(j,f,e)){var
o=a(h,b[2]);d(w[2],e_,o)}else{var
p=a(h,b[2]),q=a(g,f);n(w[2],e$,q,p)}return 1}n(p[20],q,o,k);if(A(b,c)){var
r=a(g,b);return d(w[2],fc,r)}var
i=d(s,b,c);if(i){var
t=a(g,l(b));return d(w[2],fd,t)}return i}function
W(a){function
b(b,a){if(15===a[0]){var
c=a[2][2],f=W(a[3][2]),g=W(c);return d(h,d(h,d(h,b,e),g),f)}return d(h,b,e)}return n(p[20],b,c,a)}function
O(a){switch(a){case
0:return fe;case
1:return ff;default:return fg}}function
P(a){switch(a){case
0:return fh;case
1:return fi;default:return fj}}function
z(c,b){function
e(a){var
b=a[4],e=a[3],f=a[2];d(w[2],fk,a[1]);G(c,[0,e,b]);var
g=O(f);return d(w[2],fl,g)}d(p[15],e,b);return a(m[31],fm)}function
aw(c,b){function
e(b){G(c,b);return a(m[31],fn)}d(p[15],e,b);return a(m[31],fo)}function
Q(c,q){var
e=q;for(;;){if(e){var
b=e[1],r=e[2];switch(b[0]){case
0:var
s=b[3],t=b[1],u=a(g,b[4]),v=a(g,s);c6(w[2],fp,t[1],v,u);break;case
1:var
x=b[1],y=a(g,b[2]);n(w[2],fq,x[1],y);break;case
2:d(w[2],fr,b[1]);break;case
3:var
z=b[1],A=a(g,b[2]);n(w[2],fs,z[1],A);break;case
4:var
j=b[3],k=j[2],l=b[2],i=l[2],B=j[1],C=l[1],D=b[1],E=k[1],F=P(k[2]),H=a(g,B),I=i[1],J=P(i[2]),K=a(g,C),L=P(i[2]);l_(w[2],ft,L,D,K,J,I,H,F,E);break;case
5:var
f=b[1][1];d(w[2],fu,f[1]);G(c,[0,f[3],f[4]]);var
M=O(f[2]);a(m[31],M);a(m[31],fv);break;case
6:var
h=b[1];d(w[2],fw,h[1]);G(c,[0,h[3],h[4]]);var
N=O(h[2]);a(m[31],N);a(m[31],fx);break;case
7:n(w[2],fy,b[1],b[2]);break;case
8:n(w[2],fz,b[1],b[2]);break;case
9:n(w[2],fA,b[1][1],b[2][1]);break;case
10:n(w[2],fB,b[1][1],b[2][1]);break;case
11:c6(w[2],fC,b[2][1],b[3],b[1]);break;case
12:var
R=b[1],S=a(g,b[2]);n(w[2],fD,R,S);break;case
13:d(w[2],fE,b[1]);break;case
14:var
T=b[1],U=a(g,b[2]);n(w[2],fF,T,U);break;case
15:var
o=b[3],p=b[2],V=o[2],W=p[2];c6(w[2],fG,b[1][1],p[1],o[1]);Q(c,W);a(m[36],0);Q(c,V);a(m[36],0);break;default:var
X=b[1],Y=a(g,b[2]);n(w[2],fH,X,Y)}var
e=r;continue}return a(m[52],m[28])}}function
ah(a){return d(w[4],fI,a)}var
X=[0,0];function
R(a){X[1]=0;return 0}function
S(a){return X[1]}function
b(a){if(aR[1])Q(ah,[0,a,0]);return E(a,X)}function
ax(b,a){return a[2]-b[2]|0}var
ai=a(p[48],ax);function
ay(b){var
c=b[2],d=c[2],e=b[1];return[0,e,[0,a(ai,c[1]),d]]}function
B(i){function
e(k){var
b=k;for(;;){if(b){var
f=b[2],g=b[1],h=a(i,g[1]);if(d(j,h,c)){var
b=f;continue}var
l=e(f);return[0,[0,h,g[2]],l]}return 0}}return e}function
C(c,b){var
d=a(c,b[4]),e=b[3],f=a(B(c),e);return[0,b[1],b[2],f,d]}function
az(b){return a(u,b)}function
H(a){return C(az,a)}function
J(m,l){var
b=m,a=l;for(;;){if(b){if(a){var
g=a[2],f=a[1],i=b[2],e=b[1];if(e[2]===f[2]){var
k=d(h,e[1],f[1]);if(d(j,k,c)){var
b=i,a=g;continue}var
n=J(i,g);return[0,[0,k,e[2]],n]}return f[2]<e[2]?[0,e,J(i,a)]:[0,f,J(b,g)]}return b}return a}}function
Y(e,b,c){var
f=d(h,b[4],c[4]),g=J(b[3],c[3]),i=b[2];return[0,a(e,0),i,g,f]}var
Z=[aZ,fJ,aX(0)];function
_(a){if(a){var
c=a[2],b=a[1];if(d(j,l(b[1]),e))return[0,b,c];var
f=_(c);return[0,f[1],[0,b,f[2]]]}throw Z}var
T=[aZ,fK,aX(0)];function
K(c,a){if(a){var
d=a[2],b=a[1];if(b[2]===c)return[0,b,d];var
e=K(c,d);return[0,e[1],[0,b,e[2]]]}throw T}function
r(f){var
g=f[4],o=f[3],m=f[2],n=f[1];if(0===o)switch(m){case
0:if(d(j,g,c))return 0;b([12,n,g]);throw q;case
1:if(M(g,c))return 0;b([14,n,g]);throw q;default:if(c7(g,c))return 0;b([13,n]);throw q}function
v(a){return l(a[1])}var
h=af(d(p[17],v,o));if(0===m)if(c7(N(g,h),c)){b([1,f,h]);throw q}if(2===m)if(c7(N(g,h),c)){b([2,f[1]]);return 0}if(c7(h,e)){var
s=F(g,h),w=d(k,g,d(i,s,h)),t=[0,n,m,a(B(function(a){return x(a,h)}),o),s];if(0===m)var
r=0;else
if(2===m)var
r=0;else
var
u=[0,f,t,h,w],r=1;if(!r)var
u=[3,f,h];b(u);return[0,t,0]}return[0,f,0]}function
D(o,g,f,c){var
h=g[1],p=c[3],q=g[2];try{var
k=K(q,p)[1],l=d(j,h,e)?a(u,k[1]):d(j,h,ad)?k[1]:a(m[3],fL),n=Y(o,c,C(function(a){return d(i,a,l)},f));b([4,n[1],[0,e,c],[0,l,f]]);return n}catch(a){a=t(a);if(a===T)return c;throw a}}function
$(b,a){var
c=d(i,y,a);return d(k,b,d(i,a,F(d(h,d(i,y,b),a),c)))}function
aj(q,f,H,G){var
g=q[1],j=f[3],I=q[3],t=a(q[2],0);if(0===j){z(I,[0,f,0]);a(m[3],fM)}var
J=a(p[6],j),L=a(p[5],j)[2],M=[0,l(a(p[5],j)[1]),L];function
N(b,a){var
c=b[1],d=b[2];if(A(c,l(a[1]))){var
e=a[2];return[0,l(a[1]),e]}return[0,c,d]}var
v=n(p[20],N,M,J),O=v[2],c=d(h,v[1],e),P=$(f[4],c),Q=f[3],R=a(B(function(a){return $(a,c)}),Q),S=[0,[0,a(u,c),t],R],w=[0,a(g,0),0,S,P],T=d(i,y,c),U=a(u,F(d(h,d(i,y,f[4]),c),T)),V=f[3],W=a(B(function(b){var
e=d(i,y,c);return a(u,F(d(h,d(i,y,b),c),e))}),V);b([5,[0,w,[0,a(g,0),0,W,U],f,c,t]]);var
X=r(w),k=a(p[5],X),s=K(O,k[3])[1];function
Y(a){return r(D(g,s,k,a))}var
Z=d(o[17][78],Y,H);function
_(a){return r(D(g,s,k,a))}var
aa=d(o[17][78],_,G),E=D(g,s,k,f),ab=C(function(a){return x(a,c)},E);b([3,E,c]);var
ac=r(ab);return[0,a(p[5],ac),Z,aa]}function
ak(e,i){var
b=i;for(;;){var
g=b[3],f=b[2],a=b[1],c=e[1],j=e[3];if(aR[1])z(j,[0,a,f]);try{var
h=_(a[3])[1],k=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,c,h),l=d(o[17][78],k,g),m=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,c,h),n=[0,d(o[17][78],m,f),l];return n}catch(c){c=t(c);if(c===Z){var
b=aj(e,a,f,g);continue}throw c}}}function
aa(k,o){var
f=o;for(;;){var
g=f[2],h=f[1],r=k[3],m=function(a){if(a){var
c=a[2],b=a[1],g=b[3],h=function(a){return d(j,l(a[1]),e)};if(d(p[28],h,g))return[0,b,c];var
f=m(c);return[0,f[1],[0,b,f[2]]]}throw I};if(h){var
s=h[2],u=h[1];try{var
n=m(h),v=n[2],w=n[1],a=w,i=v}catch(b){b=t(b);if(b!==I)throw b;var
a=u,i=s}if(0===a[3]){if(d(j,a[4],c)){b([2,a[1]]);var
f=[0,i,g];continue}b([12,a[1],a[4]]);throw q}var
f=ak(k,[0,a,i,g]);continue}if(aR[1])z(r,g);return g}}function
L(o,e){function
y(a){var
b=a[3];if(b)if(d(s,b[1][1],c))return[0,H(a),0];return[0,a,1]}var
f=d(v[1],0,7);function
i(z){var
l=y(z),m=l[2],a=l[1],g=a[3];if(0===g){if(d(s,a[4],c)){b([14,a[1],a[4]]);throw q}return b([2,a[1]])}try{var
o=d(v[6],f,g),i=o[2],j=o[1];if(1===m)if(j)var
k=j[1],C=d(s,k[4],a[4])?(b([7,k[1],a[1]]),k):(b([7,a[1],k[1]]),a),e=[0,[0,C],i];else
var
e=[0,[0,a],i];else
if(i){var
h=i[1];if(A(h[4],a[4]))b([8,h[1],a[1]]);else
b([8,a[1],h[1]]);var
E=A(h[4],a[4])?h:a,e=[0,j,[0,E]]}else
var
e=[0,j,[0,a]];var
p=e[1];if(p){var
r=e[2];if(r){var
u=r[1],w=p[1];if(d(s,w[4],u[4])){b([9,w,H(u)]);throw q}var
x=1}else
var
x=0}else
var
x=0;d(v[10],f,g);var
D=n(v[5],f,g,e);return D}catch(b){b=t(b);if(b===I){var
B=1===m?[0,[0,a],0]:[0,0,[0,a]];return n(v[5],f,g,B)}throw b}}d(p[15],i,e);var
h=[0,0],g=[0,0];function
k(n,f){var
c=f[1];if(c){var
i=f[2];if(i){var
k=i[1],e=c[1];if(d(j,e[4],k[4])){var
l=a(o,0);b([11,l,e,k[1]]);return E([0,l,0,e[3],e[4]],h)}}}var
m=f[2];if(c)E(c[1],g);return m?E(H(m[1]),g):0}d(v[12],k,f);return[0,h[1],g[1]]}var
U=[aZ,fN,aX(0)];function
al(g){var
b=d(v[1],0,7);function
h(e,c){try{var
a=d(v[6],b,e),g=l(c);a[1]=d(m[6],a[1],g);var
h=0;return h}catch(a){a=t(a);if(a===I){var
f=[0,l(c)];return n(v[5],b,e,f)}throw a}}function
i(a){var
b=a[3];function
c(a){return h(a[2],a[1])}return d(p[15],c,b)}d(p[15],i,g);var
e=[0,c],a=[0,-1],f=[0,0];function
j(h,g){var
b=g[1];f[1]++;var
i=d(s,b,e[1]),c=i||(-1===a[1]?1:0),j=c?(a[1]=h,e[1]=b,0):c;return j}d(v[12],j,b);if(f[1]<1)throw U;return a[1]}function
am(i,b){function
d(d,b){var
e=d[3],f=d[2],g=d[1];try{var
h=K(i,b[3])[1],j=M(h[1],c)?[0,g,[0,[0,h[1],b],f],e]:[0,g,f,[0,[0,a(u,h[1]),b],e]];return j}catch(a){a=t(a);if(a===T)return[0,[0,b,g],f,e];throw a}}return n(p[20],d,fO,b)}function
an(u,t,c,g){var
f=0;function
h(h,c){var
j=c[2],f=c[1];function
l(n,l){var
o=l[2],g=l[1],v=C(function(a){return d(i,a,f)},o),p=Y(u,C(function(a){return d(i,a,g)},j),v);b([4,p[1],[0,g,j],[0,f,o]]);var
h=r(p);if(h){var
c=h[1];if(h[2])return a(m[3],fP);if(t){var
w=d(k,g,e),q=d(i,d(k,f,e),w);b([16,c[1],q]);var
x=d(k,c[4],q),s=[0,c[1],1,c[3],x]}else
var
s=c;return[0,s,n]}return n}return n(p[20],l,h,g)}return n(p[20],h,f,c)}function
ab(c,f,b){var
g=c[3],h=c[1],a=am(al(b),b),i=a[1],j=an(h,f,a[2],a[3]),e=d(m[26],i,j);if(aR[1])z(g,e);return e}function
aA(c,l,e){var
h=c[1],n=c[3];function
q(a){return 2===a[2]?1:0}if(d(p[28],q,e))a(m[3],fQ);R(0);function
s(a){return b([6,a])}d(p[15],s,e);var
u=d(o[17][78],r,e);function
v(a){return 0===a[2]?1:0}var
i=d(p[37],v,u),w=i[1],j=L(h,i[2]),x=j[2],y=[0,d(m[26],w,j[1]),x];function
g(b,d){var
a=aa(c,d);return b<50?f(b+1|0,a):c8(f,[0,a])}function
f(e,f){var
a=L(h,f),b=a[2],c=a[1];if(0===c)return b;var
d=[0,c,b];return e<50?g(e+1|0,d):c8(g,[0,d])}function
A(a){return c9(g(0,a))}function
B(a){return c9(f(0,a))}function
k(b){try{var
a=k(B(ab(c,l,b)));return a}catch(a){a=t(a);if(a===U){if(aR[1])z(n,b);return b}throw a}}return k(A(y))}function
V(k,j,i){var
f=k,c=j,e=i;for(;;){if(e){var
g=e[2],b=e[1];switch(b[0]){case
0:if(d(ap[4][1],b[1][1],f)){var
c=[0,b,c],e=g;continue}var
e=g;continue;case
1:var
f=[0,b[1][1],f],c=[0,b,c],e=g;continue;case
2:var
e=g;continue;case
3:if(d(ap[4][1],b[1][1],f)){var
c=[0,b,c],e=g;continue}var
e=g;continue;case
4:var
l=b[3][2],n=b[2][2];if(d(ap[4][1],b[1],f)){var
f=[0,n[1],[0,l[1],f]],c=[0,b,c],e=g;continue}var
e=g;continue;case
5:var
h=b[1],o=h[3];if(d(ap[4][1],h[1][1],f)){var
f=[0,o[1],f],c=[0,b,c],e=g;continue}var
e=g;continue;case
6:if(d(ap[4][1],b[1][1],f)){var
c=[0,b,c],e=g;continue}var
e=g;continue;case
7:var
e=g;continue;case
8:var
e=g;continue;case
9:var
f=[0,b[1][1],[0,b[2][1],f]],c=[0,b,c],e=g;continue;case
10:var
f=[0,b[1][1],[0,b[2][1],f]],c=[0,b,c],e=g;continue;case
11:var
p=b[3],q=b[2];if(d(ap[4][1],b[1],f)){var
f=[0,q[1],[0,p,f]],c=[0,b,c],e=g;continue}var
e=g;continue;case
12:var
f=[0,b[1],f],c=[0,b,c],e=g;continue;case
13:var
f=[0,b[1],f],c=[0,b,c],e=g;continue;case
14:var
f=[0,b[1],f],c=[0,b,c],e=g;continue;case
15:return a(m[3],fR);default:if(d(ap[4][1],b[1],f)){var
c=[0,b,c],e=g;continue}var
e=g;continue}}return[0,f,c]}}function
ao(a){var
g=a[2],h=a[1];function
i(a){return 2===a[2]?1:0}var
j=d(p[37],i,g)[1];function
e(a){var
b=a[3];if(b)if(d(s,b[1][1],c))return[0,H(a),0];return[0,a,1]}var
f=d(v[1],0,7);function
k(a){var
b=e(a),c=b[1];return n(v[5],f,[0,c[3],c[4]],[0,b[2],a])}d(p[15],k,j);function
l(a){if(0===a[2]){var
c=e(a),g=c[1],i=c[2],j=g[4],k=g[3];try{var
h=d(v[6],f,[0,k,j]);b([10,a,h[2],i===h[1]?1:0]);throw q}catch(a){a=t(a);if(a===I)return 0;throw a}}throw[0,cJ,fS]}return d(p[15],l,h)}var
ac=[aZ,fT,aX(0)];return[0,j,s,aq,A,M,h,k,i,x,N,c,e,y,ad,l,g,u,ar,as,at,au,av,E,ae,af,F,q,ag,G,W,O,P,z,aw,g,Q,ah,b,S,R,ai,ay,B,C,H,J,Y,Z,_,T,K,r,D,$,aj,ak,aa,L,U,al,am,an,ab,aA,V,ao,ac,function(c,j){var
f=c[1],D=c[3];R(0);function
E(a){return b([6,a])}d(p[15],E,j);function
i(d,a){ao(a);var
b=aa(c,a);return d<50?h(d+1|0,b):c8(h,[0,b])}function
h(j,k){function
l(a){return 2===a[2]?1:0}var
a=d(p[37],l,k),b=a[1],c=L(f,a[2]),e=c[2],g=c[1];if(0===g)return d(m[26],b,e);var
h=[0,g,d(m[26],b,e)];return j<50?i(j+1|0,h):c8(i,[0,h])}function
F(a){return c9(i(0,a))}function
G(a){return c9(h(0,a))}function
l(b){try{var
a=l(G(ab(c,0,b)));return a}catch(a){a=t(a);if(a===U){if(aR[1])z(D,b);return b}throw a}}function
H(l){var
c=l;for(;;){var
g=c[1];if(g){var
j=c[2],b=g[1],n=c[3],o=g[2],h=a(f,0),i=a(f,0),q=d(k,b[4],e),r=[0,h,1,b[3],q],s=d(k,a(u,b[4]),e),t=b[3],v=[0,i,1,a(B(u),t),s],w=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,0],a[1]],[0,d,a[2]]]}}(b,i,v),x=d(p[17],w,j),y=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,1],a[1]],[0,d,a[2]]]}}(b,h,r),z=d(p[17],y,j),A=d(m[26],z,x),c=[0,o,A,[0,[0,b[1],[0,b,h,i]],n]];continue}return[0,c[2],c[3]]}}try{var
J=d(o[17][78],r,j),K=function(a){return 0===a[2]?1:0},s=d(p[37],K,J),M=s[2],N=s[1],O=function(a){return 2===a[2]?1:0},w=d(p[37],O,M),P=w[1],x=L(f,w[2]),Q=x[1],T=d(m[26],x[2],P),W=F([0,d(m[26],N,Q),T]),X=function(a){return 2===a[2]?1:0},y=d(p[37],X,W),Y=y[2],Z=y[1],_=S(0),A=H([0,Z,[0,[0,0,Y],0],0]),$=A[2],ad=A[1],ae=function(a){var
b=a[1],f=a[2];R(0);try{l(f);throw ag}catch(a){a=t(a);if(a===q){var
c=V(0,0,S(0)),e=c[1],g=c[2],h=function(a){return d(ap[4][1],a[2],e)},i=d(p[37],h,b)[1],j=function(a){return a[1]};return[0,d(p[17],j,i),e,b,g]}throw a}},af=d(p[17],ae,ad),ah=function(e){var
b=d(v[1],0,7),a=[0,-1],c=[0,0];function
f(c){try{d(v[6],b,c)[1]++;var
a=0;return a}catch(a){a=t(a);if(a===I)return n(v[5],b,c,[0,1]);throw a}}function
g(a){var
b=a[1];if(b)return d(p[15],f,b);throw[0,ac,a[4],a[2]]}d(p[15],g,e);function
h(e,b){var
d=c[1]<b[1]?1:0,f=d?(a[1]=e,c[1]=b[1],0):d;return f}d(v[12],h,b);return a[1]},g=function(e){try{var
c=ah(e),l=function(g){var
b=g[3];for(;;){if(b){var
d=b[1],e=b[2],f=d[3];if(c===d[1])return f;var
b=e;continue}return a(m[3],fU)}},f=d(p[37],l,e),q=f[2],r=f[1],h=function(a){var
b=a[4],d=a[3],e=a[2],f=a[1];function
g(b,a){return b===a?1:0}return[0,n(o[17][99],g,c,f),e,d,b]},s=d(p[17],h,r),u=d(p[17],h,q),i=g(s),v=i[2],w=i[1],j=g(u),x=j[2],y=j[1],b=d(ap[4][2],c,$),k=b[1],z=b[3],A=b[2],B=function(b,a){return b===a?1:0},C=n(o[17][132],B,v,x),D=[0,[0,[15,k,[0,A,w],[0,z,y]],0],[0,k[1],C]];return D}catch(a){a=t(a);if(a[1]===ac)return[0,a[2],a[3]];throw a}},C=g(af),ai=V(C[2],C[1],_)[2];return ai}catch(a){a=t(a);if(a===q)return V(0,0,S(0))[2];throw a}}]}];c_(412,dd,"Omega_plugin.Omega");var
l=a(dd[2],[0,E[17],E[16],E[12],E[13],E[14],E[15],E[22],E[5],E[6],E[2]]),fV=0;function
aS(b){var
c=a(h[10],b);return a(i[99],c)}function
bd(b){var
c=a(h[10],b);return a(i[86],c)}var
cK=[0,0],be=[0,0],bf=[0,0],cL=[0,1],cM=[0,1],fW=[0,0];function
fX(a,b){return a[1]}function
a0(b,a){b[1]=a;return 0}function
fY(a){return a0(cK,a)}var
f1=[0,0,f0,fZ,function(a){return cK[1]},fY];d(bg[4],0,f1);function
f2(a){return a0(be,a)}var
f5=[0,0,f4,f3,function(a){return be[1]},f2];d(bg[4],0,f5);function
f6(a){return a0(bf,a)}var
f9=[0,0,f8,f7,function(a){return bf[1]},f6];d(bg[4],0,f9);function
f_(a){return a0(cM,a)}var
gb=[0,1,ga,f$,function(a){return cM[1]},f_];d(bg[4],0,gb);function
gc(a){return a0(cL,a)}var
gf=[0,0,ge,gd,function(a){return cL[1]},gc];d(bg[4],0,gf);var
de=[0,0];function
dF(c){var
a=de[1];function
b(a){a[1][1]=a[2];return 0}return d(o[17][11],b,a)}function
aT(a){var
b=[0,a];de[1]=[0,[0,b,a],de[1]];return b}var
dG=aT(0);function
ag(e){var
b=a(m[22],dG[1]),c=d(m[17],gg,b);dG[1]++;return a(A[1][6],c)}var
dH=aT(0);function
gh(b){var
a=d(a1[1],gi,[0,dH[1]]);dH[1]++;return a}var
dI=aT(0);function
dJ(e){var
b=a(m[22],dI[1]),c=d(m[17],gj,b);dI[1]++;return a(A[1][6],c)}var
dK=aT(0);function
bh(a){dK[1]++;return dK[1]}var
dL=aT(1e3);function
df(a){dL[1]++;return dL[1]}var
dM=aT(0);function
dN(a){dM[1]++;return d(a1[1],gk,[0,dM[1]])}function
a2(a){return d(w[4],gl,a)}var
dg=[0,0],cN=d(v[1],0,7),dh=d(v[1],0,7);function
dO(b){dg[1]=0;return a(v[2],cN)}function
di(b){try{var
a=d(v[6],dh,b);return a}catch(a){a=t(a);if(a===I){var
c=dN(0);n(v[5],cN,c,b);n(v[5],dh,b,c);return c}throw a}}function
cO(b){try{var
a=d(v[6],cN,b);return a}catch(a){a=t(a);if(a===I){var
c=dg[1];n(v[5],cN,b,c);n(v[5],dh,c,b);dg[1]++;return c}throw a}}function
Q(b){return a(j[66][22],b)}function
dP(a){return c6(i[108],0,gm,1,[0,[0,a,0]])}function
u(b){return a(i[146],b)}function
gn(b){return a(i[99],b)}function
O(b){var
d=f(b),h=0,j=g===d?b[1]:c===d?a(e[2],b):b;return a(i[68],[0,[0,0,j],h])}function
cP(b,a){return n(H[42][1],dj[9],b,a)}function
dQ(c){return function(d){var
a=d;for(;;){if(a){var
b=a[1],e=a[2],f=b[1];if(c===b[2])return f;var
a=e;continue}throw I}}}var
bi=[0,0];function
dR(a){bi[1]=0;return 0}function
J(b){try{var
c=bi[1],d=a(dQ(b),c);return d}catch(b){b=t(b);if(b===I)return a(m[3],go);throw b}}function
gp(b){try{var
c=d(A[1][13][3],b,bi[1]);return c}catch(b){b=t(b);if(b===I)return a(m[3],gq);throw b}}function
aK(b,a){bi[1]=[0,[0,b,a],bi[1]];return 0}var
bj=[0,0];function
dS(a){return bj[1]}function
dk(a){bj[1]=0;return 0}function
dT(c,b){try{var
d=bj[1],e=a(h[96],c),f=n(o[17][119],e,b,d);return f}catch(b){b=t(b);if(b===I)return a(m[3],gr);throw b}}function
dU(d,c,b,a){bj[1]=[0,[0,d,[0,c,b,a]],bj[1]];return 0}function
dV(b){var
a=cM[1];return a?(dF(0),dO(0),dR(0),dk(0)):a}var
gt=d(o[18],a3[6],gs),gu=d(o[18],[0,dW,0],gt),gv=d(o[18],a3[5],gu),dX=d(o[18],a3[7],gv);function
bk(d,c,b){var
e=n(a3[4],d,c,b),f=a(gw[21],e);return a(h[8],f)}var
gx=a3[7];function
F(a){return bk(gy,gx,a)}function
k(a){return bk(gz,dX,a)}function
ac(a){return bk(gB,gA,a)}function
P(a){return bk(gD,gC,a)}var
aq=[c,function(a){return k(gE)}],ar=[c,function(a){return k(gF)}],as=[c,function(a){return k(gG)}],R=[c,function(a){return k(gH)}],S=[c,function(a){return k(gI)}],T=[c,function(a){return k(gJ)}],x=[c,function(a){return k(gK)}],at=[c,function(a){return k(gL)}],au=[c,function(a){return k(gM)}],y=[c,function(a){return P(gN)}],K=[c,function(a){return P(gO)}],B=[c,function(a){return P(gP)}],ah=[c,function(a){return P(gQ)}],ai=[c,function(a){return P(gR)}],a4=[c,function(a){return P(gS)}],av=[c,function(a){return P(gT)}],bl=[c,function(a){return ac(gU)}],bm=[c,function(a){return ac(gV)}],bn=[c,function(a){return ac(gW)}],bo=[c,function(a){return k(gX)}],bp=[c,function(a){return ac(gY)}],bq=[c,function(a){return ac(gZ)}],br=[c,function(a){return ac(g0)}],bs=[c,function(a){return ac(g1)}],bt=[c,function(a){return ac(g2)}],bu=[c,function(a){return ac(g3)}],bv=[c,function(a){return ac(g4)}],C=[c,function(a){return k(g5)}],bw=[c,function(a){return k(g6)}],bx=[c,function(a){return k(g7)}],aw=[c,function(a){return k(g8)}],ax=[c,function(a){return k(g9)}],by=[c,function(a){return k(g_)}],bz=[c,function(a){return k(g$)}],bA=[c,function(a){return k(ha)}],bB=[c,function(a){return k(hb)}],bC=[c,function(a){return k(hc)}],bD=[c,function(a){return k(hd)}],bE=[c,function(a){return k(he)}],bF=[c,function(a){return k(hf)}],bG=[c,function(a){return k(hg)}],bH=[c,function(a){return k(hh)}],bI=[c,function(a){return k(hi)}],bJ=[c,function(a){return k(hj)}],U=[c,function(a){return k(hk)}],L=[c,function(a){return k(hl)}],bK=[c,function(a){return k(hm)}],bL=[c,function(a){return k(hn)}],bM=[c,function(a){return k(ho)}],bN=[c,function(a){return k(hp)}],bO=[c,function(a){return k(hq)}],bP=[c,function(a){return k(hr)}],bQ=[c,function(a){return k(hs)}],bR=[c,function(a){return k(ht)}],bS=[c,function(a){return k(hu)}],bT=[c,function(a){return k(hv)}],bU=[c,function(a){return k(hw)}],bV=[c,function(a){return k(hx)}],bW=[c,function(a){return k(hy)}],V=[c,function(a){return k(hz)}],bX=[c,function(a){return k(hA)}],bY=[c,function(a){return k(hB)}],bZ=[c,function(a){return k(hC)}],b0=[c,function(a){return k(hD)}],b1=[c,function(a){return k(hE)}],W=[c,function(a){return k(hF)}],b2=[c,function(a){return k(hG)}],b3=[c,function(a){return k(hH)}],b4=[c,function(a){return k(hI)}],b5=[c,function(a){return k(hJ)}],b6=[c,function(a){return k(hK)}],b7=[c,function(a){return k(hL)}],b8=[c,function(a){return k(hM)}],b9=[c,function(a){return k(hN)}],b_=[c,function(a){return k(hO)}],b$=[c,function(a){return P(hP)}],dY=[c,function(a){return k(hQ)}],dZ=[c,function(a){return P(hR)}],d0=[c,function(a){return P(hS)}],d1=[c,function(a){return k(hT)}],d2=[c,function(a){return k(hU)}],ca=[c,function(a){return k(hV)}],d3=[c,function(a){return k(hW)}],d4=[c,function(a){return k(hX)}],d5=[c,function(a){return k(hY)}],d6=[c,function(a){return k(hZ)}],d7=[c,function(a){return k(h0)}],ay=[c,function(a){return k(h1)}],az=[c,function(a){return k(h2)}],aj=[c,function(a){return P(h3)}],ak=[c,function(a){return P(h4)}],a5=[c,function(a){return P(h5)}],a6=[c,function(a){return P(h6)}],aA=[c,function(a){return F(h7)}],cb=[c,function(a){return F(h8)}],cc=[c,function(a){return F(h9)}],aB=[c,function(a){return F(h_)}],aC=[c,function(a){return F(h$)}],cd=[c,function(a){return F(ia)}],ce=[c,function(a){return F(ib)}],cf=[c,function(a){return F(ic)}],X=[c,function(a){return F(id)}],aD=[c,function(a){return F(ie)}],aE=[c,function(a){return F(ig)}],cg=[c,function(a){return k(ih)}],ch=[c,function(a){return k(ii)}],ci=[c,function(a){return k(ij)}],d8=[c,function(a){return k(ik)}],d9=[c,function(a){return k(il)}],d_=[c,function(a){return k(im)}],d$=[c,function(a){return k(io)}],cj=[c,function(a){return k(ip)}],ea=[c,function(a){return k(iq)}],eb=[c,function(a){return k(ir)}],ec=[c,function(a){return k(is)}],ed=[c,function(a){return k(it)}],ck=[c,function(a){return k(iu)}],cl=[c,function(a){return k(iv)}],cm=[c,function(a){return k(iw)}],cn=[c,function(a){return k(ix)}],co=[c,function(a){return k(iy)}],cp=[c,function(a){return k(iz)}],cq=[c,function(a){return k(iA)}],cr=[c,function(a){return k(iB)}],cs=[c,function(a){return k(iC)}],ct=[c,function(a){return k(iD)}],cu=[c,function(a){return k(iE)}],cv=[c,function(a){return k(iF)}],cw=[c,function(a){return k(iG)}],cx=[c,function(a){return k(iH)}],cy=[c,function(a){return k(iI)}],cz=[c,function(a){return k(iJ)}],al=[c,function(a){return F(iK)}],aF=[c,function(a){return F(iL)}],aG=[c,function(a){return F(iM)}],Y=[c,function(a){return F(iN)}],cA=[c,function(a){return F(iO)}],aH=[c,function(a){return F(iP)}],cB=[c,function(a){return F(iQ)}];function
aI(o,b){var
i=a(iR[2],0),j=f(b),p=a(iS[17],i),q=g===j?b[1]:c===j?a(e[2],b):b,k=d(h[3],p,q);if(10===k[0]){var
l=k[1][1];if(d(dj[2],i,[1,l]))return[1,l]}var
r=d(m[17],o,iT),s=a(am[3],r);return n(ad[3],0,iU,s)}var
ee=[c,function(a){return aI(iV,ai)}],ef=[c,function(a){return aI(iW,a4)}],eg=[c,function(a){return aI(iX,ah)}],dl=[c,function(a){return aI(iY,aj)}],aL=[c,function(a){return aI(iZ,ak)}],i1=[c,function(a){return aI(i0,a5)}],i3=[c,function(a){return aI(i2,a6)}],cQ=[c,function(a){return aI(i4,al)}];function
i5(b){var
c=a(A[1][6],b);return a(h[10],c)}function
ae(i,d){var
b=f(y),j=[0,i,d],k=g===b?y[1]:c===b?a(e[2],y):y;return a(h[21],[0,k,j])}function
cR(i,d){var
b=f(K),j=[0,i,d],k=g===b?K[1]:c===b?a(e[2],K):K;return a(h[21],[0,k,j])}function
eh(i,d){var
b=f(ah),j=[0,i,d],k=g===b?ah[1]:c===b?a(e[2],ah):ah;return a(h[21],[0,k,j])}function
cS(j,i,d){var
b=f(Y),k=[0,j,i,d],l=g===b?Y[1]:c===b?a(e[2],Y):Y;return a(h[21],[0,l,k])}function
cC(h,d){var
b=f(x),i=g===b?x[1]:c===b?a(e[2],x):x;return cS(i,h,d)}function
i6(i,d){var
b=f(aj),j=[0,i,d],k=g===b?aj[1]:c===b?a(e[2],aj):aj;return a(h[21],[0,k,j])}function
aM(i,d){var
b=f(ak),j=[0,i,d],k=g===b?ak[1]:c===b?a(e[2],ak):ak;return a(h[21],[0,k,j])}function
aN(d){var
b=f(B),i=[0,d],j=g===b?B[1]:c===b?a(e[2],B):B;return a(h[21],[0,j,i])}function
cD(i,d){var
b=f(aF),j=[0,i,d],k=g===b?aF[1]:c===b?a(e[2],aF):aF;return a(h[21],[0,k,j])}function
cT(i,d){var
b=f(aG),j=[0,i,d],k=g===b?aG[1]:c===b?a(e[2],aG):aG;return a(h[21],[0,k,j])}function
aJ(d){var
b=f(al),i=[0,d],j=g===b?al[1]:c===b?a(e[2],al):al;return a(h[21],[0,j,i])}function
i7(h,d){var
b=f(at),i=g===b?at[1]:c===b?a(e[2],at):at;return cS(i,h,d)}function
aO(d){var
b=f(av),i=[0,d],j=g===b?av[1]:c===b?a(e[2],av):av;return a(h[21],[0,j,i])}function
G(b){function
i(b){if(d(l[1],b,l[12])){var
j=f(aq);return g===j?aq[1]:c===j?a(e[2],aq):aq}var
o=[0,i(d(l[9],b,l[13]))],p=l[11],q=d(l[10],b,l[13]);if(d(l[1],q,p))var
k=f(ar),r=g===k?ar[1]:c===k?a(e[2],ar):ar,m=r;else
var
n=f(as),s=g===n?as[1]:c===n?a(e[2],as):as,m=s;return a(h[21],[0,m,o])}if(d(l[1],b,l[11])){var
j=f(R);return g===j?R[1]:c===j?a(e[2],R):R}var
o=[0,i(a(l[15],b))];if(d(l[4],b,l[11]))var
k=f(S),p=g===k?S[1]:c===k?a(e[2],S):S,m=p;else
var
n=f(T),q=g===n?T[1]:c===n?a(e[2],T):T,m=q;return a(h[21],[0,m,o])}function
aU(l,H){function
j(b,a){return n(h[95],l,b,a)}var
m=d(h[83],l,H),b=m[2],i=m[1],k=d(h[3],l,i);if(b){var
o=b[2];if(o){var
p=o[2];if(p){if(!p[2]){var
q=f(Y),M=g===q?Y[1]:c===q?a(e[2],Y):Y;if(j(M,i))return[1,16,b]}}else{var
r=f(ay),N=g===r?ay[1]:c===r?a(e[2],ay):ay;if(j(i,N))return[1,17,b];var
s=f(az),O=g===s?az[1]:c===s?a(e[2],az):az;if(j(i,O))return[1,18,b];var
t=f(aj),P=g===t?aj[1]:c===t?a(e[2],aj):aj;if(j(i,P))return[1,19,b];var
u=f(a6),Q=g===u?a6[1]:c===u?a(e[2],a6):a6;if(j(i,Q))return[1,20,b];var
v=f(a5),R=g===v?a5[1]:c===v?a(e[2],a5):a5;if(j(i,R))return[1,21,b];var
w=f(ak),S=g===w?ak[1]:c===w?a(e[2],ak):ak;if(j(i,S))return[1,22,b];var
x=f(aF),T=g===x?aF[1]:c===x?a(e[2],aF):aF;if(j(i,T))return[1,25,b];var
y=f(aG),U=g===y?aG[1]:c===y?a(e[2],aG):aG;if(j(i,U))return[1,26,b];var
z=f(cz),V=g===z?cz[1]:c===z?a(e[2],cz):cz;if(j(i,V))return[1,30,b];var
A=f(aA),W=g===A?aA[1]:c===A?a(e[2],aA):aA;if(j(i,W))return[1,31,b];var
B=f(cb),X=g===B?cb[1]:c===B?a(e[2],cb):cb;if(j(i,X))return[1,32,b];var
C=f(cc),Z=g===C?cc[1]:c===C?a(e[2],cc):cc;if(j(i,Z))return[1,33,b];var
D=f(aB),_=g===D?aB[1]:c===D?a(e[2],aB):aB;if(j(i,_))return[1,34,b]}}else{var
E=f(al),$=g===E?al[1]:c===E?a(e[2],al):al;if(j(i,$))return[1,29,b]}}else{var
F=f(aH),aa=g===F?aH[1]:c===F?a(e[2],aH):aH;if(j(i,aa))return[1,27,b];var
G=f(cB),ab=g===G?cB[1]:c===G?a(e[2],cB):cB;if(j(i,ab))return[1,28,b]}switch(k[0]){case
1:if(!b)return[0,k[1]];break;case
6:if(k[1]){if(!b){var
I=a(am[3],i8);return n(ad[6],0,0,I)}}else
if(!b)return[2,k[2],k[3]];break;case
10:var
J=a(dm[36],[1,k[1][1]]);return[1,[0,a(dn[20],J)],b];case
11:var
K=a(dm[36],[2,k[1][1]]);return[1,[0,a(dn[20],K)],b];case
12:var
L=a(dm[36],[3,k[1][1]]);return[1,[0,a(dn[20],L)],b]}return 0}var
ei=dj[9];function
a7(k,b,q){var
l=n(dp[80],0,k,b),r=n(ei,k,b,q),m=d(h[83],b,r),i=m[2],j=m[1];d(h[3],b,j);if(!i){var
o=f(x),s=g===o?x[1]:c===o?a(e[2],x):x;if(d(l,j,s))return[1,23,i];var
p=f(X),t=g===p?X[1]:c===p?a(e[2],X):X;if(d(l,j,t))return[1,24,i]}return 0}function
cE(k,H){function
j(b,a){return n(h[95],k,b,a)}var
l=d(h[83],k,H),b=l[2],i=l[1],m=d(h[3],k,i);if(b){var
o=b[2];if(o){if(!o[2]){var
p=f(y),I=g===p?y[1]:c===p?a(e[2],y):y;if(j(i,I))return[1,0,b];var
q=f(K),J=g===q?K[1]:c===q?a(e[2],K):K;if(j(i,J))return[1,1,b];var
r=f(ah),L=g===r?ah[1]:c===r?a(e[2],ah):ah;if(j(i,L))return[1,2,b];var
s=f(cd),M=g===s?cd[1]:c===s?a(e[2],cd):cd;if(j(i,M))return[1,6,b];var
t=f(ce),N=g===t?ce[1]:c===t?a(e[2],ce):ce;if(j(i,N))return[1,7,b];var
u=f(aC),O=g===u?aC[1]:c===u?a(e[2],aC):aC;if(j(i,O))return[1,8,b]}}else{var
v=f(ai),P=g===v?ai[1]:c===v?a(e[2],ai):ai;if(j(i,P))return[1,3,b];var
w=f(a4),Q=g===w?a4[1]:c===w?a(e[2],a4):a4;if(j(i,Q))return[1,5,b];var
x=f(B),U=g===x?B[1]:c===x?a(e[2],B):B;if(j(i,U))return[1,4,b];var
z=f(cf),V=g===z?cf[1]:c===z?a(e[2],cf):cf;if(j(i,V))return[1,9,b];var
A=f(aD),W=g===A?aD[1]:c===A?a(e[2],aD):aD;if(j(i,W))return[1,10,b];var
C=f(S),X=g===C?S[1]:c===C?a(e[2],S):S;if(j(i,X))return[1,13,b];var
D=f(T),Y=g===D?T[1]:c===D?a(e[2],T):T;if(j(i,Y))return[1,12,b];var
E=f(av),Z=g===E?av[1]:c===E?a(e[2],av):av;if(j(i,Z))return[1,15,b]}}else{var
F=f(aE),_=g===F?aE[1]:c===F?a(e[2],aE):aE;if(j(i,_))return[1,11,b];var
G=f(R),$=g===G?R[1]:c===G?a(e[2],R):R;if(j(i,$))return[1,14,b];if(1===m[0])return[0,m[1]]}return 0}function
ej(j,u){function
b(b,a){return n(h[95],j,b,a)}function
i(t){var
o=d(h[83],j,t),k=o[2],n=o[1];if(k){if(!k[2]){var
p=k[1],q=f(as),u=g===q?as[1]:c===q?a(e[2],as):as;if(b(n,u)){var
v=i(p),w=d(l[8],l[13],v);return d(l[6],l[12],w)}var
r=f(ar),x=g===r?ar[1]:c===r?a(e[2],ar):ar;if(b(n,x)){var
y=i(p);return d(l[8],l[13],y)}}}else{var
s=f(aq),z=g===s?aq[1]:c===s?a(e[2],aq):aq;if(b(n,z))return l[12]}return a(m[3],i9)}var
p=d(h[83],j,u),k=p[2],o=p[1];if(k){if(!k[2]){var
q=k[1],r=f(S),v=g===r?S[1]:c===r?a(e[2],S):S;if(b(o,v))return i(q);var
s=f(T),w=g===s?T[1]:c===s?a(e[2],T):T;if(b(o,w)){var
x=i(q);return a(l[17],x)}}}else{var
t=f(R),y=g===t?R[1]:c===t?a(e[2],R):R;if(b(o,y))return l[11]}return a(m[3],i_)}function
cU(C,B,e,b){function
c(e,g,u){var
b=d(h[3],C,u);if(5===b[0]){var
an=b[3],ap=b[2],aq=[0,c(e,g,b[1]),ap,an];return a(h[17],aq)}if(g){var
i=g[1];if(typeof
i==="number")switch(i){case
0:var
k=g[2];switch(b[0]){case
6:var
G=b[2],H=b[1],I=[0,H,G,c(e+1|0,k,b[3])];return a(h[18],I);case
7:var
J=b[2],K=b[1],L=[0,K,J,c(e+1|0,k,b[3])];return a(h[19],L);case
8:var
M=b[3],N=b[2],O=b[1],P=[0,O,N,M,c(e+1|0,k,b[4])];return a(h[20],P);case
14:var
f=0;break;default:var
f=1}break;case
1:var
p=g[2];switch(b[0]){case
6:var
T=b[3],U=b[1],V=[0,U,c(e,p,b[2]),T];return a(h[18],V);case
7:var
W=b[3],X=b[1],Y=[0,X,c(e,p,b[2]),W];return a(h[19],Y);case
8:var
Z=b[4],_=b[2],$=b[1],aa=[0,$,_,c(e,p,b[3]),Z];return a(h[20],aa);case
14:var
f=0;break;default:var
f=1}break;case
2:var
ab=g[2];switch(b[0]){case
9:var
ac=b[2],ad=[0,c(e,ab,b[1]),ac];return a(h[21],ad);case
14:var
f=0;break;default:var
f=1}break;default:var
ae=g[2];switch(b[0]){case
9:var
af=b[1],q=a(o[19][8],b[2]);q[1]=c(e,ae,ao(q,0)[1]);return a(h[21],[0,af,q]);case
14:var
f=0;break;default:var
f=1}}else
if(0===i[0]){var
y=i[1],ag=g[2];switch(b[0]){case
9:var
ah=b[1],r=a(o[19][8],b[2]),z=y-1|0,A=y-1|0,ai=c(e,ag,ao(r,z)[z+1]);ao(r,A)[A+1]=ai;return a(h[21],[0,ah,r]);case
14:var
f=0;break;default:var
f=1}}else{var
s=i[1],aj=g[2];switch(b[0]){case
13:var
ak=b[3],al=b[2],am=b[1],t=a(o[19][8],b[4]);t[s+1]=c(e,aj,ao(t,s)[s+1]);return a(h[30],[0,am,al,ak,t]);case
14:var
f=0;break;default:var
f=1}}if(f){var
D=a(o[17][1],g),E=a(m[22],D),F=d(m[17],i$,E);return a(m[3],F)}var
v=b[1],l=v[2],n=l[3],w=v[1],j=w[2],Q=l[2],R=l[1],x=a(o[19][8],n),S=c(e+(n.length-1)|0,g,ao(n,j)[j+1]);ao(x,j)[j+1]=S;return a(h[31],[0,w,[0,R,Q,x]])}return d(B,e,u)}return c(1,e,b)}function
ek(s,r,q){var
b=r,e=q;for(;;){var
c=d(h[3],s,e);if(5===c[0]){var
e=c[1];continue}if(b){var
g=b[1];if(typeof
g==="number")switch(g){case
0:var
i=b[2];switch(c[0]){case
6:var
b=i,e=c[3];continue;case
7:var
b=i,e=c[3];continue;case
8:var
b=i,e=c[4];continue;case
14:var
f=0;break;default:var
f=1}break;case
1:var
j=b[2];switch(c[0]){case
6:var
b=j,e=c[2];continue;case
7:var
b=j,e=c[2];continue;case
8:var
b=j,e=c[3];continue;case
14:var
f=0;break;default:var
f=1}break;case
2:var
w=b[2];switch(c[0]){case
9:var
b=w,e=c[1];continue;case
14:var
f=0;break;default:var
f=1}break;default:var
x=b[2];switch(c[0]){case
9:var
b=x,e=ao(c[2],0)[1];continue;case
14:var
f=0;break;default:var
f=1}}else
if(0===g[0]){var
y=b[2],z=g[1];switch(c[0]){case
9:var
n=z-1|0,b=y,e=ao(c[2],n)[n+1];continue;case
14:var
f=0;break;default:var
f=1}}else{var
p=g[1],A=b[2];switch(c[0]){case
13:var
b=A,e=ao(c[4],p)[p+1];continue;case
14:var
f=0;break;default:var
f=1}}if(f){var
t=a(o[17][1],b),u=a(m[22],t),v=d(m[17],ja,u);return a(m[3],v)}var
k=c[1],l=k[1][2],e=ao(k[2][3],l)[l+1];continue}return e}}function
dq(f,e,d,c){var
b=[0,a(h[9],0)],g=cU(f,function(d,c){b[1]=c;return a(h[9],d)},d,c),i=b[1],j=[0,[0,a(A[1][6],jb)],e,g];return[0,a(h[19],j),i]}function
Z(h){function
b(b){var
c=a(H[42][6],b),e=a(o[17][9],h);function
f(c,a){return cP(b,a)}var
g=cU(a(H[42][4],b),f,e,c);return d(i[5],g,2)}return a(q[67][8],b)}function
aP(c){var
b=c;for(;;)switch(b[0]){case
0:var
d=b[2],e=b[1];a(m[31],jc);aP(e);a(m[31],jd);aP(d);return a(m[31],je);case
1:var
f=b[1];a(m[31],jf);var
b=f;continue;case
2:var
g=b[2],h=b[1];a(m[31],jg);aP(h);a(m[31],jh);aP(g);return a(m[31],ji);case
3:var
i=a(A[1][8],b[1]);return a(m[31],i);case
4:var
j=a(l[16],b[1]);return a(m[31],j);default:return a(m[31],jj)}}function
_(c){var
b=c;for(;;)switch(b[0]){case
0:return a(m[3],jk);case
1:var
b=b[1];continue;case
2:var
b=b[1];continue;case
3:return cO(b[1]);case
4:return-1;default:return-1}}function
z(b){switch(b[0]){case
0:var
k=b[1],l=z(b[2]),d=f(y),m=[0,z(k),l],n=g===d?y[1]:c===d?a(e[2],y):y;return a(h[21],[0,n,m]);case
1:var
i=f(B),o=[0,z(b[1])],p=g===i?B[1]:c===i?a(e[2],B):B;return a(h[21],[0,p,o]);case
2:var
q=b[1],r=z(b[2]),j=f(K),s=[0,z(q),r],t=g===j?K[1]:c===j?a(e[2],K):K;return a(h[21],[0,t,s]);case
3:return a(h[10],b[1]);case
4:return G(b[1]);default:return b[1]}}function
el(i,h){var
p=0;return function(q){var
c=p,b=q;for(;;){switch(b[0]){case
0:var
d=b[1];if(2===d[0]){var
e=d[1];if(3===e[0]){var
f=d[2];if(4===f[0]){var
k=b[2],l=f[1],c=[0,[0,l,cO(e[1])],c],b=k;continue}}}break;case
4:var
m=b[1],g=bh(0);aK(i,g);return[0,g,h,a(o[17][9],c),m]}var
j=a(am[3],jl);return n(ad[3],0,0,j)}}}function
M(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1],g=c(a[2]);return[0,[2,[3,di(e)],[4,f]],g]}return[4,b[4]]}return c(b[3])}function
dr(b,a,c){var
d=n(dp[19],b,a,c);return l$(jm[4],0,0,0,0,0,0,0,b,a,d)}function
ds(i,n,b,m){function
j(j){var
p=a(H[42][6],j),q=a(H[42][5],j),r=a(o[17][9],n),k=dq(a(H[42][4],j),i,r,p),l=k[1],s=k[2];function
t(n){var
p=a(h[9],1),j=f(ck),o=[0,l,0],r=[0,i,b,a(h[9],2),p,s,m],t=g===j?ck[1]:c===j?a(e[2],ck):ck,u=a(h[21],[0,t,r]),v=[0,a(h[9],1),[0,b,0]],w=a(h[34],v),x=[0,[0,a(A[1][6],jn)],w,u],y=a(h[19],x),z=d(h[33],i,h[14]),B=[0,[0,a(A[1][6],jo)],z,y],C=[0,a(h[19],B),o],D=a(h[34],C),k=dr(q,n,a(h[21],[0,l,[0,b]])),E=k[1];return[0,E,a(h[34],[0,D,[0,k[2],0]])]}return d(em[2],0,t)}return a(q[67][8],j)}function
cV(i,h,d){var
b=f(x),j=g===b?x[1]:c===b?a(e[2],x):x;return ds(j,i,h,d)}function
en(i,h,d){var
b=f(X),j=g===b?X[1]:c===b?a(e[2],X):X;return ds(j,i,h,d)}function
a8(d,c,b){return cV(d,c,a(h[34],[0,b[1],b[2]]))}function
eo(d,c,b){return en(d,c,a(h[34],[0,b[1],b[2]]))}function
dt(c,b){function
e(e){var
i=a(H[42][5],c),j=d(H[42][8],c,b),f=d(h[3],e,j);if(6===f[0]){var
g=dr(i,e,f[2]),k=g[1];return[0,k,a(h[34],[0,b,[0,g[2],0]])]}throw[0,cJ,jp]}return d(em[2],0,e)}function
r(m,l,k){function
b(b){var
n=a(H[42][6],b),i=f(x),p=a(o[17][9],m),q=g===i?x[1]:c===i?a(e[2],x):x,j=dq(a(H[42][4],b),q,p,n),r=j[2],s=j[1];function
t(c){return ek(a(H[42][4],b),c,r)}var
u=d(o[17][69],t,l),v=[0,k,d(o[18],u,[0,s,0])];return dt(b,a(h[34],v))}return a(q[67][8],b)}function
a9(e,f){function
b(b){var
c=a(H[42][4],b);function
j(l,k){if(0===l)return cP(b,k);var
e=d(h[3],c,k);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
m=e[1],n=f[2],g=d(h[3],c,f[1]);if(9===g[0]){var
i=g[2];if(2===i.length-1){var
o=g[1],p=i[1],q=cP(b,i[2]),r=j(l-1|0,n),s=[0,m,[0,a(h[21],[0,o,[0,p,q]]),r]];return a(h[21],s)}}throw[0,cJ,jr]}}throw[0,cJ,jq]}var
g=a(o[17][1],e),k=a(o[17][1],f)-g|0,l=a(H[42][6],b),m=a(o[17][9],e),n=cU(c,function(b,a){return j(k,a)},m,l);return d(i[5],n,2)}return a(q[67][9],b)}function
a_(i,m){var
b=m[2],h=m[1];switch(h[0]){case
0:var
n=h[2],j=h[1];if(0===b[0]){var
o=b[1],B=b[2],D=_(o),F=_(j);if(d(l[19],F,D)){var
p=a_([0,js,i],[0,n,b]),q=f(C),G=p[1],H=[0,j,p[2]],I=g===q?C[1]:c===q?a(e[2],C):C;return[0,[0,r(i,jt,I),G],H]}var
s=a_([0,ju,i],[0,h,B]),t=f(aw),J=s[1],K=[0,o,s[2]],L=g===t?aw[1]:c===t?a(e[2],aw):aw;return[0,[0,r(i,jv,L),J],K]}var
M=_(b),N=_(j);if(d(l[19],N,M)){var
u=a_([0,jw,i],[0,n,b]),v=f(C),O=u[1],P=[0,j,u[2]],Q=g===v?C[1]:c===v?a(e[2],C):C;return[0,[0,r(i,jx,Q),O],P]}var
w=f(ax),R=[0,b,h],S=0,T=g===w?ax[1]:c===w?a(e[2],ax):ax;return[0,[0,r(i,jy,T),S],R];case
4:var
af=h[1];switch(b[0]){case
0:var
k=0;break;case
4:var
ag=[4,d(E[12],af,b[1])];return[0,[0,Z(i),0],ag];default:var
k=1}break;default:var
k=0}if(!k)if(0===b[0]){var
x=b[1],U=b[2],V=_(h),W=_(x);if(d(l[19],W,V)){var
y=a_([0,jz,i],[0,h,U]),z=f(aw),X=y[1],Y=[0,x,y[2]],$=g===z?aw[1]:c===z?a(e[2],aw):aw;return[0,[0,r(i,jA,$),X],Y]}return[0,0,[0,h,b]]}var
aa=_(b),ab=_(h);if(d(l[18],ab,aa)){var
A=f(ax),ac=[0,b,h],ad=0,ae=g===A?ax[1]:c===A?a(e[2],ax):ax;return[0,[0,r(i,jB,ae),ad],ac]}return[0,0,[0,h,b]]}function
ep(o,D,i,C,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],s=j[1],t=s[2],G=s[1];if(q===t){var
u=f(bJ),H=g===u?bJ[1]:c===u?a(e[2],bJ):bJ,v=r(b,jC,H),I=l[11],J=d(E[14],C,G),K=d(E[14],D,F),M=d(E[12],K,J);if(d(l[1],M,I)){var
w=f(V),N=g===w?V[1]:c===w?a(e[2],V):V,O=r(b,jD,N),P=[0,O,h(b,[0,k,n])];return[0,v,[0,Z([0,jF,[0,jE,b]]),P]]}return[0,v,h([0,jG,b],[0,k,n])]}if(d(l[19],q,t)){var
x=f(U),Q=h([0,jH,b],[0,k,j]),R=g===x?U[1]:c===x?a(e[2],U):U;return[0,r(b,jI,R),Q]}var
y=f(L),S=h([0,jJ,b],[0,i,n]),T=g===y?L[1]:c===y?a(e[2],L):L;return[0,r(b,jK,T),S]}var
z=f(U),W=h([0,jL,b],[0,k,0]),X=g===z?U[1]:c===z?a(e[2],U):U;return[0,r(b,jM,X),W]}var
A=m[2];if(A){var
B=f(L),Y=h([0,jN,b],[0,0,A[2]]),_=g===B?L[1]:c===B?a(e[2],L):L;return[0,r(b,jO,_),Y]}return[0,a9(o,b),0]}return h(o,[0,i,b])}function
cW(o,i,D,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],s=j[1],t=s[2],G=s[1];if(q===t){var
u=f(bM),H=g===u?bM[1]:c===u?a(e[2],bM):bM,v=r(b,jP,H),I=l[11],J=d(E[14],D,G),K=d(E[12],F,J);if(d(l[1],K,I)){var
w=f(V),M=g===w?V[1]:c===w?a(e[2],V):V,N=r(b,jQ,M),O=[0,N,h(b,[0,k,n])];return[0,v,[0,Z([0,jS,[0,jR,b]]),O]]}return[0,v,h([0,jT,b],[0,k,n])]}if(d(l[19],q,t)){var
x=f(C),P=h([0,jU,b],[0,k,j]),Q=g===x?C[1]:c===x?a(e[2],C):C;return[0,r(b,jV,Q),P]}var
y=f(L),R=h([0,jW,b],[0,i,n]),S=g===y?L[1]:c===y?a(e[2],L):L;return[0,r(b,jX,S),R]}var
z=f(C),T=h([0,jY,b],[0,k,0]),U=g===z?C[1]:c===z?a(e[2],C):C;return[0,r(b,jZ,U),T]}var
A=m[2];if(A){var
B=f(L),W=h([0,j0,b],[0,0,A[2]]),X=g===B?L[1]:c===B?a(e[2],L):L;return[0,r(b,j1,X),W]}return[0,a9(o,b),0]}return h(o,[0,i,b])}function
du(h,b){if(b){var
m=b[2];if(d(l[4],b[1][1],l[11]))var
i=f(bK),n=g===i?bK[1]:c===i?a(e[2],bK):bK,j=n;else
var
k=f(bL),p=g===k?bL[1]:c===k?a(e[2],bL):bL,j=p;var
o=r(h,j2,j);return[0,o,du(h,m)]}return[0,Z(h),0]}function
cF(i,j,b){switch(b[0]){case
0:var
v=b[2],k=cF([0,j3,i],j,b[1]),w=k[2],x=k[1],m=cF([0,j4,i],j,v),y=[0,w,m[2]],p=f(bY),z=d(o[18],x,m[1]),A=g===p?bY[1]:c===p?a(e[2],bY):bY;return[0,[0,r(i,j5,A),z],y];case
1:var
B=b[1],C=[2,B,[4,a(l[17],j)]],q=f(bZ),D=[0,Z([0,j6,i]),0],E=g===q?bZ[1]:c===q?a(e[2],bZ):bZ;return[0,[0,r(i,j7,E),D],C];case
2:var
s=b[2],F=b[1];if(4===s[0]){var
I=[2,F,[4,d(l[8],j,s[1])]],t=f(bx),J=[0,Z([0,j9,i]),0],L=g===t?bx[1]:c===t?a(e[2],bx):bx;return[0,[0,r(i,j_,L),J],I]}var
H=a(am[3],j8);return n(ad[6],0,0,H);case
3:return[0,0,[2,b,[4,j]]];case
4:var
M=[4,d(l[8],j,b[1])];return[0,[0,Z(i),0],M];default:var
N=b[1],u=f(K),O=[0,G(j),N],P=g===u?K[1]:c===u?a(e[2],K):K;return[0,0,[5,a(h[21],[0,P,O])]]}}function
cX(d){function
h(b,i){if(i){var
j=f(bN),k=h([0,j$,b],i[2]),l=g===j?bN[1]:c===j?a(e[2],bN):bN;return[0,r(b,ka,l),k]}return[0,a9(d,b),0]}return function(a){return h(d,a)}}function
eq(d){function
h(b,i){if(i){var
j=f(C),k=h([0,kb,b],i[2]),l=g===j?C[1]:c===j?a(e[2],C):C;return[0,r(b,kc,l),k]}return[0,a9(d,b),0]}return function(a){return h(d,a)}}function
cY(d){function
h(b,i){if(i){var
j=f(U),k=h([0,kd,b],i[2]),l=g===j?U[1]:c===j?a(e[2],U):U;return[0,r(b,ke,l),k]}return[0,a9(d,b),0]}return function(a){return h(d,a)}}function
cZ(i,b){switch(b[0]){case
0:var
v=b[2],j=cZ([0,kf,i],b[1]),w=j[2],x=j[1],k=cZ([0,kg,i],v),y=[0,w,k[2]],m=f(b0),z=d(o[18],x,k[1]),A=g===m?b0[1]:c===m?a(e[2],b0):b0;return[0,[0,r(i,kh,A),z],y];case
1:var
p=f(b2),C=b[1],D=0,E=g===p?b2[1]:c===p?a(e[2],b2):b2;return[0,[0,r(i,ki,E),D],C];case
2:var
q=b[2],F=b[1];if(4===q[0]){var
H=[2,F,[4,a(l[17],q[1])]],s=f(b1),I=[0,Z([0,kk,i]),0],J=g===s?b1[1]:c===s?a(e[2],b1):b1;return[0,[0,r(i,kl,J),I],H]}var
G=a(am[3],kj);return n(ad[6],0,0,G);case
3:var
t=f(W),K=[2,b,[4,l[14]]],L=0,M=g===t?W[1]:c===t?a(e[2],W):W;return[0,[0,r(i,km,M),L],K];case
4:var
N=[4,a(l[17],b[1])];return[0,[0,Z(i),0],N];default:var
u=f(B),O=[0,b[1]],P=g===u?B[1]:c===u?a(e[2],B):B;return[0,0,[5,a(h[21],[0,P,O])]]}}function
an(k,j,p){function
q(g,d){try{var
b=dT(k,d),f=b[1],l=a(h[10],b[2]),m=[0,[0,cV(j,a(h[10],f),l),0],[3,f]];return m}catch(b){b=t(b);if(a(ad[18],b)){var
c=dJ(0),e=ag(0);dU(d,c,e,g);var
i=a(h[10],e);return[0,[0,cV(j,a(h[10],c),i),0],[3,c]]}throw b}}try{var
m=cE(k,p);if(typeof
m==="number")var
i=0;else
switch(m[0]){case
0:var
s=[0,0,[3,m[1]]],i=1;break;case
1:var
u=m[1];if(typeof
u==="number")if(16<=u)var
i=0;else{switch(u){case
0:var
v=m[2];if(v){var
w=v[2];if(w)if(w[2])var
i=0,b=0;else
var
ae=w[1],L=an(k,[0,kn,j],v[1]),af=L[2],ah=L[1],M=an(k,[0,ko,j],ae),ai=M[1],N=a_(j,[0,af,M[2]]),aj=N[2],ak=d(o[18],ai,N[1]),n=[0,d(o[18],ah,ak),aj],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
x=m[2];if(x){var
z=x[2];if(z)if(z[2])var
i=0,b=0;else{var
al=z[1],P=an(k,[0,kp,j],x[1]),A=P[2],Q=P[1],R=an(k,[0,kq,j],al),C=R[2],S=R[1];if(4===C[0])var
V=cF(j,C[1],A),as=V[2],at=d(o[18],S,V[1]),D=[0,d(o[18],Q,at),as];else
if(4===A[0])var
T=f(by),am=A[1],ao=g===T?by[1]:c===T?a(e[2],by):by,ap=r(j,kr,ao),U=cF(j,am,C),aq=U[2],ar=d(o[18],S,[0,ap,U[1]]),D=[0,d(o[18],Q,ar),aq];else
var
D=q(0,p);var
n=D,b=1}else
var
i=0,b=0}else
var
i=0,b=0;break;case
2:var
E=m[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
W=f(B),au=E[1],av=[0,F[1]],aw=g===W?B[1]:c===W?a(e[2],B):B,X=f(y),ax=[0,au,a(h[21],[0,aw,av])],ay=g===X?y[1]:c===X?a(e[2],y):y,Y=an(k,j,a(h[21],[0,ay,ax])),az=Y[2],aA=Y[1],n=[0,[0,O(eg),aA],az],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
3:var
H=m[2];if(H)if(H[2])var
i=0,b=0;else
var
aB=H[1],Z=f(y),aC=[0,aB,G(l[12])],aD=g===Z?y[1]:c===Z?a(e[2],y):y,_=an(k,j,a(h[21],[0,aD,aC])),aE=_[2],aF=_[1],n=[0,[0,O(ee),aF],aE],b=1;else
var
i=0,b=0;break;case
4:var
I=m[2];if(I)if(I[2])var
i=0,b=0;else
var
$=an(k,[0,ks,j],I[1]),aG=$[1],aa=cZ(j,$[2]),aH=aa[2],n=[0,d(o[18],aG,aa[1]),aH],b=1;else
var
i=0,b=0;break;case
5:var
J=m[2];if(J)if(J[2])var
i=0,b=0;else
var
aI=J[1],ab=f(y),aJ=[0,aI,G(l[14])],aK=g===ab?y[1]:c===ab?a(e[2],y):y,ac=an(k,j,a(h[21],[0,aK,aJ])),aL=ac[2],aM=ac[1],n=[0,[0,O(ef),aM],aL],b=1;else
var
i=0,b=0;break;case
15:var
K=m[2];if(K)if(K[2])var
i=0,b=0;else
var
n=q(1,K[1]),b=1;else
var
i=0,b=0;break;case
12:case
13:case
14:try{var
aN=[0,0,[4,ej(k,p)]],n=aN,b=1}catch(c){c=t(c);if(!a(ad[18],c))throw c;var
n=q(0,p),b=1}break;default:var
i=0,b=0}if(b)var
s=n,i=1}else
var
i=0;break;default:var
i=0}if(!i)var
s=q(0,p);return s}catch(b){b=t(b);if(a(aQ[5],b))return q(0,p);throw b}}function
dv(h,d,b){switch(d[0]){case
2:var
i=d[1],t=d[2];switch(b[0]){case
2:if(3===i[0]){var
j=f(bW),u=[2,[3,i[1]],[0,d[2],b[2]]],v=g===j?bW[1]:c===j?a(e[2],bW):bW;return[0,r(h,ku,v),u]}break;case
3:var
k=f(bV),w=[2,[3,b[1]],[0,t,[4,l[12]]]],x=g===k?bV[1]:c===k?a(e[2],bV):bV;return[0,r(h,kv,x),w]}break;case
3:var
o=d[1];switch(b[0]){case
2:var
p=f(bU),y=[2,[3,o],[0,b[2],[4,l[12]]]],z=g===p?bU[1]:c===p?a(e[2],bU):bU;return[0,r(h,kw,z),y];case
3:var
q=f(bT),A=[2,[3,o],[4,l[13]]],B=g===q?bT[1]:c===q?a(e[2],bT):bT;return[0,r(h,kx,B),A]}break}aP(d);a(m[36],0);aP(b);a(m[36],0);a(m[52],m[1][27]);var
s=a(am[3],kt);return n(ad[6],0,0,s)}function
cG(i,b){switch(b[0]){case
2:var
j=b[1];if(3===j[0]){var
k=b[2],p=j[1];if(4===k[0])return[0,0,b];var
h=function(b){switch(b[0]){case
0:var
c=b[1],e=h(b[2]),f=h(c);return d(E[12],f,e);case
4:return b[1];default:var
g=a(am[3],kz);return n(ad[6],0,0,g)}},q=[2,[3,p],[4,h(k)]];return[0,[0,Z([0,kA,i]),0],q]}break;case
3:var
m=f(bS),s=[2,[3,b[1]],[4,l[12]]],t=0,u=g===m?bS[1]:c===m?a(e[2],bS):bS;return[0,[0,r(i,kB,u),t],s]}aP(b);var
o=a(am[3],ky);return n(ad[6],0,0,o)}function
a$(b,j){switch(j[0]){case
0:var
h=j[2],i=j[1];switch(h[0]){case
0:var
k=h[1],A=h[2],B=_(k);if(_(i)===B){var
m=dv([0,kC,b],i,k),n=f(bw),C=m[2],D=m[1],E=g===n?bw[1]:c===n?a(e[2],bw):bw,F=r(b,kD,E),p=a$(b,[0,C,A]);return[0,[0,F,[0,D,p[1]]],p[2]]}var
q=cG([0,kE,b],i),G=q[2],H=q[1],s=a$([0,kF,b],h),I=[0,G,s[2]];return[0,d(o[18],H,s[1]),I];case
4:var
O=h[1],x=cG([0,kI,b],i);return[0,x[1],[0,x[2],[4,O]]];default:var
J=_(h);if(_(i)===J){var
t=dv(b,i,h),K=t[1],u=a$(b,t[2]);return[0,[0,K,u[1]],u[2]]}var
v=cG([0,kG,b],i),L=v[2],M=v[1],w=a$([0,kH,b],h),N=[0,L,w[2]];return[0,d(o[18],M,w[1]),N]}case
4:return[0,0,j];default:var
y=cG(b,j),z=f(bX),P=y[1],Q=[0,y[2],[4,l[11]]],R=g===z?bX[1]:c===z?a(e[2],bX):bX,S=[0,r(b,kJ,R),0];return[0,d(o[18],P,S),Q]}}function
c0(i,b){if(0===b[0]){var
h=b[1];if(2===h[0])if(3===h[1][0]){var
k=h[2];if(4===k[0]){var
o=b[2];if(d(l[1],k[1],l[11])){var
m=f(V),p=g===m?V[1]:c===m?a(e[2],V):V,q=r(i,kL,p),n=c0(i,o);return[0,[0,q,n[1]],n[2]]}}}var
j=c0([0,kK,i],b[2]);return[0,j[1],[0,h,j[2]]]}return[0,0,b]}function
dw(bn){var
k=a(A[1][6],kM),m=a(A[1][6],kN),n=a(A[1][6],kO),y=G(l[11]);function
o(bo){var
p=bo;for(;;){if(p){var
b=p[1];switch(b[0]){case
0:var
ak=b[2],al=b[1],bp=p[2],bq=b[4],br=b[3],s=J(al[1]),am=z(M(al)),an=z(M(ak)),D=G(br),T=G(bq),ao=ae(cR(an,D),T),bs=cC(am,ao),bt=ak[3],bu=a(cY(kP),bt),bv=i[N],bw=Q(bu),bx=[0,d(j[66][3],bw,bv),0],by=[0,i[61],[0,i[N],0]],bJ=[0,O(aL),by],bK=[0,a(j[66][22],bJ),0],bL=[0,i[61],[0,i[N],0]],bM=[0,O(aL),bL],bN=[0,a(j[66][22],bM),0],bS=[0,o(bp),0],bT=[0,a(i[25],[0,s,0]),bS],bU=[0,a(i[75],[0,m,[0,n,[0,s,0]]]),bT],bW=a(h[10],s),bX=a(h[10],n),ap=f(bz),bV=0,bY=[0,D,an,T,a(h[10],m),bX,bW],bZ=g===ap?bz[1]:c===ap?a(e[2],bz):bz,b0=[0,u([0,a(h[21],[0,bZ,bY]),bV]),bU],b1=[0,a(i[25],[0,m,[0,n,0]]),b0],b2=[0,a(j[66][22],b1),bN],b3=aM(D,y),b4=a(i[ab],b3),b5=[0,d(j[66][21],b4,b2),bK],b6=aM(D,T),b7=[0,a(i[ab],b6),0],b8=[0,a(i[25],[0,s,0]),b7],b9=[0,a(i[75],[0,k,[0,s,0]]),b8],b$=a(h[10],s),aq=f(bA),b_=0,ca=[0,am,ao,a(h[10],k),b$],cb=g===aq?bA[1]:c===aq?a(e[2],bA):bA,cc=[0,u([0,a(h[21],[0,cb,ca]),b_]),b9],cd=[0,a(i[25],[0,k,0]),cc],ce=a(j[66][22],cd),cf=[0,d(j[66][21],ce,b5),bx],cg=a(i[ab],bs);return d(j[66][21],cg,cf);case
1:var
F=b[2],H=b[1],ar=d(l[26],H[4],F),ch=d(E[14],ar,F),ci=d(E[13],H[4],ch),cj=H[3],ck=function(a){return d(l[9],a,F)},cl=d(l[43],ck,cj),as=[0,H[1],0,cl,ar],cm=z(M(as)),av=G(F),U=G(ci),cn=as[3],co=a(cY(kQ),cn),cp=[0,i[61],[0,i[N],0]],cq=[0,O(aL),cp],cr=[0,a(j[66][22],cq),0],cs=[0,i[61],[0,i[N],0]],ct=[0,O(aL),cs],cu=[0,a(j[66][22],ct),0],cv=[0,i[41],0],cw=[0,Q(co),cv],cx=[0,bd(k),cw],cy=[0,a(i[25],[0,k,0]),cx],cz=[0,O(cQ),cy],cB=[0,a(i[75],[0,m,[0,n,0]]),cz],cE=a(h[10],n),aw=f(bD),cD=0,cF=[0,U,av,cm,a(h[10],m),cE],cG=g===aw?bD[1]:c===aw?a(e[2],bD):bD,cH=[0,u([0,a(h[21],[0,cG,cF]),cD]),cB],cI=[0,a(i[25],[0,n,[0,m,0]]),cH],cJ=[0,a(j[66][22],cI),cu],cK=aM(av,U),cL=a(i[ab],cK),cM=[0,d(j[66][21],cL,cJ),cr],cN=aM(U,y),cO=a(i[ab],cN);return d(j[66][21],cO,cM);case
3:var
ax=p[2],ay=b[2],K=b[1],v=J(K[1]),cP=function(a){return d(l[9],a,ay)},V=d(l[44],cP,K),X=z(M(K)),Z=z(M(V)),L=G(ay),az=cC(X,cR(Z,L));if(2===K[2]){var
cS=V[3],cT=a(cX(kR),cS),cU=i[N],cV=Q(cT),cZ=[0,d(j[66][3],cV,cU),0],c0=[0,o(ax),0],c1=[0,a(i[25],[0,v,0]),c0],c2=[0,a(i[75],[0,m,[0,v,0]]),c1],c4=a(h[10],v),aA=f(bP),c3=0,c5=[0,X,Z,L,a(h[10],m),c4],c6=g===aA?bP[1]:c===aA?a(e[2],bP):bP,c7=[0,u([0,a(h[21],[0,c6,c5]),c3]),c2],c8=[0,a(i[25],[0,m,0]),c7],c9=[0,a(j[66][22],c8),cZ],c_=a(i[ab],az);return d(j[66][21],c_,c9)}var
c$=V[3],da=a(cX(kS),c$),db=i[N],dc=Q(da),dd=[0,d(j[66][3],dc,db),0],de=[0,i[61],[0,i[N],0]],df=[0,O(aL),de],dg=[0,a(j[66][22],df),0],dh=[0,o(ax),0],dj=[0,a(i[25],[0,v,0]),dh],dk=[0,a(i[75],[0,m,[0,n,[0,v,0]]]),dj],dn=a(h[10],v),dp=a(h[10],m),aB=f(bC),dm=0,dq=[0,X,Z,L,a(h[10],n),dp,dn],dr=g===aB?bC[1]:c===aB?a(e[2],bC):bC,ds=[0,u([0,a(h[21],[0,dr,dq]),dm]),dk],dt=[0,a(i[25],[0,n,[0,m,0]]),ds],dv=[0,a(j[66][22],dt),dg],dw=aM(L,y),dx=a(i[ab],dw),dy=[0,d(j[66][21],dx,dv),dd],dz=a(i[ab],az);return d(j[66][21],dz,dy);case
4:var
aC=p[2],aD=b[3],B=aD[2],P=aD[1],aE=b[2],w=aE[2],_=aE[1],dA=b[1],$=ag(0);aK($,dA);var
aF=J(w[1]),aG=J(B[1]),aH=z(M(w)),aI=z(M(B));if(d(l[1],_,l[12]))if(0===B[2]){switch(w[2]){case
0:var
aJ=f(bE),dB=g===aJ?bE[1]:c===aJ?a(e[2],bE):bE,aa=dB;break;case
1:var
aO=f(bF),dL=g===aO?bF[1]:c===aO?a(e[2],bF):bF,aa=dL;break;default:var
aP=f(bR),dM=g===aP?bR[1]:c===aP?a(e[2],bR):bR,aa=dM}var
dC=G(P),dD=2===w[2]?kT:kU,dE=cW(dD,w[3],P,B[3]),dF=[0,o(aC),0],dG=[0,a(i[25],[0,$,0]),dF],dH=[0,Q(dE),dG],dI=a(h[10],aG),dJ=[0,aa,[0,aH,aI,dC,a(h[10],aF),dI]],dK=[0,u([0,a(h[21],dJ),0]),dH];return a(j[66][22],dK)}var
aQ=G(_),aR=G(P),dN=ep(kV,_,w[3],P,B[3]),dO=[0,i[61],[0,i[N],0]],dQ=[0,O(aL),dO],dR=[0,a(j[66][22],dQ),0],dS=[0,i[61],[0,i[N],0]],dT=[0,O(aL),dS],dU=[0,a(j[66][22],dT),0],dV=[0,o(aC),0],dW=[0,a(i[25],[0,$,0]),dV],dX=[0,Q(dN),dW],dY=[0,a(i[75],[0,m,[0,n,0]]),dX],d0=a(h[10],aG),d1=a(h[10],aF),d2=a(h[10],n),aT=f(bG),dZ=0,d3=[0,aH,aI,aQ,aR,a(h[10],m),d2,d1,d0],d4=g===aT?bG[1]:c===aT?a(e[2],bG):bG,d5=[0,u([0,a(h[21],[0,d4,d3]),dZ]),dY],d6=[0,a(i[25],[0,n,[0,m,0]]),d5],d7=[0,a(j[66][22],d6),dU],d8=aM(aR,y),d9=a(i[ab],d8),d_=[0,d(j[66][21],d9,d7),dR],d$=aM(aQ,y),ea=a(i[ab],d$);return d(j[66][21],ea,d_);case
5:var
C=b[1],aU=C[5],aV=C[4],ac=C[3],aW=C[2],eb=p[2],ec=C[1],aX=ag(0),ed=J(ac[1]);aK(aX,ec[1]);var
ad=z(M(aW)),ee=z(M(ac)),af=di(aU),aY=f(x),ef=cC(a(h[9],1),ad),eg=g===aY?x[1]:c===aY?a(e[2],x):x,aZ=f(x),eh=a(h[19],[0,[0,af],eg,ef]),ei=g===aZ?x[1]:c===aZ?a(e[2],x):x,a0=f(cA),ej=[0,ei,eh],ek=g===a0?cA[1]:c===a0?a(e[2],cA):cA,el=a(h[21],[0,ek,ej]),em=G(aV),a1=f(W),en=cW(er,ac[3],aV,[0,[0,l[14],aU],aW[3]]),eo=g===a1?W[1]:c===a1?a(e[2],W):W,es=[0,r([0,kZ,[0,kY,[0,kX,er]]],kW,eo),en],et=i[N],eu=dP(ad),ev=[0,d(j[66][3],eu,et),0],ew=[0,o(eb),0],ex=[0,a(i[25],[0,aX,0]),ew],ey=[0,a(i[75],[0,k,0]),ex],ez=[0,Q(es),ey],eB=a(h[10],k),eC=a(h[10],ed),a2=f(bI),eA=0,eD=[0,a(h[10],af),ee,ad,em,eC,eB],eE=g===a2?bI[1]:c===a2?a(e[2],bI):bI,eF=[0,u([0,a(h[21],[0,eE,eD]),eA]),ez],eG=[0,a(i[25],[0,af,[0,k,0]]),eF],eH=[0,a(i[75],[0,k,0]),eG],eI=[0,aS(k),eH],eJ=[0,a(i[25],[0,k,0]),eI],eK=[0,a(j[66][22],eJ),ev],eL=a(i[ab],el);return d(j[66][21],eL,eK);case
6:var
a3=p[2],eM=b[1];try{var
eN=o(a3),eO=J(eM[1]),eP=d(A[1][13][3],eO,bn),eQ=d(j[66][3],eP,eN);return eQ}catch(a){a=t(a);if(a===I){var
p=a3;continue}throw a}case
9:var
a4=b[2],ah=b[1],eR=M(ah),eS=M(a4),a5=f(au),eT=du(k0,ah[3]),eU=g===a5?au[1]:c===a5?a(e[2],au):au,a6=f(au),eV=g===a6?au[1]:c===a6?a(e[2],au):au,a7=f(at),eW=g===a7?at[1]:c===a7?a(e[2],at):at,a8=f(Y),eX=[0,eW,eV,eU],eY=g===a8?Y[1]:c===a8?a(e[2],Y):Y,eZ=a(h[21],[0,eY,eX]),e0=[0,i[41],[0,i[N],0]],e1=[0,a(k1[1],eZ),0],e2=[0,i[61],[0,i[16],e1]],e3=[0,O(dl),e2],e4=a(j[66][22],e3),e5=d(j[66][21],e4,e0),e6=J(a4[1]),e7=a(h[10],e6),e8=J(ah[1]),e9=a(h[10],e8),e_=z(eS),a9=f(bB),e$=[0,z(eR),e_,e9,e7],fa=g===a9?bB[1]:c===a9?a(e[2],bB):bB,fb=a(h[21],[0,fa,e$]),fc=Q(eT),fd=u([0,fb,0]),fe=d(j[66][3],fd,fc);return d(q[18],fe,e5);case
10:var
ai=b[2],aj=b[1],ff=b[3],fg=M(ai),fh=M(aj),fi=J(ai[1]),fj=J(aj[1]),a_=ff?l[14]:l[12],fk=cW(k2,ai[3],a_,aj[3]),fl=[0,i[N],0],fm=[0,bd(k),fl],fn=[0,a(i[25],[0,k,0]),fm],fo=[0,Q(fk),fn],fq=a(h[10],fj),fr=a(h[10],fi),fs=G(a_),ft=z(fh),a$=f(bO),fp=0,fu=[0,z(fg),ft,fs,fr,fq],fv=g===a$?bO[1]:c===a$?a(e[2],bO):bO,fw=[0,u([0,a(h[21],[0,fv,fu]),fp]),fo];return a(j[66][22],fw);case
11:var
R=b[2],fx=p[2],fy=b[3],fz=b[1],ba=ag(0);aK(ba,fz);var
bb=J(R[1]),bc=J(fy),be=z(M(R)),bf=z(M(a(l[45],R))),fA=R[3],bg=f(W),fB=a(cX(k3),fA),fC=g===bg?W[1]:c===bg?a(e[2],W):W,fD=[0,r(k5,k4,fC),fB],fE=i[N],fF=Q(fD),fG=[0,d(j[66][3],fF,fE),0],fH=[0,o(fx),0],fI=[0,a(i[25],[0,ba,0]),fH],fJ=[0,a(i[75],[0,bb,[0,bc,[0,k,0]]]),fI],fL=a(h[10],k),fM=a(h[10],bc),bh=f(bH),fK=0,fN=[0,be,bf,a(h[10],bb),fM,fL],fO=g===bh?bH[1]:c===bh?a(e[2],bH):bH,fP=[0,u([0,a(h[21],[0,fO,fN]),fK]),fJ],fQ=[0,a(i[25],[0,k,0]),fP],fR=[0,a(j[66][22],fQ),fG],fS=cC(be,aN(bf)),fT=a(i[ab],fS);return d(j[66][21],fT,fR);case
12:var
fU=k6[15],fV=J(b[1]),fW=u([0,a(h[10],fV),0]);return d(j[66][3],fW,fU);case
13:var
fX=i[N],fY=bd(J(b[1]));return d(j[66][3],fY,fX);case
14:var
fZ=b[1],f0=[0,i[N],0],f1=[0,bd(k),f0],f2=[0,a(i[25],[0,k,0]),f1],f3=[0,O(cQ),f2],f4=[0,i[61],f3],f5=[0,O(dl),f4],f6=J(fZ),f7=[0,u([0,a(h[10],f6),0]),f5];return a(j[66][22],f7);case
15:var
bi=b[3],bj=b[2],S=b[1],f8=bi[2],f9=bi[1],f_=bj[2],f$=bj[1],bk=ag(0),bl=ag(0);aK(bk,f$);aK(bl,f9);var
ga=J(S[1]),gb=S[3],gc=a(eq(k7),gb),gd=S[3],ge=a(cY(k8),gd),gf=z(M(S)),gg=[0,o(f8),0],gh=[0,a(i[25],[0,bl,0]),gg],gi=[0,Q(ge),gh],gj=[0,a(j[66][22],gi),0],gk=[0,o(f_),0],gl=[0,a(i[25],[0,bk,0]),gk],gm=[0,Q(gc),gl],gn=[0,a(j[66][22],gm),gj],bm=f(bQ),go=[0,gf,[0,a(h[10],ga),0]],gp=g===bm?bQ[1]:c===bm?a(e[2],bQ):bQ,gq=a(h[34],[0,gp,go]),gr=a(i[99],gq);return d(j[66][21],gr,gn)}}return a(q[16],0)}}return o}function
es(g,a,f){var
b=an(g,a,f),h=b[1],c=a$(a,b[2]),i=c[1],e=c0(a,c[2]),j=e[2],k=d(o[18],i,e[1]);return[0,d(o[18],h,k),j]}function
aV(t,e,s,r,q,p,n,m,c){var
f=c[2],g=c[1],k=es(t,[0,[0,q],k9],p),l=k[1],v=k[2],w=a(i[75],[0,e,0]),x=a(j[66][24],w),y=[0,r,[0,n,m,a(h[10],e)]],z=u([0,a(h[21],y),0]),A=d(j[66][3],z,x);if(a(o[17][48],l))return[0,g,f];var
b=ag(0),B=[0,a(el(b,s),v),f],C=[0,a(i[25],[0,b,0]),0],D=[0,A,[0,Q(l),C]];return[0,[0,[0,b,a(j[66][22],D)],g],B]}function
et(aa,j,h,F){var
k=F[1],ab=F[2];if(D.caml_string_equal(a(a1[3],k),k_))return h;try{var
i=aU(j,ab);if(typeof
i==="number")var
d=0;else
if(1===i[0]){var
p=i[1];if(typeof
p==="number")if(16<=p){switch(p+da|0){case
0:var
q=i[2];if(q){var
r=q[2];if(r){var
s=r[2];if(s)if(s[2])var
d=0,b=0;else{var
I=s[1],J=r[1],o=a7(aa,j,q[1]);if(typeof
o==="number")var
n=0;else
if(1===o[0]){var
M=o[1];if(typeof
M==="number")if(23===M)if(o[2])var
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
L=f(b3),ac=ae(J,aN(I)),ad=2,af=g===L?b3[1]:c===L?a(e[2],b3):b3,m=aV(j,k,0,af,ad,ac,J,I,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0}else
var
d=0,b=0}else
var
d=0,b=0;break;case
2:var
u=i[2];if(u){var
v=u[2];if(v)if(v[2])var
d=0,b=0;else
var
N=v[1],O=u[1],P=f(b4),ag=ae(O,aN(N)),ah=1,ai=g===P?b4[1]:c===P?a(e[2],b4):b4,m=aV(j,k,2,ai,ah,ag,O,N,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
3:var
w=i[2];if(w){var
x=w[2];if(x)if(x[2])var
d=0,b=0;else
var
Q=x[1],R=w[1],S=f(b8),aj=ae(Q,aN(R)),ak=2,al=g===S?b8[1]:c===S?a(e[2],b8):b8,m=aV(j,k,1,al,ak,aj,R,Q,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
4:var
y=i[2];if(y){var
z=y[2];if(z)if(z[2])var
d=0,b=0;else
var
T=z[1],U=y[1],am=aN(U),V=f(b5),an=ae(ae(T,G(l[14])),am),ao=2,ap=g===V?b5[1]:c===V?a(e[2],b5):b5,m=aV(j,k,1,ap,ao,an,U,T,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
5:var
A=i[2];if(A){var
B=A[2];if(B)if(B[2])var
d=0,b=0;else
var
W=B[1],X=A[1],Y=f(b6),aq=ae(X,aN(W)),ar=2,as=g===Y?b6[1]:c===Y?a(e[2],b6):b6,m=aV(j,k,1,as,ar,aq,X,W,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
6:var
C=i[2];if(C){var
E=C[2];if(E)if(E[2])var
d=0,b=0;else
var
Z=E[1],_=C[1],at=aN(Z),$=f(b7),au=ae(ae(_,G(l[14])),at),av=2,aw=g===$?b7[1]:c===$?a(e[2],b7):b7,m=aV(j,k,1,aw,av,au,_,Z,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;default:var
d=0,b=0}if(b)var
H=m,d=1}else
var
d=0;else
var
d=0}else
var
d=0;if(!d)var
H=h;return H}catch(b){b=t(b);if(a(aQ[5],b))return h;throw b}}function
aW(b){var
c=a(i[22],b),e=a(i[75],[0,b,0]),f=a(j[66][24],e);return d(j[66][3],f,c)}function
k$(k){dk(0);var
w=a(H[42][14],k),x=d(H[42][1],et,k),m=n(o[17][15],x,la,w),p=m[1],y=m[2],z=dS(0),A=[0,a(q[16],0),0];function
B(n,m){var
d=m[2],o=d[2],k=d[1],p=m[1],q=n[2],r=n[1];if(d[3]){var
b=ag(0),s=bh(0);aK(b,s);var
v=l[11],w=cO(k),x=[0,[0,s,1,[0,[0,l[12],w],0],v],q],y=[0,a(i[25],[0,o,[0,b,0]]),[0,r,0]],z=[0,a(i[75],[0,b,0]),y],A=[0,aS(b),z],t=f(b_),B=[0,a(i[25],[0,k,[0,b,0]]),A],C=[0,p,0],D=g===t?b_[1]:c===t?a(e[2],b_):b_,E=a(h[34],[0,D,C]),F=[0,a(i[99],E),B];return[0,a(j[66][22],F),x]}var
u=f(b9),G=[0,a(i[25],[0,k,[0,o,0]]),[0,r,0]],H=[0,p,0],I=g===u?b9[1]:c===u?a(e[2],b9):b9,J=a(h[34],[0,I,H]),K=[0,a(i[99],J),G];return[0,a(j[66][22],K),q]}var
r=n(o[17][15],B,A,z),s=r[1],b=d(o[18],y,r[2]);if(cK[1])d(l[33],a2,b);if(bf[1])try{n(l[64],[0,bh,df,a2],0,b);var
E=a(q[16],0);return E}catch(b){b=t(b);if(b===l[27]){var
C=a(l[39],0),u=n(l[65],0,0,C)[2];if(be[1])d(l[36],a2,u);var
D=a(dw(p),u);return d(j[66][3],s,D)}throw b}try{var
v=d(l[68],[0,bh,df,a2],b);if(be[1])d(l[36],a2,v);var
G=a(dw(p),v),I=d(j[66][3],s,G);return I}catch(b){b=t(b);if(b===l[28]){var
F=a(am[3],lb);return d(j[66][5],0,F)}throw b}}var
eu=a(q[67][9],k$);function
lc(b){var
m=dp[80];function
n(a){return d(m,0,a)}var
aa=d(H[42][1],n,b);function
k(n,L){function
b(w){try{var
o=cE(w,L);if(typeof
o==="number")var
m=0;else
if(1===o[0]){var
x=o[1];if(typeof
x==="number")if(12<=x)var
m=0;else{switch(x){case
6:var
y=o[2];if(y){var
z=y[2];if(z)if(z[2])var
m=0,b=0;else
var
A=z[1],B=y[1],ab=[0,k([0,ld,n],A),0],N=f(bl),ac=[0,k([0,le,n],B),ab],ad=[0,B,[0,A,0]],af=g===N?bl[1]:c===N?a(e[2],bl):bl,ah=aO(A),aj=[0,a8(n,ae(aO(B),ah),[0,af,ad]),ac],u=a(j[66][22],aj),b=1;else
var
m=0,b=0}else
var
m=0,b=0;break;case
7:var
C=o[2];if(C){var
D=C[2];if(D)if(D[2])var
m=0,b=0;else
var
E=D[1],F=C[1],ak=[0,k([0,lf,n],E),0],O=f(bm),al=[0,k([0,lg,n],F),ak],am=[0,F,[0,E,0]],an=g===O?bm[1]:c===O?a(e[2],bm):bm,ao=aO(E),ap=[0,a8(n,cR(aO(F),ao),[0,an,am]),al],u=a(j[66][22],ap),b=1;else
var
m=0,b=0}else
var
m=0,b=0;break;case
8:var
H=o[2];if(H){var
I=H[2];if(I)if(I[2])var
m=0,b=0;else
var
r=I[1],s=H[1],v=ag(0),P=f(aB),aq=0,ar=0,as=[0,r,s],at=g===P?aB[1]:c===P?a(e[2],aB):aB,au=p([0,[0,v,a(h[21],[0,at,as])],ar]),Q=f(bo),av=[0,s,[0,r,[0,a(h[10],v),0]]],aw=g===Q?bo[1]:c===Q?a(e[2],bo):bo,ax=a8(n,G(l[11]),[0,aw,av]),ay=[0,d(j[66][3],ax,au),aq],az=[0,k([0,lh,n],r),0],R=f(aA),aF=[0,k([0,li,n],s),az],aG=0,aH=[0,r,s],aI=g===R?aA[1]:c===R?a(e[2],aA):aA,aJ=[0,p([0,[0,v,a(h[21],[0,aI,aH])],aG]),aF],S=f(bn),aK=[0,s,[0,r,[0,a(h[10],v),0]]],aL=g===S?bn[1]:c===S?a(e[2],bn):bn,aM=aO(r),aN=[0,a8(n,eh(aO(s),aM),[0,aL,aK]),aJ],aP=[0,a(j[66][22],aN),ay],T=f(ch),aR=a(i[25],[0,v,0]),aS=[0,r,[0,s,0]],aT=g===T?ch[1]:c===T?a(e[2],ch):ch,aU=a(h[34],[0,aT,aS]),aV=a(i[99],aU),aW=d(j[66][3],aV,aR),u=d(j[66][21],aW,aP),b=1;else
var
m=0,b=0}else
var
m=0,b=0;break;case
9:var
J=o[2];if(J)if(J[2])var
m=0,b=0;else
var
U=J[1],V=f(aE),aX=g===V?aE[1]:c===V?a(e[2],aE):aE,W=f(aD),aY=[0,aX],aZ=g===W?aD[1]:c===W?a(e[2],aD):aD,X=f(aC),a0=[0,U,a(h[21],[0,aZ,aY])],a1=g===X?aC[1]:c===X?a(e[2],aC):aC,Y=a(h[21],[0,a1,a0]),_=f(cg),a2=k(n,Y),a3=[0,U,0],a4=g===_?cg[1]:c===_?a(e[2],cg):cg,a5=eo([0,lj,n],Y,[0,a4,a3]),u=d(j[66][3],a5,a2),b=1;else
var
m=0,b=0;break;case
10:var
K=o[2];if(K)if(K[2])var
m=0,b=0;else
var
a6=K[1],$=function(i){try{var
d=cE(w,i);if(typeof
d==="number")var
b=0;else
if(1===d[0]){var
e=d[1];if(typeof
e==="number"){if(10===e){var
f=d[2];if(f)if(f[2])var
b=0,c=0;else
var
h=$(f[1]),c=1;else
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
g=0;return g}catch(b){b=t(b);if(a(aQ[5],b))return 0;throw b}},aa=function(i,m){try{var
l=cE(w,m);if(typeof
l==="number")var
b=0;else
if(1===l[0]){var
q=l[1];if(typeof
q==="number")if(10===q){var
n=l[2];if(n)if(n[2])var
b=0;else
var
o=n[1],r=f(bp),u=aa([0,lk,i],o),v=[0,o,0],x=g===r?bp[1]:c===r?a(e[2],bp):bp,s=f(ai),y=[0,x,v],z=[0,aO(o)],A=g===s?ai[1]:c===s?a(e[2],ai):ai,B=a8(i,a(h[21],[0,A,z]),y),p=d(j[66][3],B,u),b=1;else
var
b=0}else
var
b=0;else
var
b=0}else
var
b=0;if(!b)var
p=k(i,m);return p}catch(b){b=t(b);if(a(aQ[5],b))return k(i,m);throw b}},a7=$(a6)?Z(n):aa(n,L),u=a7,b=1;else
var
m=0,b=0;break;case
11:if(o[2])var
m=0,b=0;else
var
u=Z(n),b=1;break;default:var
m=0,b=0}if(b)var
M=u,m=1}else
var
m=0}else
var
m=0;if(!m)var
M=a(q[16],0);return M}catch(b){b=t(b);if(a(aQ[5],b))return a(q[16],0);throw b}}return d(q[72][1],q[55],b)}function
p(b){if(b){var
m=b[2],i=b[1],l=i[1],ab=i[2],n=function(ac){try{var
n=aU(ac,ab);if(typeof
n==="number")var
i=0;else
if(1===n[0]){var
q=n[1];if(typeof
q==="number")if(16<=q){switch(q+da|0){case
0:var
r=n[2];if(r){var
s=r[2];if(s){var
v=s[2];if(v)if(v[2])var
i=0,b=0;else{var
H=v[1],I=s[1],J=f(X),ad=r[1],ae=g===J?X[1]:c===J?a(e[2],X):X;if(d(aa,ad,ae))var
af=[0,p(m),0],ag=[0,aW(l),af],ah=[0,k(ll,H),ag],ai=[0,k(lm,I),ah],K=f(bv),aj=0,ak=[0,I,H,a(h[10],l)],al=g===K?bv[1]:c===K?a(e[2],bv):bv,am=[0,u([0,a(h[21],[0,al,ak]),aj]),ai],L=a(j[66][22],am);else
var
L=p(m);var
o=L,b=1}else
var
i=0,b=0}else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
w=n[2];if(w){var
x=w[2];if(x)if(x[2])var
i=0,b=0;else
var
M=x[1],N=w[1],an=[0,p(m),0],ao=[0,aW(l),an],ap=[0,k(ln,M),ao],aq=[0,k(lo,N),ap],O=f(bu),ar=0,as=[0,N,M,a(h[10],l)],at=g===O?bu[1]:c===O?a(e[2],bu):bu,au=[0,u([0,a(h[21],[0,at,as]),ar]),aq],o=a(j[66][22],au),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
15:var
y=n[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else
var
P=z[1],Q=y[1],av=[0,p(m),0],aw=[0,aW(l),av],ax=[0,k(lp,P),aw],ay=[0,k(lq,Q),ax],R=f(bq),az=0,aA=[0,Q,P,a(h[10],l)],aB=g===R?bq[1]:c===R?a(e[2],bq):bq,aC=[0,u([0,a(h[21],[0,aB,aA]),az]),ay],o=a(j[66][22],aC),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
16:var
A=n[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else
var
S=B[1],T=A[1],aD=[0,p(m),0],aE=[0,aW(l),aD],aF=[0,k(lr,S),aE],aG=[0,k(ls,T),aF],U=f(br),aH=0,aI=[0,T,S,a(h[10],l)],aJ=g===U?br[1]:c===U?a(e[2],br):br,aK=[0,u([0,a(h[21],[0,aJ,aI]),aH]),aG],o=a(j[66][22],aK),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
17:var
C=n[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
V=D[1],W=C[1],aL=[0,p(m),0],aM=[0,aW(l),aL],aN=[0,k(lt,V),aM],aO=[0,k(lu,W),aN],Y=f(bs),aP=0,aR=[0,W,V,a(h[10],l)],aS=g===Y?bs[1]:c===Y?a(e[2],bs):bs,aT=[0,u([0,a(h[21],[0,aS,aR]),aP]),aO],o=a(j[66][22],aT),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
18:var
E=n[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
Z=F[1],_=E[1],aV=[0,p(m),0],aX=[0,aW(l),aV],aY=[0,k(lv,Z),aX],aZ=[0,k(lw,_),aY],$=f(bt),a0=0,a1=[0,_,Z,a(h[10],l)],a2=g===$?bt[1]:c===$?a(e[2],bt):bt,a3=[0,u([0,a(h[21],[0,a2,a1]),a0]),aZ],o=a(j[66][22],a3),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;default:var
i=0,b=0}if(b)var
G=o,i=1}else
var
i=0;else
var
i=0}else
var
i=0;if(!i)var
G=p(m);return G}catch(b){b=t(b);if(a(aQ[5],b))return p(m);throw b}};return d(q[72][1],q[55],n)}return a(q[16],0)}var
r=a(H[42][14],b);return p(a(o[17][9],r))}var
ev=a(q[67][9],lc);function
ew(a){if(typeof
a==="number")if(18<=a)switch(a+dz|0){case
0:return dY;case
1:return dZ;case
2:return d0;case
3:return d2;case
4:return d1;case
13:return d8;case
14:return d9;case
15:return d_;case
16:return d$}throw I}function
ex(a){if(typeof
a==="number")if(18<=a)switch(a+dz|0){case
0:return d3;case
1:return d4;case
2:return d5;case
3:return d6;case
4:return d7;case
13:return ea;case
14:return eb;case
15:return ec;case
16:return ed}throw I}var
ba=[aZ,lx,aX(0)];function
$(j,i,X){var
d=aU(i,X);if(typeof
d!=="number")switch(d[0]){case
1:var
l=d[1];if(typeof
l==="number")if(16<=l)switch(l+da|0){case
0:var
n=d[2];if(n){var
o=n[2];if(o){var
p=o[2];if(p){if(!p[2]){var
B=p[1],C=o[1],k=a7(j,i,n[1]);if(typeof
k!=="number"&&1===k[0]){var
q=k[1];if(typeof
q==="number")if(23===q){if(!k[2]){var
D=f(b$),Y=[0,C,B],Z=g===D?b$[1]:c===D?a(e[2],b$):b$;return a(h[21],[0,Z,Y])}}else
if(24===q)if(!k[2]){var
E=f(ci),_=[0,C,B],aa=g===E?ci[1]:c===E?a(e[2],ci):ci;return a(h[21],[0,aa,_])}}throw ba}var
b=1}else
var
b=0}else
var
b=1}else
var
b=1;break;case
9:var
u=d[2];if(u){var
v=u[2];if(v){if(!v[2]){var
G=v[1],H=u[1],ag=$(j,i,G),J=f(cm),ah=[0,H,G,$(j,i,H),ag],ai=g===J?cm[1]:c===J?a(e[2],cm):cm;return a(h[21],[0,ai,ah])}var
b=1}else
var
b=1}else
var
b=1;break;case
10:var
w=d[2];if(w){var
x=w[2];if(x){if(!x[2]){var
K=x[1],L=w[1],aj=$(j,i,K),M=f(cl),ak=[0,L,K,$(j,i,L),aj],al=g===M?cl[1]:c===M?a(e[2],cl):cl;return a(h[21],[0,al,ak])}var
b=1}else
var
b=1}else
var
b=1;break;case
11:if(!d[2]){var
N=f(cq);return g===N?cq[1]:c===N?a(e[2],cq):cq}var
b=0;break;case
12:if(!d[2]){var
O=f(cs);return g===O?cs[1]:c===O?a(e[2],cs):cs}var
b=0;break;case
13:var
y=d[2];if(y){if(!y[2]){var
P=y[1],Q=f(cp),am=[0,P,$(j,i,P)],an=g===Q?cp[1]:c===Q?a(e[2],cp):cp;return a(h[21],[0,an,am])}var
b=0}else
var
b=1;break;case
14:var
z=d[2];if(z){var
A=z[2];if(A){if(!A[2]){var
R=A[1],S=z[1],ao=$(j,i,R),T=f(co),ap=[0,S,R,$(j,i,S),ao],aq=g===T?co[1]:c===T?a(e[2],co):co;return a(h[21],[0,aq,ap])}var
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
r=d[2];if(r){var
s=r[2];if(s)if(!s[2]){var
ab=s[1],ac=r[1];try{var
m=ew(l),F=f(m),ad=[0,ac,ab],ae=g===F?m[1]:c===F?a(e[2],m):m,af=a(h[21],[0,ae,ad]);return af}catch(a){a=t(a);if(a===I)throw ba;throw a}}}}break;case
2:var
U=d[2],V=d[1],ar=$(j,i,U),W=f(cn),as=[0,V,U,$(j,i,V),ar],at=g===W?cn[1]:c===W?a(e[2],cn):cn;return a(h[21],[0,at,as])}throw ba}function
cH(d,c,b){var
e=a(q[67][4],b);return n(i[13],d,c,e)}function
aa(b,e){function
c(f){var
c=cH(A[1][10][1],b,f),g=a(e,c),h=a(i[2],c);return d(j[66][3],h,g)}var
f=a(q[67][8],c),g=a(i[75],[0,b,0]),h=a(j[66][24],g);return d(j[66][3],h,f)}function
dx(b,g){function
c(c){var
h=d(a1[5],b,ly),e=cH(A[1][10][1],h,c),k=d(a1[5],b,lz),f=cH(A[1][10][1],k,c),l=[0,d(g,e,f),0],m=[0,a(i[2],f),l],n=[0,a(i[2],e),m];return a(j[66][22],n)}var
e=a(q[67][8],c),f=a(i[75],[0,b,0]),h=a(j[66][24],f);return d(j[66][3],h,e)}function
lA(m){var
a2=a(H[42][7],m),D=a(q[67][4],m),k=a(q[67][5],m);function
w(a){return $(D,k,a)}function
b(o){if(o){var
v=o[1];if(1===v[0]){var
p=o[2],r=v[3],s=v[1],y=v[2];if(cL[1]){var
z=function(o){try{var
e=a7(D,o,r);if(typeof
e==="number")var
c=0;else
if(1===e[0]){var
g=e[1];if(typeof
g==="number")if(1<(g-23|0)>>>0)var
c=0;else
var
q=d(a1[5],s,lC),k=cH(A[1][10][1],q,m),l=cS(r,a(h[10],s),y),u=b([0,[0,k,l],p]),v=n(i[139],[0,k],l,i[N]),f=d(j[66][3],v,u),c=1;else
var
c=0}else
var
c=0;if(!c)var
f=b(p);return f}catch(c){c=t(c);if(a(aQ[5],c))return b(p);throw c}};return d(q[72][1],q[55],z)}}var
l=o[2],k=a(c1[2][1][1],v),x=function(y){try{var
r=aU(y,a(c1[2][1][3],v));if(typeof
r==="number")var
m=0;else
switch(r[0]){case
1:var
M=r[1];if(typeof
M==="number")if(18<=M){switch(M+dz|0){case
7:var
N=r[2];if(N){var
O=N[2];if(O)if(O[2])var
m=0,n=0;else
var
a1=O[1],a3=N[1],a4=dx(k,function(c,a){return b([0,[0,c,a3],[0,[0,a,a1],l]])}),a5=aS(k),z=d(j[66][3],a5,a4),n=1;else
var
m=0,n=0}else
var
m=0,n=0;break;case
8:var
P=r[2];if(P){var
Q=P[2];if(Q)if(Q[2])var
m=0,n=0;else
var
a6=Q[1],a8=P[1],a9=0,a_=[0,aa(k,function(a){return b([0,[0,a,a6],l])}),a9],a$=[0,aa(k,function(a){return b([0,[0,a,a8],l])}),a_],bb=aS(k),z=d(j[66][21],bb,a$),n=1;else
var
m=0,n=0}else
var
m=0,n=0;break;case
9:if(r[2])var
m=0,n=0;else
var
z=aS(k),n=1;break;case
11:var
R=r[2];if(R)if(R[2])var
m=0,n=0;else{var
s=aU(y,R[1]);if(typeof
s==="number")var
p=0;else
switch(s[0]){case
1:var
E=s[1];if(typeof
E==="number")if(16<=E){switch(E+da|0){case
0:var
T=s[2];if(T){var
U=T[2];if(U){var
V=U[2];if(V)if(V[2])var
p=0,q=0,o=0;else{var
F=V[1],G=U[1],av=T[1];if(bf[1]){var
W=a7(D,y,av);if(typeof
W==="number")var
B=0;else
if(1===W[0]){var
X=W[1];if(typeof
X==="number"){if(23===X)var
bc=0,bd=[0,aa(k,function(a){return b(l)}),bc],aB=f(ca),be=[0,G,F,a(h[10],k)],bg=g===aB?ca[1]:c===aB?a(e[2],ca):ca,bh=a(h[21],[0,bg,be]),bi=[0,a(i[99],bh),bd],aC=a(j[66][22],bi),ap=1;else
if(24===X)var
bj=0,bk=[0,aa(k,function(a){return b(l)}),bj],aD=f(cj),bl=[0,G,F,a(h[10],k)],bm=g===aD?cj[1]:c===aD?a(e[2],cj):cj,bn=a(h[21],[0,bm,bl]),bo=[0,a(i[99],bn),bk],aC=a(j[66][22],bo),ap=1;else
var
B=0,ap=0;if(ap)var
aw=aC,B=1}else
var
B=0}else
var
B=0;if(!B)var
aw=b(l);var
ax=aw}else{var
Y=a7(D,y,av);if(typeof
Y==="number")var
C=0;else
if(1===Y[0]){var
Z=Y[1];if(typeof
Z==="number"){if(23===Z)var
aF=f(az),bp=b(l),bq=[0,G,F],br=g===aF?az[1]:c===aF?a(e[2],az):az,bs=a(h[21],[0,br,bq]),bt=d(c1[2][1][5],bs,v),bu=a(i[6],bt),aG=d(j[66][3],bu,bp),aq=1;else
if(24===Z)var
aH=f(ay),bv=b(l),bw=[0,G,F],bx=g===aH?ay[1]:c===aH?a(e[2],ay):ay,by=a(h[21],[0,bx,bw]),bz=d(c1[2][1][5],by,v),bA=a(i[6],bz),aG=d(j[66][3],bA,bv),aq=1;else
var
C=0,aq=0;if(aq)var
aE=aG,C=1}else
var
C=0}else
var
C=0;if(!C)var
aE=b(l);var
ax=aE}var
A=ax,o=1}else
var
q=1,o=0}else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;case
9:var
ab=s[2];if(ab){var
ac=ab[2];if(ac)if(ac[2])var
p=0,q=0,o=0;else
var
aL=ac[1],ad=ab[1],bK=w(ad),bL=0,bM=[0,aa(k,function(a){var
c=aJ(aL);return b([0,[0,a,cT(aJ(ad),c)],l])}),bL],aM=f(cu),bN=0,bO=[0,ad,aL,bK,a(h[10],k)],bP=g===aM?cu[1]:c===aM?a(e[2],cu):cu,bQ=[0,u([0,a(h[21],[0,bP,bO]),bN]),bM],A=a(j[66][22],bQ),o=1;else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;case
10:var
ae=s[2];if(ae){var
af=ae[2];if(af)if(af[2])var
p=0,q=0,o=0;else
var
aN=af[1],aO=ae[1],bR=0,bS=[0,aa(k,function(a){var
c=aJ(aN);return b([0,[0,a,cD(aJ(aO),c)],l])}),bR],aP=f(ct),bT=0,bU=[0,aO,aN,a(h[10],k)],bV=g===aP?ct[1]:c===aP?a(e[2],ct):ct,bW=[0,u([0,a(h[21],[0,bV,bU]),bT]),bS],A=a(j[66][22],bW),o=1;else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;case
13:var
ag=s[2];if(ag)if(ag[2])var
q=1,o=0;else
var
ah=ag[1],bX=w(ah),bY=0,bZ=[0,aa(k,function(a){return b([0,[0,a,ah],l])}),bY],aR=f(cx),b0=0,b1=[0,ah,bX,a(h[10],k)],b2=g===aR?cx[1]:c===aR?a(e[2],cx):cx,b3=[0,u([0,a(h[21],[0,b2,b1]),b0]),bZ],A=a(j[66][22],b3),o=1;else
var
p=0,q=0,o=0;break;case
14:var
ai=s[2];if(ai){var
aj=ai[2];if(aj)if(aj[2])var
p=0,q=0,o=0;else
var
J=aj[1],K=ai[1],b4=w(K),b5=w(J),b6=0,b7=[0,aa(k,function(a){var
c=cD(aJ(K),J);return b([0,[0,a,cT(cD(K,aJ(J)),c)],l])}),b6],aT=f(cw),b8=0,b9=[0,K,J,b4,b5,a(h[10],k)],b_=g===aT?cw[1]:c===aT?a(e[2],cw):cw,b$=[0,u([0,a(h[21],[0,b_,b9]),b8]),b7],A=a(j[66][22],b$),o=1;else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;default:var
q=1,o=0}if(o)var
aA=A,q=2}else
var
q=1;else
var
q=1;switch(q){case
0:var
x=0;break;case
1:var
_=s[2];if(_){var
$=_[2];if($)if($[2])var
p=0,x=0;else{var
bB=$[1],bC=_[1];try{var
H=ex(E),bD=0,bE=[0,aa(k,function(a){return b(l)}),bD],aK=f(H),bF=0,bG=[0,bC,bB,a(h[10],k)],bH=g===aK?H[1]:c===aK?a(e[2],H):H,bI=[0,u([0,a(h[21],[0,bH,bG]),bF]),bE],bJ=a(j[66][22],bI),aI=bJ}catch(a){a=t(a);if(a!==I)throw a;var
aI=b(l)}var
aA=aI,x=1}else
var
p=0,x=0}else
var
p=0,x=0;break;default:var
x=1}if(x)var
S=aA,p=1;break;case
2:var
aV=s[2],ak=s[1],cb=w(ak),cc=0,cd=[0,aa(k,function(a){return b([0,[0,a,cD(ak,aJ(aV))],l])}),cc],aW=f(cv),ce=0,cf=[0,ak,aV,cb,a(h[10],k)],cg=g===aW?cv[1]:c===aW?a(e[2],cv):cv,ch=[0,u([0,a(h[21],[0,cg,cf]),ce]),cd],S=a(j[66][22],ch),p=1;break;default:var
p=0}if(!p)var
S=b(l);var
z=S,n=1}else
var
m=0,n=0;break;case
12:var
al=r[2];if(al){var
am=al[2];if(am)if(am[2])var
m=0,n=0;else
var
aX=am[1],aY=al[1],ci=dx(k,function(c,a){var
e=[0,[0,a,d(h[33],aX,aY)],l];return b([0,[0,c,d(h[33],aY,aX)],e])}),ck=aS(k),z=d(j[66][3],ck,ci),n=1;else
var
m=0,n=0}else
var
m=0,n=0;break;case
0:case
1:case
2:case
3:case
4:var
as=r[2];if(as){var
at=as[2];if(at)if(at[2])var
m=0,n=0;else
var
au=b(l),n=2;else
var
m=0,n=0}else
var
m=0,n=0;break;default:var
m=0,n=0}switch(n){case
0:var
ar=0;break;case
1:var
au=z,ar=1;break;default:var
ar=1}if(ar)var
L=au,m=1}else
var
m=0;else
var
m=0;break;case
2:var
an=r[2],ao=r[1],cl=a(a2,an);if(d(lB[e2],y,cl))var
cm=w(ao),cn=0,co=[0,aa(k,function(a){return b([0,[0,a,cT(aJ(ao),an)],l])}),cn],aZ=f(cy),cp=0,cq=[0,ao,an,cm,a(h[10],k)],cr=g===aZ?cy[1]:c===aZ?a(e[2],cy):cy,cs=[0,u([0,a(h[21],[0,cr,cq]),cp]),co],a0=a(j[66][22],cs);else
var
a0=b(l);var
L=a0,m=1;break;default:var
m=0}if(!m)var
L=b(l);return L}catch(c){c=t(c);if(c===ba)return b(l);if(a(aQ[5],c))return b(l);throw c}};return d(q[72][1],q[55],x)}return d(j[66][3],ev,eu)}return b(a(q[67][3],m))}var
c2=a(q[67][9],lA);function
lD(b){var
k=a(q[67][2],b),m=a(q[67][4],b),n=a(q[67][5],b);function
u(a){return $(m,n,a)}function
l(k){function
b(b){function
m(d){var
c=aU(b,k);return a(q[16],c)}function
n(b){if(typeof
b!=="number")switch(b[0]){case
1:var
p=b[1];if(typeof
p==="number"){var
r=p-27|0;if(!(2<r>>>0))switch(r){case
0:if(!b[2])return c2;break;case
1:break;default:var
s=b[2];if(s)if(!s[2]){var
B=i[16],C=O(cQ),D=d(j[66][3],C,B);return d(j[66][3],D,c2)}}}break;case
2:var
E=l(b[2]);return d(j[66][3],i[16],E)}try{var
w=u(k),x=i[16],y=function(d){var
b=f(cr),i=[0,k,w],j=g===b?cr[1]:c===b?a(e[2],cr):cr;return dt(d,a(h[21],[0,j,i]))},z=a(q[67][8],y),A=d(j[66][3],z,x),o=A}catch(b){b=t(b);if(b===ba)var
m=f(aH),v=g===m?aH[1]:c===m?a(e[2],aH):aH,n=a(i[e2],v);else{if(!a(q[71][9],b))throw b;var
n=d(q[21],0,b)}var
o=n}return d(j[66][3],o,c2)}var
o=a(q[71][10],m);return d(q[72][1],o,n)}return d(q[72][1],q[55],b)}return l(k)}var
ey=a(q[67][9],lD);function
lE(b){a(a3[3],lF);dV(0);return ey}var
lG=a(q[16],0),dy=[0,fV,l,aS,bd,fW,cK,be,bf,cL,cM,fX,a0,aT,dF,ag,gh,dJ,bh,df,dN,a2,cO,di,dO,Q,dP,u,gn,O,cP,dQ,aK,gp,J,dR,dU,dT,dk,dS,dV,dW,dX,bk,F,k,ac,P,aq,ar,as,R,S,T,x,at,au,y,K,B,ah,ai,a4,av,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,C,bw,bx,aw,ax,by,bz,bA,bB,bC,bD,bE,bF,bG,bH,bI,bJ,U,L,bK,bL,bM,bN,bO,bP,bQ,bR,bS,bT,bU,bV,bW,V,bX,bY,bZ,b0,b1,W,b2,b3,b4,b5,b6,b7,b8,b9,b_,b$,dY,dZ,d0,d1,d2,ca,d3,d4,d5,d6,d7,ay,az,aj,ak,a5,a6,aA,cb,cc,aB,aC,cd,ce,cf,X,aD,aE,cg,ch,ci,d8,d9,d_,d$,cj,ea,eb,ec,ed,ck,cl,cm,cn,co,cp,cq,cr,cs,ct,cu,cv,cw,cx,cy,cz,al,aF,aG,Y,cA,aH,cB,aI,ee,ef,eg,dl,aL,i1,i3,cQ,i5,ae,cR,eh,cS,cC,i6,aM,aN,cD,cT,aJ,i7,aO,G,aU,ei,a7,cE,ej,cU,ek,dq,Z,aP,_,z,el,M,dr,ds,cV,en,a8,eo,dt,r,a9,a_,ep,cW,du,cF,cX,eq,cY,cZ,an,dv,cG,a$,c0,dw,es,aV,et,aW,eu,ev,ew,ex,ba,$,cH,aa,dx,c2,ey,d(q[72][1],lG,lE)];c_(440,dy,"Omega_plugin.Coq_omega");a(lH[10],c3);function
cI(b){var
c=d(p[17],A[1][6],lI),e=a(A[5][4],c),f=a(A[6][4],b),g=d(A[13][2],[0,e],f),h=a(c4[4][12],g);return a(c4[13][22],h)}function
c5(b){var
c=d(o[17][141],lJ[33],b);function
e(b){if(c$(b,lK)){if(c$(b,lL)){if(c$(b,lM)){if(c$(b,lN)){var
c=d(m[17],lO,b),e=a(am[3],c);return n(ad[6],0,0,e)}return cI(lP)}return cI(lQ)}return cI(lR)}return cI(lS)}var
f=d(p[17],e,c),g=dy[264],h=a(j[66][22],f),i=a(j[66][32],h);return d(j[66][3],i,g)}var
lT=0,lV=[0,[0,lU,function(a){return c5(0)}],lT];eB(c4[10][8],c3,lW,0,0,lV);var
lX=0,l0=[0,[0,lZ,function(a){return c5(lY)}],lX];function
l1(a,b){return c5(d(p[17],A[1][8],a))}var
l7=[0,[0,[0,l6,[0,l5,[1,[0,[5,a(l4[16],l3[7])]],l2,0]]],l1],l0];eB(c4[10][8],c3,l8,0,0,l7);var
ez=[0,c3,cI,c5];c_(446,ez,"Omega_plugin.G_omega");c_(447,[0,dd,dy,ez],"Omega_plugin");return}
