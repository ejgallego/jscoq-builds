(function(md){"use strict";var
eM="not",eN="*",P=123,e1=" 0\n",e_=108,e5="Z.lt",e9="Z.le",db="Equation E",da=" and E",dA=": ",bc=".\n",g=250,eL="Z.succ",eU="(",eZ="Z.pred",e0="not a number",eY="ZArith",eK="+ ",eR="- ",eG="------------------------\n\n",c=246,eJ="Z.sub",eF="tag_hypothesis",aR="omega",eQ="Inequation E",dB="Z",eX="Extension: cannot occur",bb=120,ab=144,eW=")",dC="plugins/omega/coq_omega.ml",e4="N",e8="State",eT="with",eP="omega'",e3="X%d",eO=" subsumes E",dE=" E",ao="Omega",e2=" states ",a1=248,eI="positive",dz=-18,eH="Equations E",dD="nat",e7="Omega: Can't solve a goal with non-linear products",c$=-16,eV="Z.ge",eS="Z.gt",ba="Coq",e6="E%d subsumes E%d.\n",H=md.jsoo_runtime,an=H.caml_check_bound,eD=H.caml_equal,a0=H.caml_fresh_oo_id,b=H.caml_new_string,c6=H.caml_notequal,f=H.caml_obj_tag,c9=H.caml_register_global,c_=H.caml_string_notequal,c8=H.caml_trampoline,c7=H.caml_trampoline_return,t=H.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):H.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):H.caml_call_gen(a,[b,c])}function
n(a,b,c,d){return a.length==3?a(b,c,d):H.caml_call_gen(a,[b,c,d])}function
cL(a,b,c,d,e){return a.length==4?a(b,c,d,e):H.caml_call_gen(a,[b,c,d,e])}function
eE(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):H.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}var
q=H.caml_get_global_data(),mc=[11,b(" + "),[2,0,[12,32,[2,0,[11,b(dE),[4,0,0,0,[11,b(bc),0]]]]]]],er=[0,[0,2],[0,1,0]],dW=[0,b(ba),[0,b("Logic"),[0,b("Decidable"),0]]],aQ=b("omega_plugin"),p=q.List,w=q.Printf,m=q.Pervasives,o=q.Util,I=q.Not_found,ap=q.Int,v=q.Hashtbl,bd=q.Assert_failure,E=q.Names,e=q.CamlinternalLazy,h=q.EConstr,D=q.Bigint,al=q.Pp,ad=q.CErrors,aW=q.Logic,A=q.Tacmach,a3=q.Coqlib,i=q.Tactics,j=q.Tacticals,s=q.Proofview,c3=q.Context,bi=q.Nameops,el=q.Reductionops,dj=q.Tacred,em=q.Refine,dm=q.Nametab,dn=q.Libnames,cO=q.Goptions,eA=q.Stdarg,ez=q.Loc,aZ=q.Ltac_plugin,eB=q.Genarg,dy=q.Mltop,aS=[0,0],fu=[0,[11,b(eQ),[4,0,0,0,[11,b(" is divided by "),[2,0,[11,b(" and the constant coefficient is rounded by substracting "),[2,0,[11,b(bc),0]]]]]]],b("Inequation E%d is divided by %s and the constant coefficient is rounded by substracting %s.\n")],fv=[0,[11,b("Constant in equation E"),[4,0,0,0,[11,b(" is not divisible by the pgcd "),[2,0,[11,b(" of its other coefficients.\n"),0]]]]],b("Constant in equation E%d is not divisible by the pgcd %s of its other coefficients.\n")],fw=[0,[12,69,[4,0,0,0,[11,b(" is trivially satisfiable.\n"),0]]],b("E%d is trivially satisfiable.\n")],fx=[0,[11,b(db),[4,0,0,0,[11,b(" is divided by the pgcd "),[2,0,[11,b(" of its coefficients.\n"),0]]]]],b("Equation E%d is divided by the pgcd %s of its coefficients.\n")],fy=[0,[11,b("We state "),[2,0,[11,b(dE),[4,0,0,0,[11,b(" = "),[2,0,[12,32,[2,0,[11,b(dE),[4,0,0,0,mc]]]]]]]]]],b("We state %s E%d = %s %s E%d + %s %s E%d.\n")],fz=[0,[11,b("We define a new equation E"),[4,0,0,0,[11,b(dA),0]]],b("We define a new equation E%d: ")],fA=b(" 0"),fB=[0,[11,b("We define E"),[4,0,0,0,[11,b(dA),0]]],b("We define E%d: ")],fC=b(e1),fD=[0,[12,69,[4,0,0,0,[11,b(eO),[4,0,0,0,[11,b(bc),0]]]]],b(e6)],fE=[0,[12,69,[4,0,0,0,[11,b(eO),[4,0,0,0,[11,b(bc),0]]]]],b(e6)],fF=[0,[11,b(eH),[4,0,0,0,[11,b(da),[4,0,0,0,[11,b(" imply a contradiction on their constant factors.\n"),0]]]]],b("Equations E%d and E%d imply a contradiction on their constant factors.\n")],fG=[0,[11,b(eH),[4,0,0,0,[11,b(da),[4,0,0,0,[11,b(" state that their body is at the same time equal and different\n"),0]]]]],b("Equations E%d and E%d state that their body is at the same time equal and different\n")],fH=[0,[12,69,[4,0,0,0,[11,b(da),[4,0,0,0,[11,b(" can be merged into E"),[4,0,0,0,[11,b(bc),0]]]]]]],b("E%d and E%d can be merged into E%d.\n")],fI=[0,[11,b(db),[4,0,0,0,[11,b(e2),[2,0,[11,b(" = 0.\n"),0]]]]],b("Equation E%d states %s = 0.\n")],fJ=[0,[11,b(eQ),[4,0,0,0,[11,b(" states 0 != 0.\n"),0]]],b("Inequation E%d states 0 != 0.\n")],fK=[0,[11,b(db),[4,0,0,0,[11,b(e2),[2,0,[11,b(" >= 0.\n"),0]]]]],b("Equation E%d states %s >= 0.\n")],fL=[0,[11,b(db),[4,0,0,0,[11,b(" is split in E"),[4,0,0,0,[11,b(da),[4,0,0,0,[11,b("\n\n"),0]]]]]]],b("Equation E%d is split in E%d and E%d\n\n")],fM=[0,[11,b("To ensure a solution in the dark shadow the equation E"),[4,0,0,0,[11,b(" is weakened by "),[2,0,[11,b(bc),0]]]]],b("To ensure a solution in the dark shadow the equation E%d is weakened by %s.\n")],fW=b("depend"),fZ=b("solve"),fX=[0,b("plugins/omega/omega.ml"),600,15],fV=b("disequation in simplify"),fU=b("Product dardk"),fT=[0,0,0,0],fR=b("TL"),fQ=b("eliminate_with_in"),fN=[0,[12,88,[4,0,0,0,0]],b(e3)],fs=b(">= 0\n"),ft=b(eG),fp=[0,[12,69,[4,0,0,0,[11,b(dA),0]]],b("E%d: ")],fq=[0,[2,0,[11,b(e1),0]],b("%s 0\n")],fr=b(eG),fm=b("equation"),fn=b("inequation"),fo=b("disequation"),fj=b("="),fk=b(">="),fl=b("!="),fc=b(eR),ff=b(eK),fg=b(""),fd=[0,[2,0,[12,32,0]],b("%s ")],fe=[0,[2,0,[12,32,[2,0,[12,32,0]]]],b("%s %s ")],fh=[0,[11,b(eK),[2,0,[12,32,0]]],b("+ %s ")],fi=[0,[11,b(eR),[2,0,[12,32,0]]],b("- %s ")],e$=b("pgcd_l"),fa=b("Omega.MakeOmegaSolver(I).UNSOLVABLE"),fb=b("Omega.MakeOmegaSolver(I).NO_CONTRADICTION"),fO=b("Omega.MakeOmegaSolver(I).FACTOR1"),fP=b("Omega.MakeOmegaSolver(I).CHOPVAR"),fS=b("Omega.MakeOmegaSolver(I).SOLVED_SYSTEM"),fY=b("Omega.MakeOmegaSolver(I).FULL_SOLUTION"),je=b(eU),jf=b("+"),jg=b(eW),jh=b("~"),ji=b(eU),jj=b(eN),jk=b(eW),jl=b("?"),jm=b("weight"),jy=[0,2],jz=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jA=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],ju=[0,2],jv=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jw=[0,2],jx=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jB=[0,2],jC=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jD=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],j4=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],[0,[0,[0,1],[0,[0,1],[0,[0,2],[0,[0,1],0]]]],0]]]],j5=[0,1],j6=[0,2],j7=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j8=[0,2],j9=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],j_=b(e7),j$=[0,2],ka=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],kh=[0,1],ki=[0,2],kj=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],kk=[0,[0,[0,1],[0,[0,1],0]],0],kl=b(e7),km=[0,2],kn=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],ko=[0,[0,[0,1],0],0],kp=[0,1],kq=[0,2],kr=[0,1],ks=[0,2],kt=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],ku=[0,1],kL=[0,0,0],kI=[0,1],kJ=[0,2],kE=[0,1],kF=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kG=[0,1],kH=[0,2],kK=[0,1],kN=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],kM=[0,2],lG=[0,b(ba),[0,b(aR),[0,b(ao),0]]],lA=b("_left"),lB=b("_right"),lm=[0,1],lf=[0,2],lg=[0,1],lh=[0,2],li=[0,1],lj=[0,2],lk=[0,1],ll=[0,1],ln=[0,[0,3],[0,1,0]],lo=[0,[0,2],[0,1,0]],lp=[0,[0,2],[0,1,0]],lq=[0,[0,1],[0,1,0]],lr=[0,[0,2],[0,1,0]],ls=[0,[0,1],[0,1,0]],lt=[0,[0,2],[0,1,0]],lu=[0,[0,1],[0,1,0]],lv=[0,[0,2],[0,1,0]],lw=[0,[0,1],[0,1,0]],lx=[0,[0,2],[0,1,0]],ly=[0,[0,1],[0,1,0]],lc=[0,0,0],ld=b("Omega can't solve this system"),la=b(e8),k$=[0,1,0],kR=[0,[0,3],0],kS=[0,[0,2],0],kT=[0,[0,3],0],kU=[0,[0,3],0],kV=[0,[0,1],[0,1,0]],kW=[0,[0,2],[0,1,0]],kX=[0,[0,2],[0,1,0]],kY=[0,[0,[0,1],0],0],kZ=[0,2],k0=[0,1],k1=[0,1],k2=[0,[0,2],[0,1,0]],k4=[0,[0,1],[0,1,0]],k5=[0,[0,3],0],k6=[0,[0,[0,1],0],0],k7=[0,[0,3],0],k9=[0,[0,2],[0,1,0]],k_=[0,[0,2],[0,1,0]],kO=b("auxiliary"),kP=b("auxiliary_1"),kQ=b("auxiliary_2"),kB=b("condense.1"),kC=[0,2],kD=[0,0,0],kA=b("reduce_factor.1"),kw=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kx=[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]],ky=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,2],0]],0]],kz=[0,[0,[0,1],0],0],kv=b("shrink.1"),kf=[0,2],kg=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],kd=[0,2],ke=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],kb=[0,2],kc=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]]],jR=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]],jS=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jT=[0,1],jU=[0,2],jV=[0,2],jW=[0,2],jX=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jY=[0,2],jZ=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],j0=[0,2],j1=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j2=[0,2],j3=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jE=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]]],jF=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jG=[0,1],jH=[0,2],jI=[0,2],jJ=[0,2],jK=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jL=[0,2],jM=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jN=[0,2],jO=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jP=[0,2],jQ=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jt=[0,b(dC),734,17],js=[0,b(dC),735,13],jr=[0,b(dC),694,9],jp=b("H"),jq=b("P"),jn=b("compile_equation."),jd=b("x"),jc=b("occurrence "),jb=b("abstract_path "),i$=b(e0),ja=b(e0),i_=b("Omega: Not a quantifier-free goal"),i6=b(eM),i4=b(e5),i2=b(eV),i1=b(eS),i0=b(e9),iZ=b(eJ),iY=b(eZ),iX=b(eL),iU=b(" is not an evaluable constant."),iV=[0,b("Coq_omega")],iS=b("True"),iR=b("False"),iQ=b("ex"),iP=b("eq"),iO=b("or"),iN=b("and"),iM=b(eM),iL=b("iff"),iK=b("imp_simp"),iJ=b("not_not"),iI=b("not_iff"),iH=b("not_imp"),iG=b("not_and"),iF=b("not_or"),iE=b("dec_True"),iD=b("dec_not_not"),iC=b("dec_False"),iB=b("dec_not"),iA=b("dec_iff"),iz=b("dec_imp"),iy=b("dec_and"),ix=b("dec_or"),iw=b("eq_ind_r"),iv=b("not_gt"),iu=b("not_ge"),it=b("not_lt"),is=b("not_le"),ir=b("not_eq"),iq=b("dec_gt"),ip=b("dec_ge"),io=b("dec_lt"),im=b("dec_le"),il=b("dec_eq_nat"),ik=b("le_gt_dec"),ij=b("pred_of_minus"),ii=b("O"),ih=b("S"),ig=b(dD),ie=b("Nat.pred"),id=b("Nat.mul"),ic=b("Nat.add"),ib=b("Nat.sub"),ia=b("gt"),h$=b("ge"),h_=b("lt"),h9=b("le"),h8=b(e5),h7=b(eV),h6=b(eS),h5=b(e9),h4=b("Zne"),h3=b("neq"),h2=b("Znot_gt_le"),h1=b("Znot_ge_lt"),h0=b("Znot_lt_ge"),hZ=b("Znot_le_gt"),hY=b("not_Zne"),hX=b("not_Zeq"),hW=b("dec_Zge"),hV=b("dec_Zgt"),hU=b("Z.lt_decidable"),hT=b("Z.le_decidable"),hS=b("dec_Zne"),hR=b("Z.eq_decidable"),hQ=b("intro_Z"),hP=b("new_var"),hO=b("Zle_left"),hN=b("Zgt_left"),hM=b("Zge_left"),hL=b("Zlt_left"),hK=b("Zne_left"),hJ=b("Zegal_left"),hI=b("fast_Zopp_involutive"),hH=b("fast_Zopp_eq_mult_neg_1"),hG=b("fast_Zopp_mult_distr_r"),hF=b("fast_Zopp_plus_distr"),hE=b("fast_Zmult_opp_comm"),hD=b("fast_Zmult_plus_distr_l"),hC=b("fast_Zred_factor6"),hB=b("fast_Zred_factor5"),hA=b("fast_Zred_factor4"),hz=b("fast_Zred_factor3"),hy=b("fast_Zred_factor2"),hx=b("fast_Zred_factor1"),hw=b("fast_Zred_factor0"),hv=b("OMEGA20"),hu=b("OMEGA19"),ht=b("OMEGA18"),hs=b("OMEGA17"),hr=b("fast_OMEGA16"),hq=b("fast_OMEGA15"),hp=b("fast_OMEGA14"),ho=b("fast_OMEGA13"),hn=b("fast_OMEGA12"),hm=b("fast_OMEGA11"),hl=b("fast_OMEGA10"),hk=b("OMEGA9"),hj=b("OMEGA8"),hi=b("OMEGA7"),hh=b("OMEGA6"),hg=b("OMEGA5"),hf=b("OMEGA4"),he=b("OMEGA3"),hd=b("OMEGA2"),hc=b("OMEGA1"),hb=b("Zmult_le_approx"),ha=b("fast_Zmult_comm"),g$=b("fast_Zplus_comm"),g_=b("fast_Zplus_permute"),g9=b("fast_Zmult_assoc_reverse"),g8=b("fast_Zplus_assoc"),g7=b("fast_Zplus_assoc_reverse"),g6=b("inj_eq"),g5=b("inj_neq"),g4=b("Znat.inj_gt"),g3=b("Znat.inj_ge"),g2=b("Znat.inj_lt"),g1=b("Znat.inj_le"),g0=b("Nat2Z.inj_succ"),gZ=b("inj_minus2"),gY=b("Nat2Z.inj_sub"),gX=b("Nat2Z.inj_mul"),gW=b("Nat2Z.inj_add"),gV=b("Z.of_nat"),gU=b(eZ),gT=b(eL),gS=b(eJ),gR=b("Z.opp"),gQ=b("Z.mul"),gP=b("Z.add"),gO=b("Gt"),gN=b("comparison"),gM=b(dB),gL=b("Zneg"),gK=b("Zpos"),gJ=b("Z0"),gI=b("xI"),gH=b("xO"),gG=b("xH"),gt=b("find_contr"),gs=b(eF),gq=b(eF),go=[0,1],gn=[0,[12,88,[4,0,0,0,0]],b(e3)],gm=b("WW"),gl=b("Zvar"),gk=b(e8),gi=b(ao),f5=[0,b(ao),[0,b("System"),0]],f6=b("Omega system time displaying flag"),f9=[0,b(ao),[0,b("Action"),0]],f_=b("Omega action display flag"),gb=[0,b(ao),[0,b("OldStyle"),0]],gc=b("Omega old style flag"),gf=[0,b("Stable"),[0,b(ao),0]],gg=b("Omega automatic reset of generated names"),gu=[0,[0,b(ba),[0,b(aR),[0,b("OmegaLemmas"),0]]],0],gA=b(ao),gB=b(ao),gC=[0,[0,b(ba),[0,b(eY),0]],0],gD=b(ao),gE=[0,[0,b(ba),[0,b(eY),[0,b("BinInt"),0]]],0],gF=b(ao),lz=b("Coq_omega.Undecidable"),l_=[0,b("plugins/omega/g_omega.ml4"),1,0],l8=[0,[0,[0,b(aR)],[0,[0,b(eT)],[0,[0,b(eN)],0]]],0],l9=b("$l"),l$=[0,b(eT)],ma=[0,b(aR)],mb=b(eP),l2=b(eX),l0=[0,b(dD),[0,b(eI),[0,b(e4),[0,b(dB),0]]]],lZ=b(eX),lU=b(aR),lV=b(aR),lK=b(e4),lL=b(dB),lM=b(dD),lN=b(eI),lP=b("zify_positive"),lQ=b("zify_nat"),lR=b("zify_op"),lS=b("zify_N"),lO=b("No Omega knowledge base for type "),lI=[0,b("PreOmega"),[0,b(aR),[0,b(ba),0]]],lX=b(aR),l6=b(eP),lD=q.Termops,k3=q.Contradiction,k8=q.Equality,jo=q.Evarutil,iT=q.Evd,iW=q.Global,gy=q.Universes,lJ=q.String,l4=q.Array,dc=[0,aS,function(f){var
j=f[1],s=f[2];function
aq(b,a){var
c=d(f[2],b,a),e=c||eD(b,a);return e}function
A(b,a){return d(f[2],a,b)}function
M(b,a){var
c=d(f[2],a,b),e=c||eD(b,a);return e}var
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
av(b){a(m[32],b);a(m[35],0);return a(m[51],m[27])}function
E(b,a){a[1]=[0,b,a[1]];return 0}function
ae(f,e){var
b=f,a=e;for(;;){if(d(j,a,c))return b;var
g=N(b,a),b=a,a=g;continue}}function
af(b){return b?n(p[20],ae,b[1],b[2]):a(m[2],e$)}function
F(b,a){var
g=M(b,c),f=A(a,c);return 0===g?0===f?x(b,a):d(k,x(d(h,b,e),a),e):0===f?d(k,x(d(k,b,e),a),e):x(b,a)}var
q=[a1,fa,a0(0)],ag=[a1,fb,a0(0)];function
G(h,f){var
b=f[2],k=f[1],o=0;function
q(i,b){var
k=d(s,b[1],c)?fc:i?ff:fg;a(m[30],k);var
f=l(b[1]);if(d(j,f,e)){var
o=a(h,b[2]);d(w[2],fd,o)}else{var
p=a(h,b[2]),q=a(g,f);n(w[2],fe,q,p)}return 1}n(p[20],q,o,k);if(A(b,c)){var
r=a(g,b);return d(w[2],fh,r)}var
i=d(s,b,c);if(i){var
t=a(g,l(b));return d(w[2],fi,t)}return i}function
W(a){function
b(b,a){if(15===a[0]){var
c=a[2][2],f=W(a[3][2]),g=W(c);return d(h,d(h,d(h,b,e),g),f)}return d(h,b,e)}return n(p[20],b,c,a)}function
O(a){switch(a){case
0:return fj;case
1:return fk;default:return fl}}function
P(a){switch(a){case
0:return fm;case
1:return fn;default:return fo}}function
z(c,b){function
e(a){var
b=a[4],e=a[3],f=a[2];d(w[2],fp,a[1]);G(c,[0,e,b]);var
g=O(f);return d(w[2],fq,g)}d(p[15],e,b);return a(m[30],fr)}function
aw(c,b){function
e(b){G(c,b);return a(m[30],fs)}d(p[15],e,b);return a(m[30],ft)}function
Q(c,q){var
e=q;for(;;){if(e){var
b=e[1],r=e[2];switch(b[0]){case
0:var
s=b[3],t=b[1],u=a(g,b[4]),v=a(g,s);cL(w[2],fu,t[1],v,u);break;case
1:var
x=b[1],y=a(g,b[2]);n(w[2],fv,x[1],y);break;case
2:d(w[2],fw,b[1]);break;case
3:var
z=b[1],A=a(g,b[2]);n(w[2],fx,z[1],A);break;case
4:var
j=b[3],k=j[2],l=b[2],i=l[2],B=j[1],C=l[1],D=b[1],E=k[1],F=P(k[2]),H=a(g,B),I=i[1],J=P(i[2]),K=a(g,C),L=P(i[2]);eE(w[2],fy,L,D,K,J,I,H,F,E);break;case
5:var
f=b[1][1];d(w[2],fz,f[1]);G(c,[0,f[3],f[4]]);var
M=O(f[2]);a(m[30],M);a(m[30],fA);break;case
6:var
h=b[1];d(w[2],fB,h[1]);G(c,[0,h[3],h[4]]);var
N=O(h[2]);a(m[30],N);a(m[30],fC);break;case
7:n(w[2],fD,b[1],b[2]);break;case
8:n(w[2],fE,b[1],b[2]);break;case
9:n(w[2],fF,b[1][1],b[2][1]);break;case
10:n(w[2],fG,b[1][1],b[2][1]);break;case
11:cL(w[2],fH,b[2][1],b[3],b[1]);break;case
12:var
R=b[1],S=a(g,b[2]);n(w[2],fI,R,S);break;case
13:d(w[2],fJ,b[1]);break;case
14:var
T=b[1],U=a(g,b[2]);n(w[2],fK,T,U);break;case
15:var
o=b[3],p=b[2],V=o[2],W=p[2];cL(w[2],fL,b[1][1],p[1],o[1]);Q(c,W);a(m[35],0);Q(c,V);a(m[35],0);break;default:var
X=b[1],Y=a(g,b[2]);n(w[2],fM,X,Y)}var
e=r;continue}return a(m[51],m[27])}}function
ah(a){return d(w[4],fN,a)}var
X=[0,0];function
R(a){X[1]=0;return 0}function
S(a){return X[1]}function
b(a){if(aS[1])Q(ah,[0,a,0]);return E(a,X)}function
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
Z=[a1,fO,a0(0)];function
_(a){if(a){var
c=a[2],b=a[1];if(d(j,l(b[1]),e))return[0,b,c];var
f=_(c);return[0,f[1],[0,b,f[2]]]}throw Z}var
T=[a1,fP,a0(0)];function
K(c,a){if(a){var
d=a[2],b=a[1];if(b[2]===c)return[0,b,d];var
e=K(c,d);return[0,e[1],[0,b,e[2]]]}throw T}function
r(f){var
g=f[4],o=f[3],m=f[2],n=f[1];if(0===o)switch(m){case
0:if(d(j,g,c))return 0;b([12,n,g]);throw q;case
1:if(M(g,c))return 0;b([14,n,g]);throw q;default:if(c6(g,c))return 0;b([13,n]);throw q}function
v(a){return l(a[1])}var
h=af(d(p[17],v,o));if(0===m)if(c6(N(g,h),c)){b([1,f,h]);throw q}if(2===m)if(c6(N(g,h),c)){b([2,f[1]]);return 0}if(c6(h,e)){var
s=F(g,h),w=d(k,g,d(i,s,h)),t=[0,n,m,a(B(function(a){return x(a,h)}),o),s];if(0===m)var
r=0;else
if(2===m)var
r=0;else
var
u=[0,f,t,h,w],r=1;if(!r)var
u=[3,f,h];b(u);return[0,t,0]}return[0,f,0]}function
D(o,g,f,c){var
h=g[1],p=c[3],q=g[2];try{var
k=K(q,p)[1],l=d(j,h,e)?a(u,k[1]):d(j,h,ad)?k[1]:a(m[2],fQ),n=Y(o,c,C(function(a){return d(i,a,l)},f));b([4,n[1],[0,e,c],[0,l,f]]);return n}catch(a){a=t(a);if(a===T)return c;throw a}}function
$(b,a){var
c=d(i,y,a);return d(k,b,d(i,a,F(d(h,d(i,y,b),a),c)))}function
aj(q,f,H,G){var
g=q[1],j=f[3],I=q[3],t=a(q[2],0);if(0===j){z(I,[0,f,0]);a(m[2],fR)}var
J=a(p[6],j),L=a(p[5],j)[2],M=[0,l(a(p[5],j)[1]),L];function
N(b,a){var
c=b[1],d=b[2];if(A(c,l(a[1]))){var
e=a[2];return[0,l(a[1]),e]}return[0,c,d]}var
v=n(p[20],N,M,J),O=v[2],c=d(h,v[1],e),P=$(f[4],c),Q=f[3],R=a(B(function(a){return $(a,c)}),Q),S=[0,[0,a(u,c),t],R],w=[0,a(g,0),0,S,P],T=d(i,y,c),U=a(u,F(d(h,d(i,y,f[4]),c),T)),V=f[3],W=a(B(function(b){var
e=d(i,y,c);return a(u,F(d(h,d(i,y,b),c),e))}),V);b([5,[0,w,[0,a(g,0),0,W,U],f,c,t]]);var
X=r(w),k=a(p[5],X),s=K(O,k[3])[1];function
Y(a){return r(D(g,s,k,a))}var
Z=d(o[17][bb],Y,H);function
_(a){return r(D(g,s,k,a))}var
aa=d(o[17][bb],_,G),E=D(g,s,k,f),ab=C(function(a){return x(a,c)},E);b([3,E,c]);var
ac=r(ab);return[0,a(p[5],ac),Z,aa]}function
ak(e,i){var
b=i;for(;;){var
g=b[3],f=b[2],a=b[1],c=e[1],j=e[3];if(aS[1])z(j,[0,a,f]);try{var
h=_(a[3])[1],k=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,c,h),l=d(o[17][bb],k,g),m=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,c,h),n=[0,d(o[17][bb],m,f),l];return n}catch(c){c=t(c);if(c===Z){var
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
f=ak(k,[0,a,i,g]);continue}if(aS[1])z(r,g);return g}}function
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
U=[a1,fS,a0(0)];function
al(g){var
b=d(v[1],0,7);function
h(e,c){try{var
a=d(v[6],b,e),g=l(c);a[1]=d(m[5],a[1],g);var
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
h=K(i,b[3])[1],j=M(h[1],c)?[0,g,[0,[0,h[1],b],f],e]:[0,g,f,[0,[0,a(u,h[1]),b],e]];return j}catch(a){a=t(a);if(a===T)return[0,[0,b,g],f,e];throw a}}return n(p[20],d,fT,b)}function
an(u,t,c,g){var
f=0;function
h(h,c){var
j=c[2],f=c[1];function
l(n,l){var
o=l[2],g=l[1],v=C(function(a){return d(i,a,f)},o),p=Y(u,C(function(a){return d(i,a,g)},j),v);b([4,p[1],[0,g,j],[0,f,o]]);var
h=r(p);if(h){var
c=h[1];if(h[2])return a(m[2],fU);if(t){var
w=d(k,g,e),q=d(i,d(k,f,e),w);b([16,c[1],q]);var
x=d(k,c[4],q),s=[0,c[1],1,c[3],x]}else
var
s=c;return[0,s,n]}return n}return n(p[20],l,h,g)}return n(p[20],h,f,c)}function
ab(c,f,b){var
g=c[3],h=c[1],a=am(al(b),b),i=a[1],j=an(h,f,a[2],a[3]),e=d(m[25],i,j);if(aS[1])z(g,e);return e}function
aA(c,l,e){var
h=c[1],n=c[3];function
q(a){return 2===a[2]?1:0}if(d(p[28],q,e))a(m[2],fV);R(0);function
s(a){return b([6,a])}d(p[15],s,e);var
u=d(o[17][bb],r,e);function
v(a){return 0===a[2]?1:0}var
i=d(p[37],v,u),w=i[1],j=L(h,i[2]),x=j[2],y=[0,d(m[25],w,j[1]),x];function
g(b,d){var
a=aa(c,d);return b<50?f(b+1|0,a):c7(f,[0,a])}function
f(e,f){var
a=L(h,f),b=a[2],c=a[1];if(0===c)return b;var
d=[0,c,b];return e<50?g(e+1|0,d):c7(g,[0,d])}function
A(a){return c8(g(0,a))}function
B(a){return c8(f(0,a))}function
k(b){try{var
a=k(B(ab(c,l,b)));return a}catch(a){a=t(a);if(a===U){if(aS[1])z(n,b);return b}throw a}}return k(A(y))}function
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
15:return a(m[2],fW);default:if(d(ap[4][1],b[1],f)){var
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
h=d(v[6],f,[0,k,j]);b([10,a,h[2],i===h[1]?1:0]);throw q}catch(a){a=t(a);if(a===I)return 0;throw a}}throw[0,bd,fX]}return d(p[15],l,h)}var
ac=[a1,fY,a0(0)];return[0,j,s,aq,A,M,h,k,i,x,N,c,e,y,ad,l,g,u,ar,as,at,au,av,E,ae,af,F,q,ag,G,W,O,P,z,aw,g,Q,ah,b,S,R,ai,ay,B,C,H,J,Y,Z,_,T,K,r,D,$,aj,ak,aa,L,U,al,am,an,ab,aA,V,ao,ac,function(c,j){var
f=c[1],D=c[3];R(0);function
E(a){return b([6,a])}d(p[15],E,j);function
i(d,a){ao(a);var
b=aa(c,a);return d<50?h(d+1|0,b):c7(h,[0,b])}function
h(j,k){function
l(a){return 2===a[2]?1:0}var
a=d(p[37],l,k),b=a[1],c=L(f,a[2]),e=c[2],g=c[1];if(0===g)return d(m[25],b,e);var
h=[0,g,d(m[25],b,e)];return j<50?i(j+1|0,h):c7(i,[0,h])}function
F(a){return c8(i(0,a))}function
G(a){return c8(h(0,a))}function
l(b){try{var
a=l(G(ab(c,0,b)));return a}catch(a){a=t(a);if(a===U){if(aS[1])z(D,b);return b}throw a}}function
H(l){var
c=l;for(;;){var
g=c[1];if(g){var
j=c[2],b=g[1],n=c[3],o=g[2],h=a(f,0),i=a(f,0),q=d(k,b[4],e),r=[0,h,1,b[3],q],s=d(k,a(u,b[4]),e),t=b[3],v=[0,i,1,a(B(u),t),s],w=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,0],a[1]],[0,d,a[2]]]}}(b,i,v),x=d(p[17],w,j),y=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,1],a[1]],[0,d,a[2]]]}}(b,h,r),z=d(p[17],y,j),A=d(m[25],z,x),c=[0,o,A,[0,[0,b[1],[0,b,h,i]],n]];continue}return[0,c[2],c[3]]}}try{var
J=d(o[17][bb],r,j),K=function(a){return 0===a[2]?1:0},s=d(p[37],K,J),M=s[2],N=s[1],O=function(a){return 2===a[2]?1:0},w=d(p[37],O,M),P=w[1],x=L(f,w[2]),Q=x[1],T=d(m[25],x[2],P),W=F([0,d(m[25],N,Q),T]),X=function(a){return 2===a[2]?1:0},y=d(p[37],X,W),Y=y[2],Z=y[1],_=S(0),A=H([0,Z,[0,[0,0,Y],0],0]),$=A[2],ad=A[1],ae=function(a){var
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
b=e;continue}return a(m[2],fZ)}},f=d(p[37],l,e),q=f[2],r=f[1],h=function(a){var
b=a[4],d=a[3],e=a[2],f=a[1];function
g(b,a){return b===a?1:0}return[0,n(o[17][94],g,c,f),e,d,b]},s=d(p[17],h,r),u=d(p[17],h,q),i=g(s),v=i[2],w=i[1],j=g(u),x=j[2],y=j[1],b=d(ap[4][2],c,$),k=b[1],z=b[3],A=b[2],B=function(b,a){return b===a?1:0},C=n(o[17][59],B,v,x),D=[0,[0,[15,k,[0,A,w],[0,z,y]],0],[0,k[1],C]];return D}catch(a){a=t(a);if(a[1]===ac)return[0,a[2],a[3]];throw a}},C=g(af),ai=V(C[2],C[1],_)[2];return ai}catch(a){a=t(a);if(a===q)return V(0,0,S(0))[2];throw a}}]}];c9(414,dc,"Omega_plugin.Omega");var
l=a(dc[2],[0,D[17],D[16],D[12],D[13],D[14],D[15],D[22],D[5],D[6],D[2]]),f0=0;function
aT(b){function
c(d){var
c=a(h[10],b);return a(i[98],c)}return a(s[63][9],c)}function
be(b){function
c(d){var
c=a(h[10],b);return a(i[85],c)}return a(s[63][9],c)}var
cM=[0,0],bf=[0,0],bg=[0,0],cN=[0,1];function
f1(d,c,b){return a(c,b)}var
f2=[0,0];function
f3(a,b){return a[1]}function
bh(b,a){b[1]=a;return 0}function
f4(a){return bh(cM,a)}var
f7=[0,0,f6,f5,function(a){return cM[1]},f4];d(cO[4],0,f7);function
f8(a){return bh(bf,a)}var
f$=[0,0,f_,f9,function(a){return bf[1]},f8];d(cO[4],0,f$);function
ga(a){return bh(bg,a)}var
gd=[0,0,gc,gb,function(a){return bg[1]},ga];d(cO[4],0,gd);function
ge(a){return bh(cN,a)}var
gh=[0,1,gg,gf,function(a){return cN[1]},ge];d(cO[4],0,gh);var
dd=[0,0];function
dF(c){var
a=dd[1];function
b(a){a[1][1]=a[2];return 0}return d(o[17][14],b,a)}function
aU(a){var
b=[0,a];dd[1]=[0,[0,b,a],dd[1]];return b}var
dG=aU(0);function
af(e){var
b=a(m[21],dG[1]),c=d(m[16],gi,b);dG[1]++;return a(E[1][6],c)}var
dH=aU(0);function
gj(b){var
a=d(bi[1],gk,[0,dH[1]]);dH[1]++;return a}var
dI=aU(0);function
dJ(e){var
b=a(m[21],dI[1]),c=d(m[16],gl,b);dI[1]++;return a(E[1][6],c)}var
dK=aU(0);function
bj(a){dK[1]++;return dK[1]}var
dL=aU(1e3);function
de(a){dL[1]++;return dL[1]}var
dM=aU(0);function
dN(a){dM[1]++;return d(bi[1],gm,[0,dM[1]])}function
a2(a){return d(w[4],gn,a)}var
df=[0,0],cP=d(v[1],0,7),dg=d(v[1],0,7);function
dO(b){df[1]=0;return a(v[2],cP)}function
dh(b){try{var
a=d(v[6],dg,b);return a}catch(a){a=t(a);if(a===I){var
c=dN(0);n(v[5],cP,c,b);n(v[5],dg,b,c);return c}throw a}}function
cQ(b){try{var
a=d(v[6],cP,b);return a}catch(a){a=t(a);if(a===I){var
c=df[1];n(v[5],cP,b,c);n(v[5],dg,c,b);df[1]++;return c}throw a}}function
R(b){return a(j[66][20],b)}function
dP(a){return cL(i[109],0,go,1,[0,[0,a,0]])}function
u(b){return a(i[147],b)}function
gp(b){return a(i[98],b)}function
O(b){var
d=f(b),h=0,j=g===d?b[1]:c===d?a(e[2],b):b;return a(i[67],[0,[0,0,j],h])}function
di(b,a){return n(A[48][1],dj[9],b,a)}function
dQ(c){return function(d){var
a=d;for(;;){if(a){var
b=a[1],e=a[2],f=b[1];if(c===b[2])return f;var
a=e;continue}throw I}}}var
bk=[0,0];function
dR(a){bk[1]=0;return 0}function
J(b){try{var
c=bk[1],d=a(dQ(b),c);return d}catch(b){b=t(b);if(b===I)return a(m[2],gq);throw b}}function
gr(b){try{var
c=d(E[1][13][3],b,bk[1]);return c}catch(b){b=t(b);if(b===I)return a(m[2],gs);throw b}}function
aK(b,a){bk[1]=[0,[0,b,a],bk[1]];return 0}var
bl=[0,0];function
dS(a){return bl[1]}function
dk(a){bl[1]=0;return 0}function
dT(c,b){try{var
d=bl[1],e=a(h[94],c),f=n(o[17][126],e,b,d);return f}catch(b){b=t(b);if(b===I)return a(m[2],gt);throw b}}function
dU(d,c,b,a){bl[1]=[0,[0,d,[0,c,b,a]],bl[1]];return 0}function
dV(b){var
a=cN[1];return a?(dF(0),dO(0),dR(0),dk(0)):a}var
gv=d(o[18],a3[6],gu),gw=d(o[18],[0,dW,0],gv),gx=d(o[18],a3[5],gw),dX=d(o[18],a3[7],gx);function
bm(d,c,b){var
e=n(a3[4],d,c,b),f=a(gy[45],e);return a(h[8],f)}var
gz=a3[7];function
F(a){return bm(gA,gz,a)}function
k(a){return bm(gB,dX,a)}function
ac(a){return bm(gD,gC,a)}function
Q(a){return bm(gF,gE,a)}var
aq=[c,function(a){return k(gG)}],ar=[c,function(a){return k(gH)}],as=[c,function(a){return k(gI)}],S=[c,function(a){return k(gJ)}],T=[c,function(a){return k(gK)}],U=[c,function(a){return k(gL)}],x=[c,function(a){return k(gM)}],at=[c,function(a){return k(gN)}],au=[c,function(a){return k(gO)}],y=[c,function(a){return Q(gP)}],K=[c,function(a){return Q(gQ)}],B=[c,function(a){return Q(gR)}],ag=[c,function(a){return Q(gS)}],ah=[c,function(a){return Q(gT)}],a4=[c,function(a){return Q(gU)}],av=[c,function(a){return Q(gV)}],bn=[c,function(a){return ac(gW)}],bo=[c,function(a){return ac(gX)}],bp=[c,function(a){return ac(gY)}],bq=[c,function(a){return k(gZ)}],br=[c,function(a){return ac(g0)}],bs=[c,function(a){return ac(g1)}],bt=[c,function(a){return ac(g2)}],bu=[c,function(a){return ac(g3)}],bv=[c,function(a){return ac(g4)}],bw=[c,function(a){return ac(g5)}],bx=[c,function(a){return ac(g6)}],C=[c,function(a){return k(g7)}],by=[c,function(a){return k(g8)}],bz=[c,function(a){return k(g9)}],aw=[c,function(a){return k(g_)}],ax=[c,function(a){return k(g$)}],bA=[c,function(a){return k(ha)}],bB=[c,function(a){return k(hb)}],bC=[c,function(a){return k(hc)}],bD=[c,function(a){return k(hd)}],bE=[c,function(a){return k(he)}],bF=[c,function(a){return k(hf)}],bG=[c,function(a){return k(hg)}],bH=[c,function(a){return k(hh)}],bI=[c,function(a){return k(hi)}],bJ=[c,function(a){return k(hj)}],bK=[c,function(a){return k(hk)}],bL=[c,function(a){return k(hl)}],V=[c,function(a){return k(hm)}],L=[c,function(a){return k(hn)}],bM=[c,function(a){return k(ho)}],bN=[c,function(a){return k(hp)}],bO=[c,function(a){return k(hq)}],bP=[c,function(a){return k(hr)}],bQ=[c,function(a){return k(hs)}],bR=[c,function(a){return k(ht)}],bS=[c,function(a){return k(hu)}],bT=[c,function(a){return k(hv)}],bU=[c,function(a){return k(hw)}],bV=[c,function(a){return k(hx)}],bW=[c,function(a){return k(hy)}],bX=[c,function(a){return k(hz)}],bY=[c,function(a){return k(hA)}],W=[c,function(a){return k(hB)}],bZ=[c,function(a){return k(hC)}],b0=[c,function(a){return k(hD)}],b1=[c,function(a){return k(hE)}],b2=[c,function(a){return k(hF)}],b3=[c,function(a){return k(hG)}],X=[c,function(a){return k(hH)}],b4=[c,function(a){return k(hI)}],b5=[c,function(a){return k(hJ)}],b6=[c,function(a){return k(hK)}],b7=[c,function(a){return k(hL)}],b8=[c,function(a){return k(hM)}],b9=[c,function(a){return k(hN)}],b_=[c,function(a){return k(hO)}],b$=[c,function(a){return k(hP)}],ca=[c,function(a){return k(hQ)}],cb=[c,function(a){return Q(hR)}],dY=[c,function(a){return k(hS)}],dZ=[c,function(a){return Q(hT)}],d0=[c,function(a){return Q(hU)}],d1=[c,function(a){return k(hV)}],d2=[c,function(a){return k(hW)}],cc=[c,function(a){return k(hX)}],d3=[c,function(a){return k(hY)}],d4=[c,function(a){return k(hZ)}],d5=[c,function(a){return k(h0)}],d6=[c,function(a){return k(h1)}],d7=[c,function(a){return k(h2)}],ay=[c,function(a){return k(h3)}],az=[c,function(a){return k(h4)}],ai=[c,function(a){return Q(h5)}],aj=[c,function(a){return Q(h6)}],a5=[c,function(a){return Q(h7)}],a6=[c,function(a){return Q(h8)}],aA=[c,function(a){return F(h9)}],cd=[c,function(a){return F(h_)}],ce=[c,function(a){return F(h$)}],aB=[c,function(a){return F(ia)}],aC=[c,function(a){return F(ib)}],cf=[c,function(a){return F(ic)}],cg=[c,function(a){return F(id)}],ch=[c,function(a){return F(ie)}],Y=[c,function(a){return F(ig)}],aD=[c,function(a){return F(ih)}],aE=[c,function(a){return F(ii)}],ci=[c,function(a){return k(ij)}],cj=[c,function(a){return k(ik)}],ck=[c,function(a){return k(il)}],d8=[c,function(a){return k(im)}],d9=[c,function(a){return k(io)}],d_=[c,function(a){return k(ip)}],d$=[c,function(a){return k(iq)}],cl=[c,function(a){return k(ir)}],ea=[c,function(a){return k(is)}],eb=[c,function(a){return k(it)}],ec=[c,function(a){return k(iu)}],ed=[c,function(a){return k(iv)}],cm=[c,function(a){return k(iw)}],cn=[c,function(a){return k(ix)}],co=[c,function(a){return k(iy)}],cp=[c,function(a){return k(iz)}],cq=[c,function(a){return k(iA)}],cr=[c,function(a){return k(iB)}],cs=[c,function(a){return k(iC)}],ct=[c,function(a){return k(iD)}],cu=[c,function(a){return k(iE)}],cv=[c,function(a){return k(iF)}],cw=[c,function(a){return k(iG)}],cx=[c,function(a){return k(iH)}],cy=[c,function(a){return k(iI)}],cz=[c,function(a){return k(iJ)}],cA=[c,function(a){return k(iK)}],cB=[c,function(a){return k(iL)}],ak=[c,function(a){return F(iM)}],aF=[c,function(a){return F(iN)}],aG=[c,function(a){return F(iO)}],M=[c,function(a){return F(iP)}],cC=[c,function(a){return F(iQ)}],aH=[c,function(a){return F(iR)}],cD=[c,function(a){return F(iS)}];function
aI(l,b){var
i=f(b),o=g===i?b[1]:c===i?a(e[2],b):b,j=d(h[3],iT[16],o);if(10===j[0]){var
k=j[1][1],r=a(iW[2],0);if(d(dj[2],r,[1,k]))return[1,k]}var
p=d(m[16],l,iU),q=a(al[3],p);return n(ad[3],0,iV,q)}var
ee=[c,function(a){return aI(iX,ah)}],ef=[c,function(a){return aI(iY,a4)}],eg=[c,function(a){return aI(iZ,ag)}],dl=[c,function(a){return aI(i0,ai)}],aL=[c,function(a){return aI(i1,aj)}],i3=[c,function(a){return aI(i2,a5)}],i5=[c,function(a){return aI(i4,a6)}],cR=[c,function(a){return aI(i6,ak)}];function
i7(b){var
c=a(E[1][6],b);return a(h[10],c)}function
ae(i,d){var
b=f(y),j=[0,i,d],k=g===b?y[1]:c===b?a(e[2],y):y;return a(h[21],[0,k,j])}function
cS(i,d){var
b=f(K),j=[0,i,d],k=g===b?K[1]:c===b?a(e[2],K):K;return a(h[21],[0,k,j])}function
eh(i,d){var
b=f(ag),j=[0,i,d],k=g===b?ag[1]:c===b?a(e[2],ag):ag;return a(h[21],[0,k,j])}function
cE(j,i){var
b=f(x),k=g===b?x[1]:c===b?a(e[2],x):x,d=f(M),l=[0,k,j,i],m=g===d?M[1]:c===d?a(e[2],M):M;return a(h[21],[0,m,l])}function
i8(i,d){var
b=f(ai),j=[0,i,d],k=g===b?ai[1]:c===b?a(e[2],ai):ai;return a(h[21],[0,k,j])}function
aM(i,d){var
b=f(aj),j=[0,i,d],k=g===b?aj[1]:c===b?a(e[2],aj):aj;return a(h[21],[0,k,j])}function
aN(d){var
b=f(B),i=[0,d],j=g===b?B[1]:c===b?a(e[2],B):B;return a(h[21],[0,j,i])}function
cF(i,d){var
b=f(aF),j=[0,i,d],k=g===b?aF[1]:c===b?a(e[2],aF):aF;return a(h[21],[0,k,j])}function
cT(i,d){var
b=f(aG),j=[0,i,d],k=g===b?aG[1]:c===b?a(e[2],aG):aG;return a(h[21],[0,k,j])}function
aJ(d){var
b=f(ak),i=[0,d],j=g===b?ak[1]:c===b?a(e[2],ak):ak;return a(h[21],[0,j,i])}function
i9(j,i){var
b=f(at),k=g===b?at[1]:c===b?a(e[2],at):at,d=f(M),l=[0,k,j,i],m=g===d?M[1]:c===d?a(e[2],M):M;return a(h[21],[0,m,l])}function
aO(d){var
b=f(av),i=[0,d],j=g===b?av[1]:c===b?a(e[2],av):av;return a(h[21],[0,j,i])}function
G(b){function
i(b){if(d(l[1],b,l[12])){var
j=f(aq);return g===j?aq[1]:c===j?a(e[2],aq):aq}var
o=[0,i(d(l[9],b,l[13]))],p=l[11],q=d(l[10],b,l[13]);if(d(l[1],q,p))var
k=f(ar),r=g===k?ar[1]:c===k?a(e[2],ar):ar,m=r;else
var
n=f(as),s=g===n?as[1]:c===n?a(e[2],as):as,m=s;return a(h[21],[0,m,o])}if(d(l[1],b,l[11])){var
j=f(S);return g===j?S[1]:c===j?a(e[2],S):S}var
o=[0,i(a(l[15],b))];if(d(l[4],b,l[11]))var
k=f(T),p=g===k?T[1]:c===k?a(e[2],T):T,m=p;else
var
n=f(U),q=g===n?U[1]:c===n?a(e[2],U):U,m=q;return a(h[21],[0,m,o])}function
aV(l,H){function
j(b,a){return n(h[93],l,b,a)}var
m=d(h[81],l,H),b=m[2],i=m[1],k=d(h[3],l,i);if(b){var
o=b[2];if(o){var
p=o[2];if(p){if(!p[2]){var
q=f(M),N=g===q?M[1]:c===q?a(e[2],M):M;if(j(N,i))return[1,16,b]}}else{var
r=f(ay),O=g===r?ay[1]:c===r?a(e[2],ay):ay;if(j(i,O))return[1,17,b];var
s=f(az),P=g===s?az[1]:c===s?a(e[2],az):az;if(j(i,P))return[1,18,b];var
t=f(ai),Q=g===t?ai[1]:c===t?a(e[2],ai):ai;if(j(i,Q))return[1,19,b];var
u=f(a6),R=g===u?a6[1]:c===u?a(e[2],a6):a6;if(j(i,R))return[1,20,b];var
v=f(a5),S=g===v?a5[1]:c===v?a(e[2],a5):a5;if(j(i,S))return[1,21,b];var
w=f(aj),T=g===w?aj[1]:c===w?a(e[2],aj):aj;if(j(i,T))return[1,22,b];var
x=f(aF),U=g===x?aF[1]:c===x?a(e[2],aF):aF;if(j(i,U))return[1,25,b];var
y=f(aG),V=g===y?aG[1]:c===y?a(e[2],aG):aG;if(j(i,V))return[1,26,b];var
z=f(cB),W=g===z?cB[1]:c===z?a(e[2],cB):cB;if(j(i,W))return[1,30,b];var
A=f(aA),X=g===A?aA[1]:c===A?a(e[2],aA):aA;if(j(i,X))return[1,31,b];var
B=f(cd),Y=g===B?cd[1]:c===B?a(e[2],cd):cd;if(j(i,Y))return[1,32,b];var
C=f(ce),Z=g===C?ce[1]:c===C?a(e[2],ce):ce;if(j(i,Z))return[1,33,b];var
D=f(aB),_=g===D?aB[1]:c===D?a(e[2],aB):aB;if(j(i,_))return[1,34,b]}}else{var
E=f(ak),$=g===E?ak[1]:c===E?a(e[2],ak):ak;if(j(i,$))return[1,29,b]}}else{var
F=f(aH),aa=g===F?aH[1]:c===F?a(e[2],aH):aH;if(j(i,aa))return[1,27,b];var
G=f(cD),ab=g===G?cD[1]:c===G?a(e[2],cD):cD;if(j(i,ab))return[1,28,b]}switch(k[0]){case
1:if(!b)return[0,k[1]];break;case
6:if(k[1]){if(!b){var
I=a(al[3],i_);return n(ad[6],0,0,I)}}else
if(!b)return[2,k[2],k[3]];break;case
10:var
J=a(dm[36],[1,k[1][1]]);return[1,[0,a(dn[22],J)],b];case
11:var
K=a(dm[36],[2,k[1][1]]);return[1,[0,a(dn[22],K)],b];case
12:var
L=a(dm[36],[3,k[1][1]]);return[1,[0,a(dn[22],L)],b]}return 0}function
cG(b,p){function
k(c,a){return n(h[93],b,c,a)}var
l=d(h[81],b,p),i=l[2],j=l[1];d(h[3],b,j);if(!i){var
m=f(x),q=g===m?x[1]:c===m?a(e[2],x):x;if(k(j,q))return[1,23,i];var
o=f(Y),r=g===o?Y[1]:c===o?a(e[2],Y):Y;if(k(j,r))return[1,24,i]}return 0}function
cH(k,H){function
j(b,a){return n(h[93],k,b,a)}var
l=d(h[81],k,H),b=l[2],i=l[1],m=d(h[3],k,i);if(b){var
o=b[2];if(o){if(!o[2]){var
p=f(y),I=g===p?y[1]:c===p?a(e[2],y):y;if(j(i,I))return[1,0,b];var
q=f(K),J=g===q?K[1]:c===q?a(e[2],K):K;if(j(i,J))return[1,1,b];var
r=f(ag),L=g===r?ag[1]:c===r?a(e[2],ag):ag;if(j(i,L))return[1,2,b];var
s=f(cf),M=g===s?cf[1]:c===s?a(e[2],cf):cf;if(j(i,M))return[1,6,b];var
t=f(cg),N=g===t?cg[1]:c===t?a(e[2],cg):cg;if(j(i,N))return[1,7,b];var
u=f(aC),O=g===u?aC[1]:c===u?a(e[2],aC):aC;if(j(i,O))return[1,8,b]}}else{var
v=f(ah),P=g===v?ah[1]:c===v?a(e[2],ah):ah;if(j(i,P))return[1,3,b];var
w=f(a4),Q=g===w?a4[1]:c===w?a(e[2],a4):a4;if(j(i,Q))return[1,5,b];var
x=f(B),R=g===x?B[1]:c===x?a(e[2],B):B;if(j(i,R))return[1,4,b];var
z=f(ch),V=g===z?ch[1]:c===z?a(e[2],ch):ch;if(j(i,V))return[1,9,b];var
A=f(aD),W=g===A?aD[1]:c===A?a(e[2],aD):aD;if(j(i,W))return[1,10,b];var
C=f(T),X=g===C?T[1]:c===C?a(e[2],T):T;if(j(i,X))return[1,13,b];var
D=f(U),Y=g===D?U[1]:c===D?a(e[2],U):U;if(j(i,Y))return[1,12,b];var
E=f(av),Z=g===E?av[1]:c===E?a(e[2],av):av;if(j(i,Z))return[1,15,b]}}else{var
F=f(aE),_=g===F?aE[1]:c===F?a(e[2],aE):aE;if(j(i,_))return[1,11,b];var
G=f(S),$=g===G?S[1]:c===G?a(e[2],S):S;if(j(i,$))return[1,14,b];if(1===m[0])return[0,m[1]]}return 0}function
ei(j,u){function
b(b,a){return n(h[93],j,b,a)}function
i(t){var
o=d(h[81],j,t),k=o[2],n=o[1];if(k){if(!k[2]){var
p=k[1],q=f(as),u=g===q?as[1]:c===q?a(e[2],as):as;if(b(n,u)){var
v=i(p),w=d(l[8],l[13],v);return d(l[6],l[12],w)}var
r=f(ar),x=g===r?ar[1]:c===r?a(e[2],ar):ar;if(b(n,x)){var
y=i(p);return d(l[8],l[13],y)}}}else{var
s=f(aq),z=g===s?aq[1]:c===s?a(e[2],aq):aq;if(b(n,z))return l[12]}return a(m[2],i$)}var
p=d(h[81],j,u),k=p[2],o=p[1];if(k){if(!k[2]){var
q=k[1],r=f(T),v=g===r?T[1]:c===r?a(e[2],T):T;if(b(o,v))return i(q);var
s=f(U),w=g===s?U[1]:c===s?a(e[2],U):U;if(b(o,w)){var
x=i(q);return a(l[17],x)}}}else{var
t=f(S),y=g===t?S[1]:c===t?a(e[2],S):S;if(b(o,y))return l[11]}return a(m[2],ja)}function
cU(C,B,e,b){function
c(e,g,u){var
b=d(h[3],C,u);if(5===b[0]){var
ao=b[3],ap=b[2],aq=[0,c(e,g,b[1]),ap,ao];return a(h[17],aq)}if(g){var
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
af=b[1],q=a(o[19][8],b[2]);q[1]=c(e,ae,an(q,0)[1]);return a(h[21],[0,af,q]);case
14:var
f=0;break;default:var
f=1}}else
if(0===i[0]){var
y=i[1],ag=g[2];switch(b[0]){case
9:var
ah=b[1],r=a(o[19][8],b[2]),z=y-1|0,A=y-1|0,ai=c(e,ag,an(r,z)[z+1]);an(r,A)[A+1]=ai;return a(h[21],[0,ah,r]);case
14:var
f=0;break;default:var
f=1}}else{var
s=i[1],aj=g[2];switch(b[0]){case
13:var
ak=b[3],al=b[2],am=b[1],t=a(o[19][8],b[4]);t[s+1]=c(e,aj,an(t,s)[s+1]);return a(h[30],[0,am,al,ak,t]);case
14:var
f=0;break;default:var
f=1}}if(f){var
D=a(o[17][1],g),E=a(m[21],D),F=d(m[16],jb,E);return a(m[2],F)}var
v=b[1],l=v[2],n=l[3],w=v[1],j=w[2],Q=l[2],R=l[1],x=a(o[19][8],n),S=c(e+(n.length-1)|0,g,an(n,j)[j+1]);an(x,j)[j+1]=S;return a(h[31],[0,w,[0,R,Q,x]])}return d(B,e,u)}return c(1,e,b)}function
ej(s,r,q){var
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
b=x,e=an(c[2],0)[1];continue;case
14:var
f=0;break;default:var
f=1}}else
if(0===g[0]){var
y=b[2],z=g[1];switch(c[0]){case
9:var
n=z-1|0,b=y,e=an(c[2],n)[n+1];continue;case
14:var
f=0;break;default:var
f=1}}else{var
p=g[1],A=b[2];switch(c[0]){case
13:var
b=A,e=an(c[4],p)[p+1];continue;case
14:var
f=0;break;default:var
f=1}}if(f){var
t=a(o[17][1],b),u=a(m[21],t),v=d(m[16],jc,u);return a(m[2],v)}var
k=c[1],l=k[1][2],e=an(k[2][3],l)[l+1];continue}return e}}function
dp(f,e,d,c){var
b=[0,a(h[9],0)],g=cU(f,function(d,c){b[1]=c;return a(h[9],d)},d,c),i=b[1],j=[0,[0,a(E[1][6],jd)],e,g];return[0,a(h[19],j),i]}function
Z(h){function
b(b){var
c=a(A[48][6],b),e=a(o[17][9],h);function
f(c,a){return di(b,a)}var
g=cU(a(A[48][4],b),f,e,c);return d(i[5],g,2)}return a(s[63][8],b)}function
aP(c){var
b=c;for(;;)switch(b[0]){case
0:var
d=b[2],e=b[1];a(m[30],je);aP(e);a(m[30],jf);aP(d);return a(m[30],jg);case
1:var
f=b[1];a(m[30],jh);var
b=f;continue;case
2:var
g=b[2],h=b[1];a(m[30],ji);aP(h);a(m[30],jj);aP(g);return a(m[30],jk);case
3:var
i=a(E[1][8],b[1]);return a(m[30],i);case
4:var
j=a(l[16],b[1]);return a(m[30],j);default:return a(m[30],jl)}}function
_(c){var
b=c;for(;;)switch(b[0]){case
0:return a(m[2],jm);case
1:var
b=b[1];continue;case
2:var
b=b[1];continue;case
3:return cQ(b[1]);case
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
ek(i,h){var
p=0;return function(q){var
c=p,b=q;for(;;){switch(b[0]){case
0:var
d=b[1];if(2===d[0]){var
e=d[1];if(3===e[0]){var
f=d[2];if(4===f[0]){var
k=b[2],l=f[1],c=[0,[0,l,cQ(e[1])],c],b=k;continue}}}break;case
4:var
m=b[1],g=bj(0);aK(i,g);return[0,g,h,a(o[17][9],c),m]}var
j=a(al[3],jn);return n(ad[3],0,0,j)}}}function
N(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1],g=c(a[2]);return[0,[2,[3,dh(e)],[4,f]],g]}return[4,b[4]]}return c(b[3])}function
dq(c,a,b){var
e=d(el[20],a,b);return eE(jo[3],c,a,0,0,0,0,0,0,e)}function
dr(i,n,b,m){function
j(j){var
p=a(A[48][6],j),q=a(A[48][5],j),r=a(o[17][9],n),k=dp(a(A[48][4],j),i,r,p),l=k[1],s=k[2];function
t(n){var
p=a(h[9],1),j=f(cm),o=[0,l,0],r=[0,i,b,a(h[9],2),p,s,m],t=g===j?cm[1]:c===j?a(e[2],cm):cm,u=a(h[21],[0,t,r]),v=[0,a(h[9],1),[0,b,0]],w=a(h[34],v),x=[0,[0,a(E[1][6],jp)],w,u],y=a(h[19],x),z=d(h[33],i,h[14]),A=[0,[0,a(E[1][6],jq)],z,y],B=[0,a(h[19],A),o],C=a(h[34],B),k=dq(q,n,a(h[21],[0,l,[0,b]])),D=k[1];return[0,D,a(h[34],[0,C,[0,k[2],0]])]}return d(em[2],0,t)}return a(s[63][8],j)}function
cV(i,h,d){var
b=f(x),j=g===b?x[1]:c===b?a(e[2],x):x;return dr(j,i,h,d)}function
en(i,h,d){var
b=f(Y),j=g===b?Y[1]:c===b?a(e[2],Y):Y;return dr(j,i,h,d)}function
a7(d,c,b){return cV(d,c,a(h[34],[0,b[1],b[2]]))}function
eo(d,c,b){return en(d,c,a(h[34],[0,b[1],b[2]]))}function
ds(c,b){function
e(e){var
i=a(A[48][5],c),j=d(A[48][8],c,b),f=d(h[3],e,j);if(6===f[0]){var
g=dq(i,e,f[2]),k=g[1];return[0,k,a(h[34],[0,b,[0,g[2],0]])]}throw[0,bd,jr]}return d(em[2],0,e)}function
r(m,l,k){function
b(b){var
n=a(A[48][6],b),i=f(x),p=a(o[17][9],m),q=g===i?x[1]:c===i?a(e[2],x):x,j=dp(a(A[48][4],b),q,p,n),r=j[2],s=j[1];function
t(c){return ej(a(A[48][4],b),c,r)}var
u=d(o[17][15],t,l),v=[0,k,d(o[18],u,[0,s,0])];return ds(b,a(h[34],v))}return a(s[63][8],b)}function
a8(e,f){function
b(b){var
c=a(A[48][4],b);function
j(l,k){if(0===l)return di(b,k);var
e=d(h[3],c,k);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
m=e[1],n=f[2],g=d(h[3],c,f[1]);if(9===g[0]){var
i=g[2];if(2===i.length-1){var
o=g[1],p=i[1],q=di(b,i[2]),r=j(l-1|0,n),s=[0,m,[0,a(h[21],[0,o,[0,p,q]]),r]];return a(h[21],s)}}throw[0,bd,jt]}}throw[0,bd,js]}var
g=a(o[17][1],e),k=a(o[17][1],f)-g|0,l=a(A[48][6],b),m=a(o[17][9],e),n=cU(c,function(b,a){return j(k,a)},m,l);return d(i[5],n,2)}return a(s[63][9],b)}function
a9(i,m){var
b=m[2],h=m[1];switch(h[0]){case
0:var
n=h[2],j=h[1];if(0===b[0]){var
o=b[1],B=b[2],E=_(o),F=_(j);if(d(l[19],F,E)){var
p=a9([0,ju,i],[0,n,b]),q=f(C),G=p[1],H=[0,j,p[2]],I=g===q?C[1]:c===q?a(e[2],C):C;return[0,[0,r(i,jv,I),G],H]}var
s=a9([0,jw,i],[0,h,B]),t=f(aw),J=s[1],K=[0,o,s[2]],L=g===t?aw[1]:c===t?a(e[2],aw):aw;return[0,[0,r(i,jx,L),J],K]}var
M=_(b),N=_(j);if(d(l[19],N,M)){var
u=a9([0,jy,i],[0,n,b]),v=f(C),O=u[1],P=[0,j,u[2]],Q=g===v?C[1]:c===v?a(e[2],C):C;return[0,[0,r(i,jz,Q),O],P]}var
w=f(ax),R=[0,b,h],S=0,T=g===w?ax[1]:c===w?a(e[2],ax):ax;return[0,[0,r(i,jA,T),S],R];case
4:var
af=h[1];switch(b[0]){case
0:var
k=0;break;case
4:var
ag=[4,d(D[12],af,b[1])];return[0,[0,Z(i),0],ag];default:var
k=1}break;default:var
k=0}if(!k)if(0===b[0]){var
x=b[1],U=b[2],V=_(h),W=_(x);if(d(l[19],W,V)){var
y=a9([0,jB,i],[0,h,U]),z=f(aw),X=y[1],Y=[0,x,y[2]],$=g===z?aw[1]:c===z?a(e[2],aw):aw;return[0,[0,r(i,jC,$),X],Y]}return[0,0,[0,h,b]]}var
aa=_(b),ab=_(h);if(d(l[18],ab,aa)){var
A=f(ax),ac=[0,b,h],ad=0,ae=g===A?ax[1]:c===A?a(e[2],ax):ax;return[0,[0,r(i,jD,ae),ad],ac]}return[0,0,[0,h,b]]}function
ep(o,E,i,C,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],s=j[1],t=s[2],G=s[1];if(q===t){var
u=f(bL),H=g===u?bL[1]:c===u?a(e[2],bL):bL,v=r(b,jE,H),I=l[11],J=d(D[14],C,G),K=d(D[14],E,F),M=d(D[12],K,J);if(d(l[1],M,I)){var
w=f(W),N=g===w?W[1]:c===w?a(e[2],W):W,O=r(b,jF,N),P=[0,O,h(b,[0,k,n])];return[0,v,[0,Z([0,jH,[0,jG,b]]),P]]}return[0,v,h([0,jI,b],[0,k,n])]}if(d(l[19],q,t)){var
x=f(V),Q=h([0,jJ,b],[0,k,j]),R=g===x?V[1]:c===x?a(e[2],V):V;return[0,r(b,jK,R),Q]}var
y=f(L),S=h([0,jL,b],[0,i,n]),T=g===y?L[1]:c===y?a(e[2],L):L;return[0,r(b,jM,T),S]}var
z=f(V),U=h([0,jN,b],[0,k,0]),X=g===z?V[1]:c===z?a(e[2],V):V;return[0,r(b,jO,X),U]}var
A=m[2];if(A){var
B=f(L),Y=h([0,jP,b],[0,0,A[2]]),_=g===B?L[1]:c===B?a(e[2],L):L;return[0,r(b,jQ,_),Y]}return[0,a8(o,b),0]}return h(o,[0,i,b])}function
cW(o,i,E,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],s=j[1],t=s[2],G=s[1];if(q===t){var
u=f(bO),H=g===u?bO[1]:c===u?a(e[2],bO):bO,v=r(b,jR,H),I=l[11],J=d(D[14],E,G),K=d(D[12],F,J);if(d(l[1],K,I)){var
w=f(W),M=g===w?W[1]:c===w?a(e[2],W):W,N=r(b,jS,M),O=[0,N,h(b,[0,k,n])];return[0,v,[0,Z([0,jU,[0,jT,b]]),O]]}return[0,v,h([0,jV,b],[0,k,n])]}if(d(l[19],q,t)){var
x=f(C),P=h([0,jW,b],[0,k,j]),Q=g===x?C[1]:c===x?a(e[2],C):C;return[0,r(b,jX,Q),P]}var
y=f(L),R=h([0,jY,b],[0,i,n]),S=g===y?L[1]:c===y?a(e[2],L):L;return[0,r(b,jZ,S),R]}var
z=f(C),T=h([0,j0,b],[0,k,0]),U=g===z?C[1]:c===z?a(e[2],C):C;return[0,r(b,j1,U),T]}var
A=m[2];if(A){var
B=f(L),V=h([0,j2,b],[0,0,A[2]]),X=g===B?L[1]:c===B?a(e[2],L):L;return[0,r(b,j3,X),V]}return[0,a8(o,b),0]}return h(o,[0,i,b])}function
dt(h,b){if(b){var
m=b[2];if(d(l[4],b[1][1],l[11]))var
i=f(bM),n=g===i?bM[1]:c===i?a(e[2],bM):bM,j=n;else
var
k=f(bN),p=g===k?bN[1]:c===k?a(e[2],bN):bN,j=p;var
o=r(h,j4,j);return[0,o,dt(h,m)]}return[0,Z(h),0]}function
cI(i,j,b){switch(b[0]){case
0:var
v=b[2],k=cI([0,j5,i],j,b[1]),w=k[2],x=k[1],m=cI([0,j6,i],j,v),y=[0,w,m[2]],p=f(b0),z=d(o[18],x,m[1]),A=g===p?b0[1]:c===p?a(e[2],b0):b0;return[0,[0,r(i,j7,A),z],y];case
1:var
B=b[1],C=[2,B,[4,a(l[17],j)]],q=f(b1),D=[0,Z([0,j8,i]),0],E=g===q?b1[1]:c===q?a(e[2],b1):b1;return[0,[0,r(i,j9,E),D],C];case
2:var
s=b[2],F=b[1];if(4===s[0]){var
I=[2,F,[4,d(l[8],j,s[1])]],t=f(bz),J=[0,Z([0,j$,i]),0],L=g===t?bz[1]:c===t?a(e[2],bz):bz;return[0,[0,r(i,ka,L),J],I]}var
H=a(al[3],j_);return n(ad[6],0,0,H);case
3:return[0,0,[2,b,[4,j]]];case
4:var
M=[4,d(l[8],j,b[1])];return[0,[0,Z(i),0],M];default:var
N=b[1],u=f(K),O=[0,G(j),N],P=g===u?K[1]:c===u?a(e[2],K):K;return[0,0,[5,a(h[21],[0,P,O])]]}}function
cX(d){function
h(b,i){if(i){var
j=f(bP),k=h([0,kb,b],i[2]),l=g===j?bP[1]:c===j?a(e[2],bP):bP;return[0,r(b,kc,l),k]}return[0,a8(d,b),0]}return function(a){return h(d,a)}}function
eq(d){function
h(b,i){if(i){var
j=f(C),k=h([0,kd,b],i[2]),l=g===j?C[1]:c===j?a(e[2],C):C;return[0,r(b,ke,l),k]}return[0,a8(d,b),0]}return function(a){return h(d,a)}}function
cY(d){function
h(b,i){if(i){var
j=f(V),k=h([0,kf,b],i[2]),l=g===j?V[1]:c===j?a(e[2],V):V;return[0,r(b,kg,l),k]}return[0,a8(d,b),0]}return function(a){return h(d,a)}}function
cZ(i,b){switch(b[0]){case
0:var
v=b[2],j=cZ([0,kh,i],b[1]),w=j[2],x=j[1],k=cZ([0,ki,i],v),y=[0,w,k[2]],m=f(b2),z=d(o[18],x,k[1]),A=g===m?b2[1]:c===m?a(e[2],b2):b2;return[0,[0,r(i,kj,A),z],y];case
1:var
p=f(b4),C=b[1],D=0,E=g===p?b4[1]:c===p?a(e[2],b4):b4;return[0,[0,r(i,kk,E),D],C];case
2:var
q=b[2],F=b[1];if(4===q[0]){var
H=[2,F,[4,a(l[17],q[1])]],s=f(b3),I=[0,Z([0,km,i]),0],J=g===s?b3[1]:c===s?a(e[2],b3):b3;return[0,[0,r(i,kn,J),I],H]}var
G=a(al[3],kl);return n(ad[6],0,0,G);case
3:var
t=f(X),K=[2,b,[4,l[14]]],L=0,M=g===t?X[1]:c===t?a(e[2],X):X;return[0,[0,r(i,ko,M),L],K];case
4:var
N=[4,a(l[17],b[1])];return[0,[0,Z(i),0],N];default:var
u=f(B),O=[0,b[1]],P=g===u?B[1]:c===u?a(e[2],B):B;return[0,0,[5,a(h[21],[0,P,O])]]}}function
am(k,j,p){function
q(g,d){try{var
b=dT(k,d),f=b[1],l=a(h[10],b[2]),m=[0,[0,cV(j,a(h[10],f),l),0],[3,f]];return m}catch(b){b=t(b);if(a(ad[20],b)){var
c=dJ(0),e=af(0);dU(d,c,e,g);var
i=a(h[10],e);return[0,[0,cV(j,a(h[10],c),i),0],[3,c]]}throw b}}try{var
m=cH(k,p);if(typeof
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
ae=w[1],L=am(k,[0,kp,j],v[1]),ag=L[2],ah=L[1],M=am(k,[0,kq,j],ae),ai=M[1],N=a9(j,[0,ag,M[2]]),aj=N[2],ak=d(o[18],ai,N[1]),n=[0,d(o[18],ah,ak),aj],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
x=m[2];if(x){var
z=x[2];if(z)if(z[2])var
i=0,b=0;else{var
al=z[1],P=am(k,[0,kr,j],x[1]),A=P[2],Q=P[1],R=am(k,[0,ks,j],al),C=R[2],S=R[1];if(4===C[0])var
V=cI(j,C[1],A),as=V[2],at=d(o[18],S,V[1]),D=[0,d(o[18],Q,at),as];else
if(4===A[0])var
T=f(bA),an=A[1],ao=g===T?bA[1]:c===T?a(e[2],bA):bA,ap=r(j,kt,ao),U=cI(j,an,C),aq=U[2],ar=d(o[18],S,[0,ap,U[1]]),D=[0,d(o[18],Q,ar),aq];else
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
W=f(B),au=E[1],av=[0,F[1]],aw=g===W?B[1]:c===W?a(e[2],B):B,X=f(y),ax=[0,au,a(h[21],[0,aw,av])],ay=g===X?y[1]:c===X?a(e[2],y):y,Y=am(k,j,a(h[21],[0,ay,ax])),az=Y[2],aA=Y[1],n=[0,[0,O(eg),aA],az],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
3:var
H=m[2];if(H)if(H[2])var
i=0,b=0;else
var
aB=H[1],Z=f(y),aC=[0,aB,G(l[12])],aD=g===Z?y[1]:c===Z?a(e[2],y):y,_=am(k,j,a(h[21],[0,aD,aC])),aE=_[2],aF=_[1],n=[0,[0,O(ee),aF],aE],b=1;else
var
i=0,b=0;break;case
4:var
I=m[2];if(I)if(I[2])var
i=0,b=0;else
var
$=am(k,[0,ku,j],I[1]),aG=$[1],aa=cZ(j,$[2]),aH=aa[2],n=[0,d(o[18],aG,aa[1]),aH],b=1;else
var
i=0,b=0;break;case
5:var
J=m[2];if(J)if(J[2])var
i=0,b=0;else
var
aI=J[1],ab=f(y),aJ=[0,aI,G(l[14])],aK=g===ab?y[1]:c===ab?a(e[2],y):y,ac=am(k,j,a(h[21],[0,aK,aJ])),aL=ac[2],aM=ac[1],n=[0,[0,O(ef),aM],aL],b=1;else
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
aN=[0,0,[4,ei(k,p)]],n=aN,b=1}catch(c){c=t(c);if(!a(ad[20],c))throw c;var
n=q(0,p),b=1}break;default:var
i=0,b=0}if(b)var
s=n,i=1}else
var
i=0;break;default:var
i=0}if(!i)var
s=q(0,p);return s}catch(b){b=t(b);if(a(aW[4],b))return q(0,p);throw b}}function
du(h,d,b){switch(d[0]){case
2:var
i=d[1],t=d[2];switch(b[0]){case
2:if(3===i[0]){var
j=f(bY),u=[2,[3,i[1]],[0,d[2],b[2]]],v=g===j?bY[1]:c===j?a(e[2],bY):bY;return[0,r(h,kw,v),u]}break;case
3:var
k=f(bX),w=[2,[3,b[1]],[0,t,[4,l[12]]]],x=g===k?bX[1]:c===k?a(e[2],bX):bX;return[0,r(h,kx,x),w]}break;case
3:var
o=d[1];switch(b[0]){case
2:var
p=f(bW),y=[2,[3,o],[0,b[2],[4,l[12]]]],z=g===p?bW[1]:c===p?a(e[2],bW):bW;return[0,r(h,ky,z),y];case
3:var
q=f(bV),A=[2,[3,o],[4,l[13]]],B=g===q?bV[1]:c===q?a(e[2],bV):bV;return[0,r(h,kz,B),A]}break}aP(d);a(m[35],0);aP(b);a(m[35],0);a(m[51],m[27]);var
s=a(al[3],kv);return n(ad[6],0,0,s)}function
cJ(i,b){switch(b[0]){case
2:var
j=b[1];if(3===j[0]){var
k=b[2],p=j[1];if(4===k[0])return[0,0,b];var
h=function(b){switch(b[0]){case
0:var
c=b[1],e=h(b[2]),f=h(c);return d(D[12],f,e);case
4:return b[1];default:var
g=a(al[3],kB);return n(ad[6],0,0,g)}},q=[2,[3,p],[4,h(k)]];return[0,[0,Z([0,kC,i]),0],q]}break;case
3:var
m=f(bU),s=[2,[3,b[1]],[4,l[12]]],t=0,u=g===m?bU[1]:c===m?a(e[2],bU):bU;return[0,[0,r(i,kD,u),t],s]}aP(b);var
o=a(al[3],kA);return n(ad[6],0,0,o)}function
a_(b,j){switch(j[0]){case
0:var
h=j[2],i=j[1];switch(h[0]){case
0:var
k=h[1],A=h[2],B=_(k);if(_(i)===B){var
m=du([0,kE,b],i,k),n=f(by),C=m[2],D=m[1],E=g===n?by[1]:c===n?a(e[2],by):by,F=r(b,kF,E),p=a_(b,[0,C,A]);return[0,[0,F,[0,D,p[1]]],p[2]]}var
q=cJ([0,kG,b],i),G=q[2],H=q[1],s=a_([0,kH,b],h),I=[0,G,s[2]];return[0,d(o[18],H,s[1]),I];case
4:var
O=h[1],x=cJ([0,kK,b],i);return[0,x[1],[0,x[2],[4,O]]];default:var
J=_(h);if(_(i)===J){var
t=du(b,i,h),K=t[1],u=a_(b,t[2]);return[0,[0,K,u[1]],u[2]]}var
v=cJ([0,kI,b],i),L=v[2],M=v[1],w=a_([0,kJ,b],h),N=[0,L,w[2]];return[0,d(o[18],M,w[1]),N]}case
4:return[0,0,j];default:var
y=cJ(b,j),z=f(bZ),P=y[1],Q=[0,y[2],[4,l[11]]],R=g===z?bZ[1]:c===z?a(e[2],bZ):bZ,S=[0,r(b,kL,R),0];return[0,d(o[18],P,S),Q]}}function
c0(i,b){if(0===b[0]){var
h=b[1];if(2===h[0])if(3===h[1][0]){var
k=h[2];if(4===k[0]){var
o=b[2];if(d(l[1],k[1],l[11])){var
m=f(W),p=g===m?W[1]:c===m?a(e[2],W):W,q=r(i,kN,p),n=c0(i,o);return[0,[0,q,n[1]],n[2]]}}}var
j=c0([0,kM,i],b[2]);return[0,j[1],[0,h,j[2]]]}return[0,0,b]}function
dv(bn){var
k=a(E[1][6],kO),m=a(E[1][6],kP),n=a(E[1][6],kQ),y=G(l[11]);function
o(bo){var
p=bo;for(;;){if(p){var
b=p[1];switch(b[0]){case
0:var
ak=b[2],al=b[1],bp=p[2],bq=b[4],br=b[3],q=J(al[1]),am=z(N(al)),an=z(N(ak)),C=G(br),U=G(bq),ao=ae(cS(an,C),U),bs=cE(am,ao),bt=ak[3],bu=a(cY(kR),bt),bv=i[P],bw=R(bu),bx=[0,d(j[66][3],bw,bv),0],by=[0,i[60],[0,i[P],0]],bz=[0,O(aL),by],bA=[0,a(j[66][20],bz),0],bL=[0,i[60],[0,i[P],0]],bM=[0,O(aL),bL],bN=[0,a(j[66][20],bM),0],bO=[0,o(bp),0],bP=[0,a(i[25],[0,q,0]),bO],bU=[0,a(i[74],[0,m,[0,n,[0,q,0]]]),bP],bW=a(h[10],q),bX=a(h[10],n),ap=f(bB),bV=0,bY=[0,C,an,U,a(h[10],m),bX,bW],bZ=g===ap?bB[1]:c===ap?a(e[2],bB):bB,b0=[0,u([0,a(h[21],[0,bZ,bY]),bV]),bU],b1=[0,a(i[25],[0,m,[0,n,0]]),b0],b2=[0,a(j[66][20],b1),bN],b3=aM(C,y),b4=a(i[ab],b3),b5=[0,d(j[66][19],b4,b2),bA],b6=aM(C,U),b7=[0,a(i[ab],b6),0],b8=[0,a(i[25],[0,q,0]),b7],b9=[0,a(i[74],[0,k,[0,q,0]]),b8],b$=a(h[10],q),aq=f(bC),b_=0,ca=[0,am,ao,a(h[10],k),b$],cb=g===aq?bC[1]:c===aq?a(e[2],bC):bC,cc=[0,u([0,a(h[21],[0,cb,ca]),b_]),b9],cd=[0,a(i[25],[0,k,0]),cc],ce=a(j[66][20],cd),cf=[0,d(j[66][19],ce,b5),bx],cg=a(i[ab],bs);return d(j[66][19],cg,cf);case
1:var
F=b[2],H=b[1],ar=d(l[26],H[4],F),ch=d(D[14],ar,F),ci=d(D[13],H[4],ch),cj=H[3],ck=function(a){return d(l[9],a,F)},cl=d(l[43],ck,cj),as=[0,H[1],0,cl,ar],cm=z(N(as)),av=G(F),V=G(ci),cn=as[3],co=a(cY(kS),cn),cp=[0,i[60],[0,i[P],0]],cq=[0,O(aL),cp],cr=[0,a(j[66][20],cq),0],cs=[0,i[60],[0,i[P],0]],ct=[0,O(aL),cs],cu=[0,a(j[66][20],ct),0],cv=[0,i[41],0],cw=[0,R(co),cv],cx=[0,be(k),cw],cy=[0,a(i[25],[0,k,0]),cx],cz=[0,O(cR),cy],cA=[0,a(i[74],[0,m,[0,n,0]]),cz],cD=a(h[10],n),aw=f(bF),cB=0,cF=[0,V,av,cm,a(h[10],m),cD],cG=g===aw?bF[1]:c===aw?a(e[2],bF):bF,cH=[0,u([0,a(h[21],[0,cG,cF]),cB]),cA],cI=[0,a(i[25],[0,n,[0,m,0]]),cH],cJ=[0,a(j[66][20],cI),cu],cK=aM(av,V),cL=a(i[ab],cK),cM=[0,d(j[66][19],cL,cJ),cr],cN=aM(V,y),cO=a(i[ab],cN);return d(j[66][19],cO,cM);case
3:var
ax=p[2],ay=b[2],K=b[1],v=J(K[1]),cP=function(a){return d(l[9],a,ay)},W=d(l[44],cP,K),Y=z(N(K)),Z=z(N(W)),L=G(ay),az=cE(Y,cS(Z,L));if(2===K[2]){var
cQ=W[3],cT=a(cX(kT),cQ),cU=i[P],cV=R(cT),cZ=[0,d(j[66][3],cV,cU),0],c0=[0,o(ax),0],c1=[0,a(i[25],[0,v,0]),c0],c2=[0,a(i[74],[0,m,[0,v,0]]),c1],c4=a(h[10],v),aA=f(bR),c3=0,c5=[0,Y,Z,L,a(h[10],m),c4],c6=g===aA?bR[1]:c===aA?a(e[2],bR):bR,c7=[0,u([0,a(h[21],[0,c6,c5]),c3]),c2],c8=[0,a(i[25],[0,m,0]),c7],c9=[0,a(j[66][20],c8),cZ],c_=a(i[ab],az);return d(j[66][19],c_,c9)}var
c$=W[3],da=a(cX(kU),c$),db=i[P],dc=R(da),dd=[0,d(j[66][3],dc,db),0],de=[0,i[60],[0,i[P],0]],df=[0,O(aL),de],dg=[0,a(j[66][20],df),0],di=[0,o(ax),0],dj=[0,a(i[25],[0,v,0]),di],dk=[0,a(i[74],[0,m,[0,n,[0,v,0]]]),dj],dn=a(h[10],v),dp=a(h[10],m),aB=f(bE),dm=0,dq=[0,Y,Z,L,a(h[10],n),dp,dn],dr=g===aB?bE[1]:c===aB?a(e[2],bE):bE,ds=[0,u([0,a(h[21],[0,dr,dq]),dm]),dk],du=[0,a(i[25],[0,n,[0,m,0]]),ds],dv=[0,a(j[66][20],du),dg],dw=aM(L,y),dx=a(i[ab],dw),dy=[0,d(j[66][19],dx,dv),dd],dz=a(i[ab],az);return d(j[66][19],dz,dy);case
4:var
aC=p[2],aD=b[3],A=aD[2],Q=aD[1],aE=b[2],w=aE[2],_=aE[1],dA=b[1],$=af(0);aK($,dA);var
aF=J(w[1]),aG=J(A[1]),aH=z(N(w)),aI=z(N(A));if(d(l[1],_,l[12]))if(0===A[2]){switch(w[2]){case
0:var
aJ=f(bG),dB=g===aJ?bG[1]:c===aJ?a(e[2],bG):bG,aa=dB;break;case
1:var
aO=f(bH),dL=g===aO?bH[1]:c===aO?a(e[2],bH):bH,aa=dL;break;default:var
aP=f(bT),dM=g===aP?bT[1]:c===aP?a(e[2],bT):bT,aa=dM}var
dC=G(Q),dD=2===w[2]?kV:kW,dE=cW(dD,w[3],Q,A[3]),dF=[0,o(aC),0],dG=[0,a(i[25],[0,$,0]),dF],dH=[0,R(dE),dG],dI=a(h[10],aG),dJ=[0,aa,[0,aH,aI,dC,a(h[10],aF),dI]],dK=[0,u([0,a(h[21],dJ),0]),dH];return a(j[66][20],dK)}var
aQ=G(_),aR=G(Q),dN=ep(kX,_,w[3],Q,A[3]),dO=[0,i[60],[0,i[P],0]],dQ=[0,O(aL),dO],dR=[0,a(j[66][20],dQ),0],dS=[0,i[60],[0,i[P],0]],dT=[0,O(aL),dS],dU=[0,a(j[66][20],dT),0],dV=[0,o(aC),0],dW=[0,a(i[25],[0,$,0]),dV],dX=[0,R(dN),dW],dY=[0,a(i[74],[0,m,[0,n,0]]),dX],d0=a(h[10],aG),d1=a(h[10],aF),d2=a(h[10],n),aS=f(bI),dZ=0,d3=[0,aH,aI,aQ,aR,a(h[10],m),d2,d1,d0],d4=g===aS?bI[1]:c===aS?a(e[2],bI):bI,d5=[0,u([0,a(h[21],[0,d4,d3]),dZ]),dY],d6=[0,a(i[25],[0,n,[0,m,0]]),d5],d7=[0,a(j[66][20],d6),dU],d8=aM(aR,y),d9=a(i[ab],d8),d_=[0,d(j[66][19],d9,d7),dR],d$=aM(aQ,y),ea=a(i[ab],d$);return d(j[66][19],ea,d_);case
5:var
B=b[1],aU=B[5],aV=B[4],ac=B[3],aW=B[2],eb=p[2],ec=B[1],aX=af(0),ed=J(ac[1]);aK(aX,ec[1]);var
ad=z(N(aW)),ee=z(N(ac)),ag=dh(aU),aY=f(x),ef=cE(a(h[9],1),ad),eg=g===aY?x[1]:c===aY?a(e[2],x):x,aZ=f(x),eh=a(h[19],[0,[0,ag],eg,ef]),ei=g===aZ?x[1]:c===aZ?a(e[2],x):x,a0=f(cC),ej=[0,ei,eh],ek=g===a0?cC[1]:c===a0?a(e[2],cC):cC,el=a(h[21],[0,ek,ej]),em=G(aV),a1=f(X),en=cW(er,ac[3],aV,[0,[0,l[14],aU],aW[3]]),eo=g===a1?X[1]:c===a1?a(e[2],X):X,es=[0,r([0,k1,[0,k0,[0,kZ,er]]],kY,eo),en],et=i[P],eu=dP(ad),ev=[0,d(j[66][3],eu,et),0],ew=[0,o(eb),0],ex=[0,a(i[25],[0,aX,0]),ew],ey=[0,a(i[74],[0,k,0]),ex],ez=[0,R(es),ey],eB=a(h[10],k),eC=a(h[10],ed),a2=f(bK),eA=0,eD=[0,a(h[10],ag),ee,ad,em,eC,eB],eE=g===a2?bK[1]:c===a2?a(e[2],bK):bK,eF=[0,u([0,a(h[21],[0,eE,eD]),eA]),ez],eG=[0,a(i[25],[0,ag,[0,k,0]]),eF],eH=[0,a(i[74],[0,k,0]),eG],eI=[0,aT(k),eH],eJ=[0,a(i[25],[0,k,0]),eI],eK=[0,a(j[66][20],eJ),ev],eL=a(i[ab],el);return d(j[66][19],eL,eK);case
6:var
a3=p[2],eM=b[1];try{var
eN=o(a3),eO=J(eM[1]),eP=d(E[1][13][3],eO,bn),eQ=d(j[66][3],eP,eN);return eQ}catch(a){a=t(a);if(a===I){var
p=a3;continue}throw a}case
9:var
a4=b[2],ah=b[1],eR=N(ah),eS=N(a4),a5=f(au),eT=dt(k2,ah[3]),eU=g===a5?au[1]:c===a5?a(e[2],au):au,a6=f(au),eV=g===a6?au[1]:c===a6?a(e[2],au):au,a7=f(at),eW=g===a7?at[1]:c===a7?a(e[2],at):at,a8=f(M),eX=[0,eW,eV,eU],eY=g===a8?M[1]:c===a8?a(e[2],M):M,eZ=a(h[21],[0,eY,eX]),e0=[0,i[41],[0,i[P],0]],e1=[0,a(k3[1],eZ),0],e2=[0,i[60],[0,i[16],e1]],e3=[0,O(dl),e2],e4=a(j[66][20],e3),e5=d(j[66][19],e4,e0),e6=J(a4[1]),e7=a(h[10],e6),e8=J(ah[1]),e9=a(h[10],e8),e_=z(eS),a9=f(bD),e$=[0,z(eR),e_,e9,e7],fa=g===a9?bD[1]:c===a9?a(e[2],bD):bD,fb=a(h[21],[0,fa,e$]),fc=R(eT),fd=u([0,fb,0]),fe=d(j[66][3],fd,fc);return d(s[15],fe,e5);case
10:var
ai=b[2],aj=b[1],ff=b[3],fg=N(ai),fh=N(aj),fi=J(ai[1]),fj=J(aj[1]),a_=ff?l[14]:l[12],fk=cW(k4,ai[3],a_,aj[3]),fl=[0,i[P],0],fm=[0,be(k),fl],fn=[0,a(i[25],[0,k,0]),fm],fo=[0,R(fk),fn],fq=a(h[10],fj),fr=a(h[10],fi),fs=G(a_),ft=z(fh),a$=f(bQ),fp=0,fu=[0,z(fg),ft,fs,fr,fq],fv=g===a$?bQ[1]:c===a$?a(e[2],bQ):bQ,fw=[0,u([0,a(h[21],[0,fv,fu]),fp]),fo];return a(j[66][20],fw);case
11:var
S=b[2],fx=p[2],fy=b[3],fz=b[1],ba=af(0);aK(ba,fz);var
bb=J(S[1]),bc=J(fy),bd=z(N(S)),bf=z(N(a(l[45],S))),fA=S[3],bg=f(X),fB=a(cX(k5),fA),fC=g===bg?X[1]:c===bg?a(e[2],X):X,fD=[0,r(k7,k6,fC),fB],fE=i[P],fF=R(fD),fG=[0,d(j[66][3],fF,fE),0],fH=[0,o(fx),0],fI=[0,a(i[25],[0,ba,0]),fH],fJ=[0,a(i[74],[0,bb,[0,bc,[0,k,0]]]),fI],fL=a(h[10],k),fM=a(h[10],bc),bh=f(bJ),fK=0,fN=[0,bd,bf,a(h[10],bb),fM,fL],fO=g===bh?bJ[1]:c===bh?a(e[2],bJ):bJ,fP=[0,u([0,a(h[21],[0,fO,fN]),fK]),fJ],fQ=[0,a(i[25],[0,k,0]),fP],fR=[0,a(j[66][20],fQ),fG],fS=cE(bd,aN(bf)),fT=a(i[ab],fS);return d(j[66][19],fT,fR);case
12:var
fU=k8[15],fV=J(b[1]),fW=u([0,a(h[10],fV),0]);return d(j[66][3],fW,fU);case
13:var
fX=i[P],fY=be(J(b[1]));return d(j[66][3],fY,fX);case
14:var
fZ=b[1],f0=[0,i[P],0],f1=[0,be(k),f0],f2=[0,a(i[25],[0,k,0]),f1],f3=[0,O(cR),f2],f4=[0,i[60],f3],f5=[0,O(dl),f4],f6=J(fZ),f7=[0,u([0,a(h[10],f6),0]),f5];return a(j[66][20],f7);case
15:var
bi=b[3],bj=b[2],T=b[1],f8=bi[2],f9=bi[1],f_=bj[2],f$=bj[1],bk=af(0),bl=af(0);aK(bk,f$);aK(bl,f9);var
ga=J(T[1]),gb=T[3],gc=a(eq(k9),gb),gd=T[3],ge=a(cY(k_),gd),gf=z(N(T)),gg=[0,o(f8),0],gh=[0,a(i[25],[0,bl,0]),gg],gi=[0,R(ge),gh],gj=[0,a(j[66][20],gi),0],gk=[0,o(f_),0],gl=[0,a(i[25],[0,bk,0]),gk],gm=[0,R(gc),gl],gn=[0,a(j[66][20],gm),gj],bm=f(bS),go=[0,gf,[0,a(h[10],ga),0]],gp=g===bm?bS[1]:c===bm?a(e[2],bS):bS,gq=a(h[34],[0,gp,go]),gr=a(i[98],gq);return d(j[66][19],gr,gn)}}return a(s[13],0)}}return o}function
es(g,a,f){var
b=am(g,a,f),h=b[1],c=a_(a,b[2]),i=c[1],e=c0(a,c[2]),j=e[2],k=d(o[18],i,e[1]);return[0,d(o[18],h,k),j]}function
aX(t,e,s,r,q,p,n,m,c){var
f=c[2],g=c[1],k=es(t,[0,[0,q],k$],p),l=k[1],v=k[2],w=a(i[74],[0,e,0]),x=a(j[66][22],w),y=[0,r,[0,n,m,a(h[10],e)]],z=u([0,a(h[21],y),0]),A=d(j[66][3],z,x);if(a(o[17][53],l))return[0,g,f];var
b=af(0),B=[0,a(ek(b,s),v),f],C=[0,a(i[25],[0,b,0]),0],D=[0,A,[0,R(l),C]];return[0,[0,[0,b,a(j[66][20],D)],g],B]}function
c1(b,a){return n(A[48][1],dj[9],b,a)}function
et(I,h,F){var
k=F[1],ac=F[2],j=a(A[48][4],I);if(H.caml_string_equal(a(bi[3],k),la))return h;try{var
i=aV(j,ac);if(typeof
i==="number")var
d=0;else
if(1===i[0]){var
p=i[1];if(typeof
p==="number")if(16<=p){switch(p+c$|0){case
0:var
q=i[2];if(q){var
r=q[2];if(r){var
s=r[2];if(s)if(s[2])var
d=0,b=0;else{var
K=s[1],L=r[1],o=cG(j,c1(I,q[1]));if(typeof
o==="number")var
n=0;else
if(1===o[0]){var
O=o[1];if(typeof
O==="number")if(23===O)if(o[2])var
n=0;else
var
M=1,n=1;else
var
n=0;else
var
n=0}else
var
n=0;if(!n)var
M=0;if(M)var
N=f(b5),ad=ae(L,aN(K)),af=2,ag=g===N?b5[1]:c===N?a(e[2],b5):b5,m=aX(j,k,0,ag,af,ad,L,K,h),b=1;else
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
P=v[1],Q=u[1],R=f(b6),ah=ae(Q,aN(P)),ai=1,aj=g===R?b6[1]:c===R?a(e[2],b6):b6,m=aX(j,k,2,aj,ai,ah,Q,P,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
3:var
w=i[2];if(w){var
x=w[2];if(x)if(x[2])var
d=0,b=0;else
var
S=x[1],T=w[1],U=f(b_),ak=ae(S,aN(T)),al=2,am=g===U?b_[1]:c===U?a(e[2],b_):b_,m=aX(j,k,1,am,al,ak,T,S,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
4:var
y=i[2];if(y){var
z=y[2];if(z)if(z[2])var
d=0,b=0;else
var
V=z[1],W=y[1],an=aN(W),X=f(b7),ao=ae(ae(V,G(l[14])),an),ap=2,aq=g===X?b7[1]:c===X?a(e[2],b7):b7,m=aX(j,k,1,aq,ap,ao,W,V,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
5:var
B=i[2];if(B){var
C=B[2];if(C)if(C[2])var
d=0,b=0;else
var
Y=C[1],Z=B[1],_=f(b8),ar=ae(Z,aN(Y)),as=2,at=g===_?b8[1]:c===_?a(e[2],b8):b8,m=aX(j,k,1,at,as,ar,Z,Y,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;case
6:var
D=i[2];if(D){var
E=D[2];if(E)if(E[2])var
d=0,b=0;else
var
$=E[1],aa=D[1],au=aN($),ab=f(b9),av=ae(ae(aa,G(l[14])),au),aw=2,ax=g===ab?b9[1]:c===ab?a(e[2],b9):b9,m=aX(j,k,1,ax,aw,av,aa,$,h),b=1;else
var
d=0,b=0}else
var
d=0,b=0;break;default:var
d=0,b=0}if(b)var
J=m,d=1}else
var
d=0;else
var
d=0}else
var
d=0;if(!d)var
J=h;return J}catch(b){b=t(b);if(a(aW[4],b))return h;throw b}}function
aY(b){var
c=a(i[22],b),e=a(i[74],[0,b,0]),f=a(j[66][22],e);return d(j[66][3],f,c)}function
lb(k){dk(0);var
w=a(A[48][13],k);function
x(a,b){return et(k,a,b)}var
m=n(o[17][18],x,lc,w),p=m[1],y=m[2],z=dS(0),B=[0,a(s[13],0),0];function
C(n,m){var
d=m[2],o=d[2],k=d[1],p=m[1],q=n[2],r=n[1];if(d[3]){var
b=af(0),s=bj(0);aK(b,s);var
v=l[11],w=cQ(k),x=[0,[0,s,1,[0,[0,l[12],w],0],v],q],y=[0,a(i[25],[0,o,[0,b,0]]),[0,r,0]],z=[0,a(i[74],[0,b,0]),y],A=[0,aT(b),z],t=f(ca),B=[0,a(i[25],[0,k,[0,b,0]]),A],C=[0,p,0],D=g===t?ca[1]:c===t?a(e[2],ca):ca,E=a(h[34],[0,D,C]),F=[0,a(i[98],E),B];return[0,a(j[66][20],F),x]}var
u=f(b$),G=[0,a(i[25],[0,k,[0,o,0]]),[0,r,0]],H=[0,p,0],I=g===u?b$[1]:c===u?a(e[2],b$):b$,J=a(h[34],[0,I,H]),K=[0,a(i[98],J),G];return[0,a(j[66][20],K),q]}var
q=n(o[17][18],C,B,z),r=q[1],b=d(o[18],y,q[2]);if(cM[1])d(l[33],a2,b);if(bg[1])try{n(l[64],[0,bj,de,a2],0,b);var
F=a(s[13],0);return F}catch(b){b=t(b);if(b===l[27]){var
D=a(l[39],0),u=n(l[65],0,0,D)[2];if(bf[1])d(l[36],a2,u);var
E=a(dv(p),u);return d(j[66][3],r,E)}throw b}try{var
v=d(l[68],[0,bj,de,a2],b);if(bf[1])d(l[36],a2,v);var
H=a(dv(p),v),I=d(j[66][3],r,H);return I}catch(b){b=t(b);if(b===l[28]){var
G=a(al[3],ld);return d(j[66][5],0,G)}throw b}}var
eu=a(s[63][9],lb);function
le(b){var
m=el[81];function
n(a){return d(m,0,a)}var
aa=d(A[48][1],n,b);function
k(n,L){function
b(w){try{var
o=cH(w,L);if(typeof
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
A=z[1],B=y[1],ab=[0,k([0,lf,n],A),0],N=f(bn),ac=[0,k([0,lg,n],B),ab],ad=[0,B,[0,A,0]],ag=g===N?bn[1]:c===N?a(e[2],bn):bn,ai=aO(A),aj=[0,a7(n,ae(aO(B),ai),[0,ag,ad]),ac],u=a(j[66][20],aj),b=1;else
var
m=0,b=0}else
var
m=0,b=0;break;case
7:var
C=o[2];if(C){var
D=C[2];if(D)if(D[2])var
m=0,b=0;else
var
E=D[1],F=C[1],ak=[0,k([0,lh,n],E),0],O=f(bo),al=[0,k([0,li,n],F),ak],am=[0,F,[0,E,0]],an=g===O?bo[1]:c===O?a(e[2],bo):bo,ao=aO(E),ap=[0,a7(n,cS(aO(F),ao),[0,an,am]),al],u=a(j[66][20],ap),b=1;else
var
m=0,b=0}else
var
m=0,b=0;break;case
8:var
H=o[2];if(H){var
I=H[2];if(I)if(I[2])var
m=0,b=0;else
var
q=I[1],r=H[1],v=af(0),P=f(aB),aq=0,ar=0,as=[0,q,r],at=g===P?aB[1]:c===P?a(e[2],aB):aB,au=p([0,[0,v,a(h[21],[0,at,as])],ar]),Q=f(bq),av=[0,r,[0,q,[0,a(h[10],v),0]]],aw=g===Q?bq[1]:c===Q?a(e[2],bq):bq,ax=a7(n,G(l[11]),[0,aw,av]),ay=[0,d(j[66][3],ax,au),aq],az=[0,k([0,lj,n],q),0],R=f(aA),aF=[0,k([0,lk,n],r),az],aG=0,aH=[0,q,r],aI=g===R?aA[1]:c===R?a(e[2],aA):aA,aJ=[0,p([0,[0,v,a(h[21],[0,aI,aH])],aG]),aF],S=f(bp),aK=[0,r,[0,q,[0,a(h[10],v),0]]],aL=g===S?bp[1]:c===S?a(e[2],bp):bp,aM=aO(q),aN=[0,a7(n,eh(aO(r),aM),[0,aL,aK]),aJ],aP=[0,a(j[66][20],aN),ay],T=f(cj),aQ=a(i[25],[0,v,0]),aR=[0,q,[0,r,0]],aS=g===T?cj[1]:c===T?a(e[2],cj):cj,aT=a(h[34],[0,aS,aR]),aU=a(i[98],aT),aV=d(j[66][3],aU,aQ),u=d(j[66][19],aV,aP),b=1;else
var
m=0,b=0}else
var
m=0,b=0;break;case
9:var
J=o[2];if(J)if(J[2])var
m=0,b=0;else
var
U=J[1],V=f(aE),aX=g===V?aE[1]:c===V?a(e[2],aE):aE,W=f(aD),aY=[0,aX],aZ=g===W?aD[1]:c===W?a(e[2],aD):aD,X=f(aC),a0=[0,U,a(h[21],[0,aZ,aY])],a1=g===X?aC[1]:c===X?a(e[2],aC):aC,Y=a(h[21],[0,a1,a0]),_=f(ci),a2=k(n,Y),a3=[0,U,0],a4=g===_?ci[1]:c===_?a(e[2],ci):ci,a5=eo([0,ll,n],Y,[0,a4,a3]),u=d(j[66][3],a5,a2),b=1;else
var
m=0,b=0;break;case
10:var
K=o[2];if(K)if(K[2])var
m=0,b=0;else
var
a6=K[1],$=function(i){try{var
d=cH(w,i);if(typeof
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
g=0;return g}catch(b){b=t(b);if(a(aW[4],b))return 0;throw b}},aa=function(i,m){try{var
l=cH(w,m);if(typeof
l==="number")var
b=0;else
if(1===l[0]){var
q=l[1];if(typeof
q==="number")if(10===q){var
n=l[2];if(n)if(n[2])var
b=0;else
var
o=n[1],r=f(br),u=aa([0,lm,i],o),v=[0,o,0],x=g===r?br[1]:c===r?a(e[2],br):br,s=f(ah),y=[0,x,v],z=[0,aO(o)],A=g===s?ah[1]:c===s?a(e[2],ah):ah,B=a7(i,a(h[21],[0,A,z]),y),p=d(j[66][3],B,u),b=1;else
var
b=0}else
var
b=0;else
var
b=0}else
var
b=0;if(!b)var
p=k(i,m);return p}catch(b){b=t(b);if(a(aW[4],b))return k(i,m);throw b}},a8=$(a6)?Z(n):aa(n,L),u=a8,b=1;else
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
M=a(s[13],0);return M}catch(b){b=t(b);if(a(aW[4],b))return a(s[13],0);throw b}}return d(s[68][1],s[51],b)}function
p(b){if(b){var
m=b[2],i=b[1],l=i[1],ab=i[2],n=function(ac){try{var
n=aV(ac,ab);if(typeof
n==="number")var
i=0;else
if(1===n[0]){var
q=n[1];if(typeof
q==="number")if(16<=q){switch(q+c$|0){case
0:var
r=n[2];if(r){var
s=r[2];if(s){var
v=s[2];if(v)if(v[2])var
i=0,b=0;else{var
H=v[1],I=s[1],J=f(Y),ad=r[1],ae=g===J?Y[1]:c===J?a(e[2],Y):Y;if(d(aa,ad,ae))var
af=[0,p(m),0],ag=[0,aY(l),af],ah=[0,k(ln,H),ag],ai=[0,k(lo,I),ah],K=f(bx),aj=0,ak=[0,I,H,a(h[10],l)],al=g===K?bx[1]:c===K?a(e[2],bx):bx,am=[0,u([0,a(h[21],[0,al,ak]),aj]),ai],L=a(j[66][20],am);else
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
M=x[1],N=w[1],an=[0,p(m),0],ao=[0,aY(l),an],ap=[0,k(lp,M),ao],aq=[0,k(lq,N),ap],O=f(bw),ar=0,as=[0,N,M,a(h[10],l)],at=g===O?bw[1]:c===O?a(e[2],bw):bw,au=[0,u([0,a(h[21],[0,at,as]),ar]),aq],o=a(j[66][20],au),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
15:var
y=n[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else
var
P=z[1],Q=y[1],av=[0,p(m),0],aw=[0,aY(l),av],ax=[0,k(lr,P),aw],ay=[0,k(ls,Q),ax],R=f(bs),az=0,aA=[0,Q,P,a(h[10],l)],aB=g===R?bs[1]:c===R?a(e[2],bs):bs,aC=[0,u([0,a(h[21],[0,aB,aA]),az]),ay],o=a(j[66][20],aC),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
16:var
A=n[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else
var
S=B[1],T=A[1],aD=[0,p(m),0],aE=[0,aY(l),aD],aF=[0,k(lt,S),aE],aG=[0,k(lu,T),aF],U=f(bt),aH=0,aI=[0,T,S,a(h[10],l)],aJ=g===U?bt[1]:c===U?a(e[2],bt):bt,aK=[0,u([0,a(h[21],[0,aJ,aI]),aH]),aG],o=a(j[66][20],aK),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
17:var
C=n[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
V=D[1],W=C[1],aL=[0,p(m),0],aM=[0,aY(l),aL],aN=[0,k(lv,V),aM],aO=[0,k(lw,W),aN],X=f(bu),aP=0,aQ=[0,W,V,a(h[10],l)],aR=g===X?bu[1]:c===X?a(e[2],bu):bu,aS=[0,u([0,a(h[21],[0,aR,aQ]),aP]),aO],o=a(j[66][20],aS),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
18:var
E=n[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
Z=F[1],_=E[1],aT=[0,p(m),0],aU=[0,aY(l),aT],aX=[0,k(lx,Z),aU],aZ=[0,k(ly,_),aX],$=f(bv),a0=0,a1=[0,_,Z,a(h[10],l)],a2=g===$?bv[1]:c===$?a(e[2],bv):bv,a3=[0,u([0,a(h[21],[0,a2,a1]),a0]),aZ],o=a(j[66][20],a3),b=1;else
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
G=p(m);return G}catch(b){b=t(b);if(a(aW[4],b))return p(m);throw b}};return d(s[68][1],s[51],n)}return a(s[13],0)}var
q=a(A[48][13],b);return p(a(o[17][9],q))}var
ev=a(s[63][9],le);function
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
a$=[a1,lz,a0(0)];function
$(i,X){var
d=aV(a(A[48][4],i),X);if(typeof
d!=="number")switch(d[0]){case
1:var
k=d[1];if(typeof
k==="number")if(16<=k)switch(k+c$|0){case
0:var
m=d[2];if(m){var
n=m[2];if(n){var
o=n[2];if(o){if(!o[2]){var
B=o[1],C=n[1],Y=c1(i,m[1]),j=cG(a(A[48][4],i),Y);if(typeof
j!=="number"&&1===j[0]){var
p=j[1];if(typeof
p==="number")if(23===p){if(!j[2]){var
D=f(cb),Z=[0,C,B],_=g===D?cb[1]:c===D?a(e[2],cb):cb;return a(h[21],[0,_,Z])}}else
if(24===p)if(!j[2]){var
E=f(ck),aa=[0,C,B],ab=g===E?ck[1]:c===E?a(e[2],ck):ck;return a(h[21],[0,ab,aa])}}throw a$}var
b=1}else
var
b=0}else
var
b=1}else
var
b=1;break;case
9:var
s=d[2];if(s){var
u=s[2];if(u){if(!u[2]){var
G=u[1],H=s[1],ah=$(i,G),J=f(co),ai=[0,H,G,$(i,H),ah],aj=g===J?co[1]:c===J?a(e[2],co):co;return a(h[21],[0,aj,ai])}var
b=1}else
var
b=1}else
var
b=1;break;case
10:var
v=d[2];if(v){var
w=v[2];if(w){if(!w[2]){var
K=w[1],L=v[1],ak=$(i,K),M=f(cn),al=[0,L,K,$(i,L),ak],am=g===M?cn[1]:c===M?a(e[2],cn):cn;return a(h[21],[0,am,al])}var
b=1}else
var
b=1}else
var
b=1;break;case
11:if(!d[2]){var
N=f(cs);return g===N?cs[1]:c===N?a(e[2],cs):cs}var
b=0;break;case
12:if(!d[2]){var
O=f(cu);return g===O?cu[1]:c===O?a(e[2],cu):cu}var
b=0;break;case
13:var
x=d[2];if(x){if(!x[2]){var
P=x[1],Q=f(cr),an=[0,P,$(i,P)],ao=g===Q?cr[1]:c===Q?a(e[2],cr):cr;return a(h[21],[0,ao,an])}var
b=0}else
var
b=1;break;case
14:var
y=d[2];if(y){var
z=y[2];if(z){if(!z[2]){var
R=z[1],S=y[1],ap=$(i,R),T=f(cq),aq=[0,S,R,$(i,S),ap],ar=g===T?cq[1]:c===T?a(e[2],cq):cq;return a(h[21],[0,ar,aq])}var
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
q=d[2];if(q){var
r=q[2];if(r)if(!r[2]){var
ac=r[1],ad=q[1];try{var
l=ew(k),F=f(l),ae=[0,ad,ac],af=g===F?l[1]:c===F?a(e[2],l):l,ag=a(h[21],[0,af,ae]);return ag}catch(a){a=t(a);if(a===I)throw a$;throw a}}}}break;case
2:var
U=d[2],V=d[1],as=$(i,U),W=f(cp),at=[0,V,U,$(i,V),as],au=g===W?cp[1]:c===W?a(e[2],cp):cp;return a(h[21],[0,au,at])}throw a$}function
c2(d,c,b){var
e=a(s[63][5],b);return n(i[13],d,c,e)}function
aa(b,e){function
c(f){var
c=c2(0,b,f),g=a(e,c),h=d(i[2],0,c);return d(j[66][3],h,g)}var
f=a(s[63][8],c),g=a(i[74],[0,b,0]),h=a(j[66][22],g);return d(j[66][3],h,f)}function
dw(b,g){function
c(c){var
e=c2(0,d(bi[5],b,lA),c),f=c2(0,d(bi[5],b,lB),c),h=[0,d(g,e,f),0],k=[0,d(i[2],0,f),h],l=[0,d(i[2],0,e),k];return a(j[66][20],l)}var
e=a(s[63][8],c),f=a(i[74],[0,b,0]),h=a(j[66][22],f);return d(j[66][3],h,e)}function
lC(b){var
a1=a(A[48][7],b);function
v(a){return $(b,a)}function
ar(a){return c1(b,a)}function
k(m){if(m){var
l=m[2],C=m[1],b=a(c3[2][1][1],C),n=function(x){try{var
r=aV(x,a(c3[2][1][3],C));if(typeof
r==="number")var
m=0;else
switch(r[0]){case
1:var
L=r[1];if(typeof
L==="number")if(18<=L){switch(L+dz|0){case
7:var
M=r[2];if(M){var
N=M[2];if(N)if(N[2])var
m=0,n=0;else
var
a2=N[1],a3=M[1],a4=dw(b,function(b,a){return k([0,[0,b,a3],[0,[0,a,a2],l]])}),a5=aT(b),y=d(j[66][3],a5,a4),n=1;else
var
m=0,n=0}else
var
m=0,n=0;break;case
8:var
O=r[2];if(O){var
P=O[2];if(P)if(P[2])var
m=0,n=0;else
var
a6=P[1],a7=O[1],a8=0,a9=[0,aa(b,function(a){return k([0,[0,a,a6],l])}),a8],a_=[0,aa(b,function(a){return k([0,[0,a,a7],l])}),a9],ba=aT(b),y=d(j[66][19],ba,a_),n=1;else
var
m=0,n=0}else
var
m=0,n=0;break;case
9:if(r[2])var
m=0,n=0;else
var
y=aT(b),n=1;break;case
11:var
Q=r[2];if(Q)if(Q[2])var
m=0,n=0;else{var
s=aV(x,Q[1]);if(typeof
s==="number")var
p=0;else
switch(s[0]){case
1:var
D=s[1];if(typeof
D==="number")if(16<=D){switch(D+c$|0){case
0:var
S=s[2];if(S){var
T=S[2];if(T){var
U=T[2];if(U)if(U[2])var
p=0,q=0,o=0;else{var
E=U[1],F=T[1],av=S[1];if(bg[1]){var
V=cG(x,ar(av));if(typeof
V==="number")var
A=0;else
if(1===V[0]){var
W=V[1];if(typeof
W==="number"){if(23===W)var
bb=0,bc=[0,aa(b,function(a){return k(l)}),bb],aB=f(cc),bd=[0,F,E,a(h[10],b)],be=g===aB?cc[1]:c===aB?a(e[2],cc):cc,bf=a(h[21],[0,be,bd]),bh=[0,a(i[98],bf),bc],aC=a(j[66][20],bh),ao=1;else
if(24===W)var
bi=0,bj=[0,aa(b,function(a){return k(l)}),bi],aD=f(cl),bk=[0,F,E,a(h[10],b)],bl=g===aD?cl[1]:c===aD?a(e[2],cl):cl,bm=a(h[21],[0,bl,bk]),bn=[0,a(i[98],bm),bj],aC=a(j[66][20],bn),ao=1;else
var
A=0,ao=0;if(ao)var
aw=aC,A=1}else
var
A=0}else
var
A=0;if(!A)var
aw=k(l);var
ax=aw}else{var
X=cG(x,ar(av));if(typeof
X==="number")var
B=0;else
if(1===X[0]){var
Y=X[1];if(typeof
Y==="number"){if(23===Y)var
aF=f(az),bo=k(l),bp=[0,F,E],bq=g===aF?az[1]:c===aF?a(e[2],az):az,br=a(h[21],[0,bq,bp]),bs=d(c3[2][1][5],br,C),bt=a(i[6],bs),aG=d(j[66][3],bt,bo),ap=1;else
if(24===Y)var
aH=f(ay),bu=k(l),bv=[0,F,E],bw=g===aH?ay[1]:c===aH?a(e[2],ay):ay,bx=a(h[21],[0,bw,bv]),by=d(c3[2][1][5],bx,C),bz=a(i[6],by),aG=d(j[66][3],bz,bu),ap=1;else
var
B=0,ap=0;if(ap)var
aE=aG,B=1}else
var
B=0}else
var
B=0;if(!B)var
aE=k(l);var
ax=aE}var
z=ax,o=1}else
var
q=1,o=0}else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;case
9:var
$=s[2];if($){var
ab=$[2];if(ab)if(ab[2])var
p=0,q=0,o=0;else
var
aL=ab[1],ac=$[1],bJ=v(ac),bK=0,bL=[0,aa(b,function(a){var
b=aJ(aL);return k([0,[0,a,cT(aJ(ac),b)],l])}),bK],aM=f(cw),bM=0,bN=[0,ac,aL,bJ,a(h[10],b)],bO=g===aM?cw[1]:c===aM?a(e[2],cw):cw,bP=[0,u([0,a(h[21],[0,bO,bN]),bM]),bL],z=a(j[66][20],bP),o=1;else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;case
10:var
ad=s[2];if(ad){var
ae=ad[2];if(ae)if(ae[2])var
p=0,q=0,o=0;else
var
aN=ae[1],aO=ad[1],bQ=0,bR=[0,aa(b,function(a){var
b=aJ(aN);return k([0,[0,a,cF(aJ(aO),b)],l])}),bQ],aP=f(cv),bS=0,bT=[0,aO,aN,a(h[10],b)],bU=g===aP?cv[1]:c===aP?a(e[2],cv):cv,bV=[0,u([0,a(h[21],[0,bU,bT]),bS]),bR],z=a(j[66][20],bV),o=1;else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;case
13:var
af=s[2];if(af)if(af[2])var
q=1,o=0;else
var
ag=af[1],bW=v(ag),bX=0,bY=[0,aa(b,function(a){return k([0,[0,a,ag],l])}),bX],aQ=f(cz),bZ=0,b0=[0,ag,bW,a(h[10],b)],b1=g===aQ?cz[1]:c===aQ?a(e[2],cz):cz,b2=[0,u([0,a(h[21],[0,b1,b0]),bZ]),bY],z=a(j[66][20],b2),o=1;else
var
p=0,q=0,o=0;break;case
14:var
ah=s[2];if(ah){var
ai=ah[2];if(ai)if(ai[2])var
p=0,q=0,o=0;else
var
H=ai[1],J=ah[1],b3=v(J),b4=v(H),b5=0,b6=[0,aa(b,function(a){var
b=cF(aJ(J),H);return k([0,[0,a,cT(cF(J,aJ(H)),b)],l])}),b5],aR=f(cy),b7=0,b8=[0,J,H,b3,b4,a(h[10],b)],b9=g===aR?cy[1]:c===aR?a(e[2],cy):cy,b_=[0,u([0,a(h[21],[0,b9,b8]),b7]),b6],z=a(j[66][20],b_),o=1;else
var
p=0,q=0,o=0}else
var
p=0,q=0,o=0;break;default:var
q=1,o=0}if(o)var
aA=z,q=2}else
var
q=1;else
var
q=1;switch(q){case
0:var
w=0;break;case
1:var
Z=s[2];if(Z){var
_=Z[2];if(_)if(_[2])var
p=0,w=0;else{var
bA=_[1],bB=Z[1];try{var
G=ex(D),bC=0,bD=[0,aa(b,function(a){return k(l)}),bC],aK=f(G),bE=0,bF=[0,bB,bA,a(h[10],b)],bG=g===aK?G[1]:c===aK?a(e[2],G):G,bH=[0,u([0,a(h[21],[0,bG,bF]),bE]),bD],bI=a(j[66][20],bH),aI=bI}catch(a){a=t(a);if(a!==I)throw a;var
aI=k(l)}var
aA=aI,w=1}else
var
p=0,w=0}else
var
p=0,w=0;break;default:var
w=1}if(w)var
R=aA,p=1;break;case
2:var
aS=s[2],aj=s[1],b$=v(aj),ca=0,cb=[0,aa(b,function(a){return k([0,[0,a,cF(aj,aJ(aS))],l])}),ca],aU=f(cx),cd=0,ce=[0,aj,aS,b$,a(h[10],b)],cf=g===aU?cx[1]:c===aU?a(e[2],cx):cx,cg=[0,u([0,a(h[21],[0,cf,ce]),cd]),cb],R=a(j[66][20],cg),p=1;break;default:var
p=0}if(!p)var
R=k(l);var
y=R,n=1}else
var
m=0,n=0;break;case
12:var
ak=r[2];if(ak){var
al=ak[2];if(al)if(al[2])var
m=0,n=0;else
var
aX=al[1],aY=ak[1],ch=dw(b,function(b,a){var
c=[0,[0,a,d(h[33],aX,aY)],l];return k([0,[0,b,d(h[33],aY,aX)],c])}),ci=aT(b),y=d(j[66][3],ci,ch),n=1;else
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
au=k(l),n=2;else
var
m=0,n=0}else
var
m=0,n=0;break;default:var
m=0,n=0}switch(n){case
0:var
aq=0;break;case
1:var
au=y,aq=1;break;default:var
aq=1}if(aq)var
K=au,m=1}else
var
m=0;else
var
m=0;break;case
2:var
am=r[2],an=r[1],cj=a(a1,am);if(d(lD[e_],x,cj))var
ck=v(an),cm=0,cn=[0,aa(b,function(a){return k([0,[0,a,cT(aJ(an),am)],l])}),cm],aZ=f(cA),co=0,cp=[0,an,am,ck,a(h[10],b)],cq=g===aZ?cA[1]:c===aZ?a(e[2],cA):cA,cr=[0,u([0,a(h[21],[0,cq,cp]),co]),cn],a0=a(j[66][20],cr);else
var
a0=k(l);var
K=a0,m=1;break;default:var
m=0}if(!m)var
K=k(l);return K}catch(b){b=t(b);if(b===a$)return k(l);if(a(aW[4],b))return k(l);throw b}};return d(s[68][1],s[51],n)}return d(j[66][3],ev,eu)}return k(a(s[63][4],b))}var
c4=a(s[63][9],lC);function
lE(b){var
k=a(s[63][3],b);function
u(a){return $(b,a)}function
l(k){function
b(b){function
m(d){var
c=aV(b,k);return a(s[13],c)}function
n(b){if(typeof
b!=="number")switch(b[0]){case
1:var
p=b[1];if(typeof
p==="number"){var
q=p-27|0;if(!(2<q>>>0))switch(q){case
0:if(!b[2])return c4;break;case
1:break;default:var
r=b[2];if(r)if(!r[2]){var
B=i[16],C=O(cR),D=d(j[66][3],C,B);return d(j[66][3],D,c4)}}}break;case
2:var
E=l(b[2]);return d(j[66][3],i[16],E)}try{var
w=u(k),x=i[16],y=function(d){var
b=f(ct),i=[0,k,w],j=g===b?ct[1]:c===b?a(e[2],ct):ct;return ds(d,a(h[21],[0,j,i]))},z=a(s[63][8],y),A=d(j[66][3],z,x),o=A}catch(b){b=t(b);if(b===a$)var
m=f(aH),v=g===m?aH[1]:c===m?a(e[2],aH):aH,n=a(i[e_],v);else{if(!a(s[67][10],b))throw b;var
n=d(s[18],0,b)}var
o=n}return d(j[66][3],o,c4)}var
o=a(s[67][11],m);return d(s[68][1],o,n)}return d(s[68][1],s[51],b)}return l(k)}var
ey=a(s[63][9],lE);function
lF(b){a(a3[3],lG);dV(0);return ey}var
lH=a(s[13],0),dx=[0,f0,l,aT,be,f1,f2,cM,bf,bg,cN,f3,bh,aU,dF,af,gj,dJ,bj,de,dN,a2,cQ,dh,dO,R,dP,u,gp,O,dQ,aK,gr,J,dR,dU,dT,dk,dS,dV,dW,dX,bm,F,k,ac,Q,aq,ar,as,S,T,U,x,at,au,y,K,B,ag,ah,a4,av,bn,bo,bp,bq,br,bs,bt,bu,bv,bw,bx,C,by,bz,aw,ax,bA,bB,bC,bD,bE,bF,bG,bH,bI,bJ,bK,bL,V,L,bM,bN,bO,bP,bQ,bR,bS,bT,bU,bV,bW,bX,bY,W,bZ,b0,b1,b2,b3,X,b4,b5,b6,b7,b8,b9,b_,b$,ca,cb,dY,dZ,d0,d1,d2,cc,d3,d4,d5,d6,d7,ay,az,ai,aj,a5,a6,aA,cd,ce,aB,aC,cf,cg,ch,Y,aD,aE,ci,cj,ck,d8,d9,d_,d$,cl,ea,eb,ec,ed,cm,cn,co,cp,cq,cr,cs,ct,cu,cv,cw,cx,cy,cz,cA,cB,ak,aF,aG,M,cC,aH,cD,aI,ee,ef,eg,dl,aL,i3,i5,cR,i7,ae,cS,eh,cE,i8,aM,aN,cF,cT,aJ,i9,aO,G,aV,cG,cH,ei,cU,ej,dp,Z,aP,_,z,ek,N,dq,dr,cV,en,a7,eo,ds,r,a8,a9,ep,cW,dt,cI,cX,eq,cY,cZ,am,du,cJ,a_,c0,dv,es,aX,c1,et,aY,eu,ev,ew,ex,a$,$,c2,aa,dw,c4,ey,d(s[68][1],lH,lF)];c9(442,dx,"Omega_plugin.Coq_omega");a(dy[12],aQ);function
cK(b){var
c=d(p[17],E[1][6],lI),e=a(E[5][4],c),f=a(E[6][4],b),g=d(E[13][2],[0,e],f),h=a(aZ[6][6],g);return a(aZ[12][21],h)}function
c5(b){var
c=d(o[17][103],lJ[33],b);function
e(b){if(c_(b,lK)){if(c_(b,lL)){if(c_(b,lM)){if(c_(b,lN)){var
c=d(m[16],lO,b),e=a(al[3],c);return n(ad[6],0,0,e)}return cK(lP)}return cK(lQ)}return cK(lR)}return cK(lS)}var
f=d(p[17],e,c),g=dx[262],h=a(j[66][20],f),i=a(j[66][29],h);return d(j[66][3],i,g)}function
lT(e){var
b=[28,[0,0,[31,d(ez[10],0,[0,[0,[0,aQ,lU],0],0])]]],c=a(E[1][6],lV);return cL(aZ[6][4],1,0,c,b)}var
lW=[0,function(b,a){return c5(0)}];n(aZ[6][9],0,[0,aQ,lX],lW);d(dy[19],lT,aQ);var
lY=0,l1=[0,function(b){return b?a(m[2],lZ):function(a){return c5(l0)}},lY],l3=[0,function(b){if(b)if(!b[2]){var
c=b[1],e=a(eB[17],eA[9]),f=a(eB[6],e),g=d(aZ[12][2][7],f,c);return function(a){return c5(d(p[17],E[1][8],g))}}return a(m[2],l2)},l1],l5=a(l4[12],l3);n(aZ[6][9],0,[0,aQ,l6],l5);function
l7(g){var
e=[0,a(E[1][7],l9)],b=eA[9],c=0;if(0===b[0]){var
f=[0,[0,ma,[0,l$,[0,[1,d(ez[10],0,[0,[0,[5,[0,b[1]]]],e])],c]]],l8];return n(aZ[9][4],[0,aQ,mb],0,f)}throw[0,bd,l_]}d(dy[19],l7,aQ);var
eC=[0,aQ,cK,c5];c9(450,eC,"Omega_plugin.G_omega");c9(451,[0,dc,dx,eC],"Omega_plugin");return});
