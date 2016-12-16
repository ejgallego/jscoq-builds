(function(lZ){"use strict";var
eB="*",eK=123,ad=145,a7=112,a_=140,eR=" 0\n",eV="Z.lt",e0="Z.le",c3="Equation E",c2=" and E",dr=": ",a9=".\n",f=250,eA="Z.succ",eG="omega_plugin",eJ="(",eP="Z.pred",eQ="not a number",eO="ZArith",ez="+ ",eF="- ",l=124,ev="------------------------\n\n",d=246,ey="Z.sub",eu="tag_hypothesis",aN="omega",eE="Inequation E",ds="Z",eN="Extension: cannot occur",o=113,eM=")",eU="N",eZ="State",a6=122,eI="with",eD="omega'",eT="X%d",eC=" subsumes E",du=" E",ao="Omega",eS=" states ",u=136,aX=248,ex="positive",dq=-18,ew="Equations E",dt="nat",eX=121,eY="Omega: Can't solve a goal with non-linear products",c1=-16,eL="Z.ge",eH="Z.gt",a8="Coq",eW="E%d subsumes E%d.\n",L=lZ.jsoo_runtime,an=L.caml_check_bound,et=L.caml_equal,aW=L.caml_fresh_oo_id,b=L.caml_new_string,cX=L.caml_notequal,h=L.caml_obj_tag,c0=L.caml_register_global,cE=L.caml_string_notequal,cZ=L.caml_trampoline,cY=L.caml_trampoline_return,w=L.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):L.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):L.caml_call_gen(a,[b,c])}function
v(a,b,c,d){return a.length==3?a(b,c,d):L.caml_call_gen(a,[b,c,d])}function
cD(a,b,c,d,e){return a.length==4?a(b,c,d,e):L.caml_call_gen(a,[b,c,d,e])}function
lY(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):L.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}var
s=L.caml_get_global_data(),lX=[11,b(" + "),[2,0,[12,32,[2,0,[11,b(du),[4,0,0,0,[11,b(a9),0]]]]]]],eg=[0,[0,2],[0,1,0]],dN=[0,b(a8),[0,b("Logic"),[0,b("Decidable"),0]]],aV=b(eG),r=s.List,z=s.Printf,n=s.Pervasives,q=s.Util,M=s.Not_found,ap=s.Int,y=s.Hashtbl,dv=s.Assert_failure,H=s.Names,g=s.CamlinternalLazy,e=s.Term,I=s.Bigint,ae=s.CErrors,p=s.Proofview,aP=s.Logic,D=s.Tacmach,C=s.Coqlib,i=s.Tactics,j=s.Tacticals,c9=s.Context,a$=s.Nameops,c6=s.Pp,c5=s.Universes,c7=s.Nametab,c8=s.Libnames,cF=s.Goptions,er=s.Constrarg,ep=s.Loc,eq=s.Genarg,eo=s.Tacinterp,cV=s.Tacenv,dp=s.Mltop,aO=[0,0],fl=[0,[11,b(eE),[4,0,0,0,[11,b(" is divided by "),[2,0,[11,b(" and the constant coefficient is rounded by substracting "),[2,0,[11,b(a9),0]]]]]]],b("Inequation E%d is divided by %s and the constant coefficient is rounded by substracting %s.\n")],fm=[0,[11,b("Constant in equation E"),[4,0,0,0,[11,b(" is not divisible by the pgcd "),[2,0,[11,b(" of its other coefficients.\n"),0]]]]],b("Constant in equation E%d is not divisible by the pgcd %s of its other coefficients.\n")],fn=[0,[12,69,[4,0,0,0,[11,b(" is trivially satisfiable.\n"),0]]],b("E%d is trivially satisfiable.\n")],fo=[0,[11,b(c3),[4,0,0,0,[11,b(" is divided by the pgcd "),[2,0,[11,b(" of its coefficients.\n"),0]]]]],b("Equation E%d is divided by the pgcd %s of its coefficients.\n")],fp=[0,[11,b("We state "),[2,0,[11,b(du),[4,0,0,0,[11,b(" = "),[2,0,[12,32,[2,0,[11,b(du),[4,0,0,0,lX]]]]]]]]]],b("We state %s E%d = %s %s E%d + %s %s E%d.\n")],fq=[0,[11,b("We define a new equation E"),[4,0,0,0,[11,b(dr),0]]],b("We define a new equation E%d: ")],fr=b(" 0"),fs=[0,[11,b("We define E"),[4,0,0,0,[11,b(dr),0]]],b("We define E%d: ")],ft=b(eR),fu=[0,[12,69,[4,0,0,0,[11,b(eC),[4,0,0,0,[11,b(a9),0]]]]],b(eW)],fv=[0,[12,69,[4,0,0,0,[11,b(eC),[4,0,0,0,[11,b(a9),0]]]]],b(eW)],fw=[0,[11,b(ew),[4,0,0,0,[11,b(c2),[4,0,0,0,[11,b(" imply a contradiction on their constant factors.\n"),0]]]]],b("Equations E%d and E%d imply a contradiction on their constant factors.\n")],fx=[0,[11,b(ew),[4,0,0,0,[11,b(c2),[4,0,0,0,[11,b(" state that their body is at the same time equal and different\n"),0]]]]],b("Equations E%d and E%d state that their body is at the same time equal and different\n")],fy=[0,[12,69,[4,0,0,0,[11,b(c2),[4,0,0,0,[11,b(" can be merged into E"),[4,0,0,0,[11,b(a9),0]]]]]]],b("E%d and E%d can be merged into E%d.\n")],fz=[0,[11,b(c3),[4,0,0,0,[11,b(eS),[2,0,[11,b(" = 0.\n"),0]]]]],b("Equation E%d states %s = 0.\n")],fA=[0,[11,b(eE),[4,0,0,0,[11,b(" states 0 != 0.\n"),0]]],b("Inequation E%d states 0 != 0.\n")],fB=[0,[11,b(c3),[4,0,0,0,[11,b(eS),[2,0,[11,b(" >= 0.\n"),0]]]]],b("Equation E%d states %s >= 0.\n")],fC=[0,[11,b(c3),[4,0,0,0,[11,b(" is split in E"),[4,0,0,0,[11,b(c2),[4,0,0,0,[11,b("\n\n"),0]]]]]]],b("Equation E%d is split in E%d and E%d\n\n")],fD=[0,[11,b("To ensure a solution in the dark shadow the equation E"),[4,0,0,0,[11,b(" is weakened by "),[2,0,[11,b(a9),0]]]]],b("To ensure a solution in the dark shadow the equation E%d is weakened by %s.\n")],fO=b("depend"),fS=b("solve"),fQ=[0,b("plugins/omega/omega.ml"),602,15],fP=b("no contradiction"),fN=b("disequation in simplify"),fM=b("Product dardk"),fL=[0,0,0,0],fI=b("tl"),fJ=b("TL"),fH=b("eliminate_with_in"),fE=[0,[12,88,[4,0,0,0,0]],b(eT)],fj=b(">= 0\n"),fk=b(ev),fg=[0,[12,69,[4,0,0,0,[11,b(dr),0]]],b("E%d: ")],fh=[0,[2,0,[11,b(eR),0]],b("%s 0\n")],fi=b(ev),fd=b("equation"),fe=b("inequation"),ff=b("disequation"),fa=b("="),fb=b(">="),fc=b("!="),e5=b(eF),e8=b(ez),e9=b(""),e6=[0,[2,0,[12,32,0]],b("%s ")],e7=[0,[2,0,[12,32,[2,0,[12,32,0]]]],b("%s %s ")],e_=[0,[11,b(ez),[2,0,[12,32,0]]],b("+ %s ")],e$=[0,[11,b(eF),[2,0,[12,32,0]]],b("- %s ")],e2=b("pgcd_l"),e3=b("Omega.MakeOmegaSolver(I).UNSOLVABLE"),e4=b("Omega.MakeOmegaSolver(I).NO_CONTRADICTION"),fF=b("Omega.MakeOmegaSolver(I).FACTOR1"),fG=b("Omega.MakeOmegaSolver(I).CHOPVAR"),fK=b("Omega.MakeOmegaSolver(I).SOLVED_SYSTEM"),fR=b("Omega.MakeOmegaSolver(I).FULL_SOLUTION"),i4=b(eJ),i5=b("+"),i6=b(eM),i7=b("~"),i8=b(eJ),i9=b(eB),i_=b(eM),i$=b("?"),ja=b("weight"),ji=[0,2],jj=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jk=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],je=[0,2],jf=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jg=[0,2],jh=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jl=[0,2],jm=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jn=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],jO=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],[0,[0,[0,1],[0,[0,1],[0,[0,2],[0,[0,1],0]]]],0]]]],jP=[0,1],jQ=[0,2],jR=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jS=[0,2],jT=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jU=b(eY),jV=[0,2],jW=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],j3=[0,1],j4=[0,2],j5=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],j6=[0,[0,[0,1],[0,[0,1],0]],0],j7=b(eY),j8=[0,2],j9=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],j_=[0,[0,[0,1],0],0],j$=[0,1],ka=[0,2],kb=[0,1],kc=[0,2],kd=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],ke=[0,1],kv=[0,0,0],ks=[0,1],kt=[0,2],ko=[0,1],kp=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kq=[0,1],kr=[0,2],ku=[0,1],kx=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],kw=[0,2],ln=[0,b(a8),[0,b(aN),[0,b(ao),0]]],li=b("_left"),lj=b("_right"),k5=[0,1],kY=[0,2],kZ=[0,1],k0=[0,2],k1=[0,1],k2=[0,2],k3=[0,1],k4=[0,1],k6=[0,[0,3],[0,1,0]],k7=[0,[0,2],[0,1,0]],k8=[0,[0,2],[0,1,0]],k9=[0,[0,1],[0,1,0]],k_=[0,[0,2],[0,1,0]],k$=[0,[0,1],[0,1,0]],la=[0,[0,2],[0,1,0]],lb=[0,[0,1],[0,1,0]],lc=[0,[0,2],[0,1,0]],ld=[0,[0,1],[0,1,0]],le=[0,[0,2],[0,1,0]],lf=[0,[0,1],[0,1,0]],kV=[0,0,0],kW=b("Omega can't solve this system"),kU=b(eZ),kT=[0,1,0],kB=[0,[0,3],0],kC=[0,[0,2],0],kD=[0,[0,3],0],kE=[0,[0,3],0],kF=[0,[0,1],[0,1,0]],kG=[0,[0,2],[0,1,0]],kH=[0,[0,2],[0,1,0]],kI=[0,[0,[0,1],0],0],kJ=[0,2],kK=[0,1],kL=[0,1],kM=[0,[0,2],[0,1,0]],kN=[0,[0,1],[0,1,0]],kO=[0,[0,3],0],kP=[0,[0,[0,1],0],0],kQ=[0,[0,3],0],kR=[0,[0,2],[0,1,0]],kS=[0,[0,2],[0,1,0]],ky=b("auxiliary"),kz=b("auxiliary_1"),kA=b("auxiliary_2"),kl=b("condense.1"),km=[0,2],kn=[0,0,0],kk=b("reduce_factor.1"),kg=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],kh=[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]],ki=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,2],0]],0]],kj=[0,[0,[0,1],0],0],kf=b("shrink.1"),j1=[0,2],j2=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jZ=[0,2],j0=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jX=[0,2],jY=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]]],jB=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]],jC=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jD=[0,2],jE=[0,1],jF=[0,2],jG=[0,2],jH=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jI=[0,2],jJ=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jK=[0,2],jL=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jM=[0,2],jN=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jo=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]]],jp=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jq=[0,2],jr=[0,1],js=[0,2],jt=[0,2],ju=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jv=[0,2],jw=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jx=[0,2],jy=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],jz=[0,2],jA=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],jc=b("H"),jd=b("P"),jb=b("compile_equation"),i3=b("x"),i2=b("occurrence "),i1=b("abstract_path "),iZ=b(eQ),i0=b(eQ),iY=b("Omega: Not a quantifier-free goal"),iU=b("not"),iS=b(eV),iQ=b(eL),iP=b(eH),iO=b(e0),iN=b(ey),iM=b(eP),iL=b(eA),iJ=b(" is not an evaluable constant"),iK=[0,b("Coq_omega")],iI=b("iff"),iH=b("imp_simp"),iG=b("not_not"),iF=b("not_iff"),iE=b("not_imp"),iD=b("not_and"),iC=b("not_or"),iB=b("dec_True"),iA=b("dec_not_not"),iz=b("dec_False"),iy=b("dec_not"),ix=b("dec_iff"),iw=b("dec_imp"),iv=b("dec_and"),iu=b("dec_or"),it=b("eq_ind_r"),is=b("not_gt"),ir=b("not_ge"),iq=b("not_lt"),ip=b("not_le"),io=b("not_eq"),im=b("dec_gt"),il=b("dec_ge"),ik=b("dec_lt"),ij=b("dec_le"),ii=b("dec_eq_nat"),ih=b("le_gt_dec"),ig=b("pred_of_minus"),ie=b("O"),id=b("S"),ic=b(dt),ib=b("Nat.pred"),ia=b("Nat.mul"),h$=b("Nat.add"),h_=b("Nat.sub"),h9=b("gt"),h8=b("ge"),h7=b("lt"),h6=b("le"),h5=b(eV),h4=b(eL),h3=b(eH),h2=b(e0),h1=b("Zne"),h0=b("neq"),hZ=b("Znot_gt_le"),hY=b("Znot_ge_lt"),hX=b("Znot_lt_ge"),hW=b("Znot_le_gt"),hV=b("not_Zne"),hU=b("not_Zeq"),hT=b("dec_Zge"),hS=b("dec_Zgt"),hR=b("Z.lt_decidable"),hQ=b("Z.le_decidable"),hP=b("dec_Zne"),hO=b("Z.eq_decidable"),hN=b("intro_Z"),hM=b("new_var"),hL=b("Zle_left"),hK=b("Zgt_left"),hJ=b("Zge_left"),hI=b("Zlt_left"),hH=b("Zne_left"),hG=b("Zegal_left"),hF=b("fast_Zopp_involutive"),hE=b("fast_Zopp_eq_mult_neg_1"),hD=b("fast_Zopp_mult_distr_r"),hC=b("fast_Zopp_plus_distr"),hB=b("fast_Zmult_opp_comm"),hA=b("fast_Zmult_plus_distr_l"),hz=b("fast_Zred_factor6"),hy=b("fast_Zred_factor5"),hx=b("fast_Zred_factor4"),hw=b("fast_Zred_factor3"),hv=b("fast_Zred_factor2"),hu=b("fast_Zred_factor1"),ht=b("fast_Zred_factor0"),hs=b("OMEGA20"),hr=b("OMEGA19"),hq=b("OMEGA18"),hp=b("OMEGA17"),ho=b("fast_OMEGA16"),hn=b("fast_OMEGA15"),hm=b("fast_OMEGA14"),hl=b("fast_OMEGA13"),hk=b("fast_OMEGA12"),hj=b("fast_OMEGA11"),hi=b("fast_OMEGA10"),hh=b("OMEGA9"),hg=b("OMEGA8"),hf=b("OMEGA7"),he=b("OMEGA6"),hd=b("OMEGA5"),hc=b("OMEGA4"),hb=b("OMEGA3"),ha=b("OMEGA2"),g$=b("OMEGA1"),g_=b("Zmult_le_approx"),g9=b("fast_Zmult_comm"),g8=b("fast_Zplus_comm"),g7=b("fast_Zplus_permute"),g6=b("fast_Zmult_assoc_reverse"),g5=b("fast_Zplus_assoc"),g4=b("fast_Zplus_assoc_reverse"),g3=b("inj_eq"),g2=b("inj_neq"),g1=b("Znat.inj_gt"),g0=b("Znat.inj_ge"),gZ=b("Znat.inj_lt"),gY=b("Znat.inj_le"),gX=b("Nat2Z.inj_succ"),gW=b("inj_minus2"),gV=b("Nat2Z.inj_sub"),gU=b("Nat2Z.inj_mul"),gT=b("Nat2Z.inj_add"),gS=b("Z.of_nat"),gR=b(eP),gQ=b(eA),gP=b(ey),gO=b("Z.opp"),gN=b("Z.mul"),gM=b("Z.add"),gL=b("Gt"),gK=b("comparison"),gJ=b(ds),gI=b("Zneg"),gH=b("Zpos"),gG=b("Z0"),gF=b("xI"),gE=b("xO"),gD=b("xH"),gs=b("find_contr"),gr=b(eu),gp=b(eu),gn=[0,1],gm=[0,[12,88,[4,0,0,0,0]],b(eT)],gl=b("WW"),gk=b("Zvar"),gj=b(eZ),gh=b(ao),f4=[0,b(ao),[0,b("System"),0]],f5=b("Omega system time displaying flag"),f8=[0,b(ao),[0,b("Action"),0]],f9=b("Omega action display flag"),ga=[0,b(ao),[0,b("OldStyle"),0]],gb=b("Omega old style flag"),ge=[0,b("Stable"),[0,b(ao),0]],gf=b("Omega automatic reset of generated names"),gt=[0,[0,b(a8),[0,b(aN),[0,b("OmegaLemmas"),0]]],0],gx=b(ao),gy=b(ao),gz=[0,[0,b(a8),[0,b(eO),0]],0],gA=b(ao),gB=[0,[0,b(a8),[0,b(eO),[0,b("BinInt"),0]]],0],gC=b(ao),lh=b("Coq_omega.Undecidable"),lT=[0,b("plugins/omega/g_omega.ml4"),1,0],lR=[0,[0,[0,b(aN)],[0,[0,b(eI)],[0,[0,b(eB)],0]]],0],lS=b("$l"),lU=[0,b(eI)],lV=[0,b(aN)],lW=b(eD),lM=b(eN),lK=[0,b(dt),[0,b(ex),[0,b(eU),[0,b(ds),0]]]],lJ=b(eN),lE=b(aN),lF=b(aN),lu=b(eU),lv=b(ds),lw=b(dt),lx=b(ex),lz=b("zify_positive"),lA=b("zify_nat"),lB=b("zify_op"),lC=b("zify_N"),ly=b("No Omega knowledge base for type "),lt=[0,b("PreOmega"),[0,b(aN),[0,b(a8),0]]],ls=b(eG),lH=b(aN),lP=b(eD),e1=s.Failure,fU=s.Reductionops,fZ=s.Contradiction,fY=s.Equality,fX=s.Evarutil,fV=s.Globnames,fW=s.Global,fT=s.Tacred,lp=s.Tacentries,lq=s.String,lr=s.Array,c4=[0,aO,function(f){var
j=f[1],s=f[2];function
as(b,a){var
d=c(f[2],b,a),e=d||et(b,a);return e}function
B(b,a){return c(f[2],a,b)}function
Q(b,a){var
d=c(f[2],a,b),e=d||et(b,a);return e}var
h=f[3],l=f[4],i=f[5];function
u(b,a){return c(f[6],b,a)[1]}function
R(b,a){return c(f[6],b,a)[2]}var
d=f[8],e=f[9],x=c(h,e,e),ae=a(f[7],e);function
m(b){return c(f[2],b,d)?a(f[7],b):b}var
g=f[10],t=f[7];function
at(b,a){return b<a?1:0}function
au(b,a){return a<b?1:0}function
av(b,a){return b<=a?1:0}function
aw(b,a){return a<=b?1:0}function
ax(b){a(n[29],b);a(n[32],0);return a(n[46],n[24])}function
F(b,a){a[1]=[0,b,a[1]];return 0}function
af(f,e){var
b=f,a=e;for(;;){if(c(j,a,d))return b;var
g=R(b,a),b=a,a=g;continue}}function
ag(b){return b?v(r[16],af,b[1],b[2]):a(n[2],e2)}function
G(b,a){var
g=Q(b,d),f=B(a,d);return 0===g?0===f?u(b,a):c(l,u(c(h,b,e),a),e):0===f?c(l,u(c(l,b,e),a),e):u(b,a)}var
k=[aX,e3,aW(0)],ah=[aX,e4,aW(0)];function
H(h,f){var
b=f[2],k=f[1],l=0;function
o(i,b){var
k=c(s,b[1],d)?e5:i?e8:e9;a(n[27],k);var
f=m(b[1]);if(c(j,f,e)){var
l=a(h,b[2]);c(z[2],e6,l)}else{var
o=a(h,b[2]),p=a(g,f);v(z[2],e7,p,o)}return 1}v(r[16],o,l,k);if(B(b,d)){var
p=a(g,b);return c(z[2],e_,p)}var
i=c(s,b,d);if(i){var
q=a(g,m(b));return c(z[2],e$,q)}return i}function
X(a){function
b(b,a){if(15===a[0]){var
d=a[2][2],f=X(a[3][2]),g=X(d);return c(h,c(h,c(h,b,e),g),f)}return c(h,b,e)}return v(r[16],b,d,a)}function
S(a){switch(a){case
0:return fa;case
1:return fb;default:return fc}}function
T(a){switch(a){case
0:return fd;case
1:return fe;default:return ff}}function
A(d,b){function
e(a){var
b=a[4],e=a[3],f=a[2];c(z[2],fg,a[1]);H(d,[0,e,b]);var
g=S(f);return c(z[2],fh,g)}c(r[11],e,b);return a(n[27],fi)}function
ay(d,b){function
e(b){H(d,b);return a(n[27],fj)}c(r[11],e,b);return a(n[27],fk)}function
I(d,p){var
e=p;for(;;){if(e){var
b=e[1],q=e[2];switch(b[0]){case
0:var
r=b[3],s=b[1],t=a(g,b[4]),u=a(g,r);cD(z[2],fl,s[1],u,t);break;case
1:var
w=b[1],x=a(g,b[2]);v(z[2],fm,w[1],x);break;case
2:c(z[2],fn,b[1]);break;case
3:var
y=b[1],A=a(g,b[2]);v(z[2],fo,y[1],A);break;case
4:var
j=b[3],k=j[2],l=b[2],i=l[2],B=j[1],C=l[1],D=b[1],E=k[1],F=T(k[2]),G=a(g,B),J=i[1],K=T(i[2]),L=a(g,C),M=T(i[2]);lY(z[2],fp,M,D,L,K,J,G,F,E);break;case
5:var
f=b[1][1];c(z[2],fq,f[1]);H(d,[0,f[3],f[4]]);var
N=S(f[2]);a(n[27],N);a(n[27],fr);break;case
6:var
h=b[1];c(z[2],fs,h[1]);H(d,[0,h[3],h[4]]);var
O=S(h[2]);a(n[27],O);a(n[27],ft);break;case
7:v(z[2],fu,b[1],b[2]);break;case
8:v(z[2],fv,b[1],b[2]);break;case
9:v(z[2],fw,b[1][1],b[2][1]);break;case
10:v(z[2],fx,b[1][1],b[2][1]);break;case
11:cD(z[2],fy,b[2][1],b[3],b[1]);break;case
12:var
P=b[1],Q=a(g,b[2]);v(z[2],fz,P,Q);break;case
13:c(z[2],fA,b[1]);break;case
14:var
R=b[1],U=a(g,b[2]);v(z[2],fB,R,U);break;case
15:var
m=b[3],o=b[2],V=m[2],W=o[2];cD(z[2],fC,b[1][1],o[1],m[1]);I(d,W);a(n[32],0);I(d,V);a(n[32],0);break;default:var
X=b[1],Y=a(g,b[2]);v(z[2],fD,X,Y)}var
e=q;continue}return a(n[46],n[24])}}function
ai(a){return c(z[4],fE,a)}var
Y=[0,0];function
U(a){Y[1]=0;return 0}function
J(a){return Y[1]}function
b(a){if(aO[1])I(ai,[0,a,0]);return F(a,Y)}function
az(b,a){return a[2]-b[2]|0}var
aj=a(r[41],az);function
aA(b){var
c=b[2],d=c[2],e=b[1];return[0,e,[0,a(aj,c[1]),d]]}function
C(i){function
e(k){var
b=k;for(;;){if(b){var
f=b[2],g=b[1],h=a(i,g[1]);if(c(j,h,d)){var
b=f;continue}var
l=e(f);return[0,[0,h,g[2]],l]}return 0}}return e}function
D(c,b){var
d=a(c,b[4]),e=b[3],f=a(C(c),e);return[0,b[1],b[2],f,d]}function
aB(b){return a(t,b)}function
K(a){return D(aB,a)}function
L(m,l){var
b=m,a=l;for(;;){if(b){if(a){var
g=a[2],f=a[1],i=b[2],e=b[1];if(e[2]===f[2]){var
k=c(h,e[1],f[1]);if(c(j,k,d)){var
b=i,a=g;continue}var
n=L(i,g);return[0,[0,k,e[2]],n]}return f[2]<e[2]?[0,e,L(i,a)]:[0,f,L(b,g)]}return b}return a}}function
Z(e,b,d){var
f=c(h,b[4],d[4]),g=L(b[3],d[3]),i=b[2];return[0,a(e,0),i,g,f]}var
_=[aX,fF,aW(0)];function
$(a){if(a){var
d=a[2],b=a[1];if(c(j,m(b[1]),e))return[0,b,d];var
f=$(d);return[0,f[1],[0,b,f[2]]]}throw _}var
V=[aX,fG,aW(0)];function
N(c,a){if(a){var
d=a[2],b=a[1];if(b[2]===c)return[0,b,d];var
e=N(c,d);return[0,e[1],[0,b,e[2]]]}throw V}function
p(f){var
g=f[4],p=f[3],n=f[2],o=f[1];if(0===p)switch(n){case
0:if(c(j,g,d))return 0;b([12,o,g]);throw k;case
1:if(Q(g,d))return 0;b([14,o,g]);throw k;default:if(cX(g,d))return 0;b([13,o]);throw k}function
w(a){return m(a[1])}var
h=ag(c(r[13],w,p));if(0===n)if(cX(R(g,h),d)){b([1,f,h]);throw k}if(2===n)if(cX(R(g,h),d)){b([2,f[1]]);return 0}if(cX(h,e)){var
s=G(g,h),x=c(l,g,c(i,s,h)),t=[0,o,n,a(C(function(a){return u(a,h)}),p),s];if(0===n)var
q=0;else
if(2===n)var
q=0;else
var
v=[0,f,t,h,x],q=1;if(!q)var
v=[3,f,h];b(v);return[0,t,0]}return[0,f,0]}function
E(o,g,f,d){var
h=g[1],p=d[3],q=g[2];try{var
k=N(q,p)[1],l=c(j,h,e)?a(t,k[1]):c(j,h,ae)?k[1]:a(n[2],fH),m=Z(o,d,D(function(a){return c(i,a,l)},f));b([4,m[1],[0,e,d],[0,l,f]]);return m}catch(a){a=w(a);if(a===V)return d;throw a}}function
aa(b,a){var
d=c(i,x,a);return c(l,b,c(i,a,G(c(h,c(i,x,b),a),d)))}function
ak(k,f,K,J){var
g=k[1],l=f[3],L=k[3],F=a(k[2],0);try{var
ac=a(r[4],l),ad=a(r[3],l)[2],ae=[0,m(a(r[3],l)[1]),ad],af=function(b,a){var
c=b[1],d=b[2];if(B(c,m(a[1]))){var
e=a[2];return[0,m(a[1]),e]}return[0,c,d]},ag=v(r[16],af,ae,ac),s=ag}catch(b){b=w(b);if(b[1]===e1)if(cE(b[2],fI))var
z=0;else{A(L,[0,f,0]);var
s=a(n[2],fJ),z=1}else
var
z=0;if(!z)throw b}var
M=s[2],d=c(h,s[1],e),O=aa(f[4],d),P=f[3],Q=a(C(function(a){return aa(a,d)}),P),R=[0,[0,a(t,d),F],Q],H=[0,a(g,0),0,R,O],S=c(i,x,d),T=a(t,G(c(h,c(i,x,f[4]),d),S)),U=f[3],V=a(C(function(b){var
e=c(i,x,d);return a(t,G(c(h,c(i,x,b),d),e))}),U);b([5,[0,H,[0,a(g,0),0,V,T],f,d,F]]);var
W=p(H),j=a(r[3],W),y=N(M,j[3])[1];function
X(a){return p(E(g,y,j,a))}var
Y=c(q[17][o],X,K);function
Z(a){return p(E(g,y,j,a))}var
_=c(q[17][o],Z,J),I=E(g,y,j,f),$=D(function(a){return u(a,d)},I);b([3,I,d]);var
ab=p($);return[0,a(r[3],ab),Y,_]}function
al(d,i){var
b=i;for(;;){var
f=b[3],e=b[2],a=b[1],g=d[1],j=d[3];if(aO[1])A(j,[0,a,e]);try{var
h=$(a[3])[1],k=function(b,c,d){return function(a){return p(E(c,d,b,a))}}(a,g,h),l=c(q[17][o],k,f),m=function(b,c,d){return function(a){return p(E(c,d,b,a))}}(a,g,h),n=[0,c(q[17][o],m,e),l];return n}catch(c){c=w(c);if(c===_){var
b=ak(d,a,e,f);continue}throw c}}}function
ab(l,p){var
f=p;for(;;){var
g=f[2],h=f[1],q=l[3],n=function(a){if(a){var
d=a[2],b=a[1],g=b[3],h=function(a){return c(j,m(a[1]),e)};if(c(r[24],h,g))return[0,b,d];var
f=n(d);return[0,f[1],[0,b,f[2]]]}throw M};if(h){var
s=h[2],t=h[1];try{var
o=n(h),u=o[2],v=o[1],a=v,i=u}catch(b){b=w(b);if(b!==M)throw b;var
a=t,i=s}if(0===a[3]){if(c(j,a[4],d)){b([2,a[1]]);var
f=[0,i,g];continue}b([12,a[1],a[4]]);throw k}var
f=al(l,[0,a,i,g]);continue}if(aO[1])A(q,g);return g}}function
O(n,e){function
x(a){var
b=a[3];if(b)if(c(s,b[1][1],d))return[0,K(a),0];return[0,a,1]}var
f=c(y[1],0,7);function
i(z){var
m=x(z),n=m[2],a=m[1],g=a[3];if(0===g){if(c(s,a[4],d)){b([14,a[1],a[4]]);throw k}return b([2,a[1]])}try{var
o=c(y[6],f,g),i=o[2],j=o[1];if(1===n)if(j)var
l=j[1],C=c(s,l[4],a[4])?(b([7,l[1],a[1]]),l):(b([7,a[1],l[1]]),a),e=[0,[0,C],i];else
var
e=[0,[0,a],i];else
if(i){var
h=i[1];if(B(h[4],a[4]))b([8,h[1],a[1]]);else
b([8,a[1],h[1]]);var
E=B(h[4],a[4])?h:a,e=[0,j,[0,E]]}else
var
e=[0,j,[0,a]];var
p=e[1];if(p){var
q=e[2];if(q){var
r=q[1],t=p[1];if(c(s,t[4],r[4])){b([9,t,K(r)]);throw k}var
u=1}else
var
u=0}else
var
u=0;c(y[9],f,g);var
D=v(y[5],f,g,e);return D}catch(b){b=w(b);if(b===M){var
A=1===n?[0,[0,a],0]:[0,0,[0,a]];return v(y[5],f,g,A)}throw b}}c(r[11],i,e);var
h=[0,0],g=[0,0];function
l(o,f){var
d=f[1];if(d){var
i=f[2];if(i){var
k=i[1],e=d[1];if(c(j,e[4],k[4])){var
l=a(n,0);b([11,l,e,k[1]]);return F([0,l,0,e[3],e[4]],h)}}}var
m=f[2];if(d)F(d[1],g);return m?F(K(m[1]),g):0}c(y[11],l,f);return[0,h[1],g[1]]}var
W=[aX,fK,aW(0)];function
am(g){var
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
i=c(s,b,e[1]),d=i||(-1===a[1]?1:0),j=d?(a[1]=h,e[1]=b,0):d;return j}c(y[11],j,b);if(f[1]<1)throw W;return a[1]}function
an(i,b){function
c(c,b){var
e=c[3],f=c[2],g=c[1];try{var
h=N(i,b[3])[1],j=Q(h[1],d)?[0,g,[0,[0,h[1],b],f],e]:[0,g,f,[0,[0,a(t,h[1]),b],e]];return j}catch(a){a=w(a);if(a===V)return[0,[0,b,g],f,e];throw a}}return v(r[16],c,fL,b)}function
ao(u,t,d,g){var
f=0;function
h(h,d){var
j=d[2],f=d[1];function
k(m,k){var
o=k[2],g=k[1],v=D(function(a){return c(i,a,f)},o),q=Z(u,D(function(a){return c(i,a,g)},j),v);b([4,q[1],[0,g,j],[0,f,o]]);var
h=p(q);if(h){var
d=h[1];if(h[2])return a(n[2],fM);if(t){var
w=c(l,g,e),r=c(i,c(l,f,e),w);b([16,d[1],r]);var
x=c(l,d[4],r),s=[0,d[1],1,d[3],x]}else
var
s=d;return[0,s,m]}return m}return v(r[16],k,h,g)}return v(r[16],h,f,d)}function
ac(d,f,b){var
g=d[3],h=d[1],a=an(am(b),b),i=a[1],j=ao(h,f,a[2],a[3]),e=c(n[22],i,j);if(aO[1])A(g,e);return e}function
aq(d,l,e){var
h=d[1],m=d[3];function
s(a){return 2===a[2]?1:0}if(c(r[24],s,e))a(n[2],fN);U(0);function
t(a){return b([6,a])}c(r[11],t,e);var
u=c(q[17][o],p,e);function
v(a){return 0===a[2]?1:0}var
i=c(r[32],v,u),x=i[1],j=O(h,i[2]),y=j[2],z=[0,c(n[22],x,j[1]),y];function
g(b,c){var
a=ab(d,c);return b<50?f(b+1|0,a):cY(f,[0,a])}function
f(e,f){var
a=O(h,f),b=a[2],c=a[1];if(0===c)return b;var
d=[0,c,b];return e<50?g(e+1|0,d):cY(g,[0,d])}function
B(a){return cZ(g(0,a))}function
C(a){return cZ(f(0,a))}function
k(b){try{var
a=k(C(ac(d,l,b)));return a}catch(a){a=w(a);if(a===W){if(aO[1])A(m,b);return b}throw a}}return k(B(z))}function
P(k,j,i){var
f=k,d=j,e=i;for(;;){if(e){var
g=e[2],b=e[1];switch(b[0]){case
0:if(c(ap[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
1:var
f=[0,b[1][1],f],d=[0,b,d],e=g;continue;case
2:var
e=g;continue;case
3:if(c(ap[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
4:var
l=b[3][2],m=b[2][2];if(c(ap[4][1],b[1],f)){var
f=[0,m[1],[0,l[1],f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
5:var
h=b[1],o=h[3];if(c(ap[4][1],h[1][1],f)){var
f=[0,o[1],f],d=[0,b,d],e=g;continue}var
e=g;continue;case
6:if(c(ap[4][1],b[1][1],f)){var
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
p=b[3],q=b[2];if(c(ap[4][1],b[1],f)){var
f=[0,q[1],[0,p,f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
12:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
13:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
14:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
15:return a(n[2],fO);default:if(c(ap[4][1],b[1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue}}return[0,f,d]}}function
aC(b,c){var
d=b[3],e=b[1];try{aq(e,0,c);var
f=a(n[2],fP);return f}catch(a){a=w(a);if(a===k)return I(d,P(0,0,J(0))[2]);throw a}}function
ar(a){var
g=a[2],h=a[1];function
i(a){return 2===a[2]?1:0}var
j=c(r[32],i,g)[1];function
e(a){var
b=a[3];if(b)if(c(s,b[1][1],d))return[0,K(a),0];return[0,a,1]}var
f=c(y[1],0,7);function
l(a){var
b=e(a),c=b[1];return v(y[5],f,[0,c[3],c[4]],[0,b[2],a])}c(r[11],l,j);function
m(a){if(0===a[2]){var
d=e(a),g=d[1],i=d[2],j=g[4],l=g[3];try{var
h=c(y[6],f,[0,l,j]);b([10,a,h[2],i===h[1]?1:0]);throw k}catch(a){a=w(a);if(a===M)return 0;throw a}}throw[0,dv,fQ]}return c(r[11],m,h)}var
ad=[aX,fR,aW(0)];return[0,j,s,as,B,Q,h,l,i,u,R,d,e,x,ae,m,g,t,at,au,av,aw,ax,F,af,ag,G,k,ah,H,X,S,T,A,ay,g,I,ai,b,J,U,aj,aA,C,D,K,L,Z,_,$,V,N,p,E,aa,ak,al,ab,O,W,am,an,ao,ac,aq,P,aC,ar,ad,function(d,j){var
f=d[1],E=d[3];U(0);function
F(a){return b([6,a])}c(r[11],F,j);function
i(c,a){ar(a);var
b=ab(d,a);return c<50?h(c+1|0,b):cY(h,[0,b])}function
h(j,k){function
l(a){return 2===a[2]?1:0}var
a=c(r[32],l,k),b=a[1],d=O(f,a[2]),e=d[2],g=d[1];if(0===g)return c(n[22],b,e);var
h=[0,g,c(n[22],b,e)];return j<50?i(j+1|0,h):cY(i,[0,h])}function
G(a){return cZ(i(0,a))}function
H(a){return cZ(h(0,a))}function
m(b){try{var
a=m(H(ac(d,0,b)));return a}catch(a){a=w(a);if(a===W){if(aO[1])A(E,b);return b}throw a}}function
I(k){var
d=k;for(;;){var
g=d[1];if(g){var
j=d[2],b=g[1],m=d[3],o=g[2],h=a(f,0),i=a(f,0),p=c(l,b[4],e),q=[0,h,1,b[3],p],s=c(l,a(t,b[4]),e),u=b[3],v=[0,i,1,a(C(t),u),s],w=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,0],a[1]],[0,d,a[2]]]}}(b,i,v),x=c(r[13],w,j),y=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,1],a[1]],[0,d,a[2]]]}}(b,h,q),z=c(r[13],y,j),A=c(n[22],z,x),d=[0,o,A,[0,[0,b[1],[0,b,h,i]],m]];continue}return[0,d[2],d[3]]}}try{var
K=c(q[17][o],p,j),L=function(a){return 0===a[2]?1:0},s=c(r[32],L,K),N=s[2],Q=s[1],R=function(a){return 2===a[2]?1:0},u=c(r[32],R,N),S=u[1],x=O(f,u[2]),T=x[1],V=c(n[22],x[2],S),X=G([0,c(n[22],Q,T),V]),Y=function(a){return 2===a[2]?1:0},z=c(r[32],Y,X),Z=z[2],_=z[1],$=J(0),B=I([0,_,[0,[0,0,Z],0],0]),aa=B[2],ae=B[1],af=function(a){var
b=a[1],f=a[2];U(0);try{m(f);throw ah}catch(a){a=w(a);if(a===k){var
d=P(0,0,J(0)),e=d[1],g=d[2],h=function(a){return c(ap[4][1],a[2],e)},i=c(r[32],h,b)[1],j=function(a){return a[1]};return[0,c(r[13],j,i),e,b,g]}throw a}},ag=c(r[13],af,ae),ai=function(e){var
b=c(y[1],0,7),a=[0,-1],d=[0,0];function
f(d){try{c(y[6],b,d)[1]++;var
a=0;return a}catch(a){a=w(a);if(a===M)return v(y[5],b,d,[0,1]);throw a}}function
g(a){var
b=a[1];if(b)return c(r[11],f,b);throw[0,ad,a[4],a[2]]}c(r[11],g,e);function
h(e,b){var
c=d[1]<b[1]?1:0,f=c?(a[1]=e,d[1]=b[1],0):c;return f}c(y[11],h,b);return a[1]},g=function(e){try{var
d=ai(e),l=function(g){var
b=g[3];for(;;){if(b){var
c=b[1],e=b[2],f=c[3];if(d===c[1])return f;var
b=e;continue}return a(n[2],fS)}},f=c(r[32],l,e),m=f[2],o=f[1],h=function(a){var
b=a[4],c=a[3],e=a[2],f=a[1];function
g(b,a){return b===a?1:0}return[0,v(q[17][87],g,d,f),e,c,b]},p=c(r[13],h,o),s=c(r[13],h,m),i=g(p),t=i[2],u=i[1],j=g(s),x=j[2],y=j[1],b=c(ap[4][2],d,aa),k=b[1],z=b[3],A=b[2],B=function(b,a){return b===a?1:0},C=v(q[17][53],B,t,x),D=[0,[0,[15,k,[0,A,u],[0,z,y]],0],[0,k[1],C]];return D}catch(a){a=w(a);if(a[1]===ad)return[0,a[2],a[3]];throw a}},D=g(ag),aj=P(D[2],D[1],$)[2];return aj}catch(a){a=w(a);if(a===k)return P(0,0,J(0))[2];throw a}}]}];c0(408,c4,"Omega_plugin.Omega");var
m=a(c4[2],[0,I[17],I[16],I[12],I[13],I[14],I[15],I[22],I[5],I[6],I[2]]);function
aQ(b){var
d=[0,function(d){var
e=c(D[48][2],b,d);return a(i[99],e)}];return a(p[62][9],d)}function
ba(d,b){var
e=c(D[14],b,d),f=a(i[85],e);return c(p[66][8],f,b)}var
cG=[0,0],bb=[0,0],bc=[0,0],cH=[0,1];function
f0(d,c,b){return a(c,b)}var
f1=[0,0];function
f2(a,b){return a[1]}function
bd(b,a){b[1]=a;return 0}function
f3(a){return bd(cG,a)}var
f6=[0,0,0,f5,f4,function(a){return cG[1]},f3];c(cF[4],0,f6);function
f7(a){return bd(bb,a)}var
f_=[0,0,0,f9,f8,function(a){return bb[1]},f7];c(cF[4],0,f_);function
f$(a){return bd(bc,a)}var
gc=[0,0,0,gb,ga,function(a){return bc[1]},f$];c(cF[4],0,gc);function
gd(a){return bd(cH,a)}var
gg=[0,1,1,gf,ge,function(a){return cH[1]},gd];c(cF[4],0,gg);var
c_=[0,0];function
dw(d){var
a=c_[1];function
b(a){a[1][1]=a[2];return 0}return c(q[17][11],b,a)}function
aR(a){var
b=[0,a];c_[1]=[0,[0,b,a],c_[1]];return b}var
dx=aR(0);function
ah(e){var
b=a(n[20],dx[1]),d=c(n[16],gh,b);dx[1]++;return a(H[1][5],d)}var
dy=aR(0);function
gi(b){var
a=c(a$[3],gj,[0,dy[1]]);dy[1]++;return a}var
dz=aR(0);function
dA(e){var
b=a(n[20],dz[1]),d=c(n[16],gk,b);dz[1]++;return a(H[1][5],d)}var
dB=aR(0);function
be(a){dB[1]++;return dB[1]}var
dC=aR(1e3);function
c$(a){dC[1]++;return dC[1]}var
dD=aR(0);function
dE(a){dD[1]++;return c(a$[3],gl,[0,dD[1]])}function
aY(a){return c(z[4],gm,a)}var
da=[0,0],cI=c(y[1],0,7),db=c(y[1],0,7);function
dF(b){da[1]=0;return a(y[2],cI)}function
dc(b){try{var
a=c(y[6],db,b);return a}catch(a){a=w(a);if(a===M){var
d=dE(0);v(y[5],cI,d,b);v(y[5],db,b,d);return d}throw a}}function
cJ(b){try{var
a=c(y[6],cI,b);return a}catch(a){a=w(a);if(a===M){var
d=da[1];v(y[5],cI,b,d);v(y[5],db,d,b);da[1]++;return d}throw a}}var
T=j[7];function
dG(a){return cD(i[110],0,gn,1,[0,[0,a,0]])}function
x(b){return a(i[148],b)}function
go(b){return a(i[99],b)}function
dd(b){return a(D[45],b)}function
R(b){var
c=h(b),e=0,j=f===c?b[1]:d===c?a(g[2],b):b;return a(i[67],[0,[0,0,j],e])}function
dH(c){return function(d){var
a=d;for(;;){if(a){var
b=a[1],e=a[2],f=b[1];if(c===b[2])return f;var
a=e;continue}throw M}}}var
bf=[0,0];function
dI(a){bf[1]=0;return 0}function
N(b){try{var
c=bf[1],d=a(dH(b),c);return d}catch(b){b=w(b);if(b===M)return a(n[2],gp);throw b}}function
gq(b){try{var
d=c(H[1][12][3],b,bf[1]);return d}catch(b){b=w(b);if(b===M)return a(n[2],gr);throw b}}function
aH(b,a){bf[1]=[0,[0,b,a],bf[1]];return 0}var
bg=[0,0];function
dJ(a){return bg[1]}function
de(a){bg[1]=0;return 0}function
dK(b){try{var
c=v(q[17][119],e[139],b,bg[1]);return c}catch(b){b=w(b);if(b===M)return a(n[2],gs);throw b}}function
dL(d,c,b,a){bg[1]=[0,[0,d,[0,c,b,a]],bg[1]];return 0}function
dM(b){var
a=cH[1];return a?(dw(0),dF(0),dI(0),de(0)):a}var
gu=c(q[18],C[9],gt),gv=c(q[18],[0,dN,0],gu),gw=c(q[18],C[8],gv),dO=c(q[18],C[10],gw),aa=c(C[6],gx,C[10]),k=c(C[6],gy,dO),af=c(C[6],gA,gz),S=c(C[6],gC,gB),aq=[d,function(b){return a(k,gD)}],ar=[d,function(b){return a(k,gE)}],as=[d,function(b){return a(k,gF)}],U=[d,function(b){return a(k,gG)}],V=[d,function(b){return a(k,gH)}],W=[d,function(b){return a(k,gI)}],A=[d,function(b){return a(k,gJ)}],at=[d,function(b){return a(k,gK)}],au=[d,function(b){return a(k,gL)}],B=[d,function(b){return a(S,gM)}],O=[d,function(b){return a(S,gN)}],F=[d,function(b){return a(S,gO)}],ai=[d,function(b){return a(S,gP)}],aj=[d,function(b){return a(S,gQ)}],aZ=[d,function(b){return a(S,gR)}],av=[d,function(b){return a(S,gS)}],bh=[d,function(b){return a(af,gT)}],bi=[d,function(b){return a(af,gU)}],bj=[d,function(b){return a(af,gV)}],bk=[d,function(b){return a(k,gW)}],bl=[d,function(b){return a(af,gX)}],bm=[d,function(b){return a(af,gY)}],bn=[d,function(b){return a(af,gZ)}],bo=[d,function(b){return a(af,g0)}],bp=[d,function(b){return a(af,g1)}],bq=[d,function(b){return a(af,g2)}],br=[d,function(b){return a(af,g3)}],G=[d,function(b){return a(k,g4)}],bs=[d,function(b){return a(k,g5)}],bt=[d,function(b){return a(k,g6)}],aw=[d,function(b){return a(k,g7)}],ax=[d,function(b){return a(k,g8)}],bu=[d,function(b){return a(k,g9)}],bv=[d,function(b){return a(k,g_)}],bw=[d,function(b){return a(k,g$)}],bx=[d,function(b){return a(k,ha)}],by=[d,function(b){return a(k,hb)}],bz=[d,function(b){return a(k,hc)}],bA=[d,function(b){return a(k,hd)}],bB=[d,function(b){return a(k,he)}],bC=[d,function(b){return a(k,hf)}],bD=[d,function(b){return a(k,hg)}],bE=[d,function(b){return a(k,hh)}],bF=[d,function(b){return a(k,hi)}],X=[d,function(b){return a(k,hj)}],P=[d,function(b){return a(k,hk)}],bG=[d,function(b){return a(k,hl)}],bH=[d,function(b){return a(k,hm)}],bI=[d,function(b){return a(k,hn)}],bJ=[d,function(b){return a(k,ho)}],bK=[d,function(b){return a(k,hp)}],bL=[d,function(b){return a(k,hq)}],bM=[d,function(b){return a(k,hr)}],bN=[d,function(b){return a(k,hs)}],bO=[d,function(b){return a(k,ht)}],bP=[d,function(b){return a(k,hu)}],bQ=[d,function(b){return a(k,hv)}],bR=[d,function(b){return a(k,hw)}],bS=[d,function(b){return a(k,hx)}],Y=[d,function(b){return a(k,hy)}],bT=[d,function(b){return a(k,hz)}],bU=[d,function(b){return a(k,hA)}],bV=[d,function(b){return a(k,hB)}],bW=[d,function(b){return a(k,hC)}],bX=[d,function(b){return a(k,hD)}],Z=[d,function(b){return a(k,hE)}],bY=[d,function(b){return a(k,hF)}],bZ=[d,function(b){return a(k,hG)}],b0=[d,function(b){return a(k,hH)}],b1=[d,function(b){return a(k,hI)}],b2=[d,function(b){return a(k,hJ)}],b3=[d,function(b){return a(k,hK)}],b4=[d,function(b){return a(k,hL)}],b5=[d,function(b){return a(k,hM)}],b6=[d,function(b){return a(k,hN)}],b7=[d,function(b){return a(S,hO)}],dP=[d,function(b){return a(k,hP)}],dQ=[d,function(b){return a(S,hQ)}],dR=[d,function(b){return a(S,hR)}],dS=[d,function(b){return a(k,hS)}],dT=[d,function(b){return a(k,hT)}],b8=[d,function(b){return a(k,hU)}],dU=[d,function(b){return a(k,hV)}],dV=[d,function(b){return a(k,hW)}],dW=[d,function(b){return a(k,hX)}],dX=[d,function(b){return a(k,hY)}],dY=[d,function(b){return a(k,hZ)}],ay=[d,function(b){return a(k,h0)}],az=[d,function(b){return a(k,h1)}],ak=[d,function(b){return a(S,h2)}],al=[d,function(b){return a(S,h3)}],a0=[d,function(b){return a(S,h4)}],a1=[d,function(b){return a(S,h5)}],aA=[d,function(b){return a(aa,h6)}],b9=[d,function(b){return a(aa,h7)}],b_=[d,function(b){return a(aa,h8)}],aB=[d,function(b){return a(aa,h9)}],aC=[d,function(b){return a(aa,h_)}],b$=[d,function(b){return a(aa,h$)}],ca=[d,function(b){return a(aa,ia)}],cb=[d,function(b){return a(aa,ib)}],_=[d,function(b){return a(aa,ic)}],aD=[d,function(b){return a(aa,id)}],aE=[d,function(b){return a(aa,ie)}],cc=[d,function(b){return a(k,ig)}],cd=[d,function(b){return a(k,ih)}],ce=[d,function(b){return a(k,ii)}],dZ=[d,function(b){return a(k,ij)}],d0=[d,function(b){return a(k,ik)}],d1=[d,function(b){return a(k,il)}],d2=[d,function(b){return a(k,im)}],cf=[d,function(b){return a(k,io)}],d3=[d,function(b){return a(k,ip)}],d4=[d,function(b){return a(k,iq)}],d5=[d,function(b){return a(k,ir)}],d6=[d,function(b){return a(k,is)}],cg=[d,function(b){return a(k,it)}],ch=[d,function(b){return a(k,iu)}],ci=[d,function(b){return a(k,iv)}],cj=[d,function(b){return a(k,iw)}],ck=[d,function(b){return a(k,ix)}],cl=[d,function(b){return a(k,iy)}],cm=[d,function(b){return a(k,iz)}],cn=[d,function(b){return a(k,iA)}],co=[d,function(b){return a(k,iB)}],cp=[d,function(b){return a(k,iC)}],cq=[d,function(b){return a(k,iD)}],cr=[d,function(b){return a(k,iE)}],cs=[d,function(b){return a(k,iF)}],ct=[d,function(b){return a(k,iG)}],cu=[d,function(b){return a(k,iH)}],cv=[d,function(b){return a(k,iI)}];function
aF(l,b){var
i=h(b),m=f===i?b[1]:d===i?a(g[2],b):b,j=a(e[a_],m);if(10===j[0]){var
k=j[1][1],q=a(fW[2],0);if(c(fT[2],q,[1,k]))return[1,k]}var
o=c(n[16],l,iJ),p=a(c6[1],o);return v(ae[3],0,iK,p)}var
d7=[d,function(a){return aF(iL,aj)}],d8=[d,function(a){return aF(iM,aZ)}],d9=[d,function(a){return aF(iN,ai)}],df=[d,function(a){return aF(iO,ak)}],aI=[d,function(a){return aF(iP,al)}],iR=[d,function(a){return aF(iQ,a0)}],iT=[d,function(a){return aF(iS,a1)}],cK=[d,function(b){return aF(iU,[d,function(b){return a(C[53],0)}])}];function
iV(b){var
c=a(H[1][5],b);return a(e[o],c)}function
ag(i,c){var
b=h(B),j=[0,i,c],k=f===b?B[1]:d===b?a(g[2],B):B;return a(e[l],[0,k,j])}function
cL(i,c){var
b=h(O),j=[0,i,c],k=f===b?O[1]:d===b?a(g[2],O):O;return a(e[l],[0,k,j])}function
d_(i,c){var
b=h(ai),j=[0,i,c],k=f===b?ai[1]:d===b?a(g[2],ai):ai;return a(e[l],[0,k,j])}function
cw(i,c){var
b=h(A),j=f===b?A[1]:d===b?a(g[2],A):A,k=a(C[41],0),m=[0,a(c5[48],k),[0,j,i,c]];return a(e[l],m)}function
iW(i,c){var
b=h(ak),j=[0,i,c],k=f===b?ak[1]:d===b?a(g[2],ak):ak;return a(e[l],[0,k,j])}function
aJ(i,c){var
b=h(al),j=[0,i,c],k=f===b?al[1]:d===b?a(g[2],al):al;return a(e[l],[0,k,j])}function
aK(c){var
b=h(F),i=[0,c],j=f===b?F[1]:d===b?a(g[2],F):F;return a(e[l],[0,j,i])}function
cx(c,b){var
d=[0,a(C[54],0),[0,c,b]];return a(e[l],d)}function
cM(c,b){var
d=[0,a(C[59],0),[0,c,b]];return a(e[l],d)}function
aG(b){var
c=[0,a(C[53],0),[0,b]];return a(e[l],c)}function
iX(i,c){var
b=h(at),j=f===b?at[1]:d===b?a(g[2],at):at,k=a(C[41],0),m=[0,a(c5[48],k),[0,j,i,c]];return a(e[l],m)}function
aL(c){var
b=h(av),i=[0,c],j=f===b?av[1]:d===b?a(g[2],av):av;return a(e[l],[0,j,i])}function
J(b){function
i(b){if(c(m[1],b,m[12])){var
j=h(aq);return f===j?aq[1]:d===j?a(g[2],aq):aq}var
p=[0,i(c(m[9],b,m[13]))],q=m[11],r=c(m[10],b,m[13]);if(c(m[1],r,q))var
k=h(ar),s=f===k?ar[1]:d===k?a(g[2],ar):ar,n=s;else
var
o=h(as),t=f===o?as[1]:d===o?a(g[2],as):as,n=t;return a(e[l],[0,n,p])}if(c(m[1],b,m[11])){var
j=h(U);return f===j?U[1]:d===j?a(g[2],U):U}var
p=[0,i(a(m[15],b))];if(c(m[4],b,m[11]))var
k=h(V),q=f===k?V[1]:d===k?a(g[2],V):V,n=q;else
var
o=h(W),r=f===o?W[1]:d===o?a(g[2],W):W,n=r;return a(e[l],[0,n,p])}function
aS(z){var
k=a(e[39],z),b=k[2],i=k[1],j=a(e[a_],i);if(b){var
l=b[2];if(l){var
m=l[2];if(m){if(!m[2]){var
E=a(C[41],0);if(c(fV[11],E,i))return[1,16,b]}}else{var
n=h(ay),F=f===n?ay[1]:d===n?a(g[2],ay):ay;if(c(e[u],i,F))return[1,17,b];var
o=h(az),G=f===o?az[1]:d===o?a(g[2],az):az;if(c(e[u],i,G))return[1,18,b];var
p=h(ak),H=f===p?ak[1]:d===p?a(g[2],ak):ak;if(c(e[u],i,H))return[1,19,b];var
q=h(a1),I=f===q?a1[1]:d===q?a(g[2],a1):a1;if(c(e[u],i,I))return[1,20,b];var
r=h(a0),J=f===r?a0[1]:d===r?a(g[2],a0):a0;if(c(e[u],i,J))return[1,21,b];var
s=h(al),K=f===s?al[1]:d===s?a(g[2],al):al;if(c(e[u],i,K))return[1,22,b];var
L=a(C[54],0);if(c(e[u],i,L))return[1,25,b];var
M=a(C[59],0);if(c(e[u],i,M))return[1,26,b];var
t=h(cv),N=f===t?cv[1]:d===t?a(g[2],cv):cv;if(c(e[u],i,N))return[1,30,b];var
v=h(aA),O=f===v?aA[1]:d===v?a(g[2],aA):aA;if(c(e[u],i,O))return[1,31,b];var
w=h(b9),P=f===w?b9[1]:d===w?a(g[2],b9):b9;if(c(e[u],i,P))return[1,32,b];var
x=h(b_),Q=f===x?b_[1]:d===x?a(g[2],b_):b_;if(c(e[u],i,Q))return[1,33,b];var
y=h(aB),R=f===y?aB[1]:d===y?a(g[2],aB):aB;if(c(e[u],i,R))return[1,34,b]}}else{var
S=a(C[53],0);if(c(e[u],i,S))return[1,29,b]}}else{var
T=a(C[50],0);if(c(e[u],i,T))return[1,27,b];var
U=a(C[51],0);if(c(e[u],i,U))return[1,28,b]}switch(j[0]){case
1:if(!b)return[0,j[1]];break;case
6:if(j[1]){if(!b)return a(ae[6],iY)}else
if(!b)return[2,j[2],j[3]];break;case
10:var
A=a(c7[37],[1,j[1][1]]);return[1,[0,a(c8[22],A)],b];case
11:var
B=a(c7[37],[2,j[1][1]]);return[1,[0,a(c8[22],B)],b];case
12:var
D=a(c7[37],[3,j[1][1]]);return[1,[0,a(c8[22],D)],b]}return 0}function
cy(m){var
j=a(e[39],m),b=j[2],i=j[1];a(e[a_],i);if(!b){var
k=h(A),n=f===k?A[1]:d===k?a(g[2],A):A;if(c(e[u],i,n))return[1,23,b];var
l=h(_),o=f===l?_[1]:d===l?a(g[2],_):_;if(c(e[u],i,o))return[1,24,b]}return 0}function
cz(E){var
j=a(e[39],E),b=j[2],i=j[1],k=a(e[a_],i);if(b){var
l=b[2];if(l){if(!l[2]){var
m=h(B),G=f===m?B[1]:d===m?a(g[2],B):B;if(c(e[u],i,G))return[1,0,b];var
n=h(O),H=f===n?O[1]:d===n?a(g[2],O):O;if(c(e[u],i,H))return[1,1,b];var
o=h(ai),I=f===o?ai[1]:d===o?a(g[2],ai):ai;if(c(e[u],i,I))return[1,2,b];var
p=h(b$),J=f===p?b$[1]:d===p?a(g[2],b$):b$;if(c(e[u],i,J))return[1,6,b];var
q=h(ca),K=f===q?ca[1]:d===q?a(g[2],ca):ca;if(c(e[u],i,K))return[1,7,b];var
r=h(aC),L=f===r?aC[1]:d===r?a(g[2],aC):aC;if(c(e[u],i,L))return[1,8,b]}}else{var
s=h(aj),M=f===s?aj[1]:d===s?a(g[2],aj):aj;if(c(e[u],i,M))return[1,3,b];var
t=h(aZ),N=f===t?aZ[1]:d===t?a(g[2],aZ):aZ;if(c(e[u],i,N))return[1,5,b];var
v=h(F),P=f===v?F[1]:d===v?a(g[2],F):F;if(c(e[u],i,P))return[1,4,b];var
w=h(cb),Q=f===w?cb[1]:d===w?a(g[2],cb):cb;if(c(e[u],i,Q))return[1,9,b];var
x=h(aD),R=f===x?aD[1]:d===x?a(g[2],aD):aD;if(c(e[u],i,R))return[1,10,b];var
y=h(V),S=f===y?V[1]:d===y?a(g[2],V):V;if(c(e[u],i,S))return[1,13,b];var
z=h(W),T=f===z?W[1]:d===z?a(g[2],W):W;if(c(e[u],i,T))return[1,12,b];var
A=h(av),X=f===A?av[1]:d===A?a(g[2],av):av;if(c(e[u],i,X))return[1,15,b]}}else{var
C=h(aE),Y=f===C?aE[1]:d===C?a(g[2],aE):aE;if(c(e[u],i,Y))return[1,11,b];var
D=h(U),Z=f===D?U[1]:d===D?a(g[2],U):U;if(c(e[u],i,Z))return[1,14,b];if(1===k[0])return[0,k[1]]}return 0}function
d$(r){function
b(r){var
k=a(e[39],r),i=k[2],j=k[1];if(i){if(!i[2]){var
l=i[1],o=h(as),s=f===o?as[1]:d===o?a(g[2],as):as;if(c(e[u],j,s)){var
t=b(l),v=c(m[8],m[13],t);return c(m[6],m[12],v)}var
p=h(ar),w=f===p?ar[1]:d===p?a(g[2],ar):ar;if(c(e[u],j,w)){var
x=b(l);return c(m[8],m[13],x)}}}else{var
q=h(aq),y=f===q?aq[1]:d===q?a(g[2],aq):aq;if(c(e[u],j,y))return m[12]}return a(n[2],iZ)}var
k=a(e[39],r),i=k[2],j=k[1];if(i){if(!i[2]){var
l=i[1],o=h(V),s=f===o?V[1]:d===o?a(g[2],V):V;if(c(e[u],j,s))return b(l);var
p=h(W),t=f===p?W[1]:d===p?a(g[2],W):W;if(c(e[u],j,t)){var
v=b(l);return a(m[17],v)}}}else{var
q=h(U),w=f===q?U[1]:d===q?a(g[2],U):U;if(c(e[u],j,w))return m[11]}return a(n[2],i0)}function
dg(C,f,b){function
d(f,h,v){var
b=a(e[a_],v);if(5===b[0]){var
ao=b[3],ap=b[2],aq=[0,d(f,h,b[1]),ap,ao];return a(e[120],aq)}if(h){var
i=h[1];if(typeof
i==="number")switch(i){case
0:var
k=h[2];switch(b[0]){case
6:var
G=b[2],H=b[1],I=[0,H,G,d(f+1|0,k,b[3])];return a(e[eX],I);case
7:var
J=b[2],K=b[1],L=[0,K,J,d(f+1|0,k,b[3])];return a(e[a6],L);case
8:var
M=b[3],N=b[2],O=b[1],P=[0,O,N,M,d(f+1|0,k,b[4])];return a(e[eK],P);case
14:var
g=0;break;default:var
g=1}break;case
1:var
p=h[2];switch(b[0]){case
6:var
T=b[3],U=b[1],V=[0,U,d(f,p,b[2]),T];return a(e[eX],V);case
7:var
W=b[3],X=b[1],Y=[0,X,d(f,p,b[2]),W];return a(e[a6],Y);case
8:var
Z=b[4],_=b[2],$=b[1],aa=[0,$,_,d(f,p,b[3]),Z];return a(e[eK],aa);case
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
af=b[1],r=a(q[19][8],b[2]);r[1]=d(f,ae,an(r,0)[1]);return a(e[l],[0,af,r]);case
14:var
g=0;break;default:var
g=1}}else
if(0===i[0]){var
z=i[1],ag=h[2];switch(b[0]){case
9:var
ah=b[1],s=a(q[19][8],b[2]),A=z-1|0,B=z-1|0,ai=d(f,ag,an(s,A)[A+1]);an(s,B)[B+1]=ai;return a(e[l],[0,ah,s]);case
14:var
g=0;break;default:var
g=1}}else{var
t=i[1],aj=h[2];switch(b[0]){case
13:var
ak=b[3],al=b[2],am=b[1],u=a(q[19][8],b[4]);u[t+1]=d(f,aj,an(u,t)[t+1]);return a(e[133],[0,am,al,ak,u]);case
14:var
g=0;break;default:var
g=1}}if(g){var
D=a(q[17][1],h),E=a(n[20],D),F=c(n[16],i1,E);return a(n[2],F)}var
w=b[1],m=w[2],o=m[3],x=w[1],j=x[2],Q=m[2],R=m[1],y=a(q[19][8],o),S=d(f+(o.length-1)|0,h,an(o,j)[j+1]);an(y,j)[j+1]=S;return a(e[134],[0,x,[0,R,Q,y]])}return c(C,f,v)}return d(1,f,b)}function
ea(r,p){var
b=r,f=p;for(;;){var
d=a(e[a_],f);if(5===d[0]){var
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
b=w,f=an(d[2],0)[1];continue;case
14:var
g=0;break;default:var
g=1}}else
if(0===h[0]){var
x=b[2],y=h[1];switch(d[0]){case
9:var
m=y-1|0,b=x,f=an(d[2],m)[m+1];continue;case
14:var
g=0;break;default:var
g=1}}else{var
o=h[1],z=b[2];switch(d[0]){case
13:var
b=z,f=an(d[4],o)[o+1];continue;case
14:var
g=0;break;default:var
g=1}}if(g){var
s=a(q[17][1],b),t=a(n[20],s),u=c(n[16],i2,t);return a(n[2],u)}var
k=d[1],l=k[1][2],f=an(k[2][3],l)[l+1];continue}return f}}function
dh(f,d,c){var
b=[0,a(e[a7],0)],g=dg(function(d,c){b[1]=c;return a(e[a7],d)},d,c),h=b[1],i=[0,[0,a(H[1][5],i3)],f,g];return[0,a(e[a6],i),h]}function
K(h){return function(b){var
d=a(D[7],b),e=a(q[17][6],h),f=dg(function(d,a){return c(D[29],b,a)},e,d),g=c(i[5],f,2);return c(p[66][8],g,b)}}function
aM(c){var
b=c;for(;;)switch(b[0]){case
0:var
d=b[2],e=b[1];a(n[27],i4);aM(e);a(n[27],i5);aM(d);return a(n[27],i6);case
1:var
f=b[1];a(n[27],i7);var
b=f;continue;case
2:var
g=b[2],h=b[1];a(n[27],i8);aM(h);a(n[27],i9);aM(g);return a(n[27],i_);case
3:var
i=a(H[1][7],b[1]);return a(n[27],i);case
4:var
j=a(m[16],b[1]);return a(n[27],j);default:return a(n[27],i$)}}function
$(c){var
b=c;for(;;)switch(b[0]){case
0:return a(n[2],ja);case
1:var
b=b[1];continue;case
2:var
b=b[1];continue;case
3:return cJ(b[1]);case
4:return-1;default:return-1}}function
E(b){switch(b[0]){case
0:var
k=b[1],m=E(b[2]),c=h(B),n=[0,E(k),m],p=f===c?B[1]:d===c?a(g[2],B):B;return a(e[l],[0,p,n]);case
1:var
i=h(F),q=[0,E(b[1])],r=f===i?F[1]:d===i?a(g[2],F):F;return a(e[l],[0,r,q]);case
2:var
s=b[1],t=E(b[2]),j=h(O),u=[0,E(s),t],v=f===j?O[1]:d===j?a(g[2],O):O;return a(e[l],[0,v,u]);case
3:return a(e[o],b[1]);case
4:return J(b[1]);default:return b[1]}}function
eb(i,h){var
n=0;return function(o){var
c=n,b=o;for(;;){switch(b[0]){case
0:var
d=b[1];if(2===d[0]){var
e=d[1];if(3===e[0]){var
f=d[2];if(4===f[0]){var
k=b[2],l=f[1],c=[0,[0,l,cJ(e[1])],c],b=k;continue}}}break;case
4:var
m=b[1],g=be(0);aH(i,g);return[0,g,h,a(q[17][6],c),m]}var
j=a(c6[1],jb);return v(ae[3],0,0,j)}}}function
Q(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1],g=c(a[2]);return[0,[2,[3,dc(e)],[4,f]],g]}return[4,b[4]]}return c(b[3])}function
cN(c){var
b=a(fX[1],0);return a(e[114],b)}function
di(b,o,j,n,i){var
p=a(D[7],i),k=dh(b,a(q[17][6],o),p),r=k[2],s=[0,k[1],0],t=a(e[a7],1),m=h(cg),u=[0,b,j,a(e[a7],2),t,r,n],v=f===m?cg[1]:d===m?a(g[2],cg):cg,w=a(e[l],[0,v,u]),x=[0,a(e[a7],1),[0,j,0]],y=a(e[59],x),z=[0,[0,a(H[1][5],jc)],y,w],A=a(e[a6],z),B=c(e[49],b,e[117]),C=[0,[0,a(H[1][5],jd)],B,A],E=[0,a(e[a6],C),s],F=a(e[59],E),G=[0,F,[0,cN(0),0]];return a(dd(a(e[59],G)),i)}function
cO(j,i,e,c){var
b=h(A),k=f===b?A[1]:d===b?a(g[2],A):A;return di(k,j,i,e,c)}function
ec(j,i,e,c){var
b=h(_),k=f===b?_[1]:d===b?a(g[2],_):_;return di(k,j,i,e,c)}function
a2(d,c,b){var
f=a(e[59],[0,b[1],b[2]]);return function(a){return cO(d,c,f,a)}}function
ed(d,c,b){var
f=a(e[59],[0,b[1],b[2]]);return function(a){return ec(d,c,f,a)}}function
t(m,l,k,b){var
n=a(D[7],b),i=h(A),o=a(q[17][6],m),p=f===i?A[1]:d===i?a(g[2],A):A,j=dh(p,o,n),r=j[2],s=j[1];function
t(a){return ea(a,r)}var
u=c(q[17][12],t,l),v=[0,k,c(q[18],u,[0,s,0])],w=a(e[59],v),x=[0,w,[0,cN(0),0]];return a(dd(a(e[59],x)),b)}function
a3(i,l){var
b=l[2],e=l[1];switch(e[0]){case
0:var
n=e[2],j=e[1];if(0===b[0]){var
o=b[1],B=b[2],C=$(o),D=$(j);if(c(m[19],D,C)){var
p=a3([0,je,i],[0,n,b]),q=h(G),E=p[1],F=[0,j,p[2]],H=f===q?G[1]:d===q?a(g[2],G):G;return[0,[0,function(a){return t(i,jf,H,a)},E],F]}var
r=a3([0,jg,i],[0,e,B]),s=h(aw),J=r[1],L=[0,o,r[2]],M=f===s?aw[1]:d===s?a(g[2],aw):aw;return[0,[0,function(a){return t(i,jh,M,a)},J],L]}var
N=$(b),O=$(j);if(c(m[19],O,N)){var
u=a3([0,ji,i],[0,n,b]),v=h(G),P=u[1],Q=[0,j,u[2]],R=f===v?G[1]:d===v?a(g[2],G):G;return[0,[0,function(a){return t(i,jj,R,a)},P],Q]}var
w=h(ax),S=[0,b,e],T=0,U=f===w?ax[1]:d===w?a(g[2],ax):ax;return[0,[0,function(a){return t(i,jk,U,a)},T],S];case
4:var
af=e[1];switch(b[0]){case
0:var
k=0;break;case
4:var
ag=[4,c(I[12],af,b[1])];return[0,[0,K(i),0],ag];default:var
k=1}break;default:var
k=0}if(!k)if(0===b[0]){var
x=b[1],V=b[2],W=$(e),X=$(x);if(c(m[19],X,W)){var
y=a3([0,jl,i],[0,e,V]),z=h(aw),Y=y[1],Z=[0,x,y[2]],_=f===z?aw[1]:d===z?a(g[2],aw):aw;return[0,[0,function(a){return t(i,jm,_,a)},Y],Z]}return[0,0,[0,e,b]]}var
aa=$(b),ab=$(e);if(c(m[18],ab,aa)){var
A=h(ax),ac=[0,b,e],ad=0,ae=f===A?ax[1]:d===A?a(g[2],ax):ax;return[0,[0,function(a){return t(i,jn,ae,a)},ad],ac]}return[0,0,[0,e,b]]}function
ee(o,D,i,C,b){function
e(b,l){var
i=l[1];if(i){var
j=l[2],k=i[2],p=i[1],q=p[2],E=p[1];if(j){var
n=j[2],r=j[1],s=r[2],F=r[1];if(q===s){var
u=h(bF),G=f===u?bF[1]:d===u?a(g[2],bF):bF,v=function(a){return t(b,jo,G,a)},H=m[11],J=c(I[14],C,F),L=c(I[14],D,E),M=c(I[12],L,J);if(c(m[1],M,H)){var
w=h(Y),N=f===w?Y[1]:d===w?a(g[2],Y):Y,O=function(a){return t(b,jp,N,a)},Q=[0,O,e(b,[0,k,n])];return[0,v,[0,K([0,jr,[0,jq,b]]),Q]]}return[0,v,e([0,js,b],[0,k,n])]}if(c(m[19],q,s)){var
x=h(X),R=e([0,jt,b],[0,k,j]),S=f===x?X[1]:d===x?a(g[2],X):X;return[0,function(a){return t(b,ju,S,a)},R]}var
y=h(P),T=e([0,jv,b],[0,i,n]),U=f===y?P[1]:d===y?a(g[2],P):P;return[0,function(a){return t(b,jw,U,a)},T]}var
z=h(X),V=e([0,jx,b],[0,k,0]),W=f===z?X[1]:d===z?a(g[2],X):X;return[0,function(a){return t(b,jy,W,a)},V]}var
A=l[2];if(A){var
B=h(P),Z=e([0,jz,b],[0,0,A[2]]),_=f===B?P[1]:d===B?a(g[2],P):P;return[0,function(a){return t(b,jA,_,a)},Z]}return[0,K(o),0]}return e(o,[0,i,b])}function
cP(o,i,C,b){function
e(b,l){var
i=l[1];if(i){var
j=l[2],k=i[2],p=i[1],q=p[2],D=p[1];if(j){var
n=j[2],r=j[1],s=r[2],E=r[1];if(q===s){var
u=h(bI),F=f===u?bI[1]:d===u?a(g[2],bI):bI,v=function(a){return t(b,jB,F,a)},H=m[11],J=c(I[14],C,E),L=c(I[12],D,J);if(c(m[1],L,H)){var
w=h(Y),M=f===w?Y[1]:d===w?a(g[2],Y):Y,N=function(a){return t(b,jC,M,a)},O=[0,N,e(b,[0,k,n])];return[0,v,[0,K([0,jE,[0,jD,b]]),O]]}return[0,v,e([0,jF,b],[0,k,n])]}if(c(m[19],q,s)){var
x=h(G),Q=e([0,jG,b],[0,k,j]),R=f===x?G[1]:d===x?a(g[2],G):G;return[0,function(a){return t(b,jH,R,a)},Q]}var
y=h(P),S=e([0,jI,b],[0,i,n]),T=f===y?P[1]:d===y?a(g[2],P):P;return[0,function(a){return t(b,jJ,T,a)},S]}var
z=h(G),U=e([0,jK,b],[0,k,0]),V=f===z?G[1]:d===z?a(g[2],G):G;return[0,function(a){return t(b,jL,V,a)},U]}var
A=l[2];if(A){var
B=h(P),W=e([0,jM,b],[0,0,A[2]]),X=f===B?P[1]:d===B?a(g[2],P):P;return[0,function(a){return t(b,jN,X,a)},W]}return[0,K(o),0]}return e(o,[0,i,b])}function
dj(e,b){if(b){var
l=b[2];if(c(m[4],b[1][1],m[11]))var
i=h(bG),n=f===i?bG[1]:d===i?a(g[2],bG):bG,j=n;else
var
k=h(bH),p=f===k?bH[1]:d===k?a(g[2],bH):bH,j=p;var
o=function(a){return t(e,jO,j,a)};return[0,o,dj(e,l)]}return[0,K(e),0]}function
cA(i,j,b){switch(b[0]){case
0:var
v=b[2],k=cA([0,jP,i],j,b[1]),w=k[2],x=k[1],n=cA([0,jQ,i],j,v),y=[0,w,n[2]],o=h(bU),z=c(q[18],x,n[1]),A=f===o?bU[1]:d===o?a(g[2],bU):bU;return[0,[0,function(a){return t(i,jR,A,a)},z],y];case
1:var
B=b[1],C=[2,B,[4,a(m[17],j)]],p=h(bV),D=[0,K([0,jS,i]),0],E=f===p?bV[1]:d===p?a(g[2],bV):bV;return[0,[0,function(a){return t(i,jT,E,a)},D],C];case
2:var
r=b[2],F=b[1];if(4===r[0]){var
G=[2,F,[4,c(m[8],j,r[1])]],s=h(bt),H=[0,K([0,jV,i]),0],I=f===s?bt[1]:d===s?a(g[2],bt):bt;return[0,[0,function(a){return t(i,jW,I,a)},H],G]}return a(ae[6],jU);case
3:return[0,0,[2,b,[4,j]]];case
4:var
L=[4,c(m[8],j,b[1])];return[0,[0,K(i),0],L];default:var
M=b[1],u=h(O),N=[0,J(j),M],P=f===u?O[1]:d===u?a(g[2],O):O;return[0,0,[5,a(e[l],[0,P,N])]]}}function
cQ(b){function
c(i,e){if(e){var
j=h(bJ),k=c([0,jX,i],e[2]),l=f===j?bJ[1]:d===j?a(g[2],bJ):bJ;return[0,function(a){return t(i,jY,l,a)},k]}return[0,K(b),0]}return function(a){return c(b,a)}}function
ef(b){function
c(i,e){if(e){var
j=h(G),k=c([0,jZ,i],e[2]),l=f===j?G[1]:d===j?a(g[2],G):G;return[0,function(a){return t(i,j0,l,a)},k]}return[0,K(b),0]}return function(a){return c(b,a)}}function
cR(b){function
c(i,e){if(e){var
j=h(X),k=c([0,j1,i],e[2]),l=f===j?X[1]:d===j?a(g[2],X):X;return[0,function(a){return t(i,j2,l,a)},k]}return[0,K(b),0]}return function(a){return c(b,a)}}function
cS(i,b){switch(b[0]){case
0:var
v=b[2],j=cS([0,j3,i],b[1]),w=j[2],x=j[1],k=cS([0,j4,i],v),y=[0,w,k[2]],n=h(bW),z=c(q[18],x,k[1]),A=f===n?bW[1]:d===n?a(g[2],bW):bW;return[0,[0,function(a){return t(i,j5,A,a)},z],y];case
1:var
o=h(bY),B=b[1],C=0,D=f===o?bY[1]:d===o?a(g[2],bY):bY;return[0,[0,function(a){return t(i,j6,D,a)},C],B];case
2:var
p=b[2],E=b[1];if(4===p[0]){var
G=[2,E,[4,a(m[17],p[1])]],r=h(bX),H=[0,K([0,j8,i]),0],I=f===r?bX[1]:d===r?a(g[2],bX):bX;return[0,[0,function(a){return t(i,j9,I,a)},H],G]}return a(ae[6],j7);case
3:var
s=h(Z),J=[2,b,[4,m[14]]],L=0,M=f===s?Z[1]:d===s?a(g[2],Z):Z;return[0,[0,function(a){return t(i,j_,M,a)},L],J];case
4:var
N=[4,a(m[17],b[1])];return[0,[0,K(i),0],N];default:var
u=h(F),O=[0,b[1]],P=f===u?F[1]:d===u?a(g[2],F):F;return[0,0,[5,a(e[l],[0,P,O])]]}}function
am(j,r){function
s(h,d){try{var
b=dK(d),g=b[1],q=a(e[o],b[2]),n=[3,g],p=0,r=a(e[o],g),s=[0,[0,function(a){return cO(j,r,q,a)},p],n];return s}catch(b){b=w(b);if(a(ae[22],b)){var
c=dA(0),f=ah(0);dL(d,c,f,h);var
l=a(e[o],f),i=[3,c],k=0,m=a(e[o],c);return[0,[0,function(a){return cO(j,m,l,a)},k],i]}throw b}}try{var
k=cz(r);if(typeof
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
ag=y[1],N=am([0,j$,j],x[1]),ai=N[2],aj=N[1],O=am([0,ka,j],ag),ak=O[1],P=a3(j,[0,ai,O[2]]),al=P[2],an=c(q[18],ak,P[1]),n=[0,c(q[18],aj,an),al],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
z=k[2];if(z){var
A=z[2];if(A)if(A[2])var
i=0,b=0;else{var
ao=A[1],Q=am([0,kb,j],z[1]),C=Q[2],S=Q[1],T=am([0,kc,j],ao),D=T[2],U=T[1];if(4===D[0])var
X=cA(j,D[1],C),au=X[2],av=c(q[18],U,X[1]),E=[0,c(q[18],S,av),au];else
if(4===C[0])var
V=h(bu),ap=C[1],aq=f===V?bu[1]:d===V?a(g[2],bu):bu,ar=function(a){return t(j,kd,aq,a)},W=cA(j,ap,D),as=W[2],at=c(q[18],U,[0,ar,W[1]]),E=[0,c(q[18],S,at),as];else
var
E=s(0,r);var
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
Y=h(F),aw=G[1],ax=[0,H[1]],ay=f===Y?F[1]:d===Y?a(g[2],F):F,Z=h(B),az=[0,aw,a(e[l],[0,ay,ax])],aA=f===Z?B[1]:d===Z?a(g[2],B):B,_=am(j,a(e[l],[0,aA,az])),aB=_[2],aC=_[1],aD=R(d9),n=[0,[0,a(p[66][8],aD),aC],aB],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
3:var
I=k[2];if(I)if(I[2])var
i=0,b=0;else
var
aE=I[1],$=h(B),aF=[0,aE,J(m[12])],aG=f===$?B[1]:d===$?a(g[2],B):B,aa=am(j,a(e[l],[0,aG,aF])),aH=aa[2],aI=aa[1],aJ=R(d7),n=[0,[0,a(p[66][8],aJ),aI],aH],b=1;else
var
i=0,b=0;break;case
4:var
K=k[2];if(K)if(K[2])var
i=0,b=0;else
var
ab=am([0,ke,j],K[1]),aK=ab[1],ac=cS(j,ab[2]),aL=ac[2],n=[0,c(q[18],aK,ac[1]),aL],b=1;else
var
i=0,b=0;break;case
5:var
L=k[2];if(L)if(L[2])var
i=0,b=0;else
var
aM=L[1],ad=h(B),aN=[0,aM,J(m[14])],aO=f===ad?B[1]:d===ad?a(g[2],B):B,af=am(j,a(e[l],[0,aO,aN])),aQ=af[2],aR=af[1],aS=R(d8),n=[0,[0,a(p[66][8],aS),aR],aQ],b=1;else
var
i=0,b=0;break;case
15:var
M=k[2];if(M)if(M[2])var
i=0,b=0;else
var
n=s(1,M[1]),b=1;else
var
i=0,b=0;break;case
12:case
13:case
14:try{var
aT=[0,0,[4,d$(r)]],n=aT,b=1}catch(c){c=w(c);if(!a(ae[22],c))throw c;var
n=s(0,r),b=1}break;default:var
i=0,b=0}if(b)var
u=n,i=1}else
var
i=0;break;default:var
i=0}if(!i)var
u=s(0,r);return u}catch(b){b=w(b);if(a(aP[4],b))return s(0,r);throw b}}function
dk(e,c,b){switch(c[0]){case
2:var
i=c[1],q=c[2];switch(b[0]){case
2:if(3===i[0]){var
j=h(bS),r=[2,[3,i[1]],[0,c[2],b[2]]],s=f===j?bS[1]:d===j?a(g[2],bS):bS;return[0,function(a){return t(e,kg,s,a)},r]}break;case
3:var
k=h(bR),u=[2,[3,b[1]],[0,q,[4,m[12]]]],v=f===k?bR[1]:d===k?a(g[2],bR):bR;return[0,function(a){return t(e,kh,v,a)},u]}break;case
3:var
l=c[1];switch(b[0]){case
2:var
o=h(bQ),w=[2,[3,l],[0,b[2],[4,m[12]]]],x=f===o?bQ[1]:d===o?a(g[2],bQ):bQ;return[0,function(a){return t(e,ki,x,a)},w];case
3:var
p=h(bP),y=[2,[3,l],[4,m[13]]],z=f===p?bP[1]:d===p?a(g[2],bP):bP;return[0,function(a){return t(e,kj,z,a)},y]}break}aM(c);a(n[32],0);aM(b);a(n[32],0);a(n[46],n[24]);return a(ae[6],kf)}function
cB(i,b){switch(b[0]){case
2:var
j=b[1];if(3===j[0]){var
k=b[2],n=j[1];if(4===k[0])return[0,0,b];var
e=function(b){switch(b[0]){case
0:var
d=b[1],f=e(b[2]),g=e(d);return c(I[12],g,f);case
4:return b[1];default:return a(ae[6],kl)}},o=[2,[3,n],[4,e(k)]];return[0,[0,K([0,km,i]),0],o]}break;case
3:var
l=h(bO),p=[2,[3,b[1]],[4,m[12]]],q=0,r=f===l?bO[1]:d===l?a(g[2],bO):bO;return[0,[0,function(a){return t(i,kn,r,a)},q],p]}aM(b);return a(ae[6],kk)}function
a4(b,j){switch(j[0]){case
0:var
e=j[2],i=j[1];switch(e[0]){case
0:var
k=e[1],A=e[2],B=$(k);if($(i)===B){var
l=dk([0,ko,b],i,k),n=h(bs),C=l[2],D=l[1],E=f===n?bs[1]:d===n?a(g[2],bs):bs,F=function(a){return t(b,kp,E,a)},o=a4(b,[0,C,A]);return[0,[0,F,[0,D,o[1]]],o[2]]}var
p=cB([0,kq,b],i),G=p[2],H=p[1],r=a4([0,kr,b],e),I=[0,G,r[2]];return[0,c(q[18],H,r[1]),I];case
4:var
O=e[1],x=cB([0,ku,b],i);return[0,x[1],[0,x[2],[4,O]]];default:var
J=$(e);if($(i)===J){var
s=dk(b,i,e),K=s[1],u=a4(b,s[2]);return[0,[0,K,u[1]],u[2]]}var
v=cB([0,ks,b],i),L=v[2],M=v[1],w=a4([0,kt,b],e),N=[0,L,w[2]];return[0,c(q[18],M,w[1]),N]}case
4:return[0,0,j];default:var
y=cB(b,j),z=h(bT),P=y[1],Q=[0,y[2],[4,m[11]]],R=f===z?bT[1]:d===z?a(g[2],bT):bT,S=[0,function(a){return t(b,kv,R,a)},0];return[0,c(q[18],P,S),Q]}}function
cT(i,b){if(0===b[0]){var
e=b[1];if(2===e[0])if(3===e[1][0]){var
k=e[2];if(4===k[0]){var
o=b[2];if(c(m[1],k[1],m[11])){var
l=h(Y),p=f===l?Y[1]:d===l?a(g[2],Y):Y,q=function(a){return t(i,kx,p,a)},n=cT(i,o);return[0,[0,q,n[1]],n[2]]}}}var
j=cT([0,kw,i],b[2]);return[0,j[1],[0,e,j[2]]]}return[0,0,b]}function
dl(bo){var
k=a(H[1][5],ky),n=a(H[1][5],kz),q=a(H[1][5],kA),z=J(m[11]);function
r(bp){var
s=bp;for(;;){if(s){var
b=s[1];switch(b[0]){case
0:var
al=b[2],am=b[1],bq=s[2],br=b[4],bs=b[3],u=N(am[1]),an=E(Q(am)),ao=E(Q(al)),F=J(bs),V=J(br),ap=ag(cL(ao,F),V),bt=cw(an,ap),bu=al[3],bF=a(cR(kB),bu),bG=i[l],bH=a(T,bF),bI=a(p[66][1],bH),bJ=[0,c(j[70][3],bI,bG),0],bO=[0,i[60],[0,i[l],0]],bP=[0,R(aI),bO],bQ=[0,a(j[70][20],bP),0],bR=[0,i[60],[0,i[l],0]],bS=[0,R(aI),bR],bT=[0,a(j[70][20],bS),0],bU=[0,r(bq),0],bV=[0,a(i[25],[0,u,0]),bU],bW=[0,a(i[74],[0,n,[0,q,[0,u,0]]]),bV],bY=a(e[o],u),bZ=a(e[o],q),aq=h(bv),bX=0,b0=[0,F,ao,V,a(e[o],n),bZ,bY],b1=f===aq?bv[1]:d===aq?a(g[2],bv):bv,b2=[0,x([0,a(e[l],[0,b1,b0]),bX]),bW],b3=[0,a(i[25],[0,n,[0,q,0]]),b2],b4=[0,a(j[70][20],b3),bT],b5=aJ(F,z),b6=a(i[ad],b5),b7=[0,c(j[70][19],b6,b4),bQ],b8=aJ(F,V),b9=[0,a(i[ad],b8),0],b_=[0,a(i[25],[0,u,0]),b9],b$=[0,a(i[74],[0,k,[0,u,0]]),b_],cb=a(e[o],u),ar=h(bw),ca=0,cc=[0,an,ap,a(e[o],k),cb],cd=f===ar?bw[1]:d===ar?a(g[2],bw):bw,ce=[0,x([0,a(e[l],[0,cd,cc]),ca]),b$],cf=[0,a(i[25],[0,k,0]),ce],cg=a(j[70][20],cf),ch=[0,c(j[70][19],cg,b7),bJ],ci=a(i[ad],bt);return c(j[70][19],ci,ch);case
1:var
G=b[2],K=b[1],as=c(m[26],K[4],G),cj=c(I[14],as,G),ck=c(I[13],K[4],cj),cl=K[3],cm=function(a){return c(m[9],a,G)},cn=c(m[43],cm,cl),av=[0,K[1],0,cn,as],co=E(Q(av)),aw=J(G),W=J(ck),cp=av[3],cq=a(cR(kC),cp),cr=[0,i[60],[0,i[l],0]],cs=[0,R(aI),cr],ct=[0,a(j[70][20],cs),0],cu=[0,i[60],[0,i[l],0]],cv=[0,R(aI),cu],cx=[0,a(j[70][20],cv),0],cy=[0,i[41],0],cz=a(T,cq),cA=[0,a(p[66][1],cz),cy],cB=function(a){return ba(k,a)},cC=[0,a(p[66][1],cB),cA],cD=[0,a(i[25],[0,k,0]),cC],cE=[0,R(cK),cD],cF=[0,a(i[74],[0,n,[0,q,0]]),cE],cH=a(e[o],q),ax=h(bz),cG=0,cI=[0,W,aw,co,a(e[o],n),cH],cJ=f===ax?bz[1]:d===ax?a(g[2],bz):bz,cM=[0,x([0,a(e[l],[0,cJ,cI]),cG]),cF],cN=[0,a(i[25],[0,q,[0,n,0]]),cM],cO=[0,a(j[70][20],cN),cx],cS=aJ(aw,W),cT=a(i[ad],cS),cU=[0,c(j[70][19],cT,cO),ct],cV=aJ(W,z),cW=a(i[ad],cV);return c(j[70][19],cW,cU);case
3:var
ay=s[2],az=b[2],L=b[1],v=N(L[1]),cX=function(a){return c(m[9],a,az)},X=c(m[44],cX,L),Y=E(Q(L)),_=E(Q(X)),O=J(az),aA=cw(Y,cL(_,O));if(2===L[2]){var
cY=X[3],cZ=a(cQ(kD),cY),c0=i[l],c1=a(T,cZ),c2=a(p[66][1],c1),c3=[0,c(j[70][3],c2,c0),0],c4=[0,r(ay),0],c6=[0,a(i[25],[0,v,0]),c4],c7=[0,a(i[74],[0,n,[0,v,0]]),c6],c9=a(e[o],v),aB=h(bL),c8=0,c_=[0,Y,_,O,a(e[o],n),c9],c$=f===aB?bL[1]:d===aB?a(g[2],bL):bL,da=[0,x([0,a(e[l],[0,c$,c_]),c8]),c7],db=[0,a(i[25],[0,n,0]),da],dd=[0,a(j[70][20],db),c3],de=a(i[ad],aA);return c(j[70][19],de,dd)}var
dg=X[3],dh=a(cQ(kE),dg),di=i[l],dk=a(T,dh),dl=a(p[66][1],dk),dm=[0,c(j[70][3],dl,di),0],dn=[0,i[60],[0,i[l],0]],dp=[0,R(aI),dn],dq=[0,a(j[70][20],dp),0],dr=[0,r(ay),0],ds=[0,a(i[25],[0,v,0]),dr],dt=[0,a(i[74],[0,n,[0,q,[0,v,0]]]),ds],dv=a(e[o],v),dw=a(e[o],n),aC=h(by),du=0,dx=[0,Y,_,O,a(e[o],q),dw,dv],dy=f===aC?by[1]:d===aC?a(g[2],by):by,dz=[0,x([0,a(e[l],[0,dy,dx]),du]),dt],dA=[0,a(i[25],[0,q,[0,n,0]]),dz],dB=[0,a(j[70][20],dA),dq],dC=aJ(O,z),dD=a(i[ad],dC),dE=[0,c(j[70][19],dD,dB),dm],dF=a(i[ad],aA);return c(j[70][19],dF,dE);case
4:var
aD=s[2],aE=b[3],B=aE[2],P=aE[1],aF=b[2],y=aF[2],$=aF[1],dH=b[1],aa=ah(0);aH(aa,dH);var
aG=N(y[1]),aL=N(B[1]),aM=E(Q(y)),aN=E(Q(B));if(c(m[1],$,m[12]))if(0===B[2]){switch(y[2]){case
0:var
aO=h(bA),dI=f===aO?bA[1]:d===aO?a(g[2],bA):bA,ab=dI;break;case
1:var
aP=h(bB),dT=f===aP?bB[1]:d===aP?a(g[2],bB):bB,ab=dT;break;default:var
aR=h(bN),dU=f===aR?bN[1]:d===aR?a(g[2],bN):bN,ab=dU}var
dJ=J(P),dK=2===y[2]?kF:kG,dL=cP(dK,y[3],P,B[3]),dM=[0,r(aD),0],dN=[0,a(i[25],[0,aa,0]),dM],dO=a(T,dL),dP=[0,a(p[66][1],dO),dN],dQ=a(e[o],aL),dR=[0,ab,[0,aM,aN,dJ,a(e[o],aG),dQ]],dS=[0,x([0,a(e[l],dR),0]),dP];return a(j[70][20],dS)}var
aS=J($),aT=J(P),dV=ee(kH,$,y[3],P,B[3]),dW=[0,i[60],[0,i[l],0]],dX=[0,R(aI),dW],dY=[0,a(j[70][20],dX),0],dZ=[0,i[60],[0,i[l],0]],d0=[0,R(aI),dZ],d1=[0,a(j[70][20],d0),0],d2=[0,r(aD),0],d3=[0,a(i[25],[0,aa,0]),d2],d4=a(T,dV),d5=[0,a(p[66][1],d4),d3],d6=[0,a(i[74],[0,n,[0,q,0]]),d5],d8=a(e[o],aL),d9=a(e[o],aG),d_=a(e[o],q),aU=h(bC),d7=0,d$=[0,aM,aN,aS,aT,a(e[o],n),d_,d9,d8],ea=f===aU?bC[1]:d===aU?a(g[2],bC):bC,eb=[0,x([0,a(e[l],[0,ea,d$]),d7]),d6],ec=[0,a(i[25],[0,q,[0,n,0]]),eb],ed=[0,a(j[70][20],ec),d1],eh=aJ(aT,z),ei=a(i[ad],eh),ej=[0,c(j[70][19],ei,ed),dY],ek=aJ(aS,z),el=a(i[ad],ek);return c(j[70][19],el,ej);case
5:var
D=b[1],aV=D[5],aW=D[4],ac=D[3],aX=D[2],em=s[2],en=D[1],aY=ah(0),eo=N(ac[1]);aH(aY,en[1]);var
ae=E(Q(aX)),ep=E(Q(ac)),af=dc(aV),aZ=h(A),eq=cw(a(e[a7],1),ae),er=f===aZ?A[1]:d===aZ?a(g[2],A):A,a0=h(A),es=a(e[a6],[0,[0,af],er,eq]),et=f===a0?A[1]:d===a0?a(g[2],A):A,eu=[0,a(C[60],0),[0,et,es]],ev=a(e[l],eu),ew=J(aW),a1=h(Z),ex=cP(eg,ac[3],aW,[0,[0,m[14],aV],aX[3]]),ey=f===a1?Z[1]:d===a1?a(g[2],Z):Z,ez=[0,kL,[0,kK,[0,kJ,eg]]],eA=[0,function(a){return t(ez,kI,ey,a)},ex],eB=i[l],eC=dG(ae),eD=[0,c(j[70][3],eC,eB),0],eE=[0,r(em),0],eF=[0,a(i[25],[0,aY,0]),eE],eG=[0,a(i[74],[0,k,0]),eF],eH=a(T,eA),eI=[0,a(p[66][1],eH),eG],eK=a(e[o],k),eL=a(e[o],eo),a2=h(bE),eJ=0,eM=[0,a(e[o],af),ep,ae,ew,eL,eK],eN=f===a2?bE[1]:d===a2?a(g[2],bE):bE,eO=[0,x([0,a(e[l],[0,eN,eM]),eJ]),eI],eP=[0,a(i[25],[0,af,[0,k,0]]),eO],eQ=[0,a(i[74],[0,k,0]),eP],eR=[0,aQ(k),eQ],eS=[0,a(i[25],[0,k,0]),eR],eT=[0,a(j[70][20],eS),eD],eU=a(i[ad],ev);return c(j[70][19],eU,eT);case
6:var
a3=s[2],eV=b[1];try{var
eW=r(a3),eX=N(eV[1]),eY=c(H[1][12][3],eX,bo),eZ=c(j[70][3],eY,eW);return eZ}catch(a){a=w(a);if(a===M){var
s=a3;continue}throw a}case
9:var
a4=b[2],ai=b[1],e0=Q(ai),e1=Q(a4),a5=h(au),e2=dj(kM,ai[3]),e3=f===a5?au[1]:d===a5?a(g[2],au):au,a8=h(au),e4=f===a8?au[1]:d===a8?a(g[2],au):au,a9=h(at),e5=f===a9?at[1]:d===a9?a(g[2],at):at,e6=a(C[41],0),e7=[0,a(c5[48],e6),[0,e5,e4,e3]],e8=a(e[l],e7),e9=[0,i[41],[0,i[l],0]],e_=[0,a(fZ[1],e8),0],e$=[0,i[60],[0,i[16],e_]],fa=[0,R(df),e$],fb=a(j[70][20],fa),fc=c(j[70][19],fb,e9),fd=N(a4[1]),fe=a(e[o],fd),ff=N(ai[1]),fg=a(e[o],ff),fh=E(e1),a_=h(bx),fi=[0,E(e0),fh,fg,fe],fj=f===a_?bx[1]:d===a_?a(g[2],bx):bx,fk=a(e[l],[0,fj,fi]),fl=a(T,e2),fm=x([0,fk,0]),fn=a(p[66][8],fm),fo=c(j[5],fn,fl),fp=a(p[66][1],fo);return c(p[15],fp,fc);case
10:var
aj=b[2],ak=b[1],fq=b[3],fr=Q(aj),fs=Q(ak),ft=N(aj[1]),fu=N(ak[1]),a$=fq?m[14]:m[12],fv=cP(kN,aj[3],a$,ak[3]),fw=[0,i[l],0],fx=function(a){return ba(k,a)},fy=[0,a(p[66][1],fx),fw],fz=[0,a(i[25],[0,k,0]),fy],fA=a(T,fv),fB=[0,a(p[66][1],fA),fz],fD=a(e[o],fu),fE=a(e[o],ft),fF=J(a$),fG=E(fs),bb=h(bK),fC=0,fH=[0,E(fr),fG,fF,fE,fD],fI=f===bb?bK[1]:d===bb?a(g[2],bK):bK,fJ=[0,x([0,a(e[l],[0,fI,fH]),fC]),fB];return a(j[70][20],fJ);case
11:var
S=b[2],fK=s[2],fL=b[3],fM=b[1],bc=ah(0);aH(bc,fM);var
bd=N(S[1]),be=N(fL),bf=E(Q(S)),bg=E(Q(a(m[45],S))),fN=S[3],bh=h(Z),fO=a(cQ(kO),fN),fP=f===bh?Z[1]:d===bh?a(g[2],Z):Z,fQ=[0,function(a){return t(kQ,kP,fP,a)},fO],fR=i[l],fS=a(T,fQ),fT=a(p[66][1],fS),fU=[0,c(j[70][3],fT,fR),0],fV=[0,r(fK),0],fW=[0,a(i[25],[0,bc,0]),fV],fX=[0,a(i[74],[0,bd,[0,be,[0,k,0]]]),fW],f1=a(e[o],k),f2=a(e[o],be),bi=h(bD),f0=0,f3=[0,bf,bg,a(e[o],bd),f2,f1],f4=f===bi?bD[1]:d===bi?a(g[2],bD):bD,f5=[0,x([0,a(e[l],[0,f4,f3]),f0]),fX],f6=[0,a(i[25],[0,k,0]),f5],f7=[0,a(j[70][20],f6),fU],f8=cw(bf,aK(bg)),f9=a(i[ad],f8);return c(j[70][19],f9,f7);case
12:var
f_=fY[15],f$=N(b[1]),ga=x([0,a(e[o],f$),0]);return c(j[70][3],ga,f_);case
13:var
gb=i[l],gc=N(b[1]),gd=function(a){return ba(gc,a)},ge=a(p[66][1],gd);return c(j[70][3],ge,gb);case
14:var
gf=b[1],gg=[0,i[l],0],gh=function(a){return ba(k,a)},gi=[0,a(p[66][1],gh),gg],gj=[0,a(i[25],[0,k,0]),gi],gk=[0,R(cK),gj],gl=[0,i[60],gk],gm=[0,R(df),gl],gn=N(gf),go=[0,x([0,a(e[o],gn),0]),gm];return a(j[70][20],go);case
15:var
bj=b[3],bk=b[2],U=b[1],gp=bj[2],gq=bj[1],gr=bk[2],gs=bk[1],bl=ah(0),bm=ah(0);aH(bl,gs);aH(bm,gq);var
gt=N(U[1]),gu=U[3],gv=a(ef(kR),gu),gw=U[3],gx=a(cR(kS),gw),gy=E(Q(U)),gz=[0,r(gp),0],gA=[0,a(i[25],[0,bm,0]),gz],gB=a(T,gx),gC=[0,a(p[66][1],gB),gA],gD=[0,a(j[70][20],gC),0],gE=[0,r(gr),0],gF=[0,a(i[25],[0,bl,0]),gE],gG=a(T,gv),gH=[0,a(p[66][1],gG),gF],gI=[0,a(j[70][20],gH),gD],bn=h(bM),gJ=[0,gy,[0,a(e[o],gt),0]],gK=f===bn?bM[1]:d===bn?a(g[2],bM):bM,gL=a(e[59],[0,gK,gJ]),gM=a(i[99],gL);return c(j[70][19],gM,gI)}}return a(p[13],0)}}return r}function
eh(a,f){var
b=am(a,f),g=b[1],d=a4(a,b[2]),h=d[1],e=cT(a,d[2]),i=e[2],j=c(q[18],h,e[1]);return[0,c(q[18],g,j),i]}function
aT(f,v,u,t,s,r,n,d){var
g=d[2],h=d[1],k=eh([0,[0,t],kT],s),m=k[1],w=k[2],y=a(i[74],[0,f,0]),z=a(p[66][8],y),A=a(j[21],z),B=[0,u,[0,r,n,a(e[o],f)]],C=x([0,a(e[l],B),0]),D=a(p[66][8],C),E=c(j[5],D,A);if(a(q[17][47],m))return[0,h,g];var
b=ah(0),F=[0,a(eb(b,v),w),g],G=[0,a(i[25],[0,b,0]),0],H=a(T,m),I=[0,a(p[66][1],H),G],J=[0,a(p[66][1],E),I];return[0,[0,[0,b,a(j[70][20],J)],h],F]}function
ei(ab,i,F){var
k=F[1],ac=F[2];if(L.caml_string_equal(a(a$[5],k),kU))return i;try{var
j=aS(ac);if(typeof
j==="number")var
e=0;else
if(1===j[0]){var
p=j[1];if(typeof
p==="number")if(16<=p){switch(p+c1|0){case
0:var
q=j[2];if(q){var
r=q[2];if(r){var
s=r[2];if(s)if(s[2])var
e=0,b=0;else{var
H=s[1],I=r[1],o=cy(c(D[29],ab,q[1]));if(typeof
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
M=h(bZ),ad=ag(I,aK(H)),ae=2,af=f===M?bZ[1]:d===M?a(g[2],bZ):bZ,l=aT(k,0,af,ae,ad,I,H,i),b=1;else
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
O=u[1],P=t[1],Q=h(b0),ah=ag(P,aK(O)),ai=1,aj=f===Q?b0[1]:d===Q?a(g[2],b0):b0,l=aT(k,2,aj,ai,ah,P,O,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
3:var
v=j[2];if(v){var
x=v[2];if(x)if(x[2])var
e=0,b=0;else
var
R=x[1],S=v[1],T=h(b4),ak=ag(R,aK(S)),al=2,am=f===T?b4[1]:d===T?a(g[2],b4):b4,l=aT(k,1,am,al,ak,S,R,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
4:var
y=j[2];if(y){var
z=y[2];if(z)if(z[2])var
e=0,b=0;else
var
U=z[1],V=y[1],an=aK(V),W=h(b1),ao=ag(ag(U,J(m[14])),an),ap=2,aq=f===W?b1[1]:d===W?a(g[2],b1):b1,l=aT(k,1,aq,ap,ao,V,U,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
5:var
A=j[2];if(A){var
B=A[2];if(B)if(B[2])var
e=0,b=0;else
var
X=B[1],Y=A[1],Z=h(b2),ar=ag(Y,aK(X)),as=2,at=f===Z?b2[1]:d===Z?a(g[2],b2):b2,l=aT(k,1,at,as,ar,Y,X,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;case
6:var
C=j[2];if(C){var
E=C[2];if(E)if(E[2])var
e=0,b=0;else
var
_=E[1],$=C[1],au=aK(_),aa=h(b3),av=ag(ag($,J(m[14])),au),aw=2,ax=f===aa?b3[1]:d===aa?a(g[2],b3):b3,l=aT(k,1,ax,aw,av,$,_,i),b=1;else
var
e=0,b=0}else
var
e=0,b=0;break;default:var
e=0,b=0}if(b)var
G=l,e=1}else
var
e=0;else
var
e=0}else
var
e=0;if(!e)var
G=i;return G}catch(b){b=w(b);if(a(aP[4],b))return i;throw b}}function
aU(b){var
d=a(i[22],b),e=a(i[74],[0,b,0]),f=a(j[70][22],e);return c(j[70][3],f,d)}var
kX=[0,function(k){de(0);var
u=a(D[48][13],k),x=c(D[48][3],ei,k),l=v(q[17][15],x,kV,u),n=l[1],y=l[2],z=dJ(0),A=[0,a(p[13],0),0];function
B(n,l){var
c=l[2],o=c[2],k=c[1],p=l[1],q=n[2],r=n[1];if(c[3]){var
b=ah(0),s=be(0);aH(b,s);var
v=m[11],w=cJ(k),x=[0,[0,s,1,[0,[0,m[12],w],0],v],q],y=[0,a(i[25],[0,o,[0,b,0]]),[0,r,0]],z=[0,a(i[74],[0,b,0]),y],A=[0,aQ(b),z],t=h(b6),B=[0,a(i[25],[0,k,[0,b,0]]),A],C=[0,p,0],D=f===t?b6[1]:d===t?a(g[2],b6):b6,E=a(e[59],[0,D,C]),F=[0,a(i[99],E),B];return[0,a(j[70][20],F),x]}var
u=h(b5),G=[0,a(i[25],[0,k,[0,o,0]]),[0,r,0]],H=[0,p,0],I=f===u?b5[1]:d===u?a(g[2],b5):b5,J=a(e[59],[0,I,H]),K=[0,a(i[99],J),G];return[0,a(j[70][20],K),q]}var
o=v(q[17][15],B,A,z),r=o[1],b=c(q[18],y,o[2]);if(cG[1])c(m[33],aY,b);if(bc[1])try{v(m[64],[0,be,c$,aY],0,b);var
F=a(p[13],0);return F}catch(b){b=w(b);if(b===m[27]){var
C=a(m[39],0),s=v(m[65],0,0,C)[2];if(bb[1])c(m[36],aY,s);var
E=a(dl(n),s);return c(j[70][3],r,E)}throw b}try{var
t=c(m[69],[0,be,c$,aY],b);if(bb[1])c(m[36],aY,t);var
H=a(dl(n),t),I=c(j[70][3],r,H);return I}catch(b){b=w(b);if(b===m[28]){var
G=a(c6[1],kW);return c(j[70][5],0,G)}throw b}}],ej=a(p[62][9],kX),lg=[0,function(b){function
n(a){return c(fU[77],0,a)}var
af=c(D[48][1],n,b);function
k(q,N){try{var
s=cz(N);if(typeof
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
B=A[1],C=z[1],ad=[0,k([0,kY,q],B),0],P=h(bh),ae=[0,k([0,kZ,q],C),ad],af=[0,C,[0,B,0]],ai=f===P?bh[1]:d===P?a(g[2],bh):bh,ak=aL(B),al=a2(q,ag(aL(C),ak),[0,ai,af]),am=[0,a(p[66][1],al),ae],v=a(j[70][20],am),b=1;else
var
n=0,b=0}else
var
n=0,b=0;break;case
7:var
D=s[2];if(D){var
E=D[2];if(E)if(E[2])var
n=0,b=0;else
var
F=E[1],G=D[1],an=[0,k([0,k0,q],F),0],Q=h(bi),ao=[0,k([0,k1,q],G),an],ap=[0,G,[0,F,0]],aq=f===Q?bi[1]:d===Q?a(g[2],bi):bi,ar=aL(F),as=a2(q,cL(aL(G),ar),[0,aq,ap]),at=[0,a(p[66][1],as),ao],v=a(j[70][20],at),b=1;else
var
n=0,b=0}else
var
n=0,b=0;break;case
8:var
H=s[2];if(H){var
I=H[2];if(I)if(I[2])var
n=0,b=0;else
var
t=I[1],u=H[1],x=ah(0),R=h(aB),au=0,av=0,aw=[0,t,u],ax=f===R?aB[1]:d===R?a(g[2],aB):aB,ay=r([0,[0,x,a(e[l],[0,ax,aw])],av]),S=h(bk),az=[0,u,[0,t,[0,a(e[o],x),0]]],aF=f===S?bk[1]:d===S?a(g[2],bk):bk,aG=a2(q,J(m[11]),[0,aF,az]),aH=a(p[66][1],aG),aI=[0,c(j[70][3],aH,ay),au],aJ=[0,k([0,k2,q],t),0],T=h(aA),aK=[0,k([0,k3,q],u),aJ],aM=0,aN=[0,t,u],aO=f===T?aA[1]:d===T?a(g[2],aA):aA,aQ=[0,r([0,[0,x,a(e[l],[0,aO,aN])],aM]),aK],U=h(bj),aR=[0,u,[0,t,[0,a(e[o],x),0]]],aS=f===U?bj[1]:d===U?a(g[2],bj):bj,aT=aL(t),aU=a2(q,d_(aL(u),aT),[0,aS,aR]),aV=[0,a(p[66][1],aU),aQ],aW=[0,a(j[70][20],aV),aI],V=h(cd),aX=a(i[25],[0,x,0]),aY=[0,t,[0,u,0]],aZ=f===V?cd[1]:d===V?a(g[2],cd):cd,a0=a(e[59],[0,aZ,aY]),a1=a(i[99],a0),a3=c(j[70][3],a1,aX),v=c(j[70][19],a3,aW),b=1;else
var
n=0,b=0}else
var
n=0,b=0;break;case
9:var
L=s[2];if(L)if(L[2])var
n=0,b=0;else
var
W=L[1],X=h(aE),a4=f===X?aE[1]:d===X?a(g[2],aE):aE,Y=h(aD),a5=[0,a4],a6=f===Y?aD[1]:d===Y?a(g[2],aD):aD,Z=h(aC),a7=[0,W,a(e[l],[0,a6,a5])],a8=f===Z?aC[1]:d===Z?a(g[2],aC):aC,_=a(e[l],[0,a8,a7]),$=h(cc),a9=k(q,_),a_=[0,W,0],a$=f===$?cc[1]:d===$?a(g[2],cc):cc,ba=ed([0,k4,q],_,[0,a$,a_]),bb=a(p[66][1],ba),v=c(j[70][3],bb,a9),b=1;else
var
n=0,b=0;break;case
10:var
M=s[2];if(M)if(M[2])var
n=0,b=0;else{var
bc=M[1],aa=function(i){try{var
d=cz(i);if(typeof
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
g=0;return g}catch(b){b=w(b);if(a(aP[4],b))return 0;throw b}},ab=function(i,n){try{var
m=cz(n);if(typeof
m==="number")var
b=0;else
if(1===m[0]){var
s=m[1];if(typeof
s==="number")if(10===s){var
o=m[2];if(o)if(o[2])var
b=0;else
var
q=o[1],t=h(bl),v=ab([0,k5,i],q),x=[0,q,0],y=f===t?bl[1]:d===t?a(g[2],bl):bl,u=h(aj),z=[0,y,x],A=[0,aL(q)],B=f===u?aj[1]:d===u?a(g[2],aj):aj,C=a2(i,a(e[l],[0,B,A]),z),D=a(p[66][1],C),r=c(j[70][3],D,v),b=1;else
var
b=0}else
var
b=0;else
var
b=0}else
var
b=0;if(!b)var
r=k(i,n);return r}catch(b){b=w(b);if(a(aP[4],b))return k(i,n);throw b}};if(aa(bc))var
bd=K(q),ac=a(p[66][1],bd);else
var
ac=ab(q,N);var
v=ac,b=1}else
var
n=0,b=0;break;case
11:if(s[2])var
n=0,b=0;else
var
be=K(q),v=a(p[66][1],be),b=1;break;default:var
n=0,b=0}if(b)var
O=v,n=1}else
var
n=0}else
var
n=0;if(!n)var
O=a(p[13],0);return O}catch(b){b=w(b);if(a(aP[4],b))return a(p[13],0);throw b}}function
r(ag){var
t=ag;for(;;){if(t){var
n=t[2],K=t[1],m=K[1],ah=K[2];try{var
q=aS(ah);if(typeof
q==="number")var
i=0;else
if(1===q[0]){var
u=q[1];if(typeof
u==="number")if(16<=u){switch(u+c1|0){case
0:var
v=q[2];if(v){var
y=v[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else{var
M=z[1],N=y[1],O=h(_),ai=v[1],aj=f===O?_[1]:d===O?a(g[2],_):_;if(c(af,ai,aj))var
ak=[0,r(n),0],al=[0,aU(m),ak],am=[0,k(k6,M),al],an=[0,k(k7,N),am],P=h(br),ao=0,ap=[0,N,M,a(e[o],m)],aq=f===P?br[1]:d===P?a(g[2],br):br,ar=[0,x([0,a(e[l],[0,aq,ap]),ao]),an],Q=a(j[70][20],ar);else
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
R=B[1],S=A[1],as=[0,r(n),0],at=[0,aU(m),as],au=[0,k(k8,R),at],av=[0,k(k9,S),au],T=h(bq),aw=0,ax=[0,S,R,a(e[o],m)],ay=f===T?bq[1]:d===T?a(g[2],bq):bq,az=[0,x([0,a(e[l],[0,ay,ax]),aw]),av],s=a(j[70][20],az),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
15:var
C=q[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
U=D[1],V=C[1],aA=[0,r(n),0],aB=[0,aU(m),aA],aC=[0,k(k_,U),aB],aD=[0,k(k$,V),aC],W=h(bm),aE=0,aF=[0,V,U,a(e[o],m)],aG=f===W?bm[1]:d===W?a(g[2],bm):bm,aH=[0,x([0,a(e[l],[0,aG,aF]),aE]),aD],s=a(j[70][20],aH),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
16:var
E=q[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
X=F[1],Y=E[1],aI=[0,r(n),0],aJ=[0,aU(m),aI],aK=[0,k(la,X),aJ],aL=[0,k(lb,Y),aK],Z=h(bn),aM=0,aN=[0,Y,X,a(e[o],m)],aO=f===Z?bn[1]:d===Z?a(g[2],bn):bn,aQ=[0,x([0,a(e[l],[0,aO,aN]),aM]),aL],s=a(j[70][20],aQ),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
17:var
G=q[2];if(G){var
H=G[2];if(H)if(H[2])var
i=0,b=0;else
var
$=H[1],aa=G[1],aR=[0,r(n),0],aT=[0,aU(m),aR],aV=[0,k(lc,$),aT],aW=[0,k(ld,aa),aV],ab=h(bo),aX=0,aY=[0,aa,$,a(e[o],m)],aZ=f===ab?bo[1]:d===ab?a(g[2],bo):bo,a0=[0,x([0,a(e[l],[0,aZ,aY]),aX]),aW],s=a(j[70][20],a0),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
18:var
I=q[2];if(I){var
J=I[2];if(J)if(J[2])var
i=0,b=0;else
var
ac=J[1],ad=I[1],a1=[0,r(n),0],a2=[0,aU(m),a1],a3=[0,k(le,ac),a2],a4=[0,k(lf,ad),a3],ae=h(bp),a5=0,a6=[0,ad,ac,a(e[o],m)],a7=f===ae?bp[1]:d===ae?a(g[2],bp):bp,a8=[0,x([0,a(e[l],[0,a7,a6]),a5]),a4],s=a(j[70][20],a8),b=1;else
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
L=r(n);return L}catch(b){b=w(b);if(a(aP[4],b)){var
t=n;continue}throw b}}return a(p[13],0)}}var
s=a(D[48][13],b);return r(a(q[17][6],s))}],ek=a(p[62][9],lg);function
el(a){if(typeof
a==="number")if(18<=a)switch(a+dq|0){case
0:return dP;case
1:return dQ;case
2:return dR;case
3:return dT;case
4:return dS;case
13:return dZ;case
14:return d0;case
15:return d1;case
16:return d2}throw M}function
em(a){if(typeof
a==="number")if(18<=a)switch(a+dq|0){case
0:return dU;case
1:return dV;case
2:return dW;case
3:return dX;case
4:return dY;case
13:return d3;case
14:return d4;case
15:return d5;case
16:return d6}throw M}var
a5=[aX,lh,aW(0)];function
ab(j,Z){var
i=aS(Z);if(typeof
i!=="number")switch(i[0]){case
1:var
m=i[1];if(typeof
m==="number")if(16<=m)switch(m+c1|0){case
0:var
o=i[2];if(o){var
p=o[2];if(p){var
q=p[2];if(q){if(!q[2]){var
C=q[1],E=p[1],k=cy(c(D[29],j,o[1]));if(typeof
k!=="number"&&1===k[0]){var
r=k[1];if(typeof
r==="number")if(23===r){if(!k[2]){var
F=h(b7),_=[0,E,C],$=f===F?b7[1]:d===F?a(g[2],b7):b7;return a(e[l],[0,$,_])}}else
if(24===r)if(!k[2]){var
G=h(ce),aa=[0,E,C],ac=f===G?ce[1]:d===G?a(g[2],ce):ce;return a(e[l],[0,ac,aa])}}throw a5}var
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
I=v[1],J=u[1],ai=ab(j,I),K=h(ci),aj=[0,J,I,ab(j,J),ai],ak=f===K?ci[1]:d===K?a(g[2],ci):ci;return a(e[l],[0,ak,aj])}var
b=1}else
var
b=1}else
var
b=1;break;case
10:var
x=i[2];if(x){var
y=x[2];if(y){if(!y[2]){var
L=y[1],N=x[1],al=ab(j,L),O=h(ch),am=[0,N,L,ab(j,N),al],an=f===O?ch[1]:d===O?a(g[2],ch):ch;return a(e[l],[0,an,am])}var
b=1}else
var
b=1}else
var
b=1;break;case
11:if(!i[2]){var
P=h(cm);return f===P?cm[1]:d===P?a(g[2],cm):cm}var
b=0;break;case
12:if(!i[2]){var
Q=h(co);return f===Q?co[1]:d===Q?a(g[2],co):co}var
b=0;break;case
13:var
z=i[2];if(z){if(!z[2]){var
R=z[1],S=h(cl),ao=[0,R,ab(j,R)],ap=f===S?cl[1]:d===S?a(g[2],cl):cl;return a(e[l],[0,ap,ao])}var
b=0}else
var
b=1;break;case
14:var
A=i[2];if(A){var
B=A[2];if(B){if(!B[2]){var
T=B[1],U=A[1],aq=ab(j,T),V=h(ck),ar=[0,U,T,ab(j,U),aq],as=f===V?ck[1]:d===V?a(g[2],ck):ck;return a(e[l],[0,as,ar])}var
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
n=el(m),H=h(n),af=[0,ae,ad],ag=f===H?n[1]:d===H?a(g[2],n):n,ah=a(e[l],[0,ag,af]);return ah}catch(a){a=w(a);if(a===M)throw a5;throw a}}}}break;case
2:var
W=i[2],X=i[1],at=ab(j,W),Y=h(cj),au=[0,X,W,ab(j,X),at],av=f===Y?cj[1]:d===Y?a(g[2],cj):cj;return a(e[l],[0,av,au])}throw a5}function
ac(b,e){var
d=[0,function(f){var
g=c(i[14],0,b),d=c(D[48][3],g,f),h=a(e,d),k=c(i[2],0,d);return c(j[70][3],k,h)}],f=a(p[62][9],d),g=a(i[74],[0,b,0]),h=a(j[70][22],g);return c(j[70][3],h,f)}function
dm(b,g){var
d=[0,function(d){var
h=c(a$[7],b,li),k=c(i[14],0,h),e=c(D[48][3],k,d),l=c(a$[7],b,lj),m=c(i[14],0,l),f=c(D[48][3],m,d),n=[0,c(g,e,f),0],o=[0,c(i[2],0,f),n],p=[0,c(i[2],0,e),o];return a(j[70][20],p)}],e=a(p[62][9],d),f=a(i[74],[0,b,0]),h=a(j[70][22],f);return c(j[70][3],h,e)}var
lk=[0,function(b){var
a2=a(D[48][7],b),v=c(D[48][3],ab,b),at=c(D[48][3],D[29],b);function
m(a3){var
z=a3;for(;;){if(z){var
k=z[2],I=z[1],au=a(c9[2][1][17],I),b=au[1],a4=au[3];try{var
t=aS(a4);if(typeof
t==="number")var
n=0;else
switch(t[0]){case
1:var
K=t[1];if(typeof
K==="number")if(18<=K){switch(K+dq|0){case
7:var
L=t[2];if(L){var
N=L[2];if(N)if(N[2])var
n=0,p=0;else
var
a6=N[1],a7=L[1],a8=dm(b,function(c,d,e){return function(b,a){return m([0,[0,b,e],[0,[0,a,d],c]])}}(k,a6,a7)),a9=aQ(b),A=c(j[70][3],a9,a8),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
8:var
O=t[2];if(O){var
P=O[2];if(P)if(P[2])var
n=0,p=0;else
var
a_=P[1],a$=O[1],ba=0,bb=[0,ac(b,function(b,c){return function(a){return m([0,[0,a,c],b])}}(k,a_)),ba],bd=[0,ac(b,function(b,c){return function(a){return m([0,[0,a,c],b])}}(k,a$)),bb],be=aQ(b),A=c(j[70][19],be,bd),p=1;else
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
E=u[1];if(typeof
E==="number")if(16<=E){switch(E+c1|0){case
0:var
S=u[2];if(S){var
T=S[2];if(T){var
U=T[2];if(U)if(U[2])var
r=0,s=0,q=0;else{var
F=U[1],G=T[1],aA=S[1];if(bc[1]){var
V=cy(a(at,aA));if(typeof
V==="number")var
C=0;else
if(1===V[0]){var
W=V[1];if(typeof
W==="number"){if(23===W)var
bf=0,bg=[0,ac(b,function(a){return function(b){return m(a)}}(k)),bf],aE=h(b8),bh=[0,G,F,a(e[o],b)],bi=f===aE?b8[1]:d===aE?a(g[2],b8):b8,bj=a(e[l],[0,bi,bh]),bk=[0,a(i[99],bj),bg],aF=a(j[70][20],bk),aq=1;else
if(24===W)var
bl=0,bm=[0,ac(b,function(a){return function(b){return m(a)}}(k)),bl],aH=h(cf),bn=[0,G,F,a(e[o],b)],bo=f===aH?cf[1]:d===aH?a(g[2],cf):cf,bp=a(e[l],[0,bo,bn]),bq=[0,a(i[99],bp),bm],aF=a(j[70][20],bq),aq=1;else
var
C=0,aq=0;if(aq)var
aB=aF,C=1}else
var
C=0}else
var
C=0;if(!C)var
aB=m(k);var
aC=aB}else{var
X=cy(a(at,aA));if(typeof
X==="number")var
D=0;else
if(1===X[0]){var
Y=X[1];if(typeof
Y==="number"){if(23===Y)var
aJ=h(az),br=m(k),bs=[0,G,F],bt=f===aJ?az[1]:d===aJ?a(g[2],az):az,bu=a(e[l],[0,bt,bs]),bv=c(c9[2][1][5],bu,I),bw=a(i[6],bv),aK=c(j[70][3],bw,br),ar=1;else
if(24===Y)var
aL=h(ay),bx=m(k),by=[0,G,F],bz=f===aL?ay[1]:d===aL?a(g[2],ay):ay,bA=a(e[l],[0,bz,by]),bB=c(c9[2][1][5],bA,I),bC=a(i[6],bB),aK=c(j[70][3],bC,bx),ar=1;else
var
D=0,ar=0;if(ar)var
aI=aK,D=1}else
var
D=0}else
var
D=0;if(!D)var
aI=m(k);var
aC=aI}var
B=aC,q=1}else
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
aO=aa[1],ab=$[1],bM=a(v,ab),bN=0,bO=[0,ac(b,function(c,d,e){return function(a){var
b=aG(d);return m([0,[0,a,cM(aG(e),b)],c])}}(k,aO,ab)),bN],aR=h(cq),bP=0,bQ=[0,ab,aO,bM,a(e[o],b)],bR=f===aR?cq[1]:d===aR?a(g[2],cq):cq,bS=[0,x([0,a(e[l],[0,bR,bQ]),bP]),bO],B=a(j[70][20],bS),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
10:var
ad=u[2];if(ad){var
ae=ad[2];if(ae)if(ae[2])var
r=0,s=0,q=0;else
var
aT=ae[1],aU=ad[1],bT=0,bU=[0,ac(b,function(c,d,e){return function(a){var
b=aG(d);return m([0,[0,a,cx(aG(e),b)],c])}}(k,aT,aU)),bT],aV=h(cp),bV=0,bW=[0,aU,aT,a(e[o],b)],bX=f===aV?cp[1]:d===aV?a(g[2],cp):cp,bY=[0,x([0,a(e[l],[0,bX,bW]),bV]),bU],B=a(j[70][20],bY),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
13:var
af=u[2];if(af)if(af[2])var
s=1,q=0;else
var
ag=af[1],bZ=a(v,ag),b0=0,b1=[0,ac(b,function(b,c){return function(a){return m([0,[0,a,c],b])}}(k,ag)),b0],aW=h(ct),b2=0,b3=[0,ag,bZ,a(e[o],b)],b4=f===aW?ct[1]:d===aW?a(g[2],ct):ct,b5=[0,x([0,a(e[l],[0,b4,b3]),b2]),b1],B=a(j[70][20],b5),q=1;else
var
r=0,s=0,q=0;break;case
14:var
ah=u[2];if(ah){var
ai=ah[2];if(ai)if(ai[2])var
r=0,s=0,q=0;else
var
aj=ai[1],ak=ah[1],b6=a(v,ak),b7=a(v,aj),b9=0,b_=[0,ac(b,function(e,a,b){return function(c){var
d=cx(aG(b),a);return m([0,[0,c,cM(cx(b,aG(a)),d)],e])}}(k,aj,ak)),b9],aX=h(cs),b$=0,ca=[0,ak,aj,b6,b7,a(e[o],b)],cb=f===aX?cs[1]:d===aX?a(g[2],cs):cs,cc=[0,x([0,a(e[l],[0,cb,ca]),b$]),b_],B=a(j[70][20],cc),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;default:var
s=1,q=0}if(q)var
aD=B,s=2}else
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
bD=_[1],bE=Z[1];try{var
H=em(E),bF=0,bG=[0,ac(b,function(a){return function(b){return m(a)}}(k)),bF],aN=h(H),bH=0,bI=[0,bE,bD,a(e[o],b)],bJ=f===aN?H[1]:d===aN?a(g[2],H):H,bK=[0,x([0,a(e[l],[0,bJ,bI]),bH]),bG],bL=a(j[70][20],bK),aM=bL}catch(a){a=w(a);if(a!==M)throw a;var
aM=m(k)}var
aD=aM,y=1}else
var
r=0,y=0}else
var
r=0,y=0;break;default:var
y=1}if(y)var
R=aD,r=1;break;case
2:var
aY=u[2],al=u[1],cd=a(v,al),ce=0,cg=[0,ac(b,function(b,c,d){return function(a){return m([0,[0,a,cx(d,aG(c))],b])}}(k,aY,al)),ce],aZ=h(cr),ch=0,ci=[0,al,aY,cd,a(e[o],b)],cj=f===aZ?cr[1]:d===aZ?a(g[2],cr):cr,ck=[0,x([0,a(e[l],[0,cj,ci]),ch]),cg],R=a(j[70][20],ck),r=1;break;default:var
r=0}if(!r)var
R=m(k);var
A=R,p=1}else
var
n=0,p=0;break;case
12:var
am=t[2];if(am){var
an=am[2];if(an)if(an[2])var
n=0,p=0;else
var
cl=an[1],cm=am[1],cn=dm(b,function(h,a,b){return function(f,d){var
g=[0,[0,d,c(e[49],a,b)],h];return m([0,[0,f,c(e[49],b,a)],g])}}(k,cl,cm)),co=aQ(b),A=c(j[70][3],co,cn),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
0:case
1:case
2:case
3:case
4:var
av=t[2];if(av){var
aw=av[2];if(aw)if(aw[2])var
n=0,p=0;else
var
ax=m(k),p=2;else
var
n=0,p=0}else
var
n=0,p=0;break;default:var
n=0,p=0}switch(p){case
0:var
as=0;break;case
1:var
ax=A,as=1;break;default:var
as=1}if(as)var
J=ax,n=1}else
var
n=0;else
var
n=0;break;case
2:var
ao=t[2],ap=t[1],cv=a(a2,ao);if(a(e[22],cv))var
cw=a(v,ap),cz=0,cA=[0,ac(b,function(b,c,d){return function(a){return m([0,[0,a,cM(aG(d),c)],b])}}(k,ao,ap)),cz],a0=h(cu),cB=0,cC=[0,ap,ao,cw,a(e[o],b)],cD=f===a0?cu[1]:d===a0?a(g[2],cu):cu,cE=[0,x([0,a(e[l],[0,cD,cC]),cB]),cA],a1=a(j[70][20],cE);else
var
a1=m(k);var
J=a1,n=1;break;default:var
n=0}if(!n)var
J=m(k);return J}catch(b){b=w(b);if(b===a5){var
z=k;continue}if(a(aP[4],b)){var
z=k;continue}throw b}}return c(j[70][3],ek,ej)}}return m(a(p[62][4],b))}],cU=a(p[62][9],lk),ll=[0,function(b){var
k=a(p[62][3],b),t=c(D[48][3],ab,b);function
m(k){var
b=aS(k);if(typeof
b!=="number")switch(b[0]){case
1:var
q=b[1];if(typeof
q==="number"){var
r=q-27|0;if(!(2<r>>>0))switch(r){case
0:if(!b[2])return cU;break;case
1:break;default:var
s=b[2];if(s)if(!s[2]){var
G=i[16],H=R(cK),I=c(j[70][3],H,G);return c(j[70][3],I,cU)}}}break;case
2:var
J=m(b[2]);return c(j[70][3],i[16],J)}try{var
v=a(t,k),x=i[16],o=h(cn),y=[0,k,v,cN(0)],z=f===o?cn[1]:d===o?a(g[2],cn):cn,A=a(e[l],[0,z,y]),B=a(D[45],A),E=a(p[66][1],B),F=c(j[70][3],E,x),n=F}catch(b){b=w(b);if(b!==a5)throw b;var
u=a(C[50],0),n=a(i[109],u)}return c(j[70][3],n,cU)}return m(k)}],en=a(p[62][9],ll);function
lm(b){a(C[11],ln);dM(0);return en}var
lo=a(p[13],0),dn=[0,m,aQ,ba,f0,f1,cG,bb,bc,cH,f2,bd,aR,dw,ah,gi,dA,be,c$,dE,aY,cJ,dc,dF,T,dG,x,go,dd,R,dH,aH,gq,N,dI,dL,dK,de,dJ,dM,dN,dO,aa,k,af,S,aq,ar,as,U,V,W,A,at,au,B,O,F,ai,aj,aZ,av,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,G,bs,bt,aw,ax,bu,bv,bw,bx,by,bz,bA,bB,bC,bD,bE,bF,X,P,bG,bH,bI,bJ,bK,bL,bM,bN,bO,bP,bQ,bR,bS,Y,bT,bU,bV,bW,bX,Z,bY,bZ,b0,b1,b2,b3,b4,b5,b6,b7,dP,dQ,dR,dS,dT,b8,dU,dV,dW,dX,dY,ay,az,ak,al,a0,a1,aA,b9,b_,aB,aC,b$,ca,cb,_,aD,aE,cc,cd,ce,dZ,d0,d1,d2,cf,d3,d4,d5,d6,cg,ch,ci,cj,ck,cl,cm,cn,co,cp,cq,cr,cs,ct,cu,cv,aF,d7,d8,d9,df,aI,iR,iT,cK,iV,ag,cL,d_,cw,iW,aJ,aK,cx,cM,aG,iX,aL,J,aS,cy,cz,d$,dg,ea,dh,K,aM,$,E,eb,Q,cN,di,cO,ec,a2,ed,t,a3,ee,cP,dj,cA,cQ,ef,cR,cS,am,dk,cB,a4,cT,dl,eh,aT,ei,aU,ej,ek,el,em,a5,ab,ac,dm,cU,en,c(p[67][1],lo,lm)];c0(434,dn,"Omega_plugin.Coq_omega");a(dp[12],ls);function
cC(b){var
d=c(r[13],H[1][5],lt),e=a(H[5][4],d),f=a(H[6][4],b),g=c(H[13][2],[0,e],f),h=a(cV[6],g);return a(eo[17],h)}function
cW(b){var
d=c(q[17][96],lq[29],b);function
e(b){if(cE(b,lu)){if(cE(b,lv)){if(cE(b,lw)){if(cE(b,lx)){var
d=c(n[16],ly,b);return a(ae[6],d)}return cC(lz)}return cC(lA)}return cC(lB)}return cC(lC)}var
g=c(r[13],e,d),h=dn[f],i=a(j[70][20],g),k=a(j[70][28],i);return c(j[70][3],k,h)}function
lD(d){var
b=[28,[0,0,[31,ep[4],[0,[0,aV,lE],0],0]]],c=a(H[1][5],lF);return cD(cV[4],1,0,c,b)}var
lG=[0,function(b,a){return cW(0)}];v(cV[9],0,[0,aV,lH],lG);c(dp[19],lD,aV);var
lI=0,lL=[0,function(b){return b?a(n[2],lJ):function(a){return cW(lK)}},lI],lN=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(eq[17],er[4]),f=a(eq[6],e),g=c(eo[2][7],f,d);return function(a){return cW(c(r[13],H[1][7],g))}}return a(n[2],lM)},lL],lO=a(lr[12],lN);v(cV[9],0,[0,aV,lP],lO);function
lQ(f){var
e=a(H[1][6],lS),b=er[4],d=0;if(0===b[0])return c(lp[4],[0,aV,lW],[0,[0,lV,[0,lU,[0,[1,ep[4],[0,[5,[0,b[1]]]],e],d]]],lR]);throw[0,dv,lT]}c(dp[19],lQ,aV);var
es=[0,aV,cC,cW];c0(444,es,"Omega_plugin.G_omega");c0(445,[0,c4,dn,es],"Omega_plugin");return});
