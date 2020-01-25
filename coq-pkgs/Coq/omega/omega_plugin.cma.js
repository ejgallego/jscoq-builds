function(lf){"use strict";var
dM="*",cQ=-16,dV=" 0\n",cP="Equation E",cO=" and E",dd=": ",aW=".\n",d=246,dS="(",dU="not a number",de="_vendor+v8.11+32bit/coq/plugins/omega/coq_omega.ml",H=123,dL="+ ",dP="- ",dJ="------------------------\n\n",aX="omega",dc=104,dO="Inequation E",dT=")",dR="with",dX="X%d",dN=" subsumes E",dg=" E",aN="Omega",dW=" states ",df=-18,dK="Equations E",Z=144,dZ="Omega: Can't solve a goal with non-linear products",aO=248,aM=100,dQ="Coq",g=250,dY="E%d subsumes E%d.\n",E=lf.jsoo_runtime,aV=E.caml_check_bound,dH=E.caml_equal,aL=E.caml_fresh_oo_id,b=E.caml_new_string,cK=E.caml_notequal,f=E.caml_obj_tag,cI=E.caml_register_global,cN=E.caml_string_notequal,cM=E.caml_trampoline,cL=E.caml_trampoline_return,t=E.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):E.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):E.caml_call_gen(a,[b,c])}function
o(a,b,c,d){return a.length==3?a(b,c,d):E.caml_call_gen(a,[b,c,d])}function
cJ(a,b,c,d,e){return a.length==4?a(b,c,d,e):E.caml_call_gen(a,[b,c,d,e])}function
dI(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):E.caml_call_gen(a,[b,c,d,e,f])}function
ld(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
le(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
q=E.caml_get_global_data(),lc=[11,b(" + "),[2,0,[12,32,[2,0,[11,b(dg),[4,0,0,0,[11,b(aW),0]]]]]]],dC=[0,[0,2],[0,0,0]],cG=b("omega_plugin"),p=q.Stdlib__list,w=q.Stdlib__printf,n=q.Stdlib,m=q.Util,I=q.Not_found,ae=q.Int,v=q.Stdlib__hashtbl,bb=q.Assert_failure,D=q.Names,e=q.CamlinternalLazy,h=q.EConstr,C=q.Bigint,ac=q.Pp,_=q.CErrors,aD=q.Logic,dr=q.Coqlib,j=q.Tactics,k=q.Tacticals,r=q.Proofview,z=q.Context,a2=q.Nameops,F=q.Tacmach,c5=q.Reductionops,dx=q.Refine,c3=q.Nametab,c4=q.Libnames,cZ=q.Tacred,a0=q.Goptions,dG=q.Ltac_plugin__Tacentries,kB=q.Termops,jW=q.Contradiction,j1=q.Equality,ig=q.Evarutil,hM=q.Global,hN=q.Evd,fk=q.UnivGen,kM=q.Stdlib__string,kK=q.Ltac_plugin__Tacenv,kL=q.Ltac_plugin__Tacinterp,kI=q.Mltop,kY=q.Deprecation,k8=q.Stdarg,k9=q.Genarg;cI(387,[0,0,0,0],"Omega_plugin");var
aE=[0,0],ej=[0,[11,b(dO),[4,0,0,0,[11,b(" is divided by "),[2,0,[11,b(" and the constant coefficient is rounded by subtracting "),[2,0,[11,b(aW),0]]]]]]],b("Inequation E%d is divided by %s and the constant coefficient is rounded by subtracting %s.\n")],ek=[0,[11,b("Constant in equation E"),[4,0,0,0,[11,b(" is not divisible by the pgcd "),[2,0,[11,b(" of its other coefficients.\n"),0]]]]],b("Constant in equation E%d is not divisible by the pgcd %s of its other coefficients.\n")],el=[0,[12,69,[4,0,0,0,[11,b(" is trivially satisfiable.\n"),0]]],b("E%d is trivially satisfiable.\n")],em=[0,[11,b(cP),[4,0,0,0,[11,b(" is divided by the pgcd "),[2,0,[11,b(" of its coefficients.\n"),0]]]]],b("Equation E%d is divided by the pgcd %s of its coefficients.\n")],en=[0,[11,b("We state "),[2,0,[11,b(dg),[4,0,0,0,[11,b(" = "),[2,0,[12,32,[2,0,[11,b(dg),[4,0,0,0,lc]]]]]]]]]],b("We state %s E%d = %s %s E%d + %s %s E%d.\n")],eo=[0,[11,b("We define a new equation E"),[4,0,0,0,[11,b(dd),0]]],b("We define a new equation E%d: ")],ep=b(" 0"),eq=[0,[11,b("We define E"),[4,0,0,0,[11,b(dd),0]]],b("We define E%d: ")],er=b(dV),es=[0,[12,69,[4,0,0,0,[11,b(dN),[4,0,0,0,[11,b(aW),0]]]]],b(dY)],et=[0,[12,69,[4,0,0,0,[11,b(dN),[4,0,0,0,[11,b(aW),0]]]]],b(dY)],eu=[0,[11,b(dK),[4,0,0,0,[11,b(cO),[4,0,0,0,[11,b(" imply a contradiction on their constant factors.\n"),0]]]]],b("Equations E%d and E%d imply a contradiction on their constant factors.\n")],ev=[0,[11,b(dK),[4,0,0,0,[11,b(cO),[4,0,0,0,[11,b(" state that their body is at the same time equal and different\n"),0]]]]],b("Equations E%d and E%d state that their body is at the same time equal and different\n")],ew=[0,[12,69,[4,0,0,0,[11,b(cO),[4,0,0,0,[11,b(" can be merged into E"),[4,0,0,0,[11,b(aW),0]]]]]]],b("E%d and E%d can be merged into E%d.\n")],ex=[0,[11,b(cP),[4,0,0,0,[11,b(dW),[2,0,[11,b(" = 0.\n"),0]]]]],b("Equation E%d states %s = 0.\n")],ey=[0,[11,b(dO),[4,0,0,0,[11,b(" states 0 != 0.\n"),0]]],b("Inequation E%d states 0 != 0.\n")],ez=[0,[11,b(cP),[4,0,0,0,[11,b(dW),[2,0,[11,b(" >= 0.\n"),0]]]]],b("Equation E%d states %s >= 0.\n")],eA=[0,[11,b(cP),[4,0,0,0,[11,b(" is split in E"),[4,0,0,0,[11,b(cO),[4,0,0,0,[11,b("\n\n"),0]]]]]]],b("Equation E%d is split in E%d and E%d\n\n")],eB=[0,[11,b("To ensure a solution in the dark shadow the equation E"),[4,0,0,0,[11,b(" is weakened by "),[2,0,[11,b(aW),0]]]]],b("To ensure a solution in the dark shadow the equation E%d is weakened by %s.\n")],eL=b("depend"),eO=b("solve"),eM=[0,b("_vendor+v8.11+32bit/coq/plugins/omega/omega.ml"),602,15],eK=b("disequation in simplify"),eJ=b("Product dardk"),eI=[0,0,0,0],eG=b("TL"),eF=b("eliminate_with_in"),eC=[0,[12,88,[4,0,0,0,0]],b(dX)],eh=b(">= 0\n"),ei=b(dJ),ee=[0,[12,69,[4,0,0,0,[11,b(dd),0]]],b("E%d: ")],ef=[0,[2,0,[11,b(dV),0]],b("%s 0\n")],eg=b(dJ),eb=b("equation"),ec=b("inequation"),ed=b("disequation"),d_=b("="),d$=b(">="),ea=b("!="),d3=b(dP),d6=b(dL),d7=b(""),d4=[0,[2,0,[12,32,0]],b("%s ")],d5=[0,[2,0,[12,32,[2,0,[12,32,0]]]],b("%s %s ")],d8=[0,[11,b(dL),[2,0,[12,32,0]]],b("+ %s ")],d9=[0,[11,b(dP),[2,0,[12,32,0]]],b("- %s ")],d0=b("pgcd_l"),d1=b("Omega_plugin.Omega.MakeOmegaSolver(I).UNSOLVABLE"),d2=b("Omega_plugin.Omega.MakeOmegaSolver(I).NO_CONTRADICTION"),eD=b("Omega_plugin.Omega.MakeOmegaSolver(I).FACTOR1"),eE=b("Omega_plugin.Omega.MakeOmegaSolver(I).CHOPVAR"),eH=b("Omega_plugin.Omega.MakeOmegaSolver(I).SOLVED_SYSTEM"),eN=b("Omega_plugin.Omega.MakeOmegaSolver(I).FULL_SOLUTION"),h8=b(dS),h9=b("+"),h_=b(dT),h$=b(dS),ia=b(dM),ib=b(dT),ic=b("?"),id=b("weight"),is=[0,2],it=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iu=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],io=[0,2],ip=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iq=[0,2],ir=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],iv=[0,2],iw=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],ix=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],iZ=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],[0,[0,[0,1],[0,[0,1],[0,[0,2],[0,[0,1],0]]]],0]]]],i0=[0,1],i1=[0,2],i2=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],i3=b(dZ),i4=[0,2],i5=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],jb=[0,1],jc=[0,2],jd=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],je=b(dZ),jf=[0,2],jg=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],0]],jh=[0,[0,[0,1],0],0],ji=[0,1],jj=[0,2],jk=[0,1],jl=[0,2],jm=[0,[0,[0,1],0],[0,[0,[0,2],0],0]],jn=[0,1],jE=[0,0,0],jB=[0,1],jC=[0,2],jx=[0,1],jy=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jz=[0,1],jA=[0,2],jD=[0,1],jG=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],jF=[0,2],kG=[0,b(dQ),[0,b(aX),[0,b(aN),0]]],kC=b("_eqn"),ky=b("_left"),kz=b("_right"),kh=[0,1],ka=[0,2],kb=[0,1],kc=[0,2],kd=[0,1],ke=[0,2],kf=[0,1],kg=[0,1],ki=[0,[0,3],[0,0,0]],kj=[0,[0,2],[0,0,0]],kk=[0,[0,2],[0,0,0]],kl=[0,[0,1],[0,0,0]],km=[0,[0,2],[0,0,0]],kn=[0,[0,1],[0,0,0]],ko=[0,[0,2],[0,0,0]],kp=[0,[0,1],[0,0,0]],kq=[0,[0,2],[0,0,0]],kr=[0,[0,1],[0,0,0]],ks=[0,[0,2],[0,0,0]],kt=[0,[0,1],[0,0,0]],j8=[0,0,0],j9=b("Omega can't solve this system"),j6=b("State"),j4=[0,0,0],jK=[0,[0,3],0],jL=[0,[0,2],0],jM=[0,[0,3],0],jN=[0,[0,3],0],jO=[0,[0,1],[0,0,0]],jP=[0,[0,2],[0,0,0]],jQ=[0,[0,2],[0,0,0]],jR=[0,[0,[0,1],0],0],jS=[0,2],jT=[0,1],jU=[0,1],jV=[0,[0,2],[0,0,0]],jX=[0,[0,1],[0,0,0]],jY=[0,[0,3],0],jZ=[0,[0,[0,1],0],0],j0=[0,[0,3],0],j2=[0,[0,2],[0,0,0]],j3=[0,[0,2],[0,0,0]],jH=b("auxiliary"),jI=b("auxiliary_1"),jJ=b("auxiliary_2"),ju=b("condense.1"),jv=[0,2],jw=[0,0,0],jt=b("reduce_factor.1"),jp=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]],jq=[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]],jr=[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,2],0]],0]],js=[0,[0,[0,1],0],0],jo=b("shrink.1"),i$=[0,2],ja=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],i9=[0,2],i_=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],i6=[0,2],i7=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]]],iM=[0,[0,[0,1],[0,[0,1],[0,[0,1],0]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]],iN=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],iO=[0,1],iP=[0,2],iQ=[0,2],iR=[0,2],iS=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iT=[0,2],iU=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iV=[0,2],iW=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],0],0]]],iX=[0,2],iY=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iz=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,1],[0,[0,2],0]],[0,[0,[0,2],[0,[0,2],0]],0]]]]]]],iA=[0,[0,[0,1],[0,[0,1],0]],[0,[0,[0,2],0],0]],iB=[0,1],iC=[0,2],iD=[0,2],iE=[0,2],iF=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],iG=[0,2],iH=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],iI=[0,2],iJ=[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],0],[0,[0,[0,1],[0,[0,2],0]],0]]]]],iK=[0,2],iL=[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,1],0]]]],[0,[0,[0,2],[0,[0,1],[0,[0,1],[0,[0,2],0]]]],[0,[0,[0,1],0],[0,[0,[0,2],[0,[0,1],[0,[0,2],0]]],[0,[0,[0,2],[0,[0,2],0]],0]]]]],im=[0,b(de),683,17],il=[0,b(de),684,13],ik=[0,b(de),643,9],ih=b("H"),ii=b("P"),ie=b("compile_equation."),h7=b("x"),h6=b("occurrence "),h5=b("abstract_path "),h3=b(dU),h4=b(dU),h0=b("Omega: Not a quantifier-free goal"),hY=b("not"),hX=b("Z.gt"),hW=b("Z.le"),hU=b("Z.sub"),hS=b("Z.pred"),hQ=b("Z.succ"),hO=b(" is not an evaluable constant."),hP=[0,b("Coq_omega")],fi=b("find_contr"),fg=b("tag_hypothesis"),fe=[0,1],fc=[0,[12,88,[4,0,0,0,0]],b(dX)],fb=b("WW"),e$=b("Zvar"),e9=b(aN),eQ=[0,b(aN),[0,b("System"),0]],eR=b("Omega system time displaying flag"),eU=[0,b(aN),[0,b("Action"),0]],eV=b("Omega action display flag"),eY=[0,b(aN),[0,b("OldStyle"),0]],eZ=b("Omega old style flag"),e2=[0,b("Stable"),[0,b(aN),0]],e3=b("Omega automatic reset of generated names"),e6=[0,b(aN),[0,b("UseLocalDefs"),0]],e7=b("Omega takes advantage of context variables with body"),fl=b("num.pos.xH"),fm=b("num.pos.xO"),fn=b("num.pos.xI"),fo=b("num.Z.Z0"),fp=b("num.Z.Zpos"),fq=b("num.Z.Zneg"),fr=b("num.Z.type"),fs=b("core.comparison.type"),ft=b("core.comparison.Gt"),fu=b("num.Z.add"),fv=b("num.Z.mul"),fw=b("num.Z.opp"),fx=b("num.Z.sub"),fy=b("num.Z.succ"),fz=b("num.Z.pred"),fA=b("num.Z.of_nat"),fB=b("num.Nat2Z.inj_add"),fC=b("num.Nat2Z.inj_mul"),fD=b("num.Nat2Z.inj_sub"),fE=b("plugins.omega.inj_minus2"),fF=b("num.Nat2Z.inj_succ"),fG=b("plugins.omega.inj_eq"),fH=b("plugins.omega.inj_neq"),fI=b("plugins.omega.inj_le"),fJ=b("plugins.omega.inj_lt"),fK=b("plugins.omega.inj_ge"),fL=b("plugins.omega.inj_gt"),fM=b("plugins.omega.fast_Zplus_assoc_reverse"),fN=b("plugins.omega.fast_Zplus_assoc"),fO=b("plugins.omega.fast_Zmult_assoc_reverse"),fP=b("plugins.omega.fast_Zplus_permute"),fQ=b("plugins.omega.fast_Zplus_comm"),fR=b("plugins.omega.fast_Zmult_comm"),fS=b("plugins.omega.Zmult_le_approx"),fT=b("plugins.omega.OMEGA1"),fU=b("plugins.omega.OMEGA2"),fV=b("plugins.omega.OMEGA3"),fW=b("plugins.omega.OMEGA4"),fX=b("plugins.omega.OMEGA5"),fY=b("plugins.omega.OMEGA6"),fZ=b("plugins.omega.OMEGA7"),f0=b("plugins.omega.OMEGA8"),f1=b("plugins.omega.OMEGA9"),f2=b("plugins.omega.fast_OMEGA10"),f3=b("plugins.omega.fast_OMEGA11"),f4=b("plugins.omega.fast_OMEGA12"),f5=b("plugins.omega.fast_OMEGA13"),f6=b("plugins.omega.fast_OMEGA14"),f7=b("plugins.omega.fast_OMEGA15"),f8=b("plugins.omega.fast_OMEGA16"),f9=b("plugins.omega.OMEGA17"),f_=b("plugins.omega.OMEGA18"),f$=b("plugins.omega.OMEGA19"),ga=b("plugins.omega.OMEGA20"),gb=b("plugins.omega.fast_Zred_factor0"),gc=b("plugins.omega.fast_Zred_factor1"),gd=b("plugins.omega.fast_Zred_factor2"),ge=b("plugins.omega.fast_Zred_factor3"),gf=b("plugins.omega.fast_Zred_factor4"),gg=b("plugins.omega.fast_Zred_factor5"),gh=b("plugins.omega.fast_Zred_factor6"),gi=b("plugins.omega.fast_Zmult_plus_distr_l"),gj=b("plugins.omega.fast_Zopp_plus_distr"),gk=b("plugins.omega.fast_Zopp_mult_distr_r"),gl=b("plugins.omega.fast_Zopp_eq_mult_neg_1"),gm=b("plugins.omega.Zegal_left"),gn=b("plugins.omega.Zne_left"),go=b("plugins.omega.Zlt_left"),gp=b("plugins.omega.Zge_left"),gq=b("plugins.omega.Zgt_left"),gr=b("plugins.omega.Zle_left"),gs=b("plugins.omega.new_var"),gt=b("plugins.omega.intro_Z"),gu=b("num.Z.eq_decidable"),gv=b("plugins.omega.dec_Zne"),gx=b("num.Z.le_decidable"),gz=b("num.Z.lt_decidable"),gB=b("plugins.omega.dec_Zgt"),gD=b("plugins.omega.dec_Zge"),gF=b("plugins.omega.not_Zeq"),gG=b("plugins.omega.not_Zne"),gI=b("plugins.omega.Znot_le_gt"),gK=b("plugins.omega.Znot_lt_ge"),gM=b("plugins.omega.Znot_ge_lt"),gO=b("plugins.omega.Znot_gt_le"),gQ=b("plugins.omega.neq"),gR=b("plugins.omega.Zne"),gS=b("num.Z.le"),gT=b("num.Z.lt"),gU=b("num.Z.ge"),gV=b("num.Z.gt"),gW=b("num.nat.type"),gX=b("num.nat.O"),gY=b("num.nat.S"),gZ=b("num.nat.le"),g0=b("num.nat.lt"),g1=b("num.nat.ge"),g2=b("num.nat.gt"),g3=b("num.nat.add"),g4=b("num.nat.sub"),g5=b("num.nat.mul"),g6=b("num.nat.pred"),g7=b("num.nat.pred_of_minus"),g8=b("num.nat.le_gt_dec"),g9=b("num.nat.eq_dec"),g_=b("num.nat.dec_le"),ha=b("num.nat.dec_lt"),hc=b("num.nat.dec_ge"),he=b("num.nat.dec_gt"),hg=b("num.nat.not_eq"),hh=b("num.nat.not_le"),hj=b("num.nat.not_lt"),hl=b("num.nat.not_ge"),hn=b("num.nat.not_gt"),hp=b("core.eq.ind_r"),hq=b("core.dec.or"),hr=b("core.dec.and"),hs=b("core.dec.imp"),ht=b("core.dec.iff"),hu=b("core.dec.not"),hv=b("core.dec.False"),hw=b("core.dec.not_not"),hx=b("core.dec.True"),hy=b("core.dec.not_or"),hz=b("core.dec.not_and"),hA=b("core.dec.not_imp"),hB=b("core.dec.not_iff"),hC=b("core.dec.dec_not_not"),hD=b("core.dec.imp_simp"),hE=b("core.iff.type"),hF=b("core.not.type"),hG=b("core.and.type"),hH=b("core.or.type"),hI=b("core.eq.type"),hJ=b("core.ex.type"),hK=b("core.False.type"),hL=b("core.True.type"),kx=b("Omega_plugin.Coq_omega.Undecidable"),k4=b("zify"),kN=b("N"),kO=b("Z"),kP=b("nat"),kQ=b("positive"),kS=b("zify_positive"),kT=b("zify_nat"),kU=b("zify_op"),kV=b("zify_N"),kR=b("No Omega knowledge base for type "),kJ=[0,b("PreOmega"),[0,b(aX),[0,b(dQ),0]]],kW=[0,b("Use lia instead.")],kX=[0,b("8.11.0")],k0=[0,b(aX),0],k2=b(aX),k5=[0,b(aX),[0,b(dR),[0,b(dM),0]]],k_=b(dR),k$=b(aX),lb=b("omega'");function
dh(f){var
j=f[1],s=f[2];function
aq(b,a){var
d=c(f[2],b,a),e=d||dH(b,a);return e}function
A(b,a){return c(f[2],a,b)}function
M(b,a){var
d=c(f[2],a,b),e=d||dH(b,a);return e}var
h=f[3],k=f[4],i=f[5];function
x(b,a){return c(f[6],b,a)[1]}function
N(b,a){return c(f[6],b,a)[2]}var
d=f[8],e=f[9],y=c(h,e,e),ad=a(f[7],e);function
l(b){return c(f[2],b,d)?a(f[7],b):b}var
g=f[10],u=f[7];function
ar(b,a){return b<a?1:0}function
as(b,a){return a<b?1:0}function
at(b,a){return b<=a?1:0}function
au(b,a){return a<=b?1:0}function
av(b){a(n[33],b);a(n[36],0);return a(n[52],n[28])}function
E(b,a){a[1]=[0,b,a[1]];return 0}function
af(f,e){var
b=f,a=e;for(;;){if(c(j,a,d))return b;var
g=N(b,a),b=a,a=g;continue}}function
ag(b){return b?o(p[20],af,b[1],b[2]):a(n[3],d0)}function
F(b,a){var
g=M(b,d),f=A(a,d);return 0===g?0===f?x(b,a):c(k,x(c(h,b,e),a),e):0===f?c(k,x(c(k,b,e),a),e):x(b,a)}var
q=[aO,d1,aL(0)],ah=[aO,d2,aL(0)];function
G(h,f){var
b=f[2],k=f[1],m=0;function
q(i,b){var
k=c(s,b[1],d)?d3:i?d6:d7;a(n[31],k);var
f=l(b[1]);if(c(j,f,e)){var
m=a(h,b[2]);c(w[2],d4,m)}else{var
p=a(h,b[2]),q=a(g,f);o(w[2],d5,q,p)}return 1}o(p[20],q,m,k);if(A(b,d)){var
r=a(g,b);return c(w[2],d8,r)}var
i=c(s,b,d);if(i){var
t=a(g,l(b));return c(w[2],d9,t)}return i}function
W(a){function
b(b,a){if(15===a[0]){var
d=a[2][2],f=W(a[3][2]),g=W(d);return c(h,c(h,c(h,b,e),g),f)}return c(h,b,e)}return o(p[20],b,d,a)}function
O(a){switch(a){case
0:return d_;case
1:return d$;default:return ea}}function
P(a){switch(a){case
0:return eb;case
1:return ec;default:return ed}}function
z(d,b){function
e(a){var
b=a[4],e=a[3],f=a[2];c(w[2],ee,a[1]);G(d,[0,e,b]);var
g=O(f);return c(w[2],ef,g)}c(p[15],e,b);return a(n[31],eg)}function
aw(d,b){function
e(b){G(d,b);return a(n[31],eh)}c(p[15],e,b);return a(n[31],ei)}function
Q(d,q){var
e=q;for(;;){if(e){var
b=e[1],r=e[2];switch(b[0]){case
0:var
s=b[3],t=b[1],u=a(g,b[4]),v=a(g,s);cJ(w[2],ej,t[1],v,u);break;case
1:var
x=b[1],y=a(g,b[2]);o(w[2],ek,x[1],y);break;case
2:c(w[2],el,b[1]);break;case
3:var
z=b[1],A=a(g,b[2]);o(w[2],em,z[1],A);break;case
4:var
j=b[3],k=j[2],l=b[2],i=l[2],B=j[1],C=l[1],D=b[1],E=k[1],F=P(k[2]),H=a(g,B),I=i[1],J=P(i[2]),K=a(g,C),L=P(i[2]);ld(w[2],en,L,D,K,J,I,H,F,E);break;case
5:var
f=b[1][1];c(w[2],eo,f[1]);G(d,[0,f[3],f[4]]);var
M=O(f[2]);a(n[31],M);a(n[31],ep);break;case
6:var
h=b[1];c(w[2],eq,h[1]);G(d,[0,h[3],h[4]]);var
N=O(h[2]);a(n[31],N);a(n[31],er);break;case
7:o(w[2],es,b[1],b[2]);break;case
8:o(w[2],et,b[1],b[2]);break;case
9:o(w[2],eu,b[1][1],b[2][1]);break;case
10:o(w[2],ev,b[1][1],b[2][1]);break;case
11:cJ(w[2],ew,b[2][1],b[3],b[1]);break;case
12:var
R=b[1],S=a(g,b[2]);o(w[2],ex,R,S);break;case
13:c(w[2],ey,b[1]);break;case
14:var
T=b[1],U=a(g,b[2]);o(w[2],ez,T,U);break;case
15:var
m=b[3],p=b[2],V=m[2],W=p[2];cJ(w[2],eA,b[1][1],p[1],m[1]);Q(d,W);a(n[36],0);Q(d,V);a(n[36],0);break;default:var
X=b[1],Y=a(g,b[2]);o(w[2],eB,X,Y)}var
e=r;continue}return a(n[52],n[28])}}function
ai(a){return c(w[4],eC,a)}var
X=[0,0];function
R(a){X[1]=0;return 0}function
S(a){return X[1]}function
b(a){if(aE[1])Q(ai,[0,a,0]);return E(a,X)}function
ax(b,a){return c(m[5],a[2],b[2])}var
aj=a(p[48],ax);function
ay(b){var
c=b[2],d=c[2],e=b[1];return[0,e,[0,a(aj,c[1]),d]]}function
B(i){function
e(k){var
b=k;for(;;){if(b){var
f=b[2],g=b[1],h=a(i,g[1]);if(c(j,h,d)){var
b=f;continue}var
l=e(f);return[0,[0,h,g[2]],l]}return 0}}return e}function
C(c,b){var
d=a(c,b[4]),e=b[3],f=a(B(c),e);return[0,b[1],b[2],f,d]}function
az(b){return a(u,b)}function
H(a){return C(az,a)}function
J(m,l){var
b=m,a=l;for(;;){if(b){if(a){var
g=a[2],f=a[1],i=b[2],e=b[1];if(e[2]===f[2]){var
k=c(h,e[1],f[1]);if(c(j,k,d)){var
b=i,a=g;continue}var
n=J(i,g);return[0,[0,k,e[2]],n]}return f[2]<e[2]?[0,e,J(i,a)]:[0,f,J(b,g)]}return b}return a}}function
Y(e,b,d){var
f=c(h,b[4],d[4]),g=J(b[3],d[3]),i=b[2];return[0,a(e,0),i,g,f]}var
Z=[aO,eD,aL(0)];function
_(a){if(a){var
d=a[2],b=a[1];if(c(j,l(b[1]),e))return[0,b,d];var
f=_(d);return[0,f[1],[0,b,f[2]]]}throw Z}var
T=[aO,eE,aL(0)];function
K(c,a){if(a){var
d=a[2],b=a[1];if(b[2]===c)return[0,b,d];var
e=K(c,d);return[0,e[1],[0,b,e[2]]]}throw T}function
r(f){var
g=f[4],o=f[3],m=f[2],n=f[1];if(0===o)switch(m){case
0:if(c(j,g,d))return 0;b([12,n,g]);throw q;case
1:if(M(g,d))return 0;b([14,n,g]);throw q;default:if(cK(g,d))return 0;b([13,n]);throw q}function
v(a){return l(a[1])}var
h=ag(c(p[17],v,o));if(0===m)if(cK(N(g,h),d)){b([1,f,h]);throw q}if(2===m)if(cK(N(g,h),d)){b([2,f[1]]);return 0}if(cK(h,e)){var
s=F(g,h),w=c(k,g,c(i,s,h)),t=[0,n,m,a(B(function(a){return x(a,h)}),o),s];if(0===m)var
r=0;else
if(2===m)var
r=0;else
var
u=[0,f,t,h,w],r=1;if(!r)var
u=[3,f,h];b(u);return[0,t,0]}return[0,f,0]}function
D(o,g,f,d){var
h=g[1],p=d[3],q=g[2];try{var
k=K(q,p)[1],l=c(j,h,e)?a(u,k[1]):c(j,h,ad)?k[1]:a(n[3],eF),m=Y(o,d,C(function(a){return c(i,a,l)},f));b([4,m[1],[0,e,d],[0,l,f]]);return m}catch(a){a=t(a);if(a===T)return d;throw a}}function
$(b,a){var
d=c(i,y,a);return c(k,b,c(i,a,F(c(h,c(i,y,b),a),d)))}function
ak(q,f,H,G){var
g=q[1],j=f[3],I=q[3],t=a(q[2],0);if(0===j){z(I,[0,f,0]);a(n[3],eG)}var
J=a(p[6],j),L=a(p[5],j)[2],M=[0,l(a(p[5],j)[1]),L];function
N(b,a){var
c=b[1],d=b[2];if(A(c,l(a[1]))){var
e=a[2];return[0,l(a[1]),e]}return[0,c,d]}var
v=o(p[20],N,M,J),O=v[2],d=c(h,v[1],e),P=$(f[4],d),Q=f[3],R=a(B(function(a){return $(a,d)}),Q),S=[0,[0,a(u,d),t],R],w=[0,a(g,0),0,S,P],T=c(i,y,d),U=a(u,F(c(h,c(i,y,f[4]),d),T)),V=f[3],W=a(B(function(b){var
e=c(i,y,d);return a(u,F(c(h,c(i,y,b),d),e))}),V);b([5,[0,w,[0,a(g,0),0,W,U],f,d,t]]);var
X=r(w),k=a(p[5],X),s=K(O,k[3])[1];function
Y(a){return r(D(g,s,k,a))}var
Z=c(m[22][76],Y,H);function
_(a){return r(D(g,s,k,a))}var
aa=c(m[22][76],_,G),E=D(g,s,k,f),ab=C(function(a){return x(a,d)},E);b([3,E,d]);var
ac=r(ab);return[0,a(p[5],ac),Z,aa]}function
al(d,i){var
b=i;for(;;){var
f=b[3],e=b[2],a=b[1],g=d[1],j=d[3];if(aE[1])z(j,[0,a,e]);try{var
h=_(a[3])[1],k=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,g,h),l=c(m[22][76],k,f),n=function(b,c,d){return function(a){return r(D(c,d,b,a))}}(a,g,h),o=[0,c(m[22][76],n,e),l];return o}catch(c){c=t(c);if(c===Z){var
b=ak(d,a,e,f);continue}throw c}}}function
aa(k,o){var
f=o;for(;;){var
g=f[2],h=f[1],r=k[3],m=function(a){if(a){var
d=a[2],b=a[1],g=b[3],h=function(a){return c(j,l(a[1]),e)};if(c(p[28],h,g))return[0,b,d];var
f=m(d);return[0,f[1],[0,b,f[2]]]}throw I};if(h){var
s=h[2],u=h[1];try{var
n=m(h),v=n[2],w=n[1],a=w,i=v}catch(b){b=t(b);if(b!==I)throw b;var
a=u,i=s,x=b}if(0===a[3]){if(c(j,a[4],d)){b([2,a[1]]);var
f=[0,i,g];continue}b([12,a[1],a[4]]);throw q}var
f=al(k,[0,a,i,g]);continue}if(aE[1])z(r,g);return g}}function
L(n,e){function
y(a){var
b=a[3];if(b)if(c(s,b[1][1],d))return[0,H(a),0];return[0,a,1]}var
f=c(v[1],0,7);function
i(z){var
l=y(z),m=l[2],a=l[1],g=a[3];if(0===g){if(c(s,a[4],d)){b([14,a[1],a[4]]);throw q}return b([2,a[1]])}try{var
n=c(v[6],f,g),i=n[2],j=n[1];if(1===m)if(j)var
k=j[1],C=c(s,k[4],a[4])?(b([7,k[1],a[1]]),k):(b([7,a[1],k[1]]),a),e=[0,[0,C],i];else
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
u=r[1],w=p[1];if(c(s,w[4],u[4])){b([9,w,H(u)]);throw q}var
x=1}else
var
x=0}else
var
x=0;c(v[10],f,g);var
D=o(v[5],f,g,e);return D}catch(b){b=t(b);if(b===I){var
B=1===m?[0,[0,a],0]:[0,0,[0,a]];return o(v[5],f,g,B)}throw b}}c(p[15],i,e);var
h=[0,0],g=[0,0];function
k(o,f){var
d=f[1];if(d){var
i=f[2];if(i){var
k=i[1],e=d[1];if(c(j,e[4],k[4])){var
l=a(n,0);b([11,l,e,k[1]]);return E([0,l,0,e[3],e[4]],h)}}}var
m=f[2];if(d)E(d[1],g);return m?E(H(m[1]),g):0}c(v[12],k,f);return[0,h[1],g[1]]}var
U=[aO,eH,aL(0)];function
am(g){var
b=c(v[1],0,7);function
h(e,d){try{var
a=c(v[6],b,e),g=l(d);a[1]=c(n[6],a[1],g);var
h=0;return h}catch(a){a=t(a);if(a===I){var
f=[0,l(d)];return o(v[5],b,e,f)}throw a}}function
i(a){var
b=a[3];function
d(a){return h(a[2],a[1])}return c(p[15],d,b)}c(p[15],i,g);var
e=[0,d],a=[0,-1],f=[0,0];function
j(h,g){var
b=g[1];f[1]++;var
i=c(s,b,e[1]),d=i||(-1===a[1]?1:0),j=d?(a[1]=h,e[1]=b,0):d;return j}c(v[12],j,b);if(f[1]<1)throw U;return a[1]}function
an(i,b){function
c(c,b){var
e=c[3],f=c[2],g=c[1];try{var
h=K(i,b[3])[1],j=M(h[1],d)?[0,g,[0,[0,h[1],b],f],e]:[0,g,f,[0,[0,a(u,h[1]),b],e]];return j}catch(a){a=t(a);if(a===T)return[0,[0,b,g],f,e];throw a}}return o(p[20],c,eI,b)}function
ao(u,t,d,g){var
f=0;function
h(h,d){var
j=d[2],f=d[1];function
l(m,l){var
o=l[2],g=l[1],v=C(function(a){return c(i,a,f)},o),p=Y(u,C(function(a){return c(i,a,g)},j),v);b([4,p[1],[0,g,j],[0,f,o]]);var
h=r(p);if(h){var
d=h[1];if(h[2])return a(n[3],eJ);if(t){var
w=c(k,g,e),q=c(i,c(k,f,e),w);b([16,d[1],q]);var
x=c(k,d[4],q),s=[0,d[1],1,d[3],x]}else
var
s=d;return[0,s,m]}return m}return o(p[20],l,h,g)}return o(p[20],h,f,d)}function
ab(d,f,b){var
g=d[3],h=d[1],a=an(am(b),b),i=a[1],j=ao(h,f,a[2],a[3]),e=c(n[26],i,j);if(aE[1])z(g,e);return e}function
aA(d,l,e){var
h=d[1],o=d[3];function
q(a){return 2===a[2]?1:0}if(c(p[28],q,e))a(n[3],eK);R(0);function
s(a){return b([6,a])}c(p[15],s,e);var
u=c(m[22][76],r,e);function
v(a){return 0===a[2]?1:0}var
i=c(p[37],v,u),w=i[1],j=L(h,i[2]),x=j[2],y=[0,c(n[26],w,j[1]),x];function
g(b,c){var
a=aa(d,c);return b<50?f(b+1|0,a):cL(f,[0,a])}function
f(e,f){var
a=L(h,f),b=a[2],c=a[1];if(0===c)return b;var
d=[0,c,b];return e<50?g(e+1|0,d):cL(g,[0,d])}function
A(a){return cM(g(0,a))}function
B(a){return cM(f(0,a))}function
k(b){try{var
a=k(B(ab(d,l,b)));return a}catch(a){a=t(a);if(a===U){if(aE[1])z(o,b);return b}throw a}}return k(A(y))}function
V(k,j,i){var
f=k,d=j,e=i;for(;;){if(e){var
g=e[2],b=e[1];switch(b[0]){case
0:if(c(ae[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
1:var
f=[0,b[1][1],f],d=[0,b,d],e=g;continue;case
2:var
e=g;continue;case
3:if(c(ae[4][1],b[1][1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue;case
4:var
l=b[3][2],m=b[2][2];if(c(ae[4][1],b[1],f)){var
f=[0,m[1],[0,l[1],f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
5:var
h=b[1],o=h[3];if(c(ae[4][1],h[1][1],f)){var
f=[0,o[1],f],d=[0,b,d],e=g;continue}var
e=g;continue;case
6:if(c(ae[4][1],b[1][1],f)){var
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
p=b[3],q=b[2];if(c(ae[4][1],b[1],f)){var
f=[0,q[1],[0,p,f]],d=[0,b,d],e=g;continue}var
e=g;continue;case
12:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
13:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
14:var
f=[0,b[1],f],d=[0,b,d],e=g;continue;case
15:return a(n[3],eL);default:if(c(ae[4][1],b[1],f)){var
d=[0,b,d],e=g;continue}var
e=g;continue}}return[0,f,d]}}function
ap(a){var
g=a[2],h=a[1];function
i(a){return 2===a[2]?1:0}var
j=c(p[37],i,g)[1];function
e(a){var
b=a[3];if(b)if(c(s,b[1][1],d))return[0,H(a),0];return[0,a,1]}var
f=c(v[1],0,7);function
k(a){var
b=e(a),c=b[1];return o(v[5],f,[0,c[3],c[4]],[0,b[2],a])}c(p[15],k,j);function
l(a){if(0===a[2]){var
d=e(a),g=d[1],i=d[2],j=g[4],k=g[3];try{var
h=c(v[6],f,[0,k,j]);b([10,a,h[2],i===h[1]?1:0]);throw q}catch(a){a=t(a);if(a===I)return 0;throw a}}throw[0,bb,eM]}return c(p[15],l,h)}var
ac=[aO,eN,aL(0)];return[0,j,s,aq,A,M,h,k,i,x,N,d,e,y,ad,l,g,u,ar,as,at,au,av,E,af,ag,F,q,ah,G,W,O,P,z,aw,g,Q,ai,b,S,R,aj,ay,B,C,H,J,Y,Z,_,T,K,r,D,$,ak,al,aa,L,U,am,an,ao,ab,aA,V,ap,ac,function(d,j){var
f=d[1],D=d[3];R(0);function
E(a){return b([6,a])}c(p[15],E,j);function
i(c,a){ap(a);var
b=aa(d,a);return c<50?h(c+1|0,b):cL(h,[0,b])}function
h(j,k){function
l(a){return 2===a[2]?1:0}var
a=c(p[37],l,k),b=a[1],d=L(f,a[2]),e=d[2],g=d[1];if(0===g)return c(n[26],b,e);var
h=[0,g,c(n[26],b,e)];return j<50?i(j+1|0,h):cL(i,[0,h])}function
F(a){return cM(i(0,a))}function
G(a){return cM(h(0,a))}function
l(b){try{var
a=l(G(ab(d,0,b)));return a}catch(a){a=t(a);if(a===U){if(aE[1])z(D,b);return b}throw a}}function
H(l){var
d=l;for(;;){var
g=d[1];if(g){var
j=d[2],b=g[1],m=d[3],o=g[2],h=a(f,0),i=a(f,0),q=c(k,b[4],e),r=[0,h,1,b[3],q],s=c(k,a(u,b[4]),e),t=b[3],v=[0,i,1,a(B(u),t),s],w=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,0],a[1]],[0,d,a[2]]]}}(b,i,v),x=c(p[17],w,j),y=function(b,c,d){return function(a){return[0,[0,[0,b[1],c,1],a[1]],[0,d,a[2]]]}}(b,h,r),z=c(p[17],y,j),A=c(n[26],z,x),d=[0,o,A,[0,[0,b[1],[0,b,h,i]],m]];continue}return[0,d[2],d[3]]}}try{var
J=c(m[22][76],r,j),K=function(a){return 0===a[2]?1:0},s=c(p[37],K,J),M=s[2],N=s[1],O=function(a){return 2===a[2]?1:0},w=c(p[37],O,M),P=w[1],x=L(f,w[2]),Q=x[1],T=c(n[26],x[2],P),W=F([0,c(n[26],N,Q),T]),X=function(a){return 2===a[2]?1:0},y=c(p[37],X,W),Y=y[2],Z=y[1],_=S(0),A=H([0,Z,[0,[0,0,Y],0],0]),$=A[2],ad=A[1],af=function(a){var
b=a[1],f=a[2];R(0);try{l(f);throw ah}catch(a){a=t(a);if(a===q){var
d=V(0,0,S(0)),e=d[1],g=d[2],h=function(a){return c(ae[4][1],a[2],e)},i=c(p[37],h,b)[1],j=function(a){return a[1]};return[0,c(p[17],j,i),e,b,g]}throw a}},ag=c(p[17],af,ad),ai=function(e){var
b=c(v[1],0,7),a=[0,-1],d=[0,0];function
f(d){try{c(v[6],b,d)[1]++;var
a=0;return a}catch(a){a=t(a);if(a===I)return o(v[5],b,d,[0,1]);throw a}}function
g(a){var
b=a[1];if(b)return c(p[15],f,b);throw[0,ac,a[4],a[2]]}c(p[15],g,e);function
h(e,b){var
c=d[1]<b[1]?1:0,f=c?(a[1]=e,d[1]=b[1],0):c;return f}c(v[12],h,b);return a[1]},g=function(e){try{var
d=ai(e),l=function(g){var
b=g[3];for(;;){if(b){var
c=b[1],e=b[2],f=c[3];if(d===c[1])return f;var
b=e;continue}return a(n[3],eO)}},f=c(p[37],l,e),q=f[2],r=f[1],h=function(a){var
b=a[4],c=a[3],e=a[2],f=a[1];function
g(b,a){return b===a?1:0}return[0,o(m[22][95],g,d,f),e,c,b]},s=c(p[17],h,r),u=c(p[17],h,q),i=g(s),v=i[2],w=i[1],j=g(u),x=j[2],y=j[1],b=c(ae[4][2],d,$),k=b[1],z=b[3],A=b[2],B=function(b,a){return b===a?1:0},C=o(m[22][129],B,v,x),D=[0,[0,[15,k,[0,A,w],[0,z,y]],0],[0,k[1],C]];return D}catch(a){a=t(a);if(a[1]===ac)return[0,a[2],a[3]];throw a}},C=g(ag),aj=V(C[2],C[1],_)[2];return aj}catch(a){a=t(a);if(a===q)return V(0,0,S(0))[2];throw a}}]}cI(396,[0,aE,dh],"Omega_plugin__Omega");var
l=dh([0,C[17],C[16],C[12],C[13],C[14],C[15],C[22],C[5],C[6],C[2]]);function
aP(b){var
c=a(h[11],b);return a(j[aM],c)}function
bc(b){var
c=a(h[11],b);return a(j[87],c)}var
cR=[0,0],bd=[0,0],be=[0,0],cS=[0,1],cT=[0,1];function
aY(b,c){return a(m[3],b)}function
aZ(b,a){b[1]=a;return 0}function
eP(a){return aZ(cR,a)}var
eS=[0,0,eR,eQ,function(a){return aY(cR,a)},eP];c(a0[4],0,eS);function
eT(a){return aZ(bd,a)}var
eW=[0,0,eV,eU,function(a){return aY(bd,a)},eT];c(a0[4],0,eW);function
eX(a){return aZ(be,a)}var
e0=[0,0,eZ,eY,function(a){return aY(be,a)},eX];c(a0[4],0,e0);function
e1(a){return aZ(cT,a)}var
e4=[0,1,e3,e2,function(a){return aY(cT,a)},e1];c(a0[4],0,e4);function
e5(a){return aZ(cS,a)}var
e8=[0,0,e7,e6,function(a){return aY(cS,a)},e5];c(a0[4],0,e8);var
cU=[0,0];function
a1(b){var
c=[0,b];cU[1]=[0,[0,c,b],a(m[3],cU)];return c}var
di=a1(0);function
af(f){var
b=a(m[3],di),d=a(n[22],b),e=c(n[17],e9,d);di[1]++;return a(D[1][6],e)}var
dj=a1(0);function
e_(f){var
b=a(m[3],dj),d=a(n[22],b),e=c(n[17],e$,d);dj[1]++;return a(D[1][6],e)}var
dk=a1(0);function
bf(b){dk[1]++;return a(m[3],dk)}var
dl=a1(1000);function
dm(b){dl[1]++;return a(m[3],dl)}var
dn=a1(0);function
fa(d){dn[1]++;var
b=[0,a(m[3],dn)];return c(a2[1],fb,b)}function
a3(a){return c(w[4],fc,a)}var
cV=[0,0],bg=c(v[1],0,7),cW=c(v[1],0,7);function
dp(b){try{var
a=c(v[6],cW,b);return a}catch(a){a=t(a);if(a===I){var
d=fa(0);o(v[5],bg,d,b);o(v[5],cW,b,d);return d}throw a}}function
cX(d){try{var
b=c(v[6],bg,d);return b}catch(b){b=t(b);if(b===I){var
e=a(m[3],cV);o(v[5],bg,d,e);o(v[5],cW,e,d);cV[1]++;return e}throw b}}function
P(b){return a(k[57][22],b)}function
fd(a){return cJ(j[109],0,fe,1,[0,[0,a,0]])}function
u(b){return a(j[148],b)}function
O(b){var
c=f(b),h=0,i=g===c?b[1]:d===c?a(e[2],b):b;return a(j[69],[0,[0,0,i],h])}function
cY(b,a){return o(F[32][1],cZ[9],b,a)}var
bh=[0,0];function
ff(c){return function(d){var
a=d;for(;;){if(a){var
b=a[1],e=a[2],f=b[1];if(c===b[2])return f;var
a=e;continue}throw I}}}function
J(b){try{var
c=a(m[3],bh),d=a(ff(b),c);return d}catch(b){b=t(b);if(b===I)return a(n[3],fg);throw b}}function
aF(c,b){bh[1]=[0,[0,c,b],a(m[3],bh)];return 0}var
a4=[0,0];function
fh(b){return a(m[3],a4)}function
dq(a){a4[1]=0;return 0}function
fj(e,d,c,b){a4[1]=[0,[0,e,[0,d,c,b]],a(m[3],a4)];return 0}function
i(b){return[d,function(e){var
c=a(dr[2],b),d=a(fk[15],c);return a(h[9],d)}]}var
al=i(fl),am=i(fm),an=i(fn),Q=i(fo),R=i(fp),S=i(fq),x=i(fr),bi=i(fs),ao=i(ft),y=i(fu),K=i(fv),L=i(fw),ag=i(fx),ah=i(fy),a5=i(fz),ap=i(fA),bj=i(fB),bk=i(fC),bl=i(fD),bm=i(fE),bn=i(fF),bo=i(fG),bp=i(fH),bq=i(fI),br=i(fJ),bs=i(fK),bt=i(fL),A=i(fM),bu=i(fN),bv=i(fO),aq=i(fP),ar=i(fQ),bw=i(fR),bx=i(fS),by=i(fT),bz=i(fU),bA=i(fV),bB=i(fW),bC=i(fX),bD=i(fY),bE=i(fZ),bF=i(f0),bG=i(f1),bH=i(f2),T=i(f3),M=i(f4),bI=i(f5),bJ=i(f6),bK=i(f7),bL=i(f8),bM=i(f9),bN=i(f_),bO=i(f$),bP=i(ga),bQ=i(gb),bR=i(gc),bS=i(gd),bT=i(ge),bU=i(gf),U=i(gg),bV=i(gh),bW=i(gi),bX=i(gj),bY=i(gk),V=i(gl),bZ=i(gm),b0=i(gn),b1=i(go),b2=i(gp),b3=i(gq),b4=i(gr),b5=i(gs),b6=i(gt),b7=i(gu),gw=i(gv),gy=i(gx),gA=i(gz),gC=i(gB),gE=i(gD),b8=i(gF),gH=i(gG),gJ=i(gI),gL=i(gK),gN=i(gM),gP=i(gO),as=i(gQ),at=i(gR),a6=i(gS),b9=i(gT),b_=i(gU),ai=i(gV),W=i(gW),au=i(gX),av=i(gY),aw=i(gZ),b$=i(g0),ca=i(g1),ax=i(g2),cb=i(g3),ay=i(g4),cc=i(g5),cd=i(g6),ce=i(g7),cf=i(g8),cg=i(g9),g$=i(g_),hb=i(ha),hd=i(hc),hf=i(he),ch=i(hg),hi=i(hh),hk=i(hj),hm=i(hl),ho=i(hn),ci=i(hp),cj=i(hq),ck=i(hr),cl=i(hs),cm=i(ht),cn=i(hu),co=i(hv),cp=i(hw),cq=i(hx),cr=i(hy),cs=i(hz),ct=i(hA),cu=i(hB),cv=i(hC),cw=i(hD),cx=i(hE),aj=i(hF),az=i(hG),aA=i(hH),X=i(hI),cy=i(hJ),aB=i(hK),cz=i(hL);function
aQ(m,b){var
i=a(hM[2],0),p=a(hN[17],i),j=f(b),q=g===j?b[1]:d===j?a(e[2],b):b,k=c(h[3],p,q);if(10===k[0]){var
l=k[1][1];if(c(cZ[2],i,[1,l]))return[1,l]}var
r=c(n[17],m,hO),s=a(ac[3],r);return o(_[2],0,hP,s)}var
hR=[d,function(a){return aQ(hQ,ah)}],hT=[d,function(a){return aQ(hS,a5)}],hV=[d,function(a){return aQ(hU,ag)}],ds=[d,function(a){return aQ(hW,a6)}],aG=[d,function(a){return aQ(hX,ai)}],c0=[d,function(a){return aQ(hY,aj)}];function
ad(i,c){var
b=f(y),j=[0,i,c],k=g===b?y[1]:d===b?a(e[2],y):y;return a(h[23],[0,k,j])}function
c1(i,c){var
b=f(K),j=[0,i,c],k=g===b?K[1]:d===b?a(e[2],K):K;return a(h[23],[0,k,j])}function
hZ(i,c){var
b=f(ag),j=[0,i,c],k=g===b?ag[1]:d===b?a(e[2],ag):ag;return a(h[23],[0,k,j])}function
dt(j,i,c){var
b=f(X),k=[0,j,i,c],l=g===b?X[1]:d===b?a(e[2],X):X;return a(h[23],[0,l,k])}function
cA(h,c){var
b=f(x),i=g===b?x[1]:d===b?a(e[2],x):x;return dt(i,h,c)}function
aH(i,c){var
b=f(ai),j=[0,i,c],k=g===b?ai[1]:d===b?a(e[2],ai):ai;return a(h[23],[0,k,j])}function
aI(c){var
b=f(L),i=[0,c],j=g===b?L[1]:d===b?a(e[2],L):L;return a(h[23],[0,j,i])}function
cB(i,c){var
b=f(az),j=[0,i,c],k=g===b?az[1]:d===b?a(e[2],az):az;return a(h[23],[0,k,j])}function
c2(i,c){var
b=f(aA),j=[0,i,c],k=g===b?aA[1]:d===b?a(e[2],aA):aA;return a(h[23],[0,k,j])}function
aC(c){var
b=f(aj),i=[0,c],j=g===b?aj[1]:d===b?a(e[2],aj):aj;return a(h[23],[0,j,i])}function
aJ(c){var
b=f(ap),i=[0,c],j=g===b?ap[1]:d===b?a(e[2],ap):ap;return a(h[23],[0,j,i])}function
G(b){function
i(b){if(c(l[1],b,l[12])){var
j=f(al);return g===j?al[1]:d===j?a(e[2],al):al}var
o=[0,i(c(l[9],b,l[13]))],p=l[11],q=c(l[10],b,l[13]);if(c(l[1],q,p))var
k=f(am),r=g===k?am[1]:d===k?a(e[2],am):am,m=r;else
var
n=f(an),s=g===n?an[1]:d===n?a(e[2],an):an,m=s;return a(h[23],[0,m,o])}if(c(l[1],b,l[11])){var
j=f(Q);return g===j?Q[1]:d===j?a(e[2],Q):Q}var
o=[0,i(a(l[15],b))];if(c(l[4],b,l[11]))var
k=f(R),p=g===k?R[1]:d===k?a(e[2],R):R,m=p;else
var
n=f(S),q=g===n?S[1]:d===n?a(e[2],S):S,m=q;return a(h[23],[0,m,o])}function
aR(l,H){function
j(b,a){return o(h[dc],l,b,a)}var
m=c(h[92],l,H),b=m[2],i=m[1],k=c(h[3],l,i);if(b){var
n=b[2];if(n){var
p=n[2];if(p){if(!p[2]){var
q=f(X),M=g===q?X[1]:d===q?a(e[2],X):X;if(j(M,i))return[1,16,b]}}else{var
r=f(as),N=g===r?as[1]:d===r?a(e[2],as):as;if(j(i,N))return[1,17,b];var
s=f(at),O=g===s?at[1]:d===s?a(e[2],at):at;if(j(i,O))return[1,18,b];var
t=f(a6),P=g===t?a6[1]:d===t?a(e[2],a6):a6;if(j(i,P))return[1,19,b];var
u=f(b9),Q=g===u?b9[1]:d===u?a(e[2],b9):b9;if(j(i,Q))return[1,20,b];var
v=f(b_),R=g===v?b_[1]:d===v?a(e[2],b_):b_;if(j(i,R))return[1,21,b];var
w=f(ai),S=g===w?ai[1]:d===w?a(e[2],ai):ai;if(j(i,S))return[1,22,b];var
x=f(az),T=g===x?az[1]:d===x?a(e[2],az):az;if(j(i,T))return[1,25,b];var
y=f(aA),U=g===y?aA[1]:d===y?a(e[2],aA):aA;if(j(i,U))return[1,26,b];var
z=f(cx),V=g===z?cx[1]:d===z?a(e[2],cx):cx;if(j(i,V))return[1,30,b];var
A=f(aw),W=g===A?aw[1]:d===A?a(e[2],aw):aw;if(j(i,W))return[1,31,b];var
B=f(b$),Y=g===B?b$[1]:d===B?a(e[2],b$):b$;if(j(i,Y))return[1,32,b];var
C=f(ca),Z=g===C?ca[1]:d===C?a(e[2],ca):ca;if(j(i,Z))return[1,33,b];var
D=f(ax),$=g===D?ax[1]:d===D?a(e[2],ax):ax;if(j(i,$))return[1,34,b]}}else{var
E=f(aj),aa=g===E?aj[1]:d===E?a(e[2],aj):aj;if(j(i,aa))return[1,29,b]}}else{var
F=f(aB),ab=g===F?aB[1]:d===F?a(e[2],aB):aB;if(j(i,ab))return[1,27,b];var
G=f(cz),ad=g===G?cz[1]:d===G?a(e[2],cz):cz;if(j(i,ad))return[1,28,b]}switch(k[0]){case
1:if(!b)return[0,k[1]];break;case
6:if(k[1][1]){if(!b){var
I=a(ac[3],h0);return o(_[5],0,0,I)}}else
if(!b)return[2,k[2],k[3]];break;case
10:var
J=a(c3[38],[1,k[1][1]]);return[1,[0,a(c4[20],J)],b];case
11:var
K=a(c3[38],[2,k[1][1]]);return[1,[0,a(c4[20],K)],b];case
12:var
L=a(c3[38],[3,k[1][1]]);return[1,[0,a(c4[20],L)],b]}return 0}var
h1=cZ[9];function
a7(k,b,q){var
l=o(c5[81],0,k,b),r=o(h1,k,b,q),m=c(h[92],b,r),i=m[2],j=m[1];c(h[3],b,j);if(!i){var
n=f(x),s=g===n?x[1]:d===n?a(e[2],x):x;if(c(l,j,s))return[1,23,i];var
p=f(W),t=g===p?W[1]:d===p?a(e[2],W):W;if(c(l,j,t))return[1,24,i]}return 0}function
cC(k,G){function
j(b,a){return o(h[dc],k,b,a)}var
l=c(h[92],k,G),b=l[2],i=l[1],m=c(h[3],k,i);if(b){var
n=b[2];if(n){if(!n[2]){var
p=f(y),H=g===p?y[1]:d===p?a(e[2],y):y;if(j(i,H))return[1,0,b];var
q=f(K),I=g===q?K[1]:d===q?a(e[2],K):K;if(j(i,I))return[1,1,b];var
r=f(ag),J=g===r?ag[1]:d===r?a(e[2],ag):ag;if(j(i,J))return[1,2,b];var
s=f(cb),M=g===s?cb[1]:d===s?a(e[2],cb):cb;if(j(i,M))return[1,6,b];var
t=f(cc),N=g===t?cc[1]:d===t?a(e[2],cc):cc;if(j(i,N))return[1,7,b];var
u=f(ay),O=g===u?ay[1]:d===u?a(e[2],ay):ay;if(j(i,O))return[1,8,b]}}else{var
v=f(ah),P=g===v?ah[1]:d===v?a(e[2],ah):ah;if(j(i,P))return[1,3,b];var
w=f(a5),T=g===w?a5[1]:d===w?a(e[2],a5):a5;if(j(i,T))return[1,5,b];var
x=f(L),U=g===x?L[1]:d===x?a(e[2],L):L;if(j(i,U))return[1,4,b];var
z=f(cd),V=g===z?cd[1]:d===z?a(e[2],cd):cd;if(j(i,V))return[1,9,b];var
A=f(av),W=g===A?av[1]:d===A?a(e[2],av):av;if(j(i,W))return[1,10,b];var
B=f(R),X=g===B?R[1]:d===B?a(e[2],R):R;if(j(i,X))return[1,13,b];var
C=f(S),Y=g===C?S[1]:d===C?a(e[2],S):S;if(j(i,Y))return[1,12,b];var
D=f(ap),Z=g===D?ap[1]:d===D?a(e[2],ap):ap;if(j(i,Z))return[1,15,b]}}else{var
E=f(au),_=g===E?au[1]:d===E?a(e[2],au):au;if(j(i,_))return[1,11,b];var
F=f(Q),$=g===F?Q[1]:d===F?a(e[2],Q):Q;if(j(i,$))return[1,14,b];if(1===m[0])return[0,m[1]]}return 0}function
h2(j,u){function
b(b,a){return o(h[dc],j,b,a)}function
i(t){var
o=c(h[92],j,t),k=o[2],m=o[1];if(k){if(!k[2]){var
p=k[1],q=f(an),u=g===q?an[1]:d===q?a(e[2],an):an;if(b(m,u)){var
v=i(p),w=c(l[8],l[13],v);return c(l[6],l[12],w)}var
r=f(am),x=g===r?am[1]:d===r?a(e[2],am):am;if(b(m,x)){var
y=i(p);return c(l[8],l[13],y)}}}else{var
s=f(al),z=g===s?al[1]:d===s?a(e[2],al):al;if(b(m,z))return l[12]}return a(n[3],h3)}var
p=c(h[92],j,u),k=p[2],m=p[1];if(k){if(!k[2]){var
q=k[1],r=f(R),v=g===r?R[1]:d===r?a(e[2],R):R;if(b(m,v))return i(q);var
s=f(S),w=g===s?S[1]:d===s?a(e[2],S):S;if(b(m,w)){var
x=i(q);return a(l[17],x)}}}else{var
t=f(Q),y=g===t?Q[1]:d===t?a(e[2],Q):Q;if(b(m,y))return l[11]}return a(n[3],h4)}function
c6(y,x,e,b){function
d(f,e,p){var
b=c(h[3],y,p);if(5===b[0]){var
T=b[3],U=b[2],V=[0,d(f,e,b[1]),U,T];return a(h[19],V)}if(e){var
q=e[1];if(q){var
r=q[1],z=e[2];switch(b[0]){case
9:var
D=b[1],j=a(m[24][8],b[2]),s=r-1|0,t=r-1|0,E=d(f,z,aV(j,s)[1+s]);aV(j,t)[1+t]=E;return a(h[23],[0,D,j]);case
14:var
i=0;break;default:var
i=1}}else{var
o=e[2];switch(b[0]){case
6:var
J=b[3],K=b[1],L=[0,K,d(f,o,b[2]),J];return a(h[20],L);case
7:var
M=b[3],N=b[1],O=[0,N,d(f,o,b[2]),M];return a(h[21],O);case
8:var
P=b[4],Q=b[2],R=b[1],S=[0,R,Q,d(f,o,b[3]),P];return a(h[22],S);case
14:var
i=0;break;default:var
i=1}}if(i){var
A=a(m[22][1],e),B=a(n[22],A),C=c(n[17],h5,B);return a(n[3],C)}var
u=b[1],k=u[2],l=k[3],v=u[1],g=v[2],F=k[2],G=k[1],w=a(m[24][8],l),H=aV(l,g)[1+g],I=d(c(m[4],f,l.length-1),e,H);aV(w,g)[1+g]=I;return a(h[33],[0,v,[0,G,F,w]])}return c(x,f,p)}return d(1,e,b)}function
du(g,f,e,d){var
b=[0,a(h[10],0)],i=c6(g,function(d,c){b[1]=c;return a(h[10],d)},e,d),j=a(m[3],b),k=[0,a(D[1][6],h7)],l=[0,c(z[4],k,0),f,i];return[0,a(h[21],l),j]}function
$(g){function
b(b){var
c=a(F[32][5],b),d=a(m[22][9],g);function
e(c,a){return cY(b,a)}var
f=c6(a(F[32][3],b),e,d,c);return o(j[3],0,f,2)}return a(r[67][7],b)}function
aK(b){switch(b[0]){case
0:var
c=b[2],d=b[1];a(n[31],h8);aK(d);a(n[31],h9);aK(c);return a(n[31],h_);case
1:var
e=b[2],f=b[1];a(n[31],h$);aK(f);a(n[31],ia);aK(e);return a(n[31],ib);case
2:var
g=a(D[1][8],b[1]);return a(n[31],g);case
3:var
h=a(l[16],b[1]);return a(n[31],h);default:return a(n[31],ic)}}function
Y(c){var
b=c;for(;;)switch(b[0]){case
0:return a(n[3],id);case
1:var
b=b[1];continue;case
2:return cX(b[1]);case
3:return-1;default:return-1}}function
B(b){switch(b[0]){case
0:var
j=b[1],k=B(b[2]),l=[0,B(j),k],c=f(y),m=g===c?y[1]:d===c?a(e[2],y):y;return a(h[23],[0,m,l]);case
1:var
n=b[1],o=B(b[2]),p=[0,B(n),o],i=f(K),q=g===i?K[1]:d===i?a(e[2],K):K;return a(h[23],[0,q,p]);case
2:return a(h[11],b[1]);case
3:return G(b[1]);default:return b[1]}}function
N(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1],g=c(a[2]);return[0,[1,[2,dp(e)],[3,f]],g]}return[3,b[4]]}return c(b[3])}function
dv(b,a,c){var
d=o(c5[19],b,a,c);return le(ig[4],0,0,0,0,0,0,0,0,b,a,d)}function
dw(i,p,b,n){function
j(j){var
q=a(F[32][5],j),r=a(F[32][4],j),s=a(m[22][9],p),k=du(a(F[32][3],j),i,s,q),l=k[1],t=k[2];function
u(m){var
q=a(h[10],1),s=[0,i,b,a(h[10],2),q,t,n],j=f(ci),p=[0,l,0],u=g===j?ci[1]:d===j?a(e[2],ci):ci,v=a(h[23],[0,u,s]),w=[0,a(h[10],1),[0,b,0]],x=a(h[41],w),y=[0,a(D[1][6],ih)],A=[0,c(z[4],y,0),x,v],B=a(h[21],A),C=o(h[35],i,0,h[16]),E=[0,a(D[1][6],ii)],F=[0,c(z[4],E,0),C,B],G=[0,a(h[21],F),p],H=a(h[41],G),k=dv(r,m,a(h[23],[0,l,[0,b]])),I=k[1];return[0,I,a(h[41],[0,H,[0,k[2],0]])]}return c(dx[1],0,u)}return a(r[67][7],j)}function
c7(i,h,c){var
b=f(x),j=g===b?x[1]:d===b?a(e[2],x):x;return dw(j,i,h,c)}function
a8(d,c,b){return c7(d,c,a(h[41],[0,b[1],b[2]]))}function
ij(k,j,c){var
l=a(h[41],[0,c[1],c[2]]),b=f(W),i=g===b?W[1]:d===b?a(e[2],W):W;return dw(i,k,j,l)}function
dy(d,b){function
e(e){var
i=a(F[32][4],d),j=c(F[32][7],d,b),f=c(h[3],e,j);if(6===f[0]){var
g=dv(i,e,f[2]),k=g[1];return[0,k,a(h[41],[0,b,[0,g[2],0]])]}throw[0,bb,ik]}return c(dx[1],0,e)}function
s(o,l,k){function
b(i){var
p=a(F[32][5],i),q=a(m[22][9],o),b=f(x),r=g===b?x[1]:d===b?a(e[2],x):x,j=du(a(F[32][3],i),r,q,p),u=j[2],s=j[1];function
t(v){var
b=v,e=u,w=a(F[32][3],i);for(;;){var
d=c(h[3],w,e);if(5===d[0]){var
e=d[1];continue}if(b){var
j=b[1];if(j){var
p=b[2],q=j[1];switch(d[0]){case
9:var
k=q-1|0,b=p,e=aV(d[2],k)[1+k];continue;case
14:var
f=0;break;default:var
f=1}}else{var
g=b[2];switch(d[0]){case
6:var
b=g,e=d[2];continue;case
7:var
b=g,e=d[2];continue;case
8:var
b=g,e=d[3];continue;case
14:var
f=0;break;default:var
f=1}}if(f){var
r=a(m[22][1],b),s=a(n[22],r),t=c(n[17],h6,s);return a(n[3],t)}var
l=d[1],o=l[1][2],e=aV(l[2][3],o)[1+o];continue}return e}}var
v=c(m[22][68],t,l),w=[0,k,c(m[23],v,[0,s,0])];return dy(i,a(h[41],w))}return a(r[67][7],b)}function
a9(e,f){function
b(b){var
d=a(F[32][3],b);function
k(l,j){if(0===l)return cY(b,j);var
e=c(h[3],d,j);if(9===e[0]){var
f=e[2];if(2===f.length-1){var
m=e[1],n=f[2],g=c(h[3],d,f[1]);if(9===g[0]){var
i=g[2];if(2===i.length-1){var
o=g[1],p=i[1],q=cY(b,i[2]),r=k(l-1|0,n),s=[0,m,[0,a(h[23],[0,o,[0,p,q]]),r]];return a(h[23],s)}}throw[0,bb,im]}}throw[0,bb,il]}var
g=a(m[22][1],e),i=a(m[22][1],f),l=c(m[5],i,g),n=a(F[32][5],b),p=a(m[22][9],e),q=c6(d,function(b,a){return k(l,a)},p,n);return o(j[3],0,q,2)}return a(r[67][7],b)}function
a_(i,m){var
b=m[2],h=m[1];switch(h[0]){case
0:var
n=h[2],j=h[1];if(0===b[0]){var
o=b[1],D=b[2],E=Y(o),F=Y(j);if(c(l[19],F,E)){var
p=a_([0,io,i],[0,n,b]),G=p[1],H=[0,j,p[2]],q=f(A),I=g===q?A[1]:d===q?a(e[2],A):A;return[0,[0,s(i,ip,I),G],H]}var
r=a_([0,iq,i],[0,h,D]),J=r[1],K=[0,o,r[2]],t=f(aq),L=g===t?aq[1]:d===t?a(e[2],aq):aq;return[0,[0,s(i,ir,L),J],K]}var
M=Y(b),N=Y(j);if(c(l[19],N,M)){var
u=a_([0,is,i],[0,n,b]),O=u[1],P=[0,j,u[2]],v=f(A),Q=g===v?A[1]:d===v?a(e[2],A):A;return[0,[0,s(i,it,Q),O],P]}var
w=f(ar),R=[0,b,h],S=0,T=g===w?ar[1]:d===w?a(e[2],ar):ar;return[0,[0,s(i,iu,T),S],R];case
3:var
af=h[1];switch(b[0]){case
0:var
k=0;break;case
3:var
ag=[3,c(C[12],af,b[1])];return[0,[0,$(i),0],ag];default:var
k=1}break;default:var
k=0}if(!k)if(0===b[0]){var
x=b[1],U=b[2],V=Y(h),W=Y(x);if(c(l[19],W,V)){var
y=a_([0,iv,i],[0,h,U]),X=y[1],Z=[0,x,y[2]],z=f(aq),_=g===z?aq[1]:d===z?a(e[2],aq):aq;return[0,[0,s(i,iw,_),X],Z]}return[0,0,[0,h,b]]}var
aa=Y(b),ab=Y(h);if(c(l[18],ab,aa)){var
B=f(ar),ac=[0,b,h],ad=0,ae=g===B?ar[1]:d===B?a(e[2],ar):ar;return[0,[0,s(i,ix,ae),ad],ac]}return[0,0,[0,h,b]]}function
iy(o,E,i,D,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],r=j[1],t=r[2],G=r[1];if(q===t){var
u=f(bH),H=g===u?bH[1]:d===u?a(e[2],bH):bH,v=s(b,iz,H),I=l[11],J=c(C[14],D,G),K=c(C[14],E,F),L=c(C[12],K,J);if(c(l[1],L,I)){var
w=f(U),N=g===w?U[1]:d===w?a(e[2],U):U,O=s(b,iA,N),P=[0,O,h(b,[0,k,n])];return[0,v,[0,$([0,iC,[0,iB,b]]),P]]}return[0,v,h([0,iD,b],[0,k,n])]}if(c(l[19],q,t)){var
Q=h([0,iE,b],[0,k,j]),x=f(T),R=g===x?T[1]:d===x?a(e[2],T):T;return[0,s(b,iF,R),Q]}var
S=h([0,iG,b],[0,i,n]),y=f(M),V=g===y?M[1]:d===y?a(e[2],M):M;return[0,s(b,iH,V),S]}var
W=h([0,iI,b],[0,k,0]),z=f(T),X=g===z?T[1]:d===z?a(e[2],T):T;return[0,s(b,iJ,X),W]}var
A=m[2];if(A){var
Y=h([0,iK,b],[0,0,A[2]]),B=f(M),Z=g===B?M[1]:d===B?a(e[2],M):M;return[0,s(b,iL,Z),Y]}return[0,a9(o,b),0]}return h(o,[0,i,b])}function
c8(o,i,E,b){function
h(b,m){var
i=m[1];if(i){var
j=m[2],k=i[2],p=i[1],q=p[2],F=p[1];if(j){var
n=j[2],r=j[1],t=r[2],G=r[1];if(q===t){var
u=f(bK),H=g===u?bK[1]:d===u?a(e[2],bK):bK,v=s(b,iM,H),I=l[11],J=c(C[14],E,G),K=c(C[12],F,J);if(c(l[1],K,I)){var
w=f(U),L=g===w?U[1]:d===w?a(e[2],U):U,N=s(b,iN,L),O=[0,N,h(b,[0,k,n])];return[0,v,[0,$([0,iP,[0,iO,b]]),O]]}return[0,v,h([0,iQ,b],[0,k,n])]}if(c(l[19],q,t)){var
P=h([0,iR,b],[0,k,j]),x=f(A),Q=g===x?A[1]:d===x?a(e[2],A):A;return[0,s(b,iS,Q),P]}var
R=h([0,iT,b],[0,i,n]),y=f(M),S=g===y?M[1]:d===y?a(e[2],M):M;return[0,s(b,iU,S),R]}var
T=h([0,iV,b],[0,k,0]),z=f(A),V=g===z?A[1]:d===z?a(e[2],A):A;return[0,s(b,iW,V),T]}var
B=m[2];if(B){var
W=h([0,iX,b],[0,0,B[2]]),D=f(M),X=g===D?M[1]:d===D?a(e[2],M):M;return[0,s(b,iY,X),W]}return[0,a9(o,b),0]}return h(o,[0,i,b])}function
dz(h,b){if(b){var
m=b[2];if(c(l[4],b[1][1],l[11]))var
i=f(bI),n=g===i?bI[1]:d===i?a(e[2],bI):bI,j=n;else
var
k=f(bJ),p=g===k?bJ[1]:d===k?a(e[2],bJ):bJ,j=p;var
o=s(h,iZ,j);return[0,o,dz(h,m)]}return[0,$(h),0]}function
cD(j,i,b){switch(b[0]){case
0:var
u=b[2],k=cD([0,i0,j],i,b[1]),v=k[2],w=k[1],n=cD([0,i1,j],i,u),x=[0,v,n[2]],y=c(m[23],w,n[1]),p=f(bW),z=g===p?bW[1]:d===p?a(e[2],bW):bW;return[0,[0,s(j,i2,z),y],x];case
1:var
q=b[2],A=b[1];if(3===q[0]){var
C=[1,A,[3,c(l[8],i,q[1])]],D=[0,$([0,i4,j]),0],r=f(bv),E=g===r?bv[1]:d===r?a(e[2],bv):bv;return[0,[0,s(j,i5,E),D],C]}var
B=a(ac[3],i3);return o(_[5],0,0,B);case
2:return[0,0,[1,b,[3,i]]];case
3:var
F=[3,c(l[8],i,b[1])];return[0,[0,$(j),0],F];default:var
H=b[1],I=[0,G(i),H],t=f(K),J=g===t?K[1]:d===t?a(e[2],K):K;return[0,0,[4,a(h[23],[0,J,I])]]}}function
c9(c){function
h(b,i){if(i){var
k=h([0,i6,b],i[2]),j=f(bL),l=g===j?bL[1]:d===j?a(e[2],bL):bL;return[0,s(b,i7,l),k]}return[0,a9(c,b),0]}return function(a){return h(c,a)}}function
i8(c){function
h(b,i){if(i){var
k=h([0,i9,b],i[2]),j=f(A),l=g===j?A[1]:d===j?a(e[2],A):A;return[0,s(b,i_,l),k]}return[0,a9(c,b),0]}return function(a){return h(c,a)}}function
c_(c){function
h(b,i){if(i){var
k=h([0,i$,b],i[2]),j=f(T),l=g===j?T[1]:d===j?a(e[2],T):T;return[0,s(b,ja,l),k]}return[0,a9(c,b),0]}return function(a){return h(c,a)}}function
c$(i,b){switch(b[0]){case
0:var
u=b[2],j=c$([0,jb,i],b[1]),v=j[2],w=j[1],k=c$([0,jc,i],u),x=[0,v,k[2]],y=c(m[23],w,k[1]),n=f(bX),z=g===n?bX[1]:d===n?a(e[2],bX):bX;return[0,[0,s(i,jd,z),y],x];case
1:var
p=b[2],A=b[1];if(3===p[0]){var
C=[1,A,[3,a(l[17],p[1])]],D=[0,$([0,jf,i]),0],q=f(bY),E=g===q?bY[1]:d===q?a(e[2],bY):bY;return[0,[0,s(i,jg,E),D],C]}var
B=a(ac[3],je);return o(_[5],0,0,B);case
2:var
F=[1,b,[3,l[14]]],r=f(V),G=0,H=g===r?V[1]:d===r?a(e[2],V):V;return[0,[0,s(i,jh,H),G],F];case
3:var
I=[3,a(l[17],b[1])];return[0,[0,$(i),0],I];default:var
J=[0,b[1]],t=f(L),K=g===t?L[1]:d===t?a(e[2],L):L;return[0,0,[4,a(h[23],[0,K,J])]]}}function
ak(k,j,r){function
u(p,e){try{try{var
g=a(m[3],a4),i=a(h[105],k),l=o(m[22][115],i,e,g),d=l}catch(b){b=t(b);if(b!==I)throw b;var
d=a(n[3],fi)}var
b=d[1],r=a(h[11],d[2]),s=[0,[0,c7(j,a(h[11],b),r),0],[2,b]];return s}catch(b){b=t(b);if(a(_[13],b)){var
c=e_(0),f=af(0);fj(e,c,f,p);var
q=a(h[11],f);return[0,[0,c7(j,a(h[11],c),q),0],[2,c]]}throw b}}try{var
p=cC(k,r);if(typeof
p==="number")var
i=0;else
switch(p[0]){case
0:var
v=[0,0,[2,p[1]]],i=1;break;case
1:var
w=p[1];if(typeof
w==="number")if(16<=w)var
i=0;else{switch(w){case
0:var
x=p[2];if(x){var
z=x[2];if(z)if(z[2])var
i=0,b=0;else
var
ai=z[1],P=ak(k,[0,ji,j],x[1]),aj=P[2],al=P[1],Q=ak(k,[0,jj,j],ai),am=Q[1],R=a_(j,[0,aj,Q[2]]),an=R[2],ao=c(m[23],am,R[1]),q=[0,c(m[23],al,ao),an],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
A=p[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else{var
ap=B[1],S=ak(k,[0,jk,j],A[1]),C=S[2],T=S[1],U=ak(k,[0,jl,j],ap),D=U[2],V=U[1];if(3===D[0])var
Y=cD(j,D[1],C),av=Y[2],aw=c(m[23],V,Y[1]),E=[0,c(m[23],T,aw),av];else
if(3===C[0])var
aq=C[1],W=f(bw),ar=g===W?bw[1]:d===W?a(e[2],bw):bw,as=s(j,jm,ar),X=cD(j,aq,D),at=X[2],au=c(m[23],V,[0,as,X[1]]),E=[0,c(m[23],T,au),at];else
var
E=u(0,r);var
q=E,b=1}else
var
i=0,b=0}else
var
i=0,b=0;break;case
2:var
F=p[2];if(F){var
H=F[2];if(H)if(H[2])var
i=0,b=0;else
var
ax=F[1],ay=[0,H[1]],Z=f(L),az=g===Z?L[1]:d===Z?a(e[2],L):L,aA=[0,ax,a(h[23],[0,az,ay])],$=f(y),aB=g===$?y[1]:d===$?a(e[2],y):y,aa=ak(k,j,a(h[23],[0,aB,aA])),aC=aa[2],aE=aa[1],q=[0,[0,O(hV),aE],aC],b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
3:var
J=p[2];if(J)if(J[2])var
i=0,b=0;else
var
aF=J[1],aG=[0,aF,G(l[12])],ab=f(y),aH=g===ab?y[1]:d===ab?a(e[2],y):y,ac=ak(k,j,a(h[23],[0,aH,aG])),aI=ac[2],aJ=ac[1],q=[0,[0,O(hR),aJ],aI],b=1;else
var
i=0,b=0;break;case
4:var
K=p[2];if(K)if(K[2])var
i=0,b=0;else
var
ad=ak(k,[0,jn,j],K[1]),aK=ad[1],ae=c$(j,ad[2]),aL=ae[2],q=[0,c(m[23],aK,ae[1]),aL],b=1;else
var
i=0,b=0;break;case
5:var
M=p[2];if(M)if(M[2])var
i=0,b=0;else
var
aM=M[1],aN=[0,aM,G(l[14])],ag=f(y),aO=g===ag?y[1]:d===ag?a(e[2],y):y,ah=ak(k,j,a(h[23],[0,aO,aN])),aP=ah[2],aQ=ah[1],q=[0,[0,O(hT),aQ],aP],b=1;else
var
i=0,b=0;break;case
15:var
N=p[2];if(N)if(N[2])var
i=0,b=0;else
var
q=u(1,N[1]),b=1;else
var
i=0,b=0;break;case
12:case
13:case
14:try{var
aR=[0,0,[3,h2(k,r)]],q=aR,b=1}catch(c){c=t(c);if(!a(_[13],c))throw c;var
q=u(0,r),b=1}break;default:var
i=0,b=0}if(b)var
v=q,i=1}else
var
i=0;break;default:var
i=0}if(!i)var
v=u(0,r);return v}catch(b){b=t(b);if(a(aD[4],b))return u(0,r);throw b}}function
dA(h,c,b){switch(c[0]){case
1:var
i=c[1],t=c[2];switch(b[0]){case
1:if(2===i[0]){var
u=[1,[2,i[1]],[0,c[2],b[2]]],j=f(bU),v=g===j?bU[1]:d===j?a(e[2],bU):bU;return[0,s(h,jp,v),u]}break;case
2:var
w=[1,[2,b[1]],[0,t,[3,l[12]]]],k=f(bT),x=g===k?bT[1]:d===k?a(e[2],bT):bT;return[0,s(h,jq,x),w]}break;case
2:var
m=c[1];switch(b[0]){case
1:var
y=[1,[2,m],[0,b[2],[3,l[12]]]],p=f(bS),z=g===p?bS[1]:d===p?a(e[2],bS):bS;return[0,s(h,jr,z),y];case
2:var
A=[1,[2,m],[3,l[13]]],q=f(bR),B=g===q?bR[1]:d===q?a(e[2],bR):bR;return[0,s(h,js,B),A]}break}aK(c);a(n[36],0);aK(b);a(n[36],0);a(n[52],n[28]);var
r=a(ac[3],jo);return o(_[5],0,0,r)}function
cE(i,b){switch(b[0]){case
1:var
j=b[1];if(2===j[0]){var
k=b[2],p=j[1];if(3===k[0])return[0,0,b];var
h=function(b){switch(b[0]){case
0:var
d=b[1],e=h(b[2]),f=h(d);return c(C[12],f,e);case
3:return b[1];default:var
g=a(ac[3],ju);return o(_[5],0,0,g)}},q=[1,[2,p],[3,h(k)]];return[0,[0,$([0,jv,i]),0],q]}break;case
2:var
r=[1,[2,b[1]],[3,l[12]]],m=f(bQ),t=0,u=g===m?bQ[1]:d===m?a(e[2],bQ):bQ;return[0,[0,s(i,jw,u),t],r]}aK(b);var
n=a(ac[3],jt);return o(_[5],0,0,n)}function
a$(b,j){switch(j[0]){case
0:var
h=j[2],i=j[1];switch(h[0]){case
0:var
k=h[1],A=h[2],B=Y(k);if(Y(i)===B){var
n=dA([0,jx,b],i,k),C=n[2],D=n[1],o=f(bu),E=g===o?bu[1]:d===o?a(e[2],bu):bu,F=s(b,jy,E),p=a$(b,[0,C,A]);return[0,[0,F,[0,D,p[1]]],p[2]]}var
q=cE([0,jz,b],i),G=q[2],H=q[1],r=a$([0,jA,b],h),I=[0,G,r[2]];return[0,c(m[23],H,r[1]),I];case
3:var
O=h[1],x=cE([0,jD,b],i);return[0,x[1],[0,x[2],[3,O]]];default:var
J=Y(h);if(Y(i)===J){var
t=dA(b,i,h),K=t[1],u=a$(b,t[2]);return[0,[0,K,u[1]],u[2]]}var
v=cE([0,jB,b],i),L=v[2],M=v[1],w=a$([0,jC,b],h),N=[0,L,w[2]];return[0,c(m[23],M,w[1]),N]}case
3:return[0,0,j];default:var
y=cE(b,j),P=y[1],Q=[0,y[2],[3,l[11]]],z=f(bV),R=g===z?bV[1]:d===z?a(e[2],bV):bV,S=[0,s(b,jE,R),0];return[0,c(m[23],P,S),Q]}}function
da(i,b){if(0===b[0]){var
h=b[1];if(1===h[0])if(2===h[1][0]){var
k=h[2];if(3===k[0]){var
o=b[2];if(c(l[1],k[1],l[11])){var
m=f(U),p=g===m?U[1]:d===m?a(e[2],U):U,q=s(i,jG,p),n=da(i,o);return[0,[0,q,n[1]],n[2]]}}}var
j=da([0,jF,i],b[2]);return[0,j[1],[0,h,j[2]]]}return[0,0,b]}function
dB(bp){var
i=a(D[1][6],jH),m=a(D[1][6],jI),n=a(D[1][6],jJ),y=G(l[11]);function
o(bq){var
p=bq;for(;;){if(p){var
b=p[1];switch(b[0]){case
0:var
al=b[2],am=b[1],br=p[2],bs=b[4],bt=b[3],q=J(am[1]),an=B(N(am)),ap=B(N(al)),F=G(bt),U=G(bs),aq=ad(c1(ap,F),U),bu=cA(an,aq),bv=al[3],bw=a(c_(jK),bv),bH=j[H],bI=P(bw),bJ=[0,c(k[57][3],bI,bH),0],bK=[0,j[62],[0,j[H],0]],bL=[0,O(aG),bK],bQ=[0,a(k[57][22],bL),0],bR=[0,j[62],[0,j[H],0]],bS=[0,O(aG),bR],bT=[0,a(k[57][22],bS),0],bU=[0,o(br),0],bV=[0,a(j[25],[0,q,0]),bU],bW=[0,a(j[76],[0,m,[0,n,[0,q,0]]]),bV],bY=a(h[11],q),bZ=a(h[11],n),b0=[0,F,ap,U,a(h[11],m),bZ,bY],ar=f(bx),bX=0,b1=g===ar?bx[1]:d===ar?a(e[2],bx):bx,b2=[0,u([0,a(h[23],[0,b1,b0]),bX]),bW],b3=[0,a(j[25],[0,m,[0,n,0]]),b2],b4=[0,a(k[57][22],b3),bT],b5=aH(F,y),b6=a(j[Z],b5),b7=[0,c(k[57][21],b6,b4),bQ],b8=aH(F,U),b9=[0,a(j[Z],b8),0],b_=[0,a(j[25],[0,q,0]),b9],b$=[0,a(j[76],[0,i,[0,q,0]]),b_],cb=a(h[11],q),cc=[0,an,aq,a(h[11],i),cb],as=f(by),ca=0,cd=g===as?by[1]:d===as?a(e[2],by):by,ce=[0,u([0,a(h[23],[0,cd,cc]),ca]),b$],cf=[0,a(j[25],[0,i,0]),ce],cg=a(k[57][22],cf),ch=[0,c(k[57][21],cg,b7),bJ],ci=a(j[Z],bu);return c(k[57][21],ci,ch);case
1:var
K=b[2],L=b[1],at=c(l[26],L[4],K),cj=c(C[14],at,K),ck=c(C[13],L[4],cj),cl=L[3],cm=function(a){return c(l[9],a,K)},cn=c(l[43],cm,cl),au=[0,L[1],0,cn,at],co=B(N(au)),av=G(K),W=G(ck),cp=au[3],cq=a(c_(jL),cp),cr=[0,j[62],[0,j[H],0]],cs=[0,O(aG),cr],ct=[0,a(k[57][22],cs),0],cu=[0,j[62],[0,j[H],0]],cv=[0,O(aG),cu],cw=[0,a(k[57][22],cv),0],cx=[0,j[42],0],cz=[0,P(cq),cx],cB=[0,bc(i),cz],cC=[0,a(j[25],[0,i,0]),cB],cD=[0,O(c0),cC],cE=[0,a(j[76],[0,m,[0,n,0]]),cD],cG=a(h[11],n),cH=[0,W,av,co,a(h[11],m),cG],aw=f(bB),cF=0,cI=g===aw?bB[1]:d===aw?a(e[2],bB):bB,cJ=[0,u([0,a(h[23],[0,cI,cH]),cF]),cE],cK=[0,a(j[25],[0,n,[0,m,0]]),cJ],cL=[0,a(k[57][22],cK),cw],cM=aH(av,W),cN=a(j[Z],cM),cO=[0,c(k[57][21],cN,cL),ct],cP=aH(W,y),cQ=a(j[Z],cP);return c(k[57][21],cQ,cO);case
3:var
ax=p[2],ay=b[2],M=b[1],v=J(M[1]),cR=function(a){return c(l[9],a,ay)},Y=c(l[44],cR,M),_=B(N(M)),$=B(N(Y)),Q=G(ay),az=cA(_,c1($,Q));if(2===M[2]){var
cS=Y[3],cT=a(c9(jM),cS),cU=j[H],cV=P(cT),cW=[0,c(k[57][3],cV,cU),0],cX=[0,o(ax),0],cY=[0,a(j[25],[0,v,0]),cX],cZ=[0,a(j[76],[0,m,[0,v,0]]),cY],c3=a(h[11],v),c4=[0,_,$,Q,a(h[11],m),c3],aA=f(bN),c2=0,c5=g===aA?bN[1]:d===aA?a(e[2],bN):bN,c6=[0,u([0,a(h[23],[0,c5,c4]),c2]),cZ],c7=[0,a(j[25],[0,m,0]),c6],c$=[0,a(k[57][22],c7),cW],da=a(j[Z],az);return c(k[57][21],da,c$)}var
db=Y[3],dc=a(c9(jN),db),dd=j[H],de=P(dc),df=[0,c(k[57][3],de,dd),0],dg=[0,j[62],[0,j[H],0]],dh=[0,O(aG),dg],di=[0,a(k[57][22],dh),0],dj=[0,o(ax),0],dk=[0,a(j[25],[0,v,0]),dj],dl=[0,a(j[76],[0,m,[0,n,[0,v,0]]]),dk],dn=a(h[11],v),dq=a(h[11],m),dr=[0,_,$,Q,a(h[11],n),dq,dn],aB=f(bA),dm=0,dt=g===aB?bA[1]:d===aB?a(e[2],bA):bA,du=[0,u([0,a(h[23],[0,dt,dr]),dm]),dl],dv=[0,a(j[25],[0,n,[0,m,0]]),du],dw=[0,a(k[57][22],dv),di],dx=aH(Q,y),dy=a(j[Z],dx),dA=[0,c(k[57][21],dy,dw),df],dB=a(j[Z],az);return c(k[57][21],dB,dA);case
4:var
aC=p[2],aD=b[3],A=aD[2],R=aD[1],aE=b[2],w=aE[2],aa=aE[1],dD=b[1],ab=af(0);aF(ab,dD);var
aJ=J(w[1]),aK=J(A[1]),aL=B(N(w)),aN=B(N(A));if(c(l[1],aa,l[12]))if(0===A[2]){switch(w[2]){case
0:var
aO=f(bC),dE=g===aO?bC[1]:d===aO?a(e[2],bC):bC,ac=dE;break;case
1:var
aQ=f(bD),dO=g===aQ?bD[1]:d===aQ?a(e[2],bD):bD,ac=dO;break;default:var
aR=f(bP),dP=g===aR?bP[1]:d===aR?a(e[2],bP):bP,ac=dP}var
dF=G(R),dG=2===w[2]?jO:jP,dH=c8(dG,w[3],R,A[3]),dI=[0,o(aC),0],dJ=[0,a(j[25],[0,ab,0]),dI],dK=[0,P(dH),dJ],dL=a(h[11],aK),dM=[0,ac,[0,aL,aN,dF,a(h[11],aJ),dL]],dN=[0,u([0,a(h[23],dM),0]),dK];return a(k[57][22],dN)}var
aS=G(aa),aT=G(R),dQ=iy(jQ,aa,w[3],R,A[3]),dR=[0,j[62],[0,j[H],0]],dS=[0,O(aG),dR],dT=[0,a(k[57][22],dS),0],dU=[0,j[62],[0,j[H],0]],dV=[0,O(aG),dU],dW=[0,a(k[57][22],dV),0],dX=[0,o(aC),0],dY=[0,a(j[25],[0,ab,0]),dX],dZ=[0,P(dQ),dY],d0=[0,a(j[76],[0,m,[0,n,0]]),dZ],d2=a(h[11],aK),d3=a(h[11],aJ),d4=a(h[11],n),d5=[0,aL,aN,aS,aT,a(h[11],m),d4,d3,d2],aU=f(bE),d1=0,d6=g===aU?bE[1]:d===aU?a(e[2],bE):bE,d7=[0,u([0,a(h[23],[0,d6,d5]),d1]),d0],d8=[0,a(j[25],[0,n,[0,m,0]]),d7],d9=[0,a(k[57][22],d8),dW],d_=aH(aT,y),d$=a(j[Z],d_),ea=[0,c(k[57][21],d$,d9),dT],eb=aH(aS,y),ec=a(j[Z],eb);return c(k[57][21],ec,ea);case
5:var
E=b[1],aV=E[5],aW=E[4],ae=E[3],aX=E[2],ed=p[2],ee=E[1],aY=af(0),ef=J(ae[1]);aF(aY,ee[1]);var
ag=B(N(aX)),eg=B(N(ae)),ah=dp(aV),eh=cA(a(h[10],1),ag),aZ=f(x),ei=g===aZ?x[1]:d===aZ?a(e[2],x):x,ej=[0,c(z[4],[0,ah],0),ei,eh],ek=a(h[21],ej),a0=f(x),el=g===a0?x[1]:d===a0?a(e[2],x):x,a1=f(cy),em=[0,el,ek],en=g===a1?cy[1]:d===a1?a(e[2],cy):cy,eo=a(h[23],[0,en,em]),ep=G(aW),eq=c8(dC,ae[3],aW,[0,[0,l[14],aV],aX[3]]),a2=f(V),er=g===a2?V[1]:d===a2?a(e[2],V):V,es=[0,s([0,jU,[0,jT,[0,jS,dC]]],jR,er),eq],et=j[H],eu=fd(ag),ev=[0,c(k[57][3],eu,et),0],ew=[0,o(ed),0],ex=[0,a(j[25],[0,aY,0]),ew],ey=[0,a(j[76],[0,i,0]),ex],ez=[0,P(es),ey],eB=a(h[11],i),eC=a(h[11],ef),eD=[0,a(h[11],ah),eg,ag,ep,eC,eB],a3=f(bG),eA=0,eE=g===a3?bG[1]:d===a3?a(e[2],bG):bG,eF=[0,u([0,a(h[23],[0,eE,eD]),eA]),ez],eG=[0,a(j[25],[0,ah,[0,i,0]]),eF],eH=[0,a(j[76],[0,i,0]),eG],eI=[0,aP(i),eH],eJ=[0,a(j[25],[0,i,0]),eI],eK=[0,a(k[57][22],eJ),ev],eL=a(j[Z],eo);return c(k[57][21],eL,eK);case
6:var
a4=p[2],eM=b[1];try{var
eN=o(a4),eO=J(eM[1]),eP=c(D[1][13][3],eO,bp),eQ=c(k[57][3],eP,eN);return eQ}catch(a){a=t(a);if(a===I){var
p=a4;continue}throw a}case
9:var
a5=b[2],ai=b[1],eR=N(ai),eS=N(a5),eT=dz(jV,ai[3]),a6=f(ao),eU=g===a6?ao[1]:d===a6?a(e[2],ao):ao,a7=f(ao),eV=g===a7?ao[1]:d===a7?a(e[2],ao):ao,a8=f(bi),eW=g===a8?bi[1]:d===a8?a(e[2],bi):bi,a9=f(X),eX=[0,eW,eV,eU],eY=g===a9?X[1]:d===a9?a(e[2],X):X,eZ=a(h[23],[0,eY,eX]),e0=[0,j[42],[0,j[H],0]],e1=[0,a(jW[1],eZ),0],e2=[0,j[62],[0,j[16],e1]],e3=[0,O(ds),e2],e4=a(k[57][22],e3),e5=c(k[57][21],e4,e0),e6=J(a5[1]),e7=a(h[11],e6),e8=J(ai[1]),e9=a(h[11],e8),e_=B(eS),e$=[0,B(eR),e_,e9,e7],a_=f(bz),fa=g===a_?bz[1]:d===a_?a(e[2],bz):bz,fb=a(h[23],[0,fa,e$]),fc=P(eT),fe=u([0,fb,0]),ff=c(k[57][3],fe,fc);return c(r[18],ff,e5);case
10:var
aj=b[2],ak=b[1],fg=b[3],fh=N(aj),fi=N(ak),fj=J(aj[1]),fk=J(ak[1]),a$=fg?l[14]:l[12],fl=c8(jX,aj[3],a$,ak[3]),fm=[0,j[H],0],fn=[0,bc(i),fm],fo=[0,a(j[25],[0,i,0]),fn],fp=[0,P(fl),fo],fr=a(h[11],fk),fs=a(h[11],fj),ft=G(a$),fu=B(fi),fv=[0,B(fh),fu,ft,fs,fr],ba=f(bM),fq=0,fw=g===ba?bM[1]:d===ba?a(e[2],bM):bM,fx=[0,u([0,a(h[23],[0,fw,fv]),fq]),fp];return a(k[57][22],fx);case
11:var
S=b[2],fy=p[2],fz=b[3],fA=b[1],bb=af(0);aF(bb,fA);var
bd=J(S[1]),be=J(fz),bf=B(N(S)),bg=B(N(a(l[45],S))),fB=S[3],fC=a(c9(jY),fB),bh=f(V),fD=g===bh?V[1]:d===bh?a(e[2],V):V,fE=[0,s(j0,jZ,fD),fC],fF=j[H],fG=P(fE),fH=[0,c(k[57][3],fG,fF),0],fI=[0,o(fy),0],fJ=[0,a(j[25],[0,bb,0]),fI],fK=[0,a(j[76],[0,bd,[0,be,[0,i,0]]]),fJ],fM=a(h[11],i),fN=a(h[11],be),fO=[0,bf,bg,a(h[11],bd),fN,fM],bj=f(bF),fL=0,fP=g===bj?bF[1]:d===bj?a(e[2],bF):bF,fQ=[0,u([0,a(h[23],[0,fP,fO]),fL]),fK],fR=[0,a(j[25],[0,i,0]),fQ],fS=[0,a(k[57][22],fR),fH],fT=cA(bf,aI(bg)),fU=a(j[Z],fT);return c(k[57][21],fU,fS);case
12:var
fV=j1[16],fW=J(b[1]),fX=u([0,a(h[11],fW),0]);return c(k[57][3],fX,fV);case
13:var
fY=j[H],fZ=bc(J(b[1]));return c(k[57][3],fZ,fY);case
14:var
f0=b[1],f1=[0,j[H],0],f2=[0,bc(i),f1],f3=[0,a(j[25],[0,i,0]),f2],f4=[0,O(c0),f3],f5=[0,j[62],f4],f6=[0,O(ds),f5],f7=J(f0),f8=[0,u([0,a(h[11],f7),0]),f6];return a(k[57][22],f8);case
15:var
bk=b[3],bl=b[2],T=b[1],f9=bk[2],f_=bk[1],f$=bl[2],ga=bl[1],bm=af(0),bn=af(0);aF(bm,ga);aF(bn,f_);var
gb=J(T[1]),gc=T[3],gd=a(i8(j2),gc),ge=T[3],gf=a(c_(j3),ge),gg=B(N(T)),gh=[0,o(f9),0],gi=[0,a(j[25],[0,bn,0]),gh],gj=[0,P(gf),gi],gk=[0,a(k[57][22],gj),0],gl=[0,o(f$),0],gm=[0,a(j[25],[0,bm,0]),gl],gn=[0,P(gd),gm],go=[0,a(k[57][22],gn),gk],gp=[0,gg,[0,a(h[11],gb),0]],bo=f(bO),gq=g===bo?bO[1]:d===bo?a(e[2],bO):bO,gr=a(h[41],[0,gq,gp]),gs=a(j[aM],gr);return c(k[57][21],gs,go)}}return a(r[16],0)}}return o}function
aS(O,w,N,M,L,K,J,I,v){var
x=v[2],y=v[1],g=[0,[0,L],j4],q=ak(O,g,K),E=q[1],r=a$(g,q[2]),F=r[1],s=da(g,r[2]),G=s[2],H=c(m[23],F,s[1]),t=c(m[23],E,H),Q=a(j[76],[0,w,0]),R=a(k[57][24],Q),S=[0,M,[0,J,I,a(h[11],w)]],T=u([0,a(h[23],S),0]),U=c(k[57][3],T,R);if(a(m[22][48],t))return[0,y,x];var
i=af(0),e=0,b=G;for(;;){switch(b[0]){case
0:var
f=b[1];if(1===f[0]){var
l=f[1];if(2===l[0]){var
n=f[2];if(3===n[0]){var
B=b[2],C=n[1],e=[0,[0,C,cX(l[1])],e],b=B;continue}var
d=0}else
var
d=0}else
var
d=0;break;case
3:var
D=b[1],p=bf(0);aF(i,p);var
z=[0,p,N,a(m[22][9],e),D],d=1;break;default:var
d=0}if(!d)var
A=a(ac[3],ie),z=o(_[2],0,0,A);var
V=[0,a(j[25],[0,i,0]),0],W=[0,U,[0,P(t),V]];return[0,[0,[0,i,a(k[57][22],W)],y],[0,z,x]]}}function
j5(ab,k,i,H){var
n=H[1],ac=H[2],ae=a(a2[3],n);if(c(m[20][34],ae,j6))return i;try{var
j=aR(k,ac);if(typeof
j==="number")var
h=0;else
if(1===j[0]){var
r=j[1];if(typeof
r==="number")if(16<=r){switch(r+cQ|0){case
0:var
s=j[2];if(s){var
u=s[2];if(u){var
v=u[2];if(v)if(v[2])var
h=0,b=0;else{var
J=v[1],K=u[1],q=a7(ab,k,s[1]);if(typeof
q==="number")var
p=0;else
if(1===q[0]){var
N=q[1];if(typeof
N==="number")if(23===N)if(q[2])var
p=0;else
var
L=1,p=1;else
var
p=0;else
var
p=0}else
var
p=0;if(!p)var
L=0;if(L)var
af=ad(K,aI(J)),M=f(bZ),ag=2,ah=g===M?bZ[1]:d===M?a(e[2],bZ):bZ,o=aS(k,n,0,ah,ag,af,K,J,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0}else
var
h=0,b=0}else
var
h=0,b=0;break;case
2:var
w=j[2];if(w){var
x=w[2];if(x)if(x[2])var
h=0,b=0;else
var
O=x[1],P=w[1],ai=ad(P,aI(O)),Q=f(b0),aj=1,ak=g===Q?b0[1]:d===Q?a(e[2],b0):b0,o=aS(k,n,2,ak,aj,ai,P,O,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
3:var
y=j[2];if(y){var
z=y[2];if(z)if(z[2])var
h=0,b=0;else
var
R=z[1],S=y[1],al=ad(R,aI(S)),T=f(b4),am=2,an=g===T?b4[1]:d===T?a(e[2],b4):b4,o=aS(k,n,1,an,am,al,S,R,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
4:var
A=j[2];if(A){var
B=A[2];if(B)if(B[2])var
h=0,b=0;else
var
U=B[1],V=A[1],ao=aI(V),ap=ad(ad(U,G(l[14])),ao),W=f(b1),aq=2,ar=g===W?b1[1]:d===W?a(e[2],b1):b1,o=aS(k,n,1,ar,aq,ap,V,U,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
5:var
C=j[2];if(C){var
D=C[2];if(D)if(D[2])var
h=0,b=0;else
var
X=D[1],Y=C[1],as=ad(Y,aI(X)),Z=f(b2),at=2,au=g===Z?b2[1]:d===Z?a(e[2],b2):b2,o=aS(k,n,1,au,at,as,Y,X,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;case
6:var
E=j[2];if(E){var
F=E[2];if(F)if(F[2])var
h=0,b=0;else
var
_=F[1],$=E[1],av=aI(_),aw=ad(ad($,G(l[14])),av),aa=f(b3),ax=2,ay=g===aa?b3[1]:d===aa?a(e[2],b3):b3,o=aS(k,n,1,ay,ax,aw,$,_,i),b=1;else
var
h=0,b=0}else
var
h=0,b=0;break;default:var
h=0,b=0}if(b)var
I=o,h=1}else
var
h=0;else
var
h=0}else
var
h=0;if(!h)var
I=i;return I}catch(b){b=t(b);if(a(aD[4],b))return i;throw b}}function
aT(b){var
d=a(j[22],b),e=a(j[76],[0,b,0]),f=a(k[57][24],e);return c(k[57][3],f,d)}function
j7(i){dq(0);var
w=a(F[32][13],i),x=c(F[32][1],j5,i),n=o(m[22][15],x,j8,w),p=n[1],y=n[2],z=fh(0),A=[0,a(r[16],0),0];function
B(n,m){var
c=m[2],o=c[2],i=c[1],p=m[1],q=n[2],r=n[1];if(c[3]){var
b=af(0),s=bf(0);aF(b,s);var
v=l[11],w=cX(i),x=[0,[0,s,1,[0,[0,l[12],w],0],v],q],y=[0,a(j[25],[0,o,[0,b,0]]),[0,r,0]],z=[0,a(j[76],[0,b,0]),y],A=[0,aP(b),z],B=[0,a(j[25],[0,i,[0,b,0]]),A],t=f(b6),C=[0,p,0],D=g===t?b6[1]:d===t?a(e[2],b6):b6,E=a(h[41],[0,D,C]),F=[0,a(j[aM],E),B];return[0,a(k[57][22],F),x]}var
G=[0,a(j[25],[0,i,[0,o,0]]),[0,r,0]],u=f(b5),H=[0,p,0],I=g===u?b5[1]:d===u?a(e[2],b5):b5,J=a(h[41],[0,I,H]),K=[0,a(j[aM],J),G];return[0,a(k[57][22],K),q]}var
q=o(m[22][15],B,A,z),s=q[1],b=c(m[23],y,q[2]);if(a(m[3],cR))c(l[33],a3,b);if(a(m[3],be))try{o(l[64],[0,bf,dm,a3],0,b);var
E=a(r[16],0);return E}catch(b){b=t(b);if(b===l[27]){var
C=a(l[39],0),u=o(l[65],0,0,C)[2];if(a(m[3],bd))c(l[36],a3,u);var
D=a(dB(p),u);return c(k[57][3],s,D)}throw b}try{var
v=c(l[68],[0,bf,dm,a3],b);if(a(m[3],bd))c(l[36],a3,v);var
H=a(dB(p),v),I=c(k[57][3],s,H);return I}catch(b){b=t(b);if(b===l[28]){var
G=a(ac[3],j9);return c(k[57][5],0,G)}throw b}}var
j_=a(r[67][7],j7);function
j$(b){var
i=c5[81];function
p(a){return c(i,0,a)}var
aa=c(F[32][1],p,b);function
n(m,L){function
b(w){try{var
p=cC(w,L);if(typeof
p==="number")var
i=0;else
if(1===p[0]){var
x=p[1];if(typeof
x==="number")if(12<=x)var
i=0;else{switch(x){case
6:var
y=p[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else
var
A=z[1],B=y[1],ab=[0,n([0,ka,m],A),0],ac=[0,n([0,kb,m],B),ab],N=f(bj),ae=[0,B,[0,A,0]],ag=g===N?bj[1]:d===N?a(e[2],bj):bj,ai=aJ(A),aj=[0,a8(m,ad(aJ(B),ai),[0,ag,ae]),ac],u=a(k[57][22],aj),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
7:var
C=p[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
E=D[1],F=C[1],ak=[0,n([0,kc,m],E),0],al=[0,n([0,kd,m],F),ak],O=f(bk),am=[0,F,[0,E,0]],an=g===O?bk[1]:d===O?a(e[2],bk):bk,ao=aJ(E),ap=[0,a8(m,c1(aJ(F),ao),[0,an,am]),al],u=a(k[57][22],ap),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
8:var
H=p[2];if(H){var
I=H[2];if(I)if(I[2])var
i=0,b=0;else
var
q=I[1],s=H[1],v=af(0),P=f(ax),aq=0,ar=0,as=[0,q,s],at=g===P?ax[1]:d===P?a(e[2],ax):ax,az=o([0,[0,v,a(h[23],[0,at,as])],ar]),aA=[0,s,[0,q,[0,a(h[11],v),0]]],Q=f(bm),aB=g===Q?bm[1]:d===Q?a(e[2],bm):bm,aC=a8(m,G(l[11]),[0,aB,aA]),aE=[0,c(k[57][3],aC,az),aq],aF=[0,n([0,ke,m],q),0],aG=[0,n([0,kf,m],s),aF],R=f(aw),aH=0,aI=[0,q,s],aK=g===R?aw[1]:d===R?a(e[2],aw):aw,aL=[0,o([0,[0,v,a(h[23],[0,aK,aI])],aH]),aG],aN=[0,s,[0,q,[0,a(h[11],v),0]]],S=f(bl),aO=g===S?bl[1]:d===S?a(e[2],bl):bl,aP=aJ(q),aQ=[0,a8(m,hZ(aJ(s),aP),[0,aO,aN]),aL],aR=[0,a(k[57][22],aQ),aE],aS=a(j[25],[0,v,0]),T=f(cf),aT=[0,q,[0,s,0]],aU=g===T?cf[1]:d===T?a(e[2],cf):cf,aV=a(h[41],[0,aU,aT]),aW=a(j[aM],aV),aX=c(k[57][3],aW,aS),u=c(k[57][21],aX,aR),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
9:var
J=p[2];if(J)if(J[2])var
i=0,b=0;else
var
U=J[1],V=f(au),aY=g===V?au[1]:d===V?a(e[2],au):au,W=f(av),aZ=[0,aY],a0=g===W?av[1]:d===W?a(e[2],av):av,a1=[0,U,a(h[23],[0,a0,aZ])],X=f(ay),a2=g===X?ay[1]:d===X?a(e[2],ay):ay,Y=a(h[23],[0,a2,a1]),a3=n(m,Y),Z=f(ce),a4=[0,U,0],a5=g===Z?ce[1]:d===Z?a(e[2],ce):ce,a6=ij([0,kg,m],Y,[0,a5,a4]),u=c(k[57][3],a6,a3),b=1;else
var
i=0,b=0;break;case
10:var
K=p[2];if(K)if(K[2])var
i=0,b=0;else
var
a7=K[1],_=function(i){try{var
d=cC(w,i);if(typeof
d==="number")var
b=0;else
if(1===d[0]){var
e=d[1];if(typeof
e==="number"){if(10===e){var
f=d[2];if(f)if(f[2])var
b=0,c=0;else
var
h=_(f[1]),c=1;else
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
g=0;return g}catch(b){b=t(b);if(a(aD[4],b))return 0;throw b}},aa=function(i,l){try{var
j=cC(w,l);if(typeof
j==="number")var
b=0;else
if(1===j[0]){var
q=j[1];if(typeof
q==="number")if(10===q){var
m=j[2];if(m)if(m[2])var
b=0;else
var
o=m[1],u=aa([0,kh,i],o),r=f(bn),v=[0,o,0],x=g===r?bn[1]:d===r?a(e[2],bn):bn,z=[0,aJ(o)],s=f(ah),y=[0,x,v],A=g===s?ah[1]:d===s?a(e[2],ah):ah,B=a8(i,a(h[23],[0,A,z]),y),p=c(k[57][3],B,u),b=1;else
var
b=0}else
var
b=0;else
var
b=0}else
var
b=0;if(!b)var
p=n(i,l);return p}catch(b){b=t(b);if(a(aD[4],b))return n(i,l);throw b}},a9=_(a7)?$(m):aa(m,L),u=a9,b=1;else
var
i=0,b=0;break;case
11:if(p[2])var
i=0,b=0;else
var
u=$(m),b=1;break;default:var
i=0,b=0}if(b)var
M=u,i=1}else
var
i=0}else
var
i=0;if(!i)var
M=a(r[16],0);return M}catch(b){b=t(b);if(a(aD[4],b))return a(r[16],0);throw b}}return c(r[72][1],r[54],b)}function
o(b){if(b){var
l=b[2],i=b[1],j=i[1],ab=i[2],m=function(ac){try{var
m=aR(ac,ab);if(typeof
m==="number")var
i=0;else
if(1===m[0]){var
q=m[1];if(typeof
q==="number")if(16<=q){switch(q+cQ|0){case
0:var
r=m[2];if(r){var
s=r[2];if(s){var
v=s[2];if(v)if(v[2])var
i=0,b=0;else{var
H=v[1],I=s[1],ad=r[1],J=f(W),ae=g===J?W[1]:d===J?a(e[2],W):W;if(c(aa,ad,ae))var
af=[0,o(l),0],ag=[0,aT(j),af],ah=[0,n(ki,H),ag],ai=[0,n(kj,I),ah],ak=[0,I,H,a(h[11],j)],K=f(bo),aj=0,al=g===K?bo[1]:d===K?a(e[2],bo):bo,am=[0,u([0,a(h[23],[0,al,ak]),aj]),ai],L=a(k[57][22],am);else
var
L=o(l);var
p=L,b=1}else
var
i=0,b=0}else
var
i=0,b=0}else
var
i=0,b=0;break;case
1:var
w=m[2];if(w){var
x=w[2];if(x)if(x[2])var
i=0,b=0;else
var
M=x[1],N=w[1],an=[0,o(l),0],ao=[0,aT(j),an],ap=[0,n(kk,M),ao],aq=[0,n(kl,N),ap],as=[0,N,M,a(h[11],j)],O=f(bp),ar=0,at=g===O?bp[1]:d===O?a(e[2],bp):bp,au=[0,u([0,a(h[23],[0,at,as]),ar]),aq],p=a(k[57][22],au),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
15:var
y=m[2];if(y){var
z=y[2];if(z)if(z[2])var
i=0,b=0;else
var
P=z[1],Q=y[1],av=[0,o(l),0],aw=[0,aT(j),av],ax=[0,n(km,P),aw],ay=[0,n(kn,Q),ax],aA=[0,Q,P,a(h[11],j)],R=f(bq),az=0,aB=g===R?bq[1]:d===R?a(e[2],bq):bq,aC=[0,u([0,a(h[23],[0,aB,aA]),az]),ay],p=a(k[57][22],aC),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
16:var
A=m[2];if(A){var
B=A[2];if(B)if(B[2])var
i=0,b=0;else
var
S=B[1],T=A[1],aE=[0,o(l),0],aF=[0,aT(j),aE],aG=[0,n(ko,S),aF],aH=[0,n(kp,T),aG],aJ=[0,T,S,a(h[11],j)],U=f(br),aI=0,aK=g===U?br[1]:d===U?a(e[2],br):br,aL=[0,u([0,a(h[23],[0,aK,aJ]),aI]),aH],p=a(k[57][22],aL),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
17:var
C=m[2];if(C){var
D=C[2];if(D)if(D[2])var
i=0,b=0;else
var
V=D[1],X=C[1],aM=[0,o(l),0],aN=[0,aT(j),aM],aO=[0,n(kq,V),aN],aP=[0,n(kr,X),aO],aS=[0,X,V,a(h[11],j)],Y=f(bs),aQ=0,aU=g===Y?bs[1]:d===Y?a(e[2],bs):bs,aV=[0,u([0,a(h[23],[0,aU,aS]),aQ]),aP],p=a(k[57][22],aV),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;case
18:var
E=m[2];if(E){var
F=E[2];if(F)if(F[2])var
i=0,b=0;else
var
Z=F[1],_=E[1],aW=[0,o(l),0],aX=[0,aT(j),aW],aY=[0,n(ks,Z),aX],aZ=[0,n(kt,_),aY],a1=[0,_,Z,a(h[11],j)],$=f(bt),a0=0,a2=g===$?bt[1]:d===$?a(e[2],bt):bt,a3=[0,u([0,a(h[23],[0,a2,a1]),a0]),aZ],p=a(k[57][22],a3),b=1;else
var
i=0,b=0}else
var
i=0,b=0;break;default:var
i=0,b=0}if(b)var
G=p,i=1}else
var
i=0;else
var
i=0}else
var
i=0;if(!i)var
G=o(l);return G}catch(b){b=t(b);if(a(aD[4],b))return o(l);throw b}};return c(r[72][1],r[54],m)}return a(r[16],0)}var
q=a(F[32][13],b);return o(a(m[22][9],q))}var
ku=a(r[67][7],j$);function
kv(a){if(typeof
a==="number")if(18<=a)switch(a+df|0){case
0:return gw;case
1:return gy;case
2:return gA;case
3:return gE;case
4:return gC;case
13:return g$;case
14:return hb;case
15:return hd;case
16:return hf}throw I}function
kw(a){if(typeof
a==="number")if(18<=a)switch(a+df|0){case
0:return gH;case
1:return gJ;case
2:return gL;case
3:return gN;case
4:return gP;case
13:return hi;case
14:return hk;case
15:return hm;case
16:return ho}throw I}var
ba=[aO,kx,aL(0)];function
aa(j,i,X){var
c=aR(i,X);if(typeof
c!=="number")switch(c[0]){case
1:var
l=c[1];if(typeof
l==="number")if(16<=l)switch(l+cQ|0){case
0:var
n=c[2];if(n){var
o=n[2];if(o){var
p=o[2];if(p){if(!p[2]){var
B=p[1],C=o[1],k=a7(j,i,n[1]);if(typeof
k!=="number"&&1===k[0]){var
q=k[1];if(typeof
q==="number")if(23===q){if(!k[2]){var
D=f(b7),Y=[0,C,B],Z=g===D?b7[1]:d===D?a(e[2],b7):b7;return a(h[23],[0,Z,Y])}}else
if(24===q)if(!k[2]){var
E=f(cg),_=[0,C,B],$=g===E?cg[1]:d===E?a(e[2],cg):cg;return a(h[23],[0,$,_])}}throw ba}var
b=1}else
var
b=0}else
var
b=1}else
var
b=1;break;case
9:var
u=c[2];if(u){var
v=u[2];if(v){if(!v[2]){var
G=v[1],H=u[1],ag=aa(j,i,G),ah=[0,H,G,aa(j,i,H),ag],J=f(ck),ai=g===J?ck[1]:d===J?a(e[2],ck):ck;return a(h[23],[0,ai,ah])}var
b=1}else
var
b=1}else
var
b=1;break;case
10:var
w=c[2];if(w){var
x=w[2];if(x){if(!x[2]){var
K=x[1],L=w[1],aj=aa(j,i,K),ak=[0,L,K,aa(j,i,L),aj],M=f(cj),al=g===M?cj[1]:d===M?a(e[2],cj):cj;return a(h[23],[0,al,ak])}var
b=1}else
var
b=1}else
var
b=1;break;case
11:if(!c[2]){var
N=f(co);return g===N?co[1]:d===N?a(e[2],co):co}var
b=0;break;case
12:if(!c[2]){var
O=f(cq);return g===O?cq[1]:d===O?a(e[2],cq):cq}var
b=0;break;case
13:var
y=c[2];if(y){if(!y[2]){var
P=y[1],am=[0,P,aa(j,i,P)],Q=f(cn),an=g===Q?cn[1]:d===Q?a(e[2],cn):cn;return a(h[23],[0,an,am])}var
b=0}else
var
b=1;break;case
14:var
z=c[2];if(z){var
A=z[2];if(A){if(!A[2]){var
R=A[1],S=z[1],ao=aa(j,i,R),ap=[0,S,R,aa(j,i,S),ao],T=f(cm),aq=g===T?cm[1]:d===T?a(e[2],cm):cm;return a(h[23],[0,aq,ap])}var
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
r=c[2];if(r){var
s=r[2];if(s)if(!s[2]){var
ab=s[1],ac=r[1];try{var
m=kv(l),F=f(m),ad=[0,ac,ab],ae=g===F?m[1]:d===F?a(e[2],m):m,af=a(h[23],[0,ae,ad]);return af}catch(a){a=t(a);if(a===I)throw ba;throw a}}}}break;case
2:var
U=c[2],V=c[1],ar=aa(j,i,U),as=[0,V,U,aa(j,i,V),ar],W=f(cl),at=g===W?cl[1]:d===W?a(e[2],cl):cl;return a(h[23],[0,at,as])}throw ba}function
cF(d,c,b){var
e=a(r[67][3],b);return o(j[13],d,c,e)}function
ab(b,e){function
d(f){var
d=cF(D[1][10][1],b,f),g=a(e,d),h=a(j[2],d);return c(k[57][3],h,g)}var
f=a(r[67][7],d),g=a(j[76],[0,b,0]),h=a(k[57][24],g);return c(k[57][3],h,f)}function
dD(b,g){function
d(d){var
h=c(a2[5],b,ky),e=cF(D[1][10][1],h,d),i=c(a2[5],b,kz),f=cF(D[1][10][1],i,d),l=[0,c(g,e,f),0],m=[0,a(j[2],f),l],n=[0,a(j[2],e),m];return a(k[57][22],n)}var
e=a(r[67][7],d),f=a(j[76],[0,b,0]),h=a(k[57][24],f);return c(k[57][3],h,e)}function
kA(n){var
a5=a(F[32][6],n),G=a(r[67][3],n),i=a(r[67][4],n);function
y(a){return aa(G,i,a)}function
b(p){if(p){var
x=p[1];if(1===x[0]){var
q=p[2],s=x[3],v=x[1],A=x[2];if(a(m[3],cS)){var
B=function(m){try{var
e=a7(G,m,s);if(typeof
e==="number")var
d=0;else
if(1===e[0]){var
g=e[1];if(typeof
g==="number")if(1<(g-23|0)>>>0)var
d=0;else
var
p=c(a2[5],v[1],kC),i=cF(D[1][10][1],p,n),l=dt(s,a(h[11],v[1]),A),r=b([0,[0,c(z[4],i,0),l],q]),u=o(j[140],[0,i],l,j[H]),f=c(k[57][3],u,r),d=1;else
var
d=0}else
var
d=0;if(!d)var
f=b(q);return f}catch(c){c=t(c);if(a(aD[4],c))return b(q);throw c}};return c(r[72][1],r[54],B)}}var
l=p[2],i=a(z[11][1][2],x),w=function(B){try{var
v=aR(B,a(z[11][1][4],x));if(typeof
v==="number")var
n=0;else
switch(v[0]){case
1:var
P=v[1];if(typeof
P==="number")if(18<=P){switch(P+df|0){case
7:var
Q=v[2];if(Q){var
R=Q[2];if(R)if(R[2])var
n=0,p=0;else
var
a6=R[1],a8=Q[1],a9=dD(i,function(d,a){var
e=[0,[0,c(z[4],a,0),a6],l];return b([0,[0,c(z[4],d,0),a8],e])}),a_=aP(i),C=c(k[57][3],a_,a9),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
8:var
S=v[2];if(S){var
T=S[2];if(T)if(T[2])var
n=0,p=0;else
var
a$=T[1],bb=S[1],bc=0,bd=[0,ab(i,function(a){return b([0,[0,c(z[4],a,0),a$],l])}),bc],bf=[0,ab(i,function(a){return b([0,[0,c(z[4],a,0),bb],l])}),bd],bg=aP(i),C=c(k[57][21],bg,bf),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
9:if(v[2])var
n=0,p=0;else
var
C=aP(i),p=1;break;case
11:var
U=v[2];if(U)if(U[2])var
n=0,p=0;else{var
w=aR(B,U[1]);if(typeof
w==="number")var
r=0;else
switch(w[0]){case
1:var
H=w[1];if(typeof
H==="number")if(16<=H){switch(H+cQ|0){case
0:var
W=w[2];if(W){var
X=W[2];if(X){var
Y=X[2];if(Y)if(Y[2])var
r=0,s=0,q=0;else{var
J=Y[1],K=X[1],aA=W[1];if(a(m[3],be)){var
Z=a7(G,B,aA);if(typeof
Z==="number")var
E=0;else
if(1===Z[0]){var
_=Z[1];if(typeof
_==="number"){if(23===_)var
bh=0,bi=[0,ab(i,function(a){return b(l)}),bh],bj=[0,K,J,a(h[11],i)],aG=f(b8),bk=g===aG?b8[1]:d===aG?a(e[2],b8):b8,bl=a(h[23],[0,bk,bj]),bm=[0,a(j[aM],bl),bi],aH=a(k[57][22],bm),au=1;else
if(24===_)var
bn=0,bo=[0,ab(i,function(a){return b(l)}),bn],bp=[0,K,J,a(h[11],i)],aI=f(ch),bq=g===aI?ch[1]:d===aI?a(e[2],ch):ch,br=a(h[23],[0,bq,bp]),bs=[0,a(j[aM],br),bo],aH=a(k[57][22],bs),au=1;else
var
E=0,au=0;if(au)var
aB=aH,E=1}else
var
E=0}else
var
E=0;if(!E)var
aB=b(l);var
aE=aB}else{var
$=a7(G,B,aA);if(typeof
$==="number")var
F=0;else
if(1===$[0]){var
aa=$[1];if(typeof
aa==="number"){if(23===aa)var
bt=b(l),aK=f(at),bu=[0,K,J],bv=g===aK?at[1]:d===aK?a(e[2],at):at,bw=a(h[23],[0,bv,bu]),bx=c(z[11][1][7],bw,x),by=o(j[4],0,0,bx),aL=c(k[57][3],by,bt),av=1;else
if(24===aa)var
bz=b(l),aN=f(as),bA=[0,K,J],bB=g===aN?as[1]:d===aN?a(e[2],as):as,bC=a(h[23],[0,bB,bA]),bD=c(z[11][1][7],bC,x),bE=o(j[4],0,0,bD),aL=c(k[57][3],bE,bz),av=1;else
var
F=0,av=0;if(av)var
aJ=aL,F=1}else
var
F=0}else
var
F=0;if(!F)var
aJ=b(l);var
aE=aJ}var
D=aE,q=1}else
var
s=1,q=0}else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
9:var
ae=w[2];if(ae){var
af=ae[2];if(af)if(af[2])var
r=0,s=0,q=0;else
var
aS=af[1],ag=ae[1],bO=y(ag),bP=0,bQ=[0,ab(i,function(a){var
d=aC(aS),e=c2(aC(ag),d);return b([0,[0,c(z[4],a,0),e],l])}),bP],bS=[0,ag,aS,bO,a(h[11],i)],aT=f(cs),bR=0,bT=g===aT?cs[1]:d===aT?a(e[2],cs):cs,bU=[0,u([0,a(h[23],[0,bT,bS]),bR]),bQ],D=a(k[57][22],bU),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
10:var
ah=w[2];if(ah){var
ai=ah[2];if(ai)if(ai[2])var
r=0,s=0,q=0;else
var
aU=ai[1],aV=ah[1],bV=0,bW=[0,ab(i,function(a){var
d=aC(aU),e=cB(aC(aV),d);return b([0,[0,c(z[4],a,0),e],l])}),bV],bY=[0,aV,aU,a(h[11],i)],aW=f(cr),bX=0,bZ=g===aW?cr[1]:d===aW?a(e[2],cr):cr,b0=[0,u([0,a(h[23],[0,bZ,bY]),bX]),bW],D=a(k[57][22],b0),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;case
13:var
aj=w[2];if(aj)if(aj[2])var
s=1,q=0;else
var
ak=aj[1],b1=y(ak),b2=0,b3=[0,ab(i,function(a){return b([0,[0,c(z[4],a,0),ak],l])}),b2],b5=[0,ak,b1,a(h[11],i)],aX=f(cv),b4=0,b6=g===aX?cv[1]:d===aX?a(e[2],cv):cv,b7=[0,u([0,a(h[23],[0,b6,b5]),b4]),b3],D=a(k[57][22],b7),q=1;else
var
r=0,s=0,q=0;break;case
14:var
al=w[2];if(al){var
am=al[2];if(am)if(am[2])var
r=0,s=0,q=0;else
var
M=am[1],N=al[1],b9=y(N),b_=y(M),b$=0,ca=[0,ab(i,function(a){var
d=cB(aC(N),M),e=c2(cB(N,aC(M)),d);return b([0,[0,c(z[4],a,0),e],l])}),b$],cc=[0,N,M,b9,b_,a(h[11],i)],aY=f(cu),cb=0,cd=g===aY?cu[1]:d===aY?a(e[2],cu):cu,ce=[0,u([0,a(h[23],[0,cd,cc]),cb]),ca],D=a(k[57][22],ce),q=1;else
var
r=0,s=0,q=0}else
var
r=0,s=0,q=0;break;default:var
s=1,q=0}if(q)var
aF=D,s=2}else
var
s=1;else
var
s=1;switch(s){case
0:var
A=0;break;case
1:var
ac=w[2];if(ac){var
ad=ac[2];if(ad)if(ad[2])var
r=0,A=0;else{var
bF=ad[1],bG=ac[1];try{var
L=kw(H),bH=0,bI=[0,ab(i,function(a){return b(l)}),bH],bK=[0,bG,bF,a(h[11],i)],aQ=f(L),bJ=0,bL=g===aQ?L[1]:d===aQ?a(e[2],L):L,bM=[0,u([0,a(h[23],[0,bL,bK]),bJ]),bI],bN=a(k[57][22],bM),aO=bN}catch(a){a=t(a);if(a!==I)throw a;var
aO=b(l)}var
aF=aO,A=1}else
var
r=0,A=0}else
var
r=0,A=0;break;default:var
A=1}if(A)var
V=aF,r=1;break;case
2:var
aZ=w[2],an=w[1],cf=y(an),cg=0,ci=[0,ab(i,function(a){var
d=cB(an,aC(aZ));return b([0,[0,c(z[4],a,0),d],l])}),cg],ck=[0,an,aZ,cf,a(h[11],i)],a0=f(ct),cj=0,cl=g===a0?ct[1]:d===a0?a(e[2],ct):ct,cm=[0,u([0,a(h[23],[0,cl,ck]),cj]),ci],V=a(k[57][22],cm),r=1;break;default:var
r=0}if(!r)var
V=b(l);var
C=V,p=1}else
var
n=0,p=0;break;case
12:var
ao=v[2];if(ao){var
ap=ao[2];if(ap)if(ap[2])var
n=0,p=0;else
var
a1=ap[1],a2=ao[1],cn=dD(i,function(d,a){var
e=o(h[35],a1,0,a2),f=[0,[0,c(z[4],a,0),e],l],g=o(h[35],a2,0,a1);return b([0,[0,c(z[4],d,0),g],f])}),co=aP(i),C=c(k[57][3],co,cn),p=1;else
var
n=0,p=0}else
var
n=0,p=0;break;case
0:case
1:case
2:case
3:case
4:var
ax=v[2];if(ax){var
ay=ax[2];if(ay)if(ay[2])var
n=0,p=0;else
var
az=b(l),p=2;else
var
n=0,p=0}else
var
n=0,p=0;break;default:var
n=0,p=0}switch(p){case
0:var
aw=0;break;case
1:var
az=C,aw=1;break;default:var
aw=1}if(aw)var
O=az,n=1}else
var
n=0;else
var
n=0;break;case
2:var
aq=v[2],ar=v[1],cp=a(a5,aq);if(c(kB[103],B,cp))var
cq=y(ar),cx=0,cy=[0,ab(i,function(a){var
d=c2(aC(ar),aq);return b([0,[0,c(z[4],a,0),d],l])}),cx],cA=[0,ar,aq,cq,a(h[11],i)],a3=f(cw),cz=0,cC=g===a3?cw[1]:d===a3?a(e[2],cw):cw,cD=[0,u([0,a(h[23],[0,cC,cA]),cz]),cy],a4=a(k[57][22],cD);else
var
a4=b(l);var
O=a4,n=1;break;default:var
n=0}if(!n)var
O=b(l);return O}catch(c){c=t(c);if(c===ba)return b(l);if(a(aD[4],c))return b(l);throw c}};return c(r[72][1],r[54],w)}return c(k[57][3],ku,j_)}return b(a(r[67][2],n))}var
db=a(r[67][7],kA);function
kD(b){var
i=a(r[67][1],b),m=a(r[67][3],b),n=a(r[67][4],b);function
u(a){return aa(m,n,a)}function
l(i){function
b(b){function
m(d){var
c=aR(b,i);return a(r[16],c)}function
n(b){if(typeof
b!=="number")switch(b[0]){case
1:var
p=b[1];if(typeof
p==="number"){var
q=p-27|0;if(!(2<q>>>0))switch(q){case
0:if(!b[2])return db;break;case
1:break;default:var
s=b[2];if(s)if(!s[2]){var
B=j[16],C=O(c0),D=c(k[57][3],C,B);return c(k[57][3],D,db)}}}break;case
2:var
E=l(b[2]);return c(k[57][3],j[16],E)}try{var
w=u(i),x=j[16],y=function(c){var
b=f(cp),j=[0,i,w],k=g===b?cp[1]:d===b?a(e[2],cp):cp;return dy(c,a(h[23],[0,k,j]))},z=a(r[67][7],y),A=c(k[57][3],z,x),o=A}catch(b){b=t(b);if(b===ba)var
m=f(aB),v=g===m?aB[1]:d===m?a(e[2],aB):aB,n=a(j[108],v);else{if(!a(r[71][9],b))throw b;var
n=c(r[21],0,b)}var
o=n}return c(k[57][3],o,db)}var
o=a(r[71][10],m);return c(r[72][1],o,n)}return c(r[72][1],r[54],b)}return l(i)}var
kE=a(r[67][7],kD);function
kF(e){a(dr[12],kG);if(a(m[3],cT)){var
b=a(m[3],cU),d=function(a){a[1][1]=a[2];return 0};c(m[22][11],d,b);cV[1]=0;a(v[2],bg);bh[1]=0;dq(0)}return kE}var
kH=a(r[16],0),dE=c(r[72][1],kH,kF);cI(424,[0,dE],"Omega_plugin__Coq_omega");a(kI[9],cG);function
aU(b){var
d=c(p[17],D[1][6],kJ),e=a(D[5][4],d),f=a(D[8][4],b),g=c(D[15][1],[0,e],f),h=a(kK[12],g);return a(kL[22],h)}function
cH(b){var
d=c(m[22][137],kM[33],b);function
e(b){if(cN(b,kN)){if(cN(b,kO)){if(cN(b,kP)){if(cN(b,kQ)){var
d=c(n[17],kR,b),e=a(ac[3],d);return o(_[5],0,0,e)}return aU(kS)}return aU(kT)}return aU(kU)}return aU(kV)}var
f=c(p[17],e,d),g=a(k[57][22],f),h=a(k[57][32],g);return c(k[57][3],h,dE)}var
dF=o(kY[1],kX,kW,0),kZ=0,k1=[0,[0,k0,function(a){return cH(0)}],kZ];dI(dG[8],cG,k2,0,0,k1);var
k3=0,k6=[0,[0,k5,function(d){var
a=cH(0),b=aU(k4);return c(k[57][3],b,a)}],k3];function
k7(a,b){return cH(c(p[17],D[1][8],a))}var
la=[0,[0,[0,k$,[0,k_,[1,[0,[5,a(k9[16],k8[7])]],0]]],k7],k6];dI(dG[8],cG,lb,0,[0,dF],la);cI(433,[0,cG,aU,cH,dF],"Omega_plugin__G_omega");return}
