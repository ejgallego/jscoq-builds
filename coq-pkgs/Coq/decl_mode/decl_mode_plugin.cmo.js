(function(pd){"use strict";var
dU="*",ei='"end claim" expected.',eo=104,aA="that",en="decl_mode_plugin",Z=",",eh="be",d9='"end focus" expected.',aD="such",dT="Insufficient justification.",L=250,bG="consider",bx="(",d8="Not inside a proof per cases or induction.",cr="suppose it is",cy=148,Y="Init",cl="DeclProof",cm="on",C=246,cq="suppose",T="plugins/decl_mode/decl_proof_instr.ml",dS="    ",eg="escape",d7="assume",aC="Extension: cannot occur",dR='"end cases" expected.',O=120,d6="Datatypes",cp="execute_cases ",ct="DeclReturn",dQ="for",bw=117,d5="with",co="Classic",r="",cs="then",d4="=~",bD="Previous step is not an equality.",em="~=",d3="suffices",aE=136,d2='"end induction" expected.',ef="using",cx="ProofInstr",bv="given",F="and",d1="cases",h="IDENT",el=107,dP='"thesis for ..." is not applicable here.',aB="Declarative",d0="plugins/decl_mode/decl_interp.ml",cw="by",dZ="induction",ee=".",dO="take",cn="proof",aV="thesis",ed="impossible",ab=110,bt="thus",dN="subcase_",ek="of",ec="thesis_for",dL=133,dM="Not enough sub-hypotheses to match statements.",dY="hence",cv="we have",bC=")",ck="_tmp",bB="let",az=":",bA=118,cj=116,eb="proof_instr",ej="show",bF="Logic",bz="we",t=109,dX="define",cu="reconsider",bE="claim",by="to",dW="This case should already be trapped",bu="Specif",ci="focus",aU="as",dK="No pop/stop expected here",d_=146,d$="per",ea="declmode",ag="have",dV="end",J=pd.jsoo_runtime,S=J.caml_check_bound,b=J.caml_new_string,K=J.caml_obj_tag,ay=J.caml_register_global,H=J.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):J.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):J.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):J.caml_call_gen(a,[b,c,d])}function
E(a,b,c,d,e){return a.length==4?a(b,c,d,e):J.caml_call_gen(a,[b,c,d,e])}function
N(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):J.caml_call_gen(a,[b,c,d,e,f])}function
ch(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):J.caml_call_gen(a,[b,c,d,e,f,g])}var
l=J.caml_get_global_data(),d=l.Pp,i=l.CErrors,aY=l.Environ,aF=l.Context,x=l.Proof,G=l.Proof_global,g=l.Util,B=l.Goal,u=l.Evd,cB=l.Pfedit,ah=l.Option,v=l.Loc,s=l.Ltac_plugin,f=l.Term,z=l.Vars,at=l.Namegen,ac=l.Global,M=l.Assert_failure,U=l.Pretyping,o=l.Names,R=l.Nameops,D=l.Termops,cN=l.Detyping,I=l.CamlinternalLazy,bL=l.Coqlib,cG=l.Universes,P=l.CClosure,_=l.Not_found,bf=l.Reductionops,m=l.Tacmach,p=l.Tactics,n=l.Proofview,j=l.Tacticals,bU=l.Rtree,bP=l.Inductiveops,au=l.Inductive,dp=l.Sigma,af=l.Printer,$=l.Pervasives,cV=l.Locusops,cZ=l.Reduction,c3=l.Typing,c1=l.Unification,cO=l.Evarutil,cX=l.CWarnings,bY=l.Ppconstr,cf=l.Compat,b1=l.Egramml,bn=l.Vernac_classifier,b0=l.Vernacinterp,k=l.Pcoq,an=l.Genarg,aa=l.CList,cg=l.Gramext,bH=[0,0],eE=b('"end claim"'),eF=b('"end focus"'),eG=b('"end proof"'),eB=b("lonely suppose"),eD=b('"end induction" or start a new case'),eC=b('"end cases" or start a new case'),eA=b("no previous statement to use"),et=b("get_info"),eI=b('"(_ | _)" is not allowed here'),eJ=[0,b("simple_pattern")],eQ=b("Anonymous pattern variable"),eX=[0,b(d0),303,18],eY=[0,b(d0),315,18],e9=b('"thesis for" is not applicable here.'),e5=b(" does not occur in pattern."),e6=b("Variable "),e7=[0,b(cr)],e4=[3,[0,0]],eZ=b("No proof per cases/induction/inversion in progress."),e0=b("expected."),e1=b("none"),e2=b("Wrong number of extra arguments: "),e3=[0,b(cr)],eV=b("undetected disjunctive pattern"),eW=b("empty pattern list"),eM=b(bD),eN=b(bD),eL=[0,0],eO=b("__"),gp=b(ck),gt=b(dM),gq=b("Matching hypothesis not found."),gr=b("Last statements do not match a complete hypothesis."),gs=b(ck),gu=b(dM),gw=[0,b(T),814,18],gI=b(dK),gJ=b("Case pattern belongs to wrong inductive type."),gK=b(dK),gL=b("we should pop here"),gM=b("tree is expected to end here"),gN=b("Premature end of branch"),gS=b('Only "suppose it is" can be used here.'),g9=[0,b(T),1284,2],g7=b("Nothing to skip"),g8=[0,b(cp)],g_=[0,b(T),1263,9],g$=b("Nothing to split"),ha=[0,b(cp)],hb=b("End of branch with garbage left"),hc=[0,b(cp)],hd=b("wrong stack size"),hq=[0,b(T),1429,1],hr=[0,b(T),1426,1],hs=[0,b(T),1432,1],ht=[0,b(T),1439,1],hu=b("Not applicable"),hw=b(co),hx=b(co),hz=b("hd"),hB=b('"end induction" generated an ill-formed fixpoint'),hp=[0,0],hh=b("_fix"),hi=b("_main_arg"),hg=b(dW),hl=b(ei),hm=b(d9),hn=b(dW),hk=b(d2),hj=b(dR),hf=[3,0],g3=b("missing case"),g1=[0,b(T),1220,30],g0=[0,b(T),1222,2],gY=b(ed),gW=b(ed),gV=[0,[0,0,0],0],gT=b(dN),gU=b("wrong place for cases"),gO=b("cannot give an induction hypothesis (wrong inductive type)."),gP=[0,b(ec)],gQ=b("cannot give an induction hypothesis (wrong parameters)."),gR=[0,b(ec)],gH=b(dN),gG=b('Do not mix "suppose" with "suppose it is".'),gF=b("wrong stack state"),gD=[0,b(T),935,5],gE=[0,b(T),926,3],gC=b("anonymous_matched"),gB=b("Case analysis must be done on an inductive object."),gA=b("map_tree: not a splitting node"),gz=b("map_tree_rp: not a splitting node"),gx=b(dP),gv=b(ck),gn=b("_cofact"),gm=b("_hyp"),gl=b("_claim"),gj=b("No previous equality."),gk=b("_eq"),gg=b(bD),gh=b(bD),ge=b("_fact"),gd=b('"then" and "hence" require at least one previous fact'),gc=b(dP),gb=b("I could not relate this statement to the thesis."),f$=b("cannot happen"),ga=[0,b("concl_refiner")],f_=[0,2,1],f9=[0,2,2],fN=b(aB),fM=b(dT),fI=b(dT),fx=b("Weird case occurred ..."),fw=b(d8),fy=b(d8),fs=b(d2),fr=b(dR),ft=b(ei),fu=b(d9),fv=b('"end proof" expected.'),fq=b("Lonely suppose on stack."),fo=b('"return" cannot be used outside of Declarative Proof Mode.'),fj=[0,0],fe=b('You are inside a proof per cases/induction.\nPlease "suppose" something or "end" it now.'),e$=b("Cannot clear "),fb=[0,b("Strict"),[0,b("Proofs"),0]],fc=b("strict proofs"),fz=b("___"),fB=b("No automation registered"),fJ=b(ea),fK=b("declmode-insufficient-justification"),fO=b(F),fP=[0,b(Y),[0,b(bF),0]],fQ=b("and_rect"),fR=[0,b(Y),[0,b(bF),0]],fS=b("prod"),fT=[0,b(Y),[0,b(d6),0]],fU=b("prod_rect"),fV=[0,b(Y),[0,b(d6),0]],fW=b("ex"),fX=[0,b(Y),[0,b(bF),0]],fY=b("ex_ind"),fZ=[0,b(Y),[0,b(bF),0]],f0=b("sig"),f1=[0,b(Y),[0,b(bu),0]],f2=b("sig_rect"),f3=[0,b(Y),[0,b(bu),0]],f4=b("sigT"),f5=[0,b(Y),[0,b(bu),0]],f6=b("sigT_rect"),f7=[0,b(Y),[0,b(bu),0]],g4=b(ea),g5=b("declmode-missing-case"),hL=b(F),hM=b("be such that"),hN=b("such that"),ir=b(bC),is=b(bx),ii=b(bC),ij=b(bx),hZ=b(eg),h3=b(dY),h2=b(cs),h1=b(bt),h0=b(ag),h4=b(bt),h5=b(dS),h6=b(d3),h7=b(cv),h8=b(d7),h9=b(bB),h_=b(bB),h$=b(bv),ia=b(bv),ib=b("from "),ic=b(bG),id=b(bG),ie=b(bE),ig=b("focus on"),ih=b(aU),ik=b(dX),il=b(aU),im=b(cu),io=b(cv),ip=b(cq),iu=b(cv),iv=b(F),it=b(d5),iq=b(cr),iw=b(dO),ix=b(d$),iy=b(dV),iJ=b(bC),iK=b(az),iL=b(bx),iF=b(bC),iG=b(az),iH=b(bx),iz=b("unknown emphasis"),iA=b(dS),iB=b("*   "),iC=b("**  "),iD=b("*** "),hY=b(em),hX=b(d4),hV=b(cm),hW=b(ek),hS=b(cn),hT=b(bE),hU=b(ci),hR=b(dZ),hQ=b(d1),hO=b("to show"),hP=b("to have"),hH=b(dQ),hI=b(aV),hJ=b(aV),hG=b(ef),hD=b(Z),hE=b(cw),hF=b("by *"),hC=b(az),kW=[0,0],jR=b(ct),jO=b(ct),jL=[0,[4,b(aB)],0],jK=b(aC),jI=b(ct),jF=b(aC),jD=b(cl),jA=b(cl),jx=[0,[4,b(aB)],0],jw=b(aC),ju=b(cl),jr=b(aC),jk=[0,[0,1]],jg=b(cx),jb=b(cx),i_=b(aC),i8=b(cx),i5=b(aC),i3=[0,[4,b(co)],0],iY=b(aB),iW=b("Nothing left to prove here."),iX=b(aB),iT=b(ee),iU=b("Subproof completed, now type "),iP=b("thesis :="),iQ=b("============================"),iR=b("  "),iS=b("     *** Declarative Mode ***"),iM=b(en),iN=b(en),iZ=b(eb),i0=b("vernac:proof_command"),i2=b(eb),jm=[0,1],jo=b(aB),jB=[0,[0,[0,b(cn)],0],0],jP=[0,[0,[0,b("return")],0],0],jS=b(aV),jT=b("statement"),jU=b("constr_or_thesis"),jV=b("statement_or_thesis"),jW=b("justification_items"),jX=b("justification_method"),jY=b("simple_cut_or_thesis"),jZ=b("simple_cut"),j0=b("elim_type"),j1=b("block_type"),j2=b("elim_obj"),j3=b("elim_step"),j4=b("rew_step"),j5=b("cut_step"),j6=b("loc_id"),j7=b("hyp"),j8=b("consider_vars"),j9=b("consider_hyps"),j_=b("assume_vars"),j$=b("assume_hyps"),ka=b("assume_clause"),kb=b("suff_vars"),kc=b("suff_hyps"),kd=b("suff_clause"),ke=b("let_vars"),kf=b("let_hyps"),kg=b("given_vars"),kh=b("given_hyps"),ki=b("suppose_vars"),kj=b("suppose_hyps"),kk=b("suppose_clause"),kl=b("intro_step"),km=b("emphasis"),kn=b("bare_proof_instr"),kq=[0,[10,[0,b(r),b(aV)]],0],kt=[10,[0,b(r),b(dQ)]],ku=[10,[0,b(r),b(aV)]],ky=[10,[0,b(r),b(az)]],kM=[10,[0,b(r),b(az)]],kZ=[10,[0,b(r),b(Z)]],k0=[10,[0,b(r),b(cw)]],k2=[0,[10,[0,b(r),b(cw)]],[0,[10,[0,b(r),b(dU)]],0]],k8=[10,[0,b(r),b(ef)]],lf=[0,[10,[0,b(h),b(dZ)]],0],lh=[0,[10,[0,b(h),b(d1)]],0],ll=[0,[10,[0,b(h),b(bE)]],0],ln=[0,[10,[0,b(h),b(ci)]],0],lp=[0,[10,[0,b(h),b(cn)]],0],lv=[10,[0,b(h),b(cm)]],lx=[10,[0,b(h),b(ek)]],lC=[10,[0,b(h),b("from")]],lD=[10,[0,b(h),b(bG)]],lF=[10,[0,b(h),b(d$)]],lH=[10,[0,b(h),b(d3)]],lL=[10,[0,b(r),b(em)]],lN=[10,[0,b(r),b(d4)]],lR=[10,[0,b(r),b(cs)]],lT=[10,[0,b(r),b(cs)]],lV=[10,[0,b(h),b(bt)]],lX=[10,[0,b(h),b(bt)]],lZ=[10,[0,b(h),b(dY)]],l3=[10,[0,b(h),b(ag)]],l5=[10,[0,b(h),b(bE)]],l7=[10,[0,b(h),b(cm)]],l8=[10,[0,b(h),b(ci)]],l_=[10,[0,b(r),b(dV)]],ma=[0,[10,[0,b(h),b(eg)]],0],mj=[10,[0,b(r),b(az)]],mn=[0,[10,[0,b(r),b(Z)]],[0,0,0]],mp=[10,[0,b(h),b(aA)]],mq=[10,[0,b(h),b(aD)]],mu=[0,[10,[0,b(h),b(F)]],[0,0,0]],mw=[10,[0,b(h),b(bG)]],mx=[10,[0,b(h),b(F)]],mD=[0,[10,[0,b(r),b(Z)]],[0,0,0]],mF=[10,[0,b(h),b(aA)]],mG=[10,[0,b(h),b(aD)]],mK=[0,[10,[0,b(h),b(F)]],[0,0,0]],mM=[10,[0,b(h),b(ag)]],mN=[10,[0,b(h),b(bz)]],mO=[10,[0,b(h),b(F)]],mT=[10,[0,b(h),b(ag)]],mU=[10,[0,b(h),b(bz)]],mZ=[10,[0,b(h),b(ej)]],m0=[10,[0,b(h),b(by)]],m2=[0,[10,[0,b(r),b(Z)]],[0,0,0]],m4=[10,[0,b(h),b(aA)]],m5=[10,[0,b(h),b(aD)]],m9=[0,[10,[0,b(h),b(F)]],[0,0,0]],m$=[10,[0,b(h),b(ag)]],na=[10,[0,b(h),b(by)]],nb=[10,[0,b(h),b(F)]],nd=[10,[0,b(h),b(ej)]],ne=[10,[0,b(h),b(by)]],ni=[10,[0,b(h),b(ag)]],nj=[10,[0,b(h),b(by)]],np=[0,[10,[0,b(r),b(Z)]],[0,0,0]],nr=[10,[0,b(h),b(aA)]],ns=[10,[0,b(h),b(aD)]],nt=[10,[0,b(h),b(eh)]],nx=[0,[10,[0,b(h),b(F)]],[0,0,0]],nz=[10,[0,b(r),b(bB)]],nA=[10,[0,b(h),b(F)]],nG=[0,[10,[0,b(r),b(Z)]],[0,0,0]],nI=[10,[0,b(h),b(aA)]],nJ=[10,[0,b(h),b(aD)]],nN=[0,[10,[0,b(h),b(F)]],[0,0,0]],nP=[10,[0,b(h),b(bv)]],nQ=[10,[0,b(h),b(F)]],nW=[0,[10,[0,b(r),b(Z)]],[0,0,0]],nZ=[10,[0,b(h),b(aA)]],n0=[10,[0,b(h),b(aD)]],n3=[0,[10,[0,b(h),b(eh)]],0],n8=[0,[10,[0,b(h),b(F)]],[0,0,0]],n_=[10,[0,b(h),b(ag)]],n$=[10,[0,b(h),b(bz)]],oa=[10,[0,b(h),b(F)]],of=[10,[0,b(h),b(ag)]],og=[10,[0,b(h),b(bz)]],ol=[10,[0,b(h),b(cq)]],oq=[10,[0,b(h),b(F)]],ou=[10,[0,b(r),b(Z)]],ov=[10,[0,b(r),b(d5)]],oy=b("0"),oz=[10,[0,b(h),b("is")]],oA=[10,[0,b(h),b("it")]],oB=[10,[0,b(h),b(cq)]],oD=[10,[0,b(r),b(bB)]],oG=[10,[0,b(r),b(Z)]],oH=[10,[0,b(h),b(dO)]],oJ=[10,[0,b(h),b(d7)]],oL=[10,[0,b(h),b(bv)]],oO=[10,[0,b(r),b(aU)]],oP=[10,[0,b(h),b(dX)]],oS=[10,[0,b(r),b(aU)]],oT=[10,[0,b(h),b(cu)]],oW=[10,[0,b(r),b(aU)]],oX=[10,[0,b(h),b(cu)]],o1=[0,[10,[0,b(r),b(dU)]],0],o3=[0,[10,[0,b(r),b("**")]],0],o5=[0,[10,[0,b(r),b("***")]],0],pb=[0,[10,[0,b(r),b(ee)]],0],eU=l.Constrintern,hy=l.Failure,hA=l.Type_errors,he=l.Glob_ops,gZ=l.Invalid_argument,fl=l.Feedback,fd=l.Goptions,iO=l.Mltop,jl=l.G_vernac;function
ep(a){bH[1]=1;return 0}function
eq(a){bH[1]=0;return 0}function
er(a){return bH[1]}var
aW=a(u[3][6],0);function
cz(d){var
b=a(x[34][1],d),e=b[2],f=a(g[17][3],b[1]),h=c(B[4][5],e,f);return c(u[3][3],h,aW)?1:0}function
cA(c){try{var
b=cz(a(cB[9],0));return b}catch(a){a=H(a);if(a===G[11])return 2;throw a}}function
es(b){return 1===cA(0)?a(i[7],b):0}function
bI(d,b){var
e=c(B[4][5],d,b),a=c(u[3][3],e,aW);return a?a[1]:c(i[10],0,et)}function
eu(b,a){var
d=c(B[4][5],b,a);return c(u[3][3],d,aW)}var
aX=a(x[16],0),ev=c(x[18],0,aX);function
ew(f){var
b=a(x[34][1],f),c=b[2],d=bI(c,a(g[17][3],b[1]))[1];function
h(a){return e(x[19],ev,d,1)}return a(G[27],h)}function
ex(c){function
b(b,a){return e(x[23],aX,a,0)}return a(G[27],b)}function
cC(d){try{var
b=c(x[26],aX,d);return b}catch(b){b=H(b);if(b===x[25]){var
e=a(x[34][3],d);return bI(e[2],e[1])[1]}throw b}}function
ey(a){return c(x[26],aX,a)}function
ez(c){var
b=a(aY[9],c);return b?a(aF[2][1][1],b[1]):a(i[7],eA)}var
y=[0,ep,eq,er,cz,cA,es,aW,bI,eu,ey,cC,ez,function(k){var
b=cC(k);if(b){var
c=b[1];if(typeof
c==="number")switch(c){case
0:var
g=b[2];if(g){var
h=g[1];if(typeof
h==="number")var
f=0;else
var
j=h[1],f=1}else
var
f=0;if(!f){var
l=a(d[3],eB);return e(i[3],0,0,l)}break;case
1:return eE;default:return eF}else
var
j=c[1];return 0===j?eC:eD}return eG},ew,ex];ay(412,y,"Decl_mode_plugin.Decl_mode");function
cD(b){var
c=b[3],d=b[2],e=b[1];return a(g[17][47],c)?d:[4,e,d,c]}function
ap(d,b,a){var
e=c(d,b,a[2]);return[0,a[1],e]}function
bJ(d,a,b){return[0,a,c(d,a,b)]}function
bK(b,a){return 0===a[0]?[0,c(s[8][7],b,a[1])]:[1,a[1]]}function
aG(b,a){var
d=a[2];return[0,c(o[1][10][4],b,a[1]),d]}function
cE(j,b,d){if(0===d[0]){var
e=d[1],f=e[2],g=f[1],k=f[2],l=e[1],m=a(s[8][7],b),n=[0,[0,l,[0,g,c(ah[15],m,k)]]];return[0,aG(g,b),n]}var
h=d[1],o=[1,ap(j,b,h)],i=h[1],p=i?aG(i[1],b):b;return[0,p,o]}function
aq(c,b,a){function
d(a,b){return cE(c,a,b)}return e(g[17][cj],d,b,a)[2]}function
aZ(k,j,b){var
d=c(k,j,b[1]),e=d[1],l=d[2],m=b[3],i=a(s[8][5],e),n=a(a(ah[15],i),m),o=b[2],f=a(s[8][7],e),h=a(g[17][12],f);return[0,l,a(a(ah[15],h),o),n]}function
cF(d,b){function
f(d,b){var
e=b[2],f=e[1],g=e[2],h=b[1],i=a(s[8][7],d),j=[0,h,[0,f,c(ah[15],i,g)]];return[0,aG(f,d),j]}return e(g[17][cj],f,b,d)}function
eH(d,a){var
f=a[2],h=a[1],i=s[8][7];function
j(a,b){return cE(i,a,b)}var
b=e(g[17][cj],j,d,h),c=b[1],k=b[2];return[0,c,[0,k,bK(c,f)]]}function
a0(m,l){var
f=m,b=l;for(;;){switch(b[0]){case
0:var
n=b[2],f=aG(b[3],f),b=n;continue;case
1:var
o=b[4],p=b[3],q=a(g[17][15],a0),r=e(ah[17],q,f,p);return e(g[17][15],a0,r,o);case
2:var
h=b[2];if(h){var
j=h[1];if(0!==j[0])return aG(j[1][2],f)}break;case
3:var
s=b[1],t=a(d[3],eI);return c(v[9],[0,s],[0,i[5],eJ,t]);case
4:var
k=b[3],u=a(g[17][10],[0,k[1],[0,b[4],k[2]]]);return e(g[17][15],a0,f,u);case
7:var
b=b[3];continue}return f}}function
a1(d,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return[0,a1(d,b[1])];case
1:return[1,a1(d,b[1])];case
2:return[2,a1(d,b[1])];case
3:var
o=b[1],p=function(a,b){return ap(bK,a,b)};return[3,aZ(function(a,b){return bJ(p,a,b)},d,o)];case
4:var
q=b[2],r=b[1],t=s[8][7],u=function(a,b){return ap(t,a,b)};return[4,r,aZ(function(a,b){return bJ(u,a,b)},d,q)];case
5:return[5,aZ(eH,d,b[1])];case
6:return[6,aq(s[8][7],d,b[1])];case
7:return[7,aq(s[8][7],d,b[1])];case
8:return[8,aq(s[8][7],d,b[1])];case
9:var
v=b[1],w=aq(s[8][7],d,b[2]);return[9,c(s[8][7],d,v),w];case
10:return[10,ap(s[8][7],d,b[1])];case
11:return[11,ap(s[8][7],d,b[1])];case
12:var
x=b[3],y=b[1],f=cF(b[2],d),n=f[2];return[12,y,n,c(s[8][7],f[1],x)];case
13:var
z=b[1];return[13,z,c(s[8][7],d,b[2])];case
14:return[14,aq(s[8][7],d,b[1])];case
15:var
h=b[2],A=b[3],i=cF(b[1],d),B=i[2];return[15,B,h,aq(bK,a0(i[1],h),A)];case
16:var
C=b[1],D=a(s[8][7],d);return[16,c(g[17][12],D,C)];case
17:var
e=b[2],E=b[1];if(0===e[0])var
j=[0,c(s[8][7],d,e[1])];else
var
k=e[1],l=s[8][7],m=function(a,b){return ap(l,a,b)},j=[1,aZ(function(a,b){return bJ(m,a,b)},d,k)];return[17,E,j];default:return[18,b[1]]}}function
eK(b,a){var
c=a1(b,a[2]);return[0,a[1],c]}function
ar(d,c,b,a){return d?N(U[11],0,eL,c,b,a[1])[1]:N(U[11],0,0,c,b,a[1])[1]}var
a2=[C,function(b){return a(cG[46],bL[30])}];function
aH(d,c,b,a){var
f=e(d,c,b,a[2]);return[0,a[1],f]}function
cH(k,c,b){function
d(c,d){if(0===c[0]){var
e=c[1],f=e[2],g=f[2],b=f[1],i=e[1];return g?[6,v[4],[0,b],0,g[1][1],d]:[6,v[4],[0,b],0,[13,[0,i,[1,[0,b]],0,0]],d]}var
h=c[1],j=a(k,h[2]);return[6,v[4],h[1],0,j,d]}return e(g[17][16],d,c,b)}var
cI=[12,v[4],0];function
bM(i,b,h,d){if(d){var
j=d[1],m=d[2],e=a(f[34],h),g=e[1],n=e[3],k=[0,g,c(z[13],b,e[2])],o=g?[0,a(f[t],g[1]),b]:[0,a(f[ab],0),b],p=0===j[0]?[0,k]:[1,c(i,k,j[1])],l=bM(i,o,n,m);return[0,[0,p,l[1]],l[2]]}return[0,0,c(z[13],b,h)]}function
cJ(f,e,d,c,a,b){var
g=cH(f,a,b);return bM(e,0,N(U[11],0,0,d,c,g)[1],a)}function
as(c,b,a){function
d(a,b){return a}return cJ(function(a){return a[1]},d,c,b,a,cI)[1]}var
eP=a(o[1][6],eO);function
cK(b,a){if(0===a[0]){var
f=a[2],d=a[1];if(f){var
h=b[1];b[1]=[0,[0,[0,d,f[1]],h[1]],h[2]];return a}var
i=b[1],j=i[2],k=i[1],e=c(at[25],eP,j);b[1]=[0,[0,[0,d,e],k],[0,e,j]];return[0,d,[0,e]]}var
l=a[4],m=a[3],n=a[2],o=a[1];function
p(a){return cK(b,a)}return[1,o,n,c(g[17][12],p,m),l]}function
bN(b){if(0===b[0]){var
j=b[2],m=b[1];if(j)return[1,[0,m,j[1]]];var
n=a(d[3],eQ);return e(i[3],0,0,n)}var
k=b[2],l=k[1],o=b[3],p=b[1],q=a(ac[26],l)[1],s=c(g[17][12],bN,o),f=q[6],h=s;for(;;){if(0<f){var
r=[0,[13,[0,v[4],[5,l,f],0,0]],h],f=f-1|0,h=r;continue}return cD([0,p,[0,[0,v[4],[3,k],0]],h])}}function
eR(b){var
c=b[2],d=c[2],a=c[1],e=b[1];if(d){var
f=d[1];return function(b){return[6,v[4],[0,a],0,f[1],b]}}return function(b){return[6,v[4],[0,a],0,[13,[0,e,[1,[0,a]],0,0]],b]}}function
eS(a,c){var
b=a[2];return[6,v[4],[0,b],0,[13,[0,a[1],[1,[0,b]],0,0]],c]}function
eT(a,b){var
c=a[1],d=bN(a[2]);return[7,v[4],[0,c],d,b]}function
cL(b,a){if(0===a[0])return b;var
c=a[4],d=a[3],f=c?[0,[0,c[1],a],b]:b;return e(g[17][15],cL,f,d)}function
a3(j,b,i,h){if(h){var
k=h[2],d=a(j,i),e=d[1],l=d[3],m=[0,e,c(z[13],b,d[2])];if(e){var
g=a3(j,[0,a(f[t],e[1]),b],l,k);return[0,[0,m,g[1]],g[2],g[3]]}throw[0,M,eX]}return[0,0,b,c(z[13],b,i)]}function
cM(b,i,h){if(h){var
j=h[2],d=a(f[36],i),e=d[1],k=d[4],l=d[2],m=c(z[13],b,d[3]),n=[0,e,[0,c(z[13],b,l),m]];if(e){var
g=cM([0,a(f[t],e[1]),b],k,j);return[0,[0,n,g[1]],g[2],g[3]]}throw[0,M,eY]}return[0,0,b,c(z[13],b,i)]}function
a4(k,j,d,b){var
f=e(k,j,d,b[1]),l=f[2],m=f[1],n=b[3],o=a(s[12][28],0),p=a(s[12][2][6],o),q=c(ah[15],p,n),r=b[2];function
h(a){return N(U[11],0,0,m,d,a[1])[1]}var
i=a(g[17][12],h);return[0,l,a(a(ah[15],i),r),q]}function
bO(d,a,c,b){return[0,a,e(d,a,c,b)]}function
e8(b,j,h){var
d=h[2],k=h[1];if(0===d[0])var
m=d[1][1],n=function(a,b){return a},l=cJ(function(a){return a[1]},n,b,j,k,m),f=[0,l[1],[0,l[2]]];else
var
q=d[1]?a(i[7],e9):[0,as(b,j,k),d],f=q;var
o=f[1];function
p(f,e){var
a=f[1],d=a[1];return d?c(aY[30],[0,d[1],a[2]],e):b}return[0,e(g[17][16],p,o,b),f]}function
e_(b){var
c=b[2],d=c[2],a=c[1],e=b[1];if(d){var
f=d[1];return function(b){return[5,v[4],[0,a],0,f[1],b]}}return function(b){return[5,v[4],[0,a],0,[13,[0,e,[1,[0,a]],0,0]],b]}}function
a5(m,h,j,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return[0,a5(m,h,j,b[1])];case
1:return[1,a5(m,h,j,b[1])];case
2:return[2,a5(m,h,j,b[1])];case
3:var
bd=b[1],be=1,bf=function(b,c,a){return 0===a[0]?[0,ar(be,b,c,a[1])]:[1,a[1]]},bg=function(a,b,c){return aH(bf,a,b,c)};return[3,a4(function(a,b,c){return bO(bg,a,b,c)},h,j,bd)];case
4:var
bh=b[2],bi=b[1],ak=a(y[12],h),ah=c(aY[36],ak,h),ae=e(P[42],0,P[9],h),af=a(P[36],ah),ag=c(P[47],ae,af),p=a(f[aE],ag);if(9===p[0]){var
F=p[2],G=K(a2),ai=p[1],aj=L===G?a2[1]:C===G?a(I[2],a2):a2;if(c(f[132],ai,aj))if(3===F.length-1)var
z=S(F,0)[1],B=1;else
var
B=0;else
var
B=0;if(!B)var
z=a(i[7],eN)}else
var
z=a(i[7],eM);var
bj=function(a,b,c){return N(U[11],0,[0,[0,z]],a,b,c[1])[1]},bk=function(a,b,c){return aH(bj,a,b,c)};return[4,bi,a4(function(a,b,c){return bO(bk,a,b,c)},h,j,bh)];case
5:return[5,a4(e8,h,j,b[1])];case
6:return[6,as(h,j,b[1])];case
7:return[7,as(h,j,b[1])];case
8:return[8,as(h,j,b[1])];case
9:var
bl=b[1],bm=as(h,j,b[2]);return[9,ar(0,h,j,bl),bm];case
10:var
bn=b[1],bo=1;return[10,aH(function(a,b,c){return ar(bo,a,b,c)},h,j,bn)];case
11:var
bp=b[1],bq=1;return[11,aH(function(a,b,c){return ar(bq,a,b,c)},h,j,bp)];case
12:var
$=b[2],br=b[1],bb=e(g[17][16],e_,$,b[3][1]),bc=N(U[11],0,0,h,j,bb)[1],_=a3(f[35],0,bc,$);return[12,br,_[1],_[3]];case
13:var
bs=b[1];return[13,bs,ar(1,h,j,b[2])];case
14:return[14,as(h,j,b[1])];case
15:var
aa=b[3],ab=b[2],n=b[1],O=m[1];if(O){var
Q=O[1];if(typeof
Q==="number")var
E=0;else
var
k=Q[2],E=1}else
var
E=0;if(!E)var
k=a(i[7],eZ)[2];var
T=a(ac[26],k[3]),r=T[1][6]-k[7]|0,au=T[2];if(1-(a(g[17][1],n)===r?1:0)){var
av=a(d[3],e0),aw=a(d[13],0),ax=0===r?a(d[3],e1):a(d[16],r),ay=a(d[3],e2),az=c(d[12],ay,ax),aA=c(d[12],az,aw),aB=c(d[12],aA,av);e(i[6],0,e3,aB)}var
aC=[0,[0,v[4],[2,k[3]],0]],aD=k[6],aF=function(a){return ch(cN[6],0,0,0,h,u[16],a)},aG=c(g[17][12],aF,aD),aI=function(a){return[1,[0,a[1],a[2][1]]]},aJ=c(g[17][12],aI,n),aK=function(a){return[13,[0,v[4],e4,0,0]]},aL=c(g[17][48],au[6],aK),aM=c(g[18],aJ,aL),aN=c(g[18],aG,aM),aO=cD([0,v[4],aC,aN]),H=c(eU[8],h,ab),q=H[2],ao=H[1];if(q)if(q[2])var
ap=a(d[3],eV),l=e(i[3],0,0,ap);else
var
J=q[1],M=J[2],aq=J[1],an=cL(0,M),al=function(d,b,a){return[0,[0,d,c(o[1][13][3],b,a)],a]},am=e(o[1][11][11],al,aq,an),l=[0,ao,a(g[17][6],am),M];else
var
at=a(d[3],eW),l=e(i[3],0,0,at);var
V=l[3],W=l[2],X=l[1],Y=[0,[0,0,X]],aP=cH(function(b){if(0===b[0])return b[1][1];var
f=b[1];if(f){var
g=f[1];if(1-c(o[1][13][2],g,X)){var
h=a(d[3],e5),j=a(R[1],g),k=a(d[3],e6),l=c(d[12],k,j),m=c(d[12],l,h);e(i[6],0,e7,m)}return[12,v[4],0]}return[12,v[4],0]},aa,cI),aQ=cK(Y,V),Z=a(g[17][6],Y[1][1]),aR=bN(aQ),aS=e(g[17][16],eT,W,[7,v[4],0,[14,v[4],aR,[0,aO]],aP]),aT=e(g[17][16],eS,Z,aS),aU=e(g[17][16],eR,n,aT),aV=N(U[11],0,0,h,j,aU)[1],s=a3(f[34],0,aV,n),aW=s[1],t=a3(f[34],s[2],s[3],Z),aX=t[1],w=cM(t[2],t[3],W),aZ=w[2],a0=w[1],x=a(f[36],w[3]),a1=x[4],a6=x[3],a7=x[2],a8=function(b,a){var
c=a[2];return 0===c[0]?[0,b[1],[0,b[2]]]:[0,a[1],[1,c[1]]]};return[15,aW,[0,aX,a0,a7,a6,V,ab],bM(a8,aZ,a(D[56],a1),aa)[1]];case
16:var
bt=b[1],bu=function(a){return N(U[11],0,0,h,j,a[1])[1]};return[16,c(g[17][12],bu,bt)];case
17:var
A=b[2],bv=b[1];if(0===A[0])var
ad=[0,N(U[11],0,0,h,j,A[1][1])[1]];else
var
a9=A[1],a_=1,a$=function(a,b,c){return ar(a_,a,b,c)},ba=function(a,b,c){return aH(a$,a,b,c)},ad=[1,a4(function(a,b,c){return bO(ba,a,b,c)},h,j,a9)];return[17,bv,ad];default:return[18,b[1]]}}var
aI=[0,eK,function(d,c,b,a){var
e=a5(d,c,b,a[2]);return[0,a[1],e]}];ay(431,aI,"Decl_mode_plugin.Decl_interp");function
ai(p,k){var
f=k[2],b=k[1],q=o[1][10][1];function
r(b,a){return c(o[1][10][4],a,b)}var
s=e(g[17][15],r,q,p),t=c(B[4][1],f,b),v=c(B[4][2],f,b),w=c(B[4][4],f,b),l=[0,a(u[98],f)];try{var
F=N(cO[53],t,l,v,w,s),h=F}catch(b){b=H(b);if(b[1]!==cO[51])throw b;var
x=a(R[1],b[2]),y=a(d[3],e$),z=c(d[12],y,x),h=e(i[6],0,0,z)}var
m=l[1],A=h[2],C=h[1],D=c(B[4][5],m,b),j=E(B[4][6],m,C,A,D),n=j[1];return[0,[0,n,0],E(B[4][8],j[3],b,n,j[2])]}function
ad(a){return c(y[8],a[2],a[1])}var
cP=[0,0];function
fa(a){cP[1]=a;return 0}function
cQ(a){return cP[1]}c(fd[4],0,[0,1,0,fc,fb,cQ,fa]);function
cR(g,b){var
d=a(m[1],b),h=a(m[7],b),i=a(m[2],b),j=c(B[4][2],i,d),k=a(m[2],b),l=a(g,c(B[4][5],k,d)),n=a(m[2],b),f=E(B[4][6],n,j,h,l),o=f[1];return[0,[0,o,0],e(B[4][7],f[3],d,f[2])]}function
V(b,a){return cR(function(a){return e(u[3][2],a,y[7],b)},a)}function
aJ(a){return cR(function(a){return c(u[3][4],a,y[7])},a)}function
aK(b){var
d=a(m[8],b),f=e(P[42],0,P[9],d);return function(b){var
d=a(P[36],b);return c(P[47],f,d)}}function
cS(g,b){var
d=c(au[4],g,b),e=d[2],f=0===e[6]?1:0,h=d[1],i=f?1-a(bP[19],[0,b,h,e]):f;return i}function
cT(b){var
c=1-a(x[7],b);if(c){var
d=a(y[10],b);if(d)if(typeof
d[1]!=="number")return a(i[7],fe);var
e=0}else
var
e=c;return e}function
ff(c,b){var
d=a(u[69],b),f=a(u[98],d);function
h(a,b){return E(u[95],a[1],a[2],0,b)}return e(g[17][16],h,c,f)}function
fg(b){return 95===J.caml_string_get(a(o[1][8],b),0)?1:0}function
fh(d){function
e(a){if(a){var
d=a[1],b=e(a[2]),f=function(e){var
a=[0,d,0];function
b(b){return ai(a,b)}return c(j[21],b,e)};return c(j[5],f,b)}return j[1]}var
b=a(m[9],d);if(b)var
h=a(D[84],b[2]),f=c(g[17][29],fg,h);else
var
f=0;return a(e(f),d)}function
aj(b,a){return c(p[138],[0,b],a)}function
fi(a){return V(fj,a)}function
fk(d){var
b=a(n[67][1],fi);a(cB[21],b);var
c=a(G[12],0);return a(y[14],c)}function
bQ(c){var
d=a(ac[2],0);function
b(g,f){var
c=a(n[37],n[55]),b=e(x[29],d,c,f);return[0,b[1],b[2][1]]}return a(G[26],b)?0:e(fl[4],0,0,3)}function
ak(b){return a(y[15],0)}function
fm(a){return ak(0)}function
fn(c){try{bQ(0);var
b=ak(0);return b}catch(b){b=H(b);if(b===_)return a(i[7],fo);throw b}}function
fp(a){return fn(0)}function
cU(b){if(a(x[7],b)){var
c=a(y[11],b);if(c){var
g=c[1];if(typeof
g!=="number"){var
l=a(d[3],fx);return e(i[3],0,0,l)}if(0===g){var
h=c[2];if(h)if(typeof
h[1]!=="number")return ak(0)}}return a(i[7],fw)}var
f=a(y[10],b);if(f){var
j=f[1];if(typeof
j!=="number")return 0;if(0===j){var
k=f[2];if(k)if(typeof
k[1]!=="number"){bQ(0);return ak(0)}}}return a(i[7],fy)}var
fA=a(o[1][6],fz);function
bR(i,d){var
b=[0,o[1][10][1]];function
g(h,d){var
i=a(f[aE],h);if(1===i[0]){b[1]=c(o[1][10][4],i[1],b[1]);return a(j[1],d)}var
g=c(m[20],fA,d);b[1]=c(o[1][10][4],g,b[1]);var
k=a(p[75],[0,g,0]),l=a(n[67][8],k),q=N(p[d_],0,[0,g],h,0,cV[7]),r=a(n[67][8],q);return e(j[5],r,l,d)}var
h=e(j[32],g,i,d),k=0,l=b[1],q=[0,function(b){function
d(d){var
b=a(aF[2][1][1],d);if(c(o[1][10][3],b,l))return j[1];var
e=[0,b,0];function
f(a){return ai(e,a)}return a(j[21],f)}var
f=a(m[9],b);return e(j[32],d,f,b)},k],r=[0,function(a){return h},q];return c(j[7],r,d)}var
fC=a(d[3],fB),fD=c(i[2],0,fC),cW=[0,c(n[18],0,fD)];function
fE(a){cW[1]=a;return 0}function
fF(a){return cW[1]}var
fG=a(n[13],0),a6=c(n[14],fG,fF);function
fH(b){return a(d[22],fI)}var
fL=E(cX[2],fK,fJ,0,fH);function
aL(d,b){function
f(b){return cQ(0)?a(i[7],fM):(c(fL,0,0),a(y[1],0),[0,0,a(u[69],b)])}var
g=a(n[67][8],p[41]),h=[0,c(j[5],d,g),0],k=a(j[20],h);return e(j[4],k,f,b)}function
cY(d,b){var
e=a(n[67][8],a6);function
f(a){return bR(d,a)}return aL(c(j[5],f,e),b)}function
W(b,a){return[C,function(c){return e(bL[4],fN,b,a)}]}var
a7=W(fP,fO),a8=W(fR,fQ),a9=W(fT,fS),a_=W(fV,fU),a$=W(fX,fW),ba=W(fZ,fY),bb=W(f1,f0),bc=W(f3,f2),bd=W(f5,f4),be=W(f7,f6);function
bS(d,b,a){if(a){var
e=a[2],f=a[1];return d===f[1]?c(g[18],b,e):[0,f,bS(d,b,e)]}throw _}function
f8(d,b,p,k){var
q=b[2],r=a(aK(k),q),j=a(f[39],r),l=j[2],n=a(f[aE],j[1]);if(11===n[0]){var
h=n[1],i=h[1],s=h[2];if(cS(d,i)){var
o=c(au[4],d,i),t=c(au[17],h,[0,o[1],o[2]]),v=function(q,o){var
r=[0,a(f[127],[0,[0,i,q+1|0],s]),l],t=a(f[59],r),v=c(f[76],o,l),w=c(cZ[26],d,v)[1];function
n(i,d,b){if(b){var
g=i+1|0,p=b[2],q=b[1],h=n(g,[0,a(f[ab],g),d],p),r=h[3],s=h[2],t=h[1],u=a(aF[1][1][3],q),v=c(z[13],d,u),j=a(m[8],k),l=e(P[42],0,P[14],j),o=a(P[36],v);return[0,t,s,[0,[0,g,c(P[46],l,o)],r]]}return[0,i,d,0]}var
x=a(g[17][6],w),h=n(b[3],0,x),j=h[3],y=h[1],A=[0,t,a(g[17][6],h[2])],B=a(f[59],A),C=e(u[96],b[1],[0,B,f9],b[5]),D=bS(b[1],j,b[4]);function
F(b,a){return E(u[95],a[1],a[2],0,b)}var
G=e(g[17][15],F,C,j),H=a(g[17][6],j);function
I(a){return c(g[22][3],[0,a[1],a[2],y,D,G],p)}return c(g[17][11],I,H)};return c(g[19][14],v,t)}}return 0}function
c0(b,g){var
a=g;for(;;){if(a){var
d=a[2],e=a[1],f=e[1],h=e[2];if(c(u[88],b,f)){var
a=d;continue}var
i=c0(b,d);return[0,[0,f,c(bf[92],b,h)],i]}return 0}}function
c2(h,W,n){var
j=a(m[7],n),s=a(u[69],n),b=a(m[8],n),k=e(c3[4],b,[0,s],j),X=a(f[el],k);function
v(w,u,q,o){if(o){var
x=o[2],y=o[1],z=y[1],b=c(D[55],q,y[2]),Y=e(at[9],w,b,0),h=e(p[14],u,Y,n),A=c(aY[30],[0,h,b],w),Z=e(c3[4],A,[0,s],b),B=a(f[el],Z),_=[0,[0,z,a(f[t],h)],q];if(a(g[17][47],x)){var
$=c(D[55],_,W);return[0,B,b,e(f[50],h,b,$)]}var
r=v(A,[0,h,u],[0,[0,z,a(f[t],h)],q],x),k=r[2],E=r[1],m=e(f[50],h,b,r[3]),aa=a(f[t],h);if(c(D[54],aa,k)){var
l=e(f[50],h,b,k);if(0===E){if(0===X){var
F=K(a$),ab=[0,b,l],ac=L===F?a$[1]:C===F?a(I[2],a$):a$,G=K(ba),ad=a(f[O],[0,ac,ab]),ae=[0,b,l,j,m],af=L===G?ba[1]:C===G?a(I[2],ba):ba;return[0,0,ad,a(f[O],[0,af,ae])]}var
H=K(bb),ag=[0,b,l],ah=L===H?bb[1]:C===H?a(I[2],bb):bb,J=a(f[O],[0,ah,ag]),M=K(bc),ai=[0,b,l,a(f[bA],[0,0,J,j]),m],aj=L===M?bc[1]:C===M?a(I[2],bc):bc;return[0,2,J,a(f[O],[0,aj,ai])]}var
N=K(bd),ak=[0,b,l],al=L===N?bd[1]:C===N?a(I[2],bd):bd,P=a(f[O],[0,al,ak]),Q=K(be),am=[0,b,l,a(f[bA],[0,0,P,j]),m],an=L===Q?be[1]:C===Q?a(I[2],be):be;return[0,2,P,a(f[O],[0,an,am])]}if(0===B)if(0===E){var
R=K(a7),ao=[0,b,k],ap=L===R?a7[1]:C===R?a(I[2],a7):a7,S=K(a8),aq=a(f[O],[0,ap,ao]),ar=[0,b,k,j,m],as=L===S?a8[1]:C===S?a(I[2],a8):a8;return[0,0,aq,a(f[O],[0,as,ar])]}var
T=K(a9),au=[0,b,k],av=L===T?a9[1]:C===T?a(I[2],a9):a9,U=a(f[O],[0,av,au]),V=K(a_),aw=[0,b,k,a(f[bA],[0,0,U,j]),m],ax=L===V?a_[1]:C===V?a(I[2],a_):a_;return[0,2,U,a(f[O],[0,ax,aw])]}var
ay=a(d[3],f$);return e(i[3],0,ga,ay)}var
l=v(b,0,0,h)[3],o=[0,l,[0,a(f[ab],1)]];return a(f[O],o)}function
aM(C,B,k,d){try{var
o=a(m[8],d),b=a(m[7],d),t=ff([0,[0,0,b],k],d),h=a(g[22][2],0),F=0,v=0,w=function(b,a){return c($[5],b,a[1])},x=[0,0,b,e(g[17][15],w,v,k),[0,[0,0,b],0],t];c(g[22][3],x,h);var
q=function(c){for(;;){var
b=a(g[22][9],h);try{var
j=b[2],l=[0,a(c1[5],0)],m=ch(c1[8],o,b[5],1,l,B,j);if(0<c)var
f=q(c-1|0);else
var
n=e(u[96],b[1],[0,C,f_],m),p=bS(b[1],k,b[4]),f=[0,b[1],b[2],b[3],p,n];return f}catch(c){c=H(c);if(a(i[21],c)){f8(o,b,h,d);continue}throw c}}};try{var
j=q(F)}catch(a){a=H(a);if(a===g[22][1])throw _;throw a}var
y=a(f[ab],0),z=c(bf[92],j[5],y),A=[0,c0(j[5],j[4]),z],l=A}catch(b){b=H(b);if(b!==_)throw b;var
l=a(i[7],gb)}var
r=l[2],s=l[1];if(a(g[17][47],s)){var
D=a(p[45],r);return c(n[67][8],D,d)}var
E=c2(s,r,d);return c(m[45],E,d)}function
c4(d,c,b){return 0===b[0]?b[1]:b[1]?a(i[7],gc):a(m[7],c)}function
c5(h,e,r,b){if(h)try{var
o=a(m[8],b),p=a(y[12],o),q=[0,a(f[t],p),0],d=q}catch(b){b=H(b);if(b[1]!==i[5])throw b;var
d=a(i[7],gd)}else
var
d=0;function
k(b){var
f=e[2];return f?bR(c(g[18],d,f[1]),b):a(j[1],b)}function
l(b){var
d=e[3];if(d){var
f=d[1],g=a(s[12][28],0),h=c(s[12][19],g,f);return c(n[67][8],h,b)}return c(n[67][8],a6,b)}return aL(c(j[5],k,l),b)}function
c6(r,q,p,g,b){var
h=ad(b),i=g[1],k=i[1];if(k)var
d=k[1];else
var
y=a(o[1][6],ge),d=c(m[20],y,b);var
l=e(r,h,b,i[2]),s=[0,function(b){return q?aM(a(f[t],d),l,0,b):a(j[1],b)},0];function
u(a){return c5(p,g,h,a)}var
v=[0,c(j[5],aJ,u),s],w=aj(d,l),x=a(n[67][8],w);return e(j[11],x,v,b)}var
Q=[C,function(b){return a(cG[46],bL[30])}];function
gf(h,e){var
j=c(m[19],e,h),k=a(aK(e),j),d=a(f[aE],k);if(9===d[0]){var
b=d[2],g=K(Q),l=d[1],n=L===g?Q[1]:C===g?a(I[2],Q):Q;if(c(D[64],l,n))if(3===b.length-1){var
o=S(b,2)[3],p=S(b,1)[2];return[0,S(b,0)[1],p,o]}return a(i[7],gh)}return a(i[7],gg)}function
gi(E,D,d,b){try{var
an=a(m[8],b),ao=a(y[12],an),g=ao}catch(b){b=H(b);if(b[1]!==i[5])throw b;var
g=a(i[7],gj)}var
k=gf(g,b),l=k[3],q=k[2],r=k[1];function
F(b){var
c=d[2];return c?bR(c[1],b):a(j[1],b)}function
G(b){var
e=d[3];if(e){var
f=e[1],g=a(s[12][28],0),h=c(s[12][19],g,f);return c(n[67][8],h,b)}return c(n[67][8],a6,b)}function
u(a){return aL(c(j[5],F,G),a)}var
v=d[1][1];if(v)var
h=v[1];else
var
am=a(o[1][6],gk),h=c(m[20],am,b);function
w(c,b){return E?aM(a(f[t],h),c,0,b):a(j[1],b)}if(0===D){var
x=K(Q),J=[0,r,d[1][2],l],M=L===x?Q[1]:C===x?a(I[2],Q):Q,z=a(f[O],[0,M,J]),N=0,P=[0,function(a){return w(z,a)},N],R=a(f[t],g),S=a(p[45],R),T=[0,u,[0,a(n[67][8],S),0]],U=a(p[dL],q),V=a(n[67][8],U),W=c(j[11],V,T),X=[0,c(j[5],aJ,W),P],Y=aj(h,z),Z=a(n[67][8],Y);return e(j[11],Z,X,b)}var
A=K(Q),_=[0,r,q,d[1][2]],$=L===A?Q[1]:C===A?a(I[2],Q):Q,B=a(f[O],[0,$,_]),aa=0,ab=[0,function(a){return w(B,a)},aa],ac=a(f[t],g),ad=a(p[45],ac),ae=[0,a(n[67][8],ad),[0,u,0]],af=a(p[dL],l),ag=a(n[67][8],af),ah=c(j[11],ag,ae),ai=[0,c(j[5],aJ,ah),ab],ak=aj(h,B),al=a(n[67][8],ak);return e(j[11],al,ai,b)}function
c7(h,d,b){var
k=ad(b),i=d[1];if(i)var
g=i[1];else
var
x=a(o[1][6],gl),g=c(m[20],x,b);function
l(b){if(h){var
c=d[2];return aM(a(f[t],g),c,0,b)}return a(j[1],b)}var
p=k[1],q=h?2:1,r=[0,[0,q,p]],s=0,u=[0,l,[0,function(a){return V(r,a)},s]],v=aj(g,d[2]),w=a(n[67][8],v);return e(j[11],w,u,b)}function
aN(f,e,d){if(e)var
b=e[1];else
var
k=a(o[1][6],gm),b=c(m[20],k,d);var
g=[0,a(f,b),0],h=a(p[23],b),i=[0,a(n[67][8],h),g];return c(j[7],i,d)}function
bg(d,b){var
e=j[1];function
f(d){var
b=d[1],e=b[1];function
f(d){var
e=c(p[4],0,[0,d,b[2]]);return a(n[67][8],e)}function
g(a){return aN(f,e,a)}return a(j[5],g)}return E(g[17][16],f,d,e,b)}function
c8(b,a){return a?[0,b,c8(b+1|0,a[2])]:0}function
bT(b,e){if(b){var
d=b[1][1],j=bT(b[2],e),g=c(z[8],1,j),h=d[1];if(h)var
k=a(f[t],h[1]),i=c(D[59],k,g);else
var
i=g;return a(f[bw],[0,d[1],d[2],i])}return e}function
c9(d,b){if(b){var
e=b[1],h=b[2],i=a(f[34],d)[2],j=[0,a(f[ab],e),0],g=c9(c(f[76],d,j),h);return[0,[0,[0,e,i],g[1]],g[2]]}return[0,0,d]}function
go(n,b){var
o=c(m[19],b,n),p=a(aK(b),o),e=a(f[39],p),q=e[2],r=e[1],d=a(m[8],b),h=a(f[aE],r);if(11===h[0]){var
i=h[1],j=i[1];if(cS(d,j)){var
k=c(au[4],d,j),l=c(au[17],i,[0,k[1],k[2]]);if(1-(1===l.length-1?1:0))throw _;var
s=S(l,0)[1],t=c(f[76],s,q),u=c(cZ[26],d,t)[1];return a(g[17][1],u)}}throw _}function
c_(g,f,d,b){if(0<g){var
i=a(o[1][6],gp),h=c(m[20],i,b),k=[0,h,f],l=g-1|0,q=function(a){return c_(l,k,d,a)},r=a(p[23],h),s=a(n[67][8],r);return e(j[5],s,q,b)}return c(d,f,b)}function
ae(h,l,k,b,e){if(k){if(b){var
r=b[1][1],s=b[2],q=k[2],d=k[1],y=function(e){try{var
x=go(d,e),k=x}catch(b){b=H(b);if(b!==_)throw b;var
k=a(i[7],gq)}var
m=0;function
o(a){var
d=c(g[17][8],a,q);return function(a){return ae(h,l,d,b,a)}}var
r=0,s=[0,function(a){return c_(k,r,o,a)},m],u=a(f[t],d),v=a(p[eo],u),w=[0,a(n[67][8],v),s];return c(j[7],w,e)},u=r[1];if(u)var
v=u[1],z=0,A=[0,[0,v,1],l],B=[0,function(a){return ae(h,A,q,s,a)},z],C=a(p[81],[0,[0,d,v],0]),D=[0,a(n[67][8],C),B],w=a(j[7],D);else
var
I=[0,[0,d,0],l],w=function(a){return ae(h,I,q,s,a)};var
F=c(p[4],0,[0,d,r[2]]),G=a(n[67][8],F);return E(j[33],G,w,y,e)}return a(i[7],gr)}if(b){if(h){var
J=a(o[1][6],gs),x=c(m[20],J,e),K=function(b){return a(i[7],gt)},L=[0,x,0],M=0,N=1,O=function(a){return ae(N,M,L,b,a)},P=a(p[23],x),Q=a(n[67][8],P);return E(j[33],Q,O,K,e)}return a(i[7],gu)}return a(j[1],e)}function
c$(d,b){if(d){var
f=d[1],g=d[2],h=c(m[15],b,f),i=function(a){return c$(g,a)},k=0,l=function(a){return aM(f,h,k,a)};return e(j[5],l,i,b)}return a(j[1],b)}function
da(b,d){if(b){var
e=b[1],i=da(b[2],d),j=c(z[8],1,i),g=e[1];if(g){var
h=g[1],k=a(f[t],h),l=c(D[59],k,j);return a(f[bA],[0,[0,h],e[2],l])}throw[0,M,gw]}return d}function
gy(c){var
d=c[1],e=c[2];if(d){var
f=d[1],b=a(bU[6],e)[1];if(typeof
b!=="number"&&0===b[0])if(b[1][2]===f)return 1;return 0}return 0}function
db(i,h){var
b=i;for(;;){var
e=b[1],d=a(bU[6],b[2]);if(typeof
d[1]==="number"){var
b=[0,0,a(ac[26],h)[2][12]];continue}var
f=d[2],j=function(b){var
d=a(bU[6],b)[2];function
f(a){return[0,e,a]}return c(g[19][15],f,d)};return c(g[19][15],j,f)}}function
bh(e,a,d,b){var
f=db(d,a);function
h(d,a){var
e=c(b,d,a);return[0,c(g[19][15],gy,a),e]}return[1,e,a,c(g[19][16],h,f)]}function
dc(e,d,b){if(1===b[0]){var
f=b[3],h=b[2],i=b[1],j=function(b,a){var
e=a[1];return[0,e,c(d,b,a[2])]},k=c(g[19][16],j,f);return[1,a(e,i),h,k]}return a($[2],gA)}function
dd(d,b,a){function
c(b,a){return 0}return bh(o[1][10][1],b,a,c)}function
de(v,d,b){var
l=a(m[7],b),n=a(m[8],b),h=c(m[15],b,d),w=c(D[45],d,l),x=a(aK(b),h),o=a(f[39],x),y=o[2],z=o[1];try{var
I=a(f[43],z),p=I}catch(b){b=H(b);if(b!==f[28])throw b;var
p=a(i[7],gB)}var
j=p[1],q=a(ac[26],j),r=q[1],A=q[2];if(0===v)var
k=r[6],s=0;else
var
k=r[7],s=[0,j[2]];var
t=c(g[17][99],k,y),u=t[2],B=t[1];function
C(a,d){var
e=c(m[15],b,a),f=[0,e,c(D[59],a,d)];return c(at[17],n,f)}var
E=[0,h,c(D[59],d,l)],F=c(at[17],n,E),G=e(g[17][16],C,u,F);return[0,w,[0,d,h,j,G,u,B,k,[0,s,A[12]]]]}function
bV(c,b,a){return 0<a?[0,b,bV(c,b,a-1|0)]:[2,c]}function
aO(b,c){var
i=b[1];if(c){var
d=c[2],f=c[1];if(f){var
j=f[2],k=f[1],h=k[1],m=k[2];if(0===h[0]){var
n=aO(b,[0,j,d]);return[0,a(o[1][10][5],i),n]}var
l=h[2],p=h[3],q=l[2],r=l[1],s=function(f,c){if(f===(q-1|0)){var
h=0,k=function(a,b){return[0,b,S(c,a)[a+1]]},l=aO(b,[0,e(g[17][69],k,h,p),[0,j,d]]);return[0,[0,a(o[1][10][5],i),l]]}return 0};return bh(o[1][10][1],r,m,s)}return[2,aO(b,d)]}return[3,b]}function
av(f,l,b){var
m=f[1];if(l){var
h=l[2],n=l[1];if(n){var
k=n[2],r=n[1],s=r[2],p=r[1];if(0===p[0])switch(b[0]){case
0:var
C=b[1],D=av(f,[0,k,h],b[2]);return[0,c(o[1][10][4],m,C),D];case
1:var
E=function(b,a){return bi(f,1,[0,k,h],a)};return dc(a(o[1][10][4],m),E,b);default:var
F=a(d[3],gI);return e(i[3],0,0,F)}var
t=p[3],u=p[2],v=u[2],w=u[1];switch(b[0]){case
0:var
x=b[2],j=b[1];return bh(j,w,s,function(b,a){if(b===(v-1|0)){var
d=0,i=function(b,c){return[0,c,S(a,b)[b+1]]},l=e(g[17][69],i,d,t),n=av(f,[0,l,[0,k,h]],bV(x,j,a.length-1));return[0,[0,c(o[1][10][4],m,j),n]]}return[0,[0,j,bV(x,j,a.length-1)]]});case
1:if(1-c(o[37],w,b[2]))a(i[7],gJ);if(1===b[0]){var
q=b[2],y=b[3],z=b[1],A=db(s,q),B=function(a,b){var
c=b[2],i=b[1],j=S(A,a)[a+1];if(a===(v-1|0))var
l=0,m=function(a,b){return[0,b,S(j,a)[a+1]]},d=bi(f,0,[0,e(g[17][69],m,l,t),[0,k,h]],c);else
var
d=c;return[0,i,d]};return[1,z,q,c(g[19][16],B,y)]}return a($[2],gz);default:var
G=a(d[3],gK);return e(i[3],0,0,G)}}if(2===b[0])return[2,av(f,h,b[1])];var
H=a(d[3],gL);return e(i[3],0,0,H)}if(3===b[0])return[3,b[1]];var
I=a(d[3],gM);return e(i[3],0,0,I)}function
bi(b,h,e,d){var
f=b[1];if(d){var
g=d[1],i=g[1],j=bj(b,h,e,g[2]);return[0,[0,c(o[1][10][4],f,i),j]]}var
k=aO(b,e);return[0,[0,a(o[1][10][5],f),k]]}function
bj(f,h,g,b){var
j=f[1];if(0<h)switch(b[0]){case
0:var
k=b[1],l=bj(f,h,g,b[2]);return[0,c(o[1][10][4],j,k),l];case
1:var
m=function(b,a){return bi(f,h+1|0,g,a)};return dc(a(o[1][10][4],j),m,b);case
2:return[2,bj(f,h-1|0,g,b[1])];default:var
n=a(d[3],gN);return e(i[3],0,0,n)}return av(f,g,b)}function
df(e,d){var
a=d;for(;;){if(a){var
b=a[1],f=a[2];if(c(o[2][5],b[1],e))return b[2];var
a=f;continue}throw _}}function
bW(h,k,b,j){var
l=a(f[79],k)[1],m=a(f[39],k),p=m[2],q=a(f[43],m[1])[1];if(1-c(o[37],q,b[3])){var
r=a(d[3],gO),s=a(d[13],0),t=e(af[4],j,u[16],h),v=c(d[12],t,s),w=c(d[12],v,r);e(i[6],0,gP,w)}var
n=c(g[17][99],b[7],p),x=n[2];if(1-e(g[17][24],D[64],n[1],b[6])){var
y=a(d[3],gQ),A=a(d[13],0),B=e(af[4],j,u[16],h),C=c(d[12],B,A),E=c(d[12],C,y);e(i[6],0,gR,E)}var
F=c(g[18],x,[0,h,0]),G=b[4],H=a(g[17][1],l),I=[0,c(z[8],H,G),F],J=a(f[59],I),K=c(bf[22],u[16],J);return c(f[64],l,K)}function
bX(g,j,e,b,d){if(e){var
k=e[1];if(0===k[0])var
o=k[1],p=o[2],h=o[1];else{var
i=k[1],l=i[2],y=i[1];if(0!==l[0]){var
q=l[1],A=bX(g,j,e[2],b,d),B=c(z[8],1,A),r=i[1],C=r?c(z[21],r[1],B):b;if(q){var
n=q[1],D=a(f[t],n);try{var
E=df([0,n],g[1]),s=E}catch(a){a=H(a);if(a!==_)throw a;var
s=df([0,n],g[2])[2]}var
u=bW(D,s,j,a(m[8],d))}else
var
u=a(m[7],d);return a(f[bw],[0,i[1],u,C])}var
p=l[1],h=y}var
v=bX(g,j,e[2],b,d),w=c(z[8],1,v),x=h?c(z[21],h[1],w):b;return a(f[bw],[0,h,p,x])}return b}function
dg(j,b,h,i,d){var
k=a(m[8],d),l=bX(b,h,i,bW(b[3],b[4],h,k),d);function
n(b,d){var
g=b[1];if(g){var
h=g[1],i=c(z[8],1,d);return e(f[52],h,b[2],i)}var
j=c(z[8],1,d);return a(f[bw],[0,0,b[2],j])}function
o(b,d){var
e=b[1];if(e){var
g=e[1],h=c(z[8],1,d);return E(f[51],g,b[2][1],b[2][2],h)}var
i=c(z[8],1,d);return a(f[119],[0,0,b[2][1],b[2][2],i])}var
p=e(g[17][16],o,b[2],l),q=c(g[18],j,b[1]);return e(g[17][16],n,q,p)}function
dh(g,f,c,e,d){var
b=d;for(;;){if(typeof
b==="number"){if(0===b)return a(i[7],gS);var
b=[0,dd(f,c[3],c[8])];continue}return[0,av(g,[0,[0,[0,e,c[8]],0],0],b[1])]}}function
di(a){function
b(a){return[0,a,gV]}return c(g[17][12],b,a)}function
dj(j,b){function
f(f){var
b=f[2],k=f[1];if(b)var
c=b[1],g=[0,[0,c[1],[0,j,c[2]]],b[2]];else
var
h=a(d[3],gW),g=e(i[3],0,0,h);return[0,k,g]}return c(g[17][12],f,b)}function
dk(h,f,a){function
b(b){var
a=b[1],d=b[2],e=c(o[1][10][3],a,f)?[0,h]:0;return[0,a,[0,[0,e,0],d]]}return c(g[17][12],b,a)}function
gX(k){var
b=k[2],r=k[1];if(b){var
h=b[1],l=h[1];if(b[2])if(l)var
m=b[2],n=m[1],s=m[2],t=n[2],u=n[1],v=l[1],w=[0,v,a(g[17][6],h[2])],j=[0,[0,u,[0,a(f[59],w),t]],s];else
var
p=b[2],q=p[1],x=p[2],y=q[1],j=[0,[0,y,c(g[18],h[2],q[2])],x];else
var
j=b;var
o=j}else
var
z=a(d[3],gY),o=e(i[3],0,0,z);return[0,r,o]}function
dl(a){return c(g[17][12],gX,a)}function
dm(n,b,h,d){var
p=a(f[t],d),i=c(m[19],h,d),q=a(f[79],i)[1],j=a(f[39],i),r=j[2],s=a(f[43],j[1])[1];if(c(o[37],s,b[3])){var
k=c(g[17][99],b[7],r),u=k[2],v=k[1];try{var
A=e(g[17][24],D[64],v,b[6]),l=A}catch(a){a=H(a);if(a[1]!==gZ)throw a;var
l=0}if(l){var
w=c(g[18],u,[0,p,0]),x=[0,a(f[t],n),w],y=a(f[59],x),z=c(bf[22],h[2],y);return c(f[66],q,z)}throw[0,M,g0]}throw[0,M,g1]}function
g2(b){return a(d[22],g3)}var
g6=E(cX[2],g5,g4,0,g2);function
bk(y,x,r,Q,P,w,O,h){var
l=Q,k=P,b=O;for(;;)switch(b[0]){case
0:var
T=b[2];if(k){var
U=k[2],l=dj(k[1],l),k=U,b=T;continue}var
V=a(d[3],g7);return e(i[3],0,g8,V);case
1:var
q=b[2],W=b[3],X=b[1];if(k){var
s=k[1],Y=k[2],z=a(ac[26],q),Z=z[1][6],_=a(m[7],h),u=a(m[8],h),A=c(m[15],h,s),$=a(aK(h),A),B=a(f[39],$),aa=B[2],C=a(f[43],B[1]),ad=C[2];if(c(o[37],C[1],q)){var
E=c(g[17][99],Z,aa),F=E[1],ae=E[2],af=function(a,b){var
d=c(m[15],h,a),e=[0,d,c(D[59],a,b)];return c(at[17],u,e)},ag=[0,A,c(D[59],s,_)],ah=c(at[17],u,ag),ai=e(g[17][16],af,ae,ah),aj=e(bP[76],u,q,4),ak=c(au[17],[0,q,ad],z),al=function(b){var
d=c(f[76],b,F),e=a(f[88],d);return c(p[15],e,h)},G=c(g[19][15],al,ak),am=function(b,c){return a(f[ab],b+1|0)},an=[0,aj,ai,s,c(g[19][16],am,G)],ao=a(f[129],an),ap=function(b,e,v){var
h=e[2],i=e[1],k=S(G,b)[b+1];function
m(b,c){if(c){var
d=c[1],e=m(b+1|0,c[2]),g=e[3],h=e[2],j=e[1];return S(i,b)[b+1]?[0,[0,a(f[t],d),j],[0,d,h],g+1|0]:[0,[0,a(f[t],d),j],h,g]}if(b===i.length-1)return[0,Y,0,w];throw[0,M,g9]}var
d=m(0,k),z=d[3],A=d[2],B=d[1],C=0;if(h)var
s=h[1],D=s[2],E=s[1],H=function(a){return c(o[1][10][3],a[1],E)},I=c(g[17][29],H,l),J=[0,a(f[124],[0,q,b+1|0]),F],K=dk(a(f[59],J),X,I),u=function(a){return bk(y,x,r,K,B,z,D,a)};else{c(g6,0,0);var
u=a(r,a(f[ab],1))}var
L=[0,u,C],N=[0,function(b){function
d(c){return dm(a(R[12],y),x,b,c)}var
e=c(g[17][12],d,A),f=a(p[cy],e);return c(n[67][8],f,b)},L];function
O(b){var
c=a(p[23],b);return a(n[67][8],c)}var
P=[0,c(j[32],O,k),N];return c(j[7],P,v)},aq=c(g[19][16],ap,W),ar=a(m[45],ao);return e(j[12],ar,aq,h)}throw[0,M,g_]}var
as=a(d[3],g$);return e(i[3],0,ha,as);case
2:var
av=b[1],l=dl(l),b=av;continue;default:var
H=b[1],I=H[2],J=I[1],K=H[1],aw=I[2];if(k){var
ax=a(d[3],hb);return e(i[3],0,hc,ax)}var
v=c(o[1][13][3],K,l);if(v){var
L=v[1];if(!L[1])if(!v[2]){var
az=L[2],aA=function(b){return a(f[ab],b+1|0)},aB=c(g[17][48],J+aw|0,aA),N=c(g[17][99],J,aB),aC=N[1],aD=c(g[17][8],az,N[2]),aE=c(g[17][7],aC,aD),aF=[0,a(f[t],K),aE],aG=a(r,a(f[59],aF)),aH=a(n[67][8],p[17]),aI=c(j[26],w,aH);return e(j[5],aI,aG,h)}}var
ay=a(d[3],hd);return e(i[3],0,0,ay)}}function
dn(i,b){var
d=[0,function(j){var
k=a(dp[6],j),l=a(m[7],b),f=a(m[8],b),g=ch(cN[6],0,0,0,f,u[16],i);function
d(a){return 2===a[0]?[13,[0,v[4],hf,0,0]]:c(he[7],d,a)}var
h=d(g),e=N(U[8],0,f,k,[0,[0,l]],h);return a(dp[21][5],[0,e[2],e[1]])}],e=c(p[160][1],0,d);return c(n[67][8],e,b)}function
ho(a){return V(hp,a)}function
hv(f,m){var
b=m;for(;;)if(typeof
b==="number"){a(y[14],f);return a(G[10],hw)}else
switch(b[0]){case
18:var
h=b[1];if(typeof
h==="number")return 0===h?a(G[10],hx):0;if(0===h[1])return ak(0);var
n=a(x[9],f),o=a(g[17][3],n),k=a(x[34][1],f),p=k[2],q=k[1];try{var
t=a(g[17][3],q),u=c(B[4][1],p,t),l=u}catch(b){b=H(b);if(b[1]===hy)if(J.caml_string_notequal(b[2],hz))var
j=0;else
var
l=a(ac[2],0),j=1;else
var
j=0;if(!j)throw b}try{c(bP[79],l,o);var
s=fm(0);return s}catch(b){b=H(b);if(b[1]===hA[1])if(14===b[3][0]){var
r=a(d[3],hB);return e(i[3],0,0,r)}throw b}case
0:case
1:case
2:var
b=b[1];continue;case
10:case
11:case
14:case
15:case
17:return a(y[14],f);default:return 0}}function
dq(P,r){var
v=P[2];for(;;){if(typeof
v==="number"){cT(r);var
B=1,w=1}else
switch(v[0]){case
18:var
O=v[1];if(typeof
O==="number"){if(a(x[28],r))ak(0);else{var
k=a(x[7],r)?a(y[11],r):a(y[10],r);if(k){var
H=k[1];if(typeof
H==="number")if(0===H){var
W=k[2];if(W){var
X=W[1];if(typeof
X==="number")var
S=1;else
var
Y=X[1],F=1,S=0}else
var
S=1;if(S){var
as=a(d[3],fq);e(i[3],0,0,as);var
C=1,F=0}}else
var
C=0,F=0;else
var
Y=H[1],F=1;if(F)var
C=0===Y?(a(i[7],fr),1):(a(i[7],fs),1)}else
var
C=0;if(!C){if(typeof
O==="number"){switch(O){case
0:if(k)var
s=0,l=0;else
var
l=1;break;case
1:if(k){var
_=k[1];if(typeof
_==="number")if(2===_)var
s=0,l=0,T=0;else
var
T=1;else
var
T=1;if(T)var
l=1}else
var
s=0,l=0;break;default:if(k){var
$=k[1];if(typeof
$==="number")if(2===$)var
l=1,U=0;else
var
U=1;else
var
U=1;if(U)var
s=0,l=0}else
var
s=0,l=0}if(l){bQ(0);ak(0);var
s=1}}else
var
s=0;if(!s)if(k){var
Z=k[1],aY=typeof
Z==="number"?2===Z?(a(i[7],fu),1):0:0;if(!aY)a(i[7],ft)}else
a(i[7],fv)}}var
B=0,w=1}else
var
w=0;break;case
14:case
15:var
w=0;break;case
0:case
1:case
2:var
v=v[1];continue;default:cT(r);var
B=1,w=1}if(!w){cU(r);var
B=1}var
Q=a(G[12],0);if(B){var
ao=a(x[34][1],Q),ap=ao[2],aq=[0,a(g[17][3],ao[1]),ap],ar=a(m[8],aq),aS=c(aI[1],[0,o[1][10][1],ar],P),aT=ad(aq),u=0,q=0,b=E(aI[2],aT,ar,ap,aS)[2];for(;;){if(typeof
b==="number")var
h=ho;else
switch(b[0]){case
0:var
at=b[1];if(q)throw[0,M,hq];var
q=1,b=at;continue;case
1:var
au=b[1];if(u)throw[0,M,hr];var
u=1,b=au;continue;case
2:var
av=b[1];if(!q)if(!u){var
u=1,q=1,b=av;continue}throw[0,M,hs];case
3:var
aw=b[1],h=function(a){return c6(c4,u,q,aw,a)};break;case
4:var
ax=b[2],ay=b[1];if(q)throw[0,M,ht];var
h=function(a){return gi(u,ay,ax,a)};break;case
5:var
aa=b[1],h=function(b){var
h=ad(b),s=a(o[1][6],gn),i=c(m[20],s,b),k=aa[1],d=k[1],l=bT(d,c4(h,b,k[2])),p=c8(1,d),r=c9(l,p),u=r[2],v=r[1],w=c(g[17][12],f[ab],p),x=[0,a(f[t],i),w],y=a(f[59],x),z=[0,function(a){return aM(y,u,v,a)},0],A=0,B=[0,aJ,[0,function(a){return c5(q,aa,h,a)},A]],C=[0,function(a){return bg(d,a)},B],D=[0,a(j[7],C),z],E=aj(i,l),F=a(n[67][8],E);return e(j[11],F,D,b)};break;case
6:var
az=b[1],h=function(a){return bg(az,a)};break;case
7:var
aA=b[1],h=function(a){return bg(aA,a)};break;case
8:var
aB=b[1],h=function(a){return ae(1,0,0,aB,a)};break;case
9:var
af=b[2],ag=b[1],h=function(b){var
h=a(D[67],ag),d=a(f[aE],h);if(1===d[0])return ae(0,0,[0,d[1],0],af,b);var
i=a(o[1][6],gv),g=c(m[20],i,b),k=[0,g,0],l=0,q=0;function
r(a){return ae(q,l,k,af,a)}var
s=c(p[143],[0,g],ag),t=a(n[67][8],s);return e(j[5],t,r,b)};break;case
10:var
aC=b[1],aD=0,h=function(a){return c7(aD,aC,a)};break;case
11:var
aG=b[1],aH=1,h=function(a){return c7(aH,aG,a)};break;case
12:var
aK=b[3],aO=b[2],aP=b[1],h=function(d){var
a=da(aO,aK),b=N(p[d_],0,[0,aP],a,0,cV[7]);return c(n[67][8],b,d)};break;case
13:var
ah=b[2],I=b[1],h=function(b){if(0===I[0]){var
d=I[1],f=c(m[18],b,d),g=c(aF[2][1][4],d,f),h=c(aF[2][1][5],ah,g),j=c(p[4],0,h);return c(n[67][8],j,b)}if(I[1])return a(i[7],gx);var
k=e(p[3],0,ah,2);return c(n[67][8],k,b)};break;case
14:var
al=b[1],h=function(k){var
u=ad(k),w=a(m[7],k),x=a(o[1][6],gH),l=c(m[20],x,k),y=bT(al,w),p=u[1],z=[0,[0,0,u[1]]];if(p){var
b=p[1];if(typeof
b==="number")var
g=0;else{var
q=p[2],h=b[4],r=b[3],s=b[2],t=b[1];if(typeof
r==="number")if(0===r)var
f=[0,h,[0,[0,t,s,0,[0,l,h]],q]],g=1;else
var
f=[0,h,[0,[0,t,s,0,[0,l,h]],q]],g=1;else
var
f=a(i[7],gG),g=1}}else
var
g=0;if(!g)var
v=a(d[3],gF),f=e(i[3],0,0,v);var
A=f[1],B=[0,f[2]],C=0,D=[0,function(a){return V(B,a)},C],E=0,F=[0,function(a){return ai(A,a)},E],G=[0,function(a){return bg(al,a)},F],H=[0,function(a){return V(z,a)},G],I=[0,a(j[7],H),D],J=aj(l,y),K=a(n[67][8],J);return e(j[11],K,I,k)};break;case
15:var
J=b[3],z=b[2],K=b[1],h=function(h){var
t=ad(h),x=a(o[1][6],gT),l=c(m[20],x,h),q=t[1];if(q){var
b=q[1];if(typeof
b==="number")var
s=0;else
var
w=b[1],k=b[2],v=b[3],r=b[4],u=q[2],s=1}else
var
s=0;if(!s)var
y=a(d[3],gU),f=e(i[3],0,0,y),w=f[1],k=f[2],v=f[3],r=f[4],u=f[5];var
A=dg(K,z,k,J,h),B=[0,[0,0,t[1]]],C=z[5],D=a(m[8],h),F=a(g[17][1],J),G=[0,[0,[0,w,k,dh([0,l,[0,a(g[17][1],K),F]],D,k,C,v),[0,l,r]],u]],H=0,I=[0,function(a){return V(G,a)},H],L=0,M=[0,function(a){return ai(r,a)},L],N=[0,function(e){var
b=j[1];function
d(b){if(0===b[0])var
d=b[1],f=d[2],e=d[1];else{var
g=b[1],h=g[2],i=g[1];if(0!==h[0]){var
m=function(a){return j[1]},o=function(a){return aN(m,i,a)};return a(j[5],o)}var
f=h[1],e=i}function
k(b){var
d=c(p[4],0,[0,b,f]);return a(n[67][8],d)}function
l(a){return aN(k,e,a)}return a(j[5],l)}return E(g[17][16],d,J,b,e)},M],O=z[2],P=[0,function(e){var
b=j[1];function
d(b){var
d=b[1];function
e(d){var
e=c(p[4],0,[1,d,b[2][1],b[2][2]]);return a(n[67][8],e)}function
f(a){return aN(e,d,a)}return a(j[5],f)}return E(g[17][16],d,O,b,e)},N],Q=c(g[18],K,z[1]),R=[0,function(e){var
b=j[1];function
d(b){var
d=b[1];function
e(d){var
e=c(p[4],0,[0,d,b[2]]);return a(n[67][8],e)}function
f(a){return aN(e,d,a)}return a(j[5],f)}return E(g[17][16],d,Q,b,e)},P],S=[0,function(a){return V(B,a)},R],T=[0,a(j[7],S),I],U=aj(l,A),W=a(n[67][8],U);return e(j[11],W,T,h)};break;case
16:var
aQ=b[1],h=function(a){return c$(aQ,a)};break;case
17:var
L=b[2],A=b[1],h=function(b){var
l=a(m[8],b),h=ad(b);if(0===L[0]){var
i=de(A,L[1],b),g=i[2],n=i[1]?[0,dd(l,g[3],g[8])]:1;return V([0,[0,[0,A,g,n,0],h[1]]],b)}var
d=L[1];if(0===d[1][1]){var
p=a(o[1][6],gC),k=c(m[20],p,b),q=a(f[t],k),r=[0,[0,[0,k],d[1][2]],d[2],d[3]],s=function(a){var
b=de(A,q,a),c=b[2];if(b[1])throw[0,M,gD];return V([0,[0,[0,A,c,1,0],h[1]]],a)},u=0,v=0,w=function(c,b,a){return a},x=function(a){return c6(w,v,u,r,a)};return e(j[5],x,s,b)}throw[0,M,gE]};break;default:var
am=b[1];if(typeof
am==="number")var
aR=a(d[3],hu),h=e(i[3],0,0,aR);else
var
an=am[1],h=function(y){var
w=ad(y)[1];if(w){var
l=w[1];if(typeof
l==="number"){switch(l){case
0:var
z=a(d[3],hg),q=e(i[3],0,0,z);break;case
1:var
q=a(i[7],hl);break;default:var
q=a(i[7],hm)}var
r=q[1],b=q[2],k=q[3],h=q[4]}else
var
r=l[1],b=l[2],k=l[3],h=l[4]}else
var
$=a(d[3],hn),u=e(i[3],0,0,$),r=u[1],b=u[2],k=u[3],h=u[4];var
A=0===r?0===an?r:a(i[7],hj):0===an?a(i[7],hk):r;if(typeof
k==="number")if(0===k)var
v=0;else
var
Z=a(p[99],b[1]),_=[0,a(n[67][8],Z),0],s=a(j[20],_),v=1;else
var
v=0;if(!v)if(0===A)if(typeof
k==="number")var
B=c(g[17][12],f[t],h),C=function(a){return cY(B,a)},D=a(p[eo],b[1]),E=a(n[67][8],D),s=c(j[5],E,C);else
var
F=k[1],H=[0,b[1],0],G=0,I=di(h),J=function(b){var
c=0,d=a(n[67][8],p[41]),e=[0,function(a){return aL(d,a)},c],f=[0,function(a){return ai(h,a)},e],g=[0,function(a){return dn(b,a)},f];return a(j[7],g)},K=0,s=function(a){return bk(K,b,J,I,H,G,F,a)};else
if(typeof
k==="number")var
L=0,M=c(g[17][12],f[t],h),N=[0,function(a){return cY(M,a)},L],O=[0,a(g[17][1],b[5])+1|0],P=a(p[101],O),Q=[0,a(n[67][8],P),N],R=c(g[18],b[5],[0,b[1],0]),S=a(p[cy],R),T=[0,a(n[67][8],S),Q],s=a(j[7],T);else
var
U=k[1],x=a(g[17][1],b[5]),V=function(d){var
i=a(o[1][6],hh),e=c(m[20],i,d),k=a(o[1][6],hi),g=c(m[20],k,d),r=[0,a(f[t],g),0],l=0,q=0,s=di(h);function
u(b){var
c=0,d=a(n[67][8],p[41]),f=[0,function(a){return aL(d,a)},c],g=[0,function(a){return ai(h,a)},f],i=[0,function(a){return dn(b,a)},g],k=[0,e,0],l=[0,function(a){return ai(k,a)},i];return a(j[7],l)}var
v=[0,e],w=[0,function(a){return bk(v,b,u,s,r,q,U,a)},l],y=a(p[23],g),z=[0,a(n[67][8],y),w],A=a(n[67][8],p[17]),B=[0,c(j[26],x,A),z],C=c(p[8],[0,e],x+1|0),D=[0,a(n[67][8],C),B];return c(j[7],D,d)},W=c(g[18],b[5],[0,b[1],0]),X=a(p[cy],W),Y=a(n[67][8],X),s=c(j[5],Y,V);return e(j[5],aJ,s,y)}}var
aU=c(j[5],h,fh),aV=a(n[67][1],aU),aW=a(ac[2],0),R=e(x[29],aW,aV,Q)[1];break}}else
var
R=Q;var
aX=function(b,a){return R};a(G[27],aX);return hv(R,P[2])}}var
aP=[0,fk,fp,fE,a6,c2,dq,function(b){return dq(b,a(G[12],0))},V,bk,aO,av,bi,bj,dg,dh,bW,cU,dl,dk,dj,dm,ae,bh];ay(456,aP,"Decl_mode_plugin.Decl_proof_instr");function
X(n,b){var
o=a(n,b[2]),e=b[1];if(e)var
g=e[1],h=a(d[13],0),i=a(d[3],hC),j=a(d[13],0),k=a(R[1],g),l=c(d[12],k,j),m=c(d[12],l,i),f=c(d[12],m,h);else
var
f=a(d[7],0);return c(d[12],f,o)}function
al(f,b){if(0===b[0])return a(f,b[1]);var
e=b[1];if(e){var
g=a(R[1],e[1]),h=a(d[13],0),i=a(d[3],hH),j=a(d[13],0),k=a(d[3],hI),l=c(d[12],k,j),m=c(d[12],l,i),n=c(d[12],m,h);return c(d[12],n,g)}return a(d[3],hJ)}function
am(B,A,z,b){var
h=b[3];if(h)var
t=a(A,h[1]),u=a(d[13],0),v=a(d[3],hG),w=a(d[13],0),x=c(d[12],w,v),y=c(d[12],x,u),i=c(d[12],y,t);else
var
i=a(d[7],0);var
j=b[2];if(j){var
g=j[1];if(g)var
k=function(b){return a(d[3],hD)},l=e(d[38],k,B,g),m=a(d[13],0),n=a(d[3],hE),o=a(d[13],0),p=c(d[12],o,n),q=c(d[12],p,m),f=c(d[12],q,l);else
var
f=a(d[7],0)}else
var
r=a(d[3],hF),s=a(d[13],0),f=c(d[12],s,r);var
C=a(z,b[1]),D=c(d[26],1,C),E=c(d[12],D,f);return c(d[12],E,i)}function
hK(a){return 0===a[0]?a[1]:f[113]}function
aw(a){return a}function
ax(j,f,i,l,h,e,b){if(l)var
m=a(d[13],0),n=a(d[3],hL),g=c(d[12],n,m);else
var
g=a(d[7],0);if(b){var
k=b[1];if(0===k[0]){var
o=aQ(j,f,i,0,h,e,b),p=a(d[3],e),q=a(d[13],0),r=c(d[12],q,g),s=c(d[12],r,p);return c(d[12],s,o)}var
t=k[1],u=ax(j,f,i,1,h,e,b[2]),v=X(f,t),w=a(d[13],0),x=c(d[12],w,g),y=c(d[12],x,v);return c(d[12],y,u)}return a(d[7],0)}function
aQ(f,i,h,k,e,g,b){if(b){var
j=b[1];if(0===j[0]){var
l=b[2],m=j[1],n=k?a(d[28],0):a(d[7],0),o=aQ(f,i,h,1,e,g,l),p=a(f,m),q=a(d[13],0),r=c(d[12],q,n),s=c(d[12],r,p);return c(d[12],s,o)}var
t=e?a(d[3],hM):a(d[3],hN),u=ax(f,i,h,0,e,g,b),v=a(d[13],0),w=c(d[12],v,t);return c(d[12],w,u)}return a(d[7],0)}function
dr(b){return 0===b?a(d[3],hQ):a(d[3],hR)}function
bl(h,f,bW,j,w){var
o=0,k=0,b=w[2];for(;;){if(typeof
b==="number")var
g=a(d[3],hZ);else
switch(b[0]){case
0:var
o=1,b=b[1];continue;case
1:var
k=1,b=b[1];continue;case
2:var
o=1,k=1,b=b[1];continue;case
3:var
m=b[1];if(0===o)if(0===k)var
H=function(a){return al(f,a)},I=am(f,j,function(a){return X(H,a)},m),J=a(d[13],0),K=a(d[3],h0),L=c(d[12],K,J),g=c(d[12],L,I);else
var
M=function(a){return al(f,a)},N=am(f,j,function(a){return X(M,a)},m),O=a(d[13],0),P=a(d[3],h1),Q=c(d[12],P,O),g=c(d[12],Q,N);else
if(0===k)var
S=function(a){return al(f,a)},T=am(f,j,function(a){return X(S,a)},m),U=a(d[13],0),V=a(d[3],h2),W=c(d[12],V,U),g=c(d[12],W,T);else
var
Y=function(a){return al(f,a)},Z=am(f,j,function(a){return X(Y,a)},m),_=a(d[13],0),$=a(d[3],h3),aa=c(d[12],$,_),g=c(d[12],aa,Z);break;case
4:var
ab=b[2],ac=b[1],ad=am(f,j,function(a){return X(f,a)},ab),ae=a(d[13],0),af=0===ac?a(d[3],hX):a(d[3],hY),ag=a(d[13],0),ah=k?a(d[3],h4):a(d[3],h5),ai=c(d[12],ah,ag),aj=c(d[12],ai,af),ak=c(d[12],aj,ae),g=c(d[12],ak,ad);break;case
5:var
an=b[1],ao=am(f,j,function(b){var
e=b[1],g=al(f,b[2]),i=a(d[13],0),j=a(d[3],hO),k=a(d[13],0),l=ax(h,f,aw,0,0,hP,e),m=c(d[12],l,k),n=c(d[12],m,j),o=c(d[12],n,i);return c(d[12],o,g)},an),ap=a(d[3],h6),g=c(d[12],ap,ao);break;case
6:var
aq=ax(h,f,aw,0,0,h7,b[1]),ar=a(d[3],h8),g=c(d[12],ar,aq);break;case
7:var
as=aQ(h,f,aw,0,1,h9,b[1]),at=a(d[3],h_),g=c(d[12],at,as);break;case
8:var
au=aQ(h,f,aw,0,0,h$,b[1]),av=a(d[3],ia),g=c(d[12],av,au);break;case
9:var
ay=b[2],az=a(f,b[1]),aA=a(d[3],ib),aB=a(d[13],0),aC=aQ(h,f,aw,0,0,ic,ay),aD=a(d[3],id),aE=c(d[12],aD,aC),aF=c(d[12],aE,aB),aG=c(d[12],aF,aA),g=c(d[12],aG,az);break;case
10:var
aH=X(f,b[1]),aI=a(d[13],0),aJ=a(d[3],ie),aK=c(d[12],aJ,aI),g=c(d[12],aK,aH);break;case
11:var
aL=X(f,b[1]),aM=a(d[13],0),aN=a(d[3],ig),aO=c(d[12],aN,aM),g=c(d[12],aO,aL);break;case
12:var
aP=b[2],aR=b[1],aS=a(f,b[3]),aT=a(d[3],ih),aU=a(d[13],0),aV=function(b){var
e=a(d[3],ii),f=a(h,b),g=a(d[3],ij),i=c(d[12],g,f);return c(d[12],i,e)},aW=e(d[38],d[13],aV,aP),aX=a(d[13],0),aY=a(R[1],aR),aZ=a(d[13],0),a0=a(d[3],ik),a1=c(d[12],a0,aZ),a2=c(d[12],a1,aY),a3=c(d[12],a2,aX),a4=c(d[12],a3,aW),a5=c(d[12],a4,aU),a6=c(d[12],a5,aT),g=c(d[12],a6,aS);break;case
13:var
a7=b[1],a8=a(f,b[2]),a9=a(d[13],0),a_=a(d[3],il),a$=a(d[13],0),ba=al(R[1],a7),bb=a(d[13],0),bc=a(d[3],im),bd=c(d[12],bc,bb),be=c(d[12],bd,ba),bf=c(d[12],be,a$),bg=c(d[12],bf,a_),bh=c(d[12],bg,a9),g=c(d[12],bh,a8);break;case
14:var
bi=ax(h,f,aw,0,0,io,b[1]),bj=a(d[3],ip),g=c(d[12],bj,bi);break;case
15:var
r=b[3],s=b[1],bk=b[2];if(0===r)var
t=a(d[7],0);else
var
bA=0,bB=0,bC=ax(h,function(a){return al(f,a)},hK,bB,bA,iu,r),bD=a(d[3],iv),bE=a(d[13],0),bF=c(d[12],bE,bD),t=c(d[12],bF,bC);if(0===s)var
u=a(d[7],0);else
var
br=a(d[13],0),bs=function(b){var
e=a(d[3],ir),f=a(h,b),g=a(d[3],is),i=c(d[12],g,f);return c(d[12],i,e)},bt=e(d[38],d[13],bs,s),bu=a(d[13],0),bv=a(d[3],it),bw=a(d[13],0),bx=c(d[12],bw,bv),by=c(d[12],bx,bu),bz=c(d[12],by,bt),u=c(d[12],bz,br);var
bl=a(bW,bk),bm=a(d[13],0),bn=a(d[3],iq),bo=c(d[12],bn,bm),bp=c(d[12],bo,bl),bq=c(d[12],bp,u),g=c(d[12],bq,t);break;case
16:var
bG=e(d[38],d[28],f,b[1]),bH=a(d[13],0),bI=a(d[3],iw),bJ=c(d[12],bI,bH),g=c(d[12],bJ,bG);break;case
17:var
p=b[2],bK=b[1];if(0===p[0])var
y=a(f,p[1]),z=a(d[13],0),A=a(d[3],hV),B=c(d[12],A,z),v=c(d[12],B,y);else
var
C=p[1],D=am(f,j,function(a){return X(f,a)},C),E=a(d[13],0),F=a(d[3],hW),G=c(d[12],F,E),v=c(d[12],G,D);var
bL=a(d[13],0),bM=dr(bK),bN=a(d[13],0),bO=a(d[3],ix),bP=c(d[12],bO,bN),bQ=c(d[12],bP,bM),bR=c(d[12],bQ,bL),g=c(d[12],bR,v);break;default:var
q=b[1];if(typeof
q==="number")switch(q){case
0:var
n=a(d[3],hS);break;case
1:var
n=a(d[3],hT);break;default:var
n=a(d[3],hU)}else
var
n=dr(q[1]);var
bS=a(d[13],0),bT=a(d[3],iy),bU=c(d[12],bT,bS),g=c(d[12],bU,n)}var
bX=a(d[13],0),x=w[1];if(3<x>>>0)var
bV=a(d[3],iz),l=e(i[3],0,0,bV);else
switch(x){case
0:var
l=a(d[3],iA);break;case
1:var
l=a(d[3],iB);break;case
2:var
l=a(d[3],iC);break;default:var
l=a(d[3],iD)}var
bY=c(d[12],l,bX);return c(d[12],bY,g)}}function
iE(g,f,e,b){var
h=a(e,s[2][28]),i=bY[25];return bl(function(h){var
b=h[2],e=b[2],f=b[1];if(e){var
i=e[1],j=a(d[3],iF),k=a(g,i),l=a(d[3],iG),m=a(R[1],f),n=a(d[3],iH),o=c(d[12],n,m),p=c(d[12],o,l),q=c(d[12],p,k);return c(d[12],q,j)}return a(R[1],f)},f,i,h,b)}function
iI(g,f,e,b){var
h=a(e,s[2][28]),i=bY[25];return bl(function(h){var
b=h[2],e=b[2],f=b[1];if(e){var
i=e[1],j=a(d[3],iJ),k=a(g,i),l=a(d[3],iK),m=a(R[1],f),n=a(d[3],iL),o=c(d[12],n,m),p=c(d[12],o,l),q=c(d[12],p,k);return c(d[12],q,j)}return a(R[1],f)},f,i,h,b)}var
aR=[0,bl,iE,iI,function(e,d,c,b){var
f=a(c,s[2][28]);function
g(b){return a(bY[25],b[6])}return bl(function(a){return X(e,a)},d,g,f,b)}];ay(458,aR,"Decl_mode_plugin.Ppdecl_proof");a(iO[12],iN);function
bZ(f){var
j=a(u[68],f),k=a(m[2],f),g=c(B[4][13],k,j),b=g[2],h=g[1],i=c(B[4][1],b,h),l=c(B[4][4],b,h),n=e(af[19],i,b,l),o=a(d[14],0),p=a(d[3],iP),q=a(d[14],0),r=a(d[3],iQ),s=a(d[14],0),t=c(af[61],i,b),v=c(d[12],t,s),w=c(d[12],v,r),x=c(d[12],w,q),y=c(d[12],x,p),z=c(d[12],y,o),A=c(d[12],z,n),C=c(d[24],0,A),D=a(d[3],iR),E=a(d[5],0),F=a(d[5],0),G=a(d[3],iS),H=c(d[12],G,F),I=c(d[12],H,E),J=c(d[12],I,D);return c(d[12],J,C)}function
ds(e,r,f,q,p,o,b){var
g=e?e[1]:1;if(b)if(!b[2]){var
n=b[1];if(g)return bZ([0,n,f])}var
h=a(G[12],0),i=a(y[13],h),j=a(d[3],iT),k=a(d[3],i),l=a(d[3],iU),m=c(d[12],l,k);return c(d[12],m,j)}function
iV(h,b){var
a=b[2],d=b[1],f=c(B[4][1],a,d),g=c(y[8],a,d);return e(aI[2],g,f,a)}function
dt(c){var
b=a(G[12],0);return a(x[7],b)?a(i[7],iW):(a(aP[1],0),a(G[10],iX))}function
du(b){a(aP[2],0);return a(G[10],iY)}function
dv(b){return a(aP[7],b)}var
ao=a(an[2],iZ),bm=a(k[1][10],i0),i1=a(an[4],ao),dw=e(k[13],k[9],i2,i1);E(s[2][1],ao,aR[2],aR[3],aR[4]);function
dx(d){var
a=d[2];if(typeof
a==="number")var
b=1;else
if(18===a[0])var
c=a[1],b=typeof
c==="number"?0===c?1:0:0;else
var
b=0;return b?i3:bn[7]}var
i4=0,i6=[0,[0,0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(an[4],ao),f=c(an[8],e,d);return function(a){return dv(f)}}return a($[2],i5)}],i4];function
i7(b,a){return e(b0[1],a[1],[0,i8,b],a[2])}c(aa[80],i7,i6);var
i9=0,i$=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(an[4],ao),f=c(an[8],e,d);return function(a){return dx(f)}}return a($[2],i_)},i9];function
ja(b,a){return c(bn[3],[0,jb,b],a)}c(aa[80],ja,i$);var
jc=[6,a(k[12],ao)],jd=a(an[4],ao),je=[0,[0,[1,v[4],jd,jc],0],0];function
jf(b,a){return e(b1[1],[0,jg,b],[0,bm],a)}c(aa[80],jf,je);var
jh=0,ji=0;function
jj(b,c){return a(b,jk)}e(k[1][6],bm,jm,[0,[0,0,0,[0,[0,[0,[2,jl[10]],0],jj],ji]],jh]);function
jn(b){a(k[20],k[17][8]);return a(af[84],af[85])}var
jp=[0,jo,function(b){a(k[20],bm);return a(af[84],[0,ds,af[85][2],bZ])},jn];a(G[1],jp);var
jq=0,js=[0,[0,0,function(b){return b?a($[2],jr):function(a){return dt(0)}}],jq];function
jt(b,a){return e(b0[1],a[1],[0,ju,b],a[2])}c(aa[80],jt,js);var
jv=0,jy=[0,function(b){return b?a($[2],jw):function(a){return jx}},jv];function
jz(b,a){return c(bn[3],[0,jA,b],a)}c(aa[80],jz,jy);function
jC(b,a){return e(b1[1],[0,jD,b],0,a)}c(aa[80],jC,jB);var
jE=0,jG=[0,[0,0,function(b){return b?a($[2],jF):function(a){return du(0)}}],jE];function
jH(b,a){return e(b0[1],a[1],[0,jI,b],a[2])}c(aa[80],jH,jG);var
jJ=0,jM=[0,function(b){return b?a($[2],jK):function(a){return jL}},jJ];function
jN(b,a){return c(bn[3],[0,jO,b],a)}c(aa[80],jN,jM);function
jQ(b,a){return e(b1[1],[0,jR,b],0,a)}c(aa[80],jQ,jP);function
b2(a){return a?a[1]:0}var
q=k[1][4][1],bo=a(q,jS),A=a(q,jT),bp=a(q,jU),aS=a(q,jV),bq=a(q,jW),br=a(q,jX),aT=a(q,jY),bs=a(q,jZ),b3=a(q,j0),dy=a(q,j1),dz=a(q,j2),b4=a(q,j3),b5=a(q,j4),dA=a(q,j5),b6=a(q,j6),w=a(q,j7),b7=a(q,j8),dB=a(q,j9),b8=a(q,j_),b9=a(q,j$),b_=a(q,ka),b$=a(q,kb),ca=a(q,kc),dC=a(q,kd),cb=a(q,ke),dD=a(q,kf),cc=a(q,kg),dE=a(q,kh),cd=a(q,ki),ce=a(q,kj),dF=a(q,kk),dG=a(q,kl),dH=a(q,km),dI=a(q,kn),ko=0,kp=0,kr=[0,[0,kq,function(b,a){return 0}],kp];function
ks(a,d,c,b){return[0,a]}e(k[1][6],bo,0,[0,[0,0,0,[0,[0,[0,ku,[0,kt,[0,[2,k[15][6]],0]]],ks],kr]],ko]);var
kv=0,kw=0;function
kx(b,d,a,c){return[0,[0,a],b]}var
kz=[0,[0,[0,[2,k[15][6]],[0,ky,[0,[2,k[15][1]],0]]],kx],kw];function
kA(c,b){return[0,0,[0,[1,[0,a(cf[11],b),c]],0]]}var
kB=[0,[0,[0,[2,k[15][6]],0],kA],kz];function
kC(a,b){return[0,0,a]}e(k[1][6],A,0,[0,[0,0,0,[0,[0,[0,[2,k[15][1]],0],kC],kB]],kv]);var
kD=0,kE=0;function
kF(a,b){return[0,a]}var
kG=[0,[0,0,0,[0,[0,[0,[2,k[15][1]],0],kF],kE]],kD],kH=0,kI=[0,[0,0,0,[0,[0,[0,[2,bo],0],function(a,b){return[1,a]}],kH]],kG];e(k[1][6],bp,0,kI);var
kJ=0,kK=0;function
kL(b,d,a,c){return[0,[0,a],b]}var
kN=[0,[0,[0,[2,k[15][6]],[0,kM,[0,[2,bp],0]]],kL],kK];function
kO(c,b){return[0,0,[0,[0,[1,[0,a(cf[11],b),c]],0]]]}var
kP=[0,[0,[0,[2,k[15][6]],0],kO],kN];function
kQ(a,b){return[0,0,[0,a]]}var
kR=[0,[0,0,0,[0,[0,[0,[2,k[15][1]],0],kQ],kP]],kJ],kS=0,kT=[0,[0,0,0,[0,[0,[0,[2,bo],0],function(a,b){return[0,0,[1,a]]}],kS]],kR];e(k[1][6],aS,0,kT);var
kU=0,kV=0,kX=[0,[0,0,function(a){return kW}],kV];function
kY(a,c,b){return[0,a]}var
k1=[0,[0,[0,k0,[0,[7,[2,k[15][1]],kZ,0],0]],kY],kX],k3=[0,[0,0,0,[0,[0,k2,function(c,b,a){return 0}],k1]],kU];e(k[1][6],bq,0,k3);var
k4=0,k5=0,k6=[0,[0,0,function(a){return 0}],k5];function
k7(a,c,b){return[0,a]}e(k[1][6],br,0,[0,[0,0,0,[0,[0,[0,k8,[0,[2,s[3][18]],0]],k7],k6]],k4]);var
k9=0,k_=0,k$=[0,[0,0,0,[0,[0,[0,[2,aS],[0,[2,bq],[0,[2,br],0]]],function(c,b,a,d){return[0,a,b,c]}],k_]],k9];e(k[1][6],aT,0,k$);var
la=0,lb=0,lc=[0,[0,0,0,[0,[0,[0,[2,A],[0,[2,bq],[0,[2,br],0]]],function(c,b,a,d){return[0,a,b,c]}],lb]],la];e(k[1][6],bs,0,lc);var
ld=0,le=0,lg=[0,[0,lf,function(b,a){return 1}],le],li=[0,[0,0,0,[0,[0,lh,function(b,a){return 0}],lg]],ld];e(k[1][6],b3,0,li);var
lj=0,lk=0,lm=[0,[0,ll,function(b,a){return 1}],lk],lo=[0,[0,ln,function(b,a){return 2}],lm],lq=[0,[0,lp,function(b,a){return 0}],lo],lr=[0,[0,0,0,[0,[0,[0,[2,b3],0],function(a,b){return[0,a]}],lq]],lj];e(k[1][6],dy,0,lr);var
ls=0,lt=0;function
lu(a,c,b){return[0,a]}var
lw=[0,[0,[0,lv,[0,[2,k[15][1]],0]],lu],lt],ly=[0,[0,0,0,[0,[0,[0,lx,[0,[2,bs],0]],function(a,c,b){return[1,a]}],lw]],ls];e(k[1][6],dz,0,ly);var
lz=0,lA=0;function
lB(b,e,a,d,c){return[9,b,a]}var
lE=[0,[0,[0,lD,[0,[2,b7],[0,lC,[0,[2,k[15][1]],0]]]],lB],lA],lG=[0,[0,[0,lF,[0,[2,b3],[0,[2,dz],0]]],function(b,a,d,c){return[17,a,b]}],lE],lI=[0,[0,0,0,[0,[0,[0,lH,[0,[2,dC],[0,[2,bq],[0,[2,br],0]]]],function(c,b,a,e,d){return[5,[0,a,b,c]]}],lG]],lz];e(k[1][6],b4,0,lI);var
lJ=0,lK=0,lM=[0,[0,[0,lL,[0,[2,bs],0]],function(a,c,b){return[0,1,a]}],lK],lO=[0,[0,0,0,[0,[0,[0,lN,[0,[2,bs],0]],function(a,c,b){return[0,0,a]}],lM]],lJ];e(k[1][6],b5,0,lO);var
lP=0,lQ=0,lS=[0,[0,[0,lR,[0,[2,b4],0]],function(a,c,b){return[0,a]}],lQ],lU=[0,[0,[0,lT,[0,[2,aT],0]],function(a,c,b){return[0,[3,a]]}],lS],lW=[0,[0,[0,lV,[0,[2,b5],0]],function(a,c,b){return[1,[4,a[1],a[2]]]}],lU],lY=[0,[0,[0,lX,[0,[2,aT],0]],function(a,c,b){return[1,[3,a]]}],lW],l0=[0,[0,[0,lZ,[0,[2,aT],0]],function(a,c,b){return[2,[3,a]]}],lY],l1=[0,[0,[0,[2,b4],0],function(a,b){return a}],l0],l2=[0,[0,[0,[2,b5],0],function(a,b){return[4,a[1],a[2]]}],l1],l4=[0,[0,[0,l3,[0,[2,aT],0]],function(a,c,b){return[3,a]}],l2],l6=[0,[0,[0,l5,[0,[2,A],0]],function(a,c,b){return[10,a]}],l4],l9=[0,[0,[0,l8,[0,l7,[0,[2,A],0]]],function(a,d,c,b){return[11,a]}],l6],l$=[0,[0,[0,l_,[0,[2,dy],0]],function(a,c,b){return[18,a]}],l9],mb=[0,[0,0,0,[0,[0,ma,function(b,a){return 0}],l$]],lP];e(k[1][6],dA,0,mb);var
mc=0,md=0;function
me(d,c,b){return[0,a(cf[11],c),[0,d,b]]}e(k[1][6],b6,0,[0,[0,0,0,[0,[0,[0,[2,k[15][6]],0],me],md]],mc]);var
mf=0,mg=0,mh=[0,[0,[0,[2,b6],0],function(b,c){return a(b,0)}],mg];function
mi(c,e,b,d){return a(b,[0,c])}e(k[1][6],w,0,[0,[0,0,0,[0,[0,[0,[2,b6],[0,mj,[0,[2,k[15][1]],0]]],mi],mh]],mf]);var
mk=0,ml=0,mm=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],ml],mo=[0,[0,[0,[2,w],mn],function(b,d,a,c){return[0,[0,a],b]}],mm],mr=[0,[0,0,0,[0,[0,[0,[2,w],[0,mq,[0,mp,[0,[2,dB],0]]]],function(b,e,d,a,c){return[0,[0,a],b]}],mo]],mk];e(k[1][6],b7,0,mr);var
ms=0,mt=0,mv=[0,[0,[0,[2,A],mu],function(b,d,a,c){return[0,[1,a],b]}],mt],my=[0,[0,[0,[2,A],[0,mx,[0,mw,[0,[2,b7],0]]]],function(b,e,d,a,c){return[0,[1,a],b]}],mv],mz=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],my]],ms];e(k[1][6],dB,0,mz);var
mA=0,mB=0,mC=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],mB],mE=[0,[0,[0,[2,w],mD],function(b,d,a,c){return[0,[0,a],b]}],mC],mH=[0,[0,0,0,[0,[0,[0,[2,w],[0,mG,[0,mF,[0,[2,b9],0]]]],function(b,e,d,a,c){return[0,[0,a],b]}],mE]],mA];e(k[1][6],b8,0,mH);var
mI=0,mJ=0,mL=[0,[0,[0,[2,A],mK],function(b,d,a,c){return[0,[1,a],b]}],mJ],mP=[0,[0,[0,[2,A],[0,mO,[0,mN,[0,mM,[0,[2,b8],0]]]]],function(b,f,e,d,a,c){return[0,[1,a],b]}],mL],mQ=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],mP]],mI];e(k[1][6],b9,0,mQ);var
mR=0,mS=0,mV=[0,[0,[0,mU,[0,mT,[0,[2,b8],0]]],function(a,d,c,b){return a}],mS],mW=[0,[0,0,0,[0,[0,[0,[2,b9],0],function(a,b){return a}],mV]],mR];e(k[1][6],b_,0,mW);var
mX=0,mY=0,m1=[0,[0,[0,[2,w],[0,m0,[0,mZ,[0,[2,bp],0]]]],function(b,e,d,a,c){return[0,[0,[0,a],0],b]}],mY],m3=[0,[0,[0,[2,w],m2],function(a,d,b,c){return[0,[0,[0,b],a[1]],a[2]]}],m1],m6=[0,[0,0,0,[0,[0,[0,[2,w],[0,m5,[0,m4,[0,[2,ca],0]]]],function(a,e,d,b,c){return[0,[0,[0,b],a[1]],a[2]]}],m3]],mX];e(k[1][6],b$,0,m6);var
m7=0,m8=0,m_=[0,[0,[0,[2,A],m9],function(a,d,b,c){return[0,[0,[1,b],a[1]],a[2]]}],m8],nc=[0,[0,[0,[2,A],[0,nb,[0,na,[0,m$,[0,[2,b$],0]]]]],function(a,f,e,d,b,c){return[0,[0,[1,b],a[1]],a[2]]}],m_],nf=[0,[0,0,0,[0,[0,[0,[2,A],[0,ne,[0,nd,[0,[2,bp],0]]]],function(b,e,d,a,c){return[0,[0,[1,a],0],b]}],nc]],m7];e(k[1][6],ca,0,nf);var
ng=0,nh=0,nk=[0,[0,[0,nj,[0,ni,[0,[2,b$],0]]],function(a,d,c,b){return a}],nh],nl=[0,[0,0,0,[0,[0,[0,[2,ca],0],function(a,b){return a}],nk]],ng];e(k[1][6],dC,0,nl);var
nm=0,nn=0,no=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],nn],nq=[0,[0,[0,[2,w],np],function(b,d,a,c){return[0,[0,a],b]}],no],nu=[0,[0,0,0,[0,[0,[0,[2,w],[0,nt,[0,ns,[0,nr,[0,[2,dD],0]]]]],function(b,f,e,d,a,c){return[0,[0,a],b]}],nq]],nm];e(k[1][6],cb,0,nu);var
nv=0,nw=0,ny=[0,[0,[0,[2,A],nx],function(b,d,a,c){return[0,[1,a],b]}],nw],nB=[0,[0,[0,[2,A],[0,nA,[0,nz,[0,[2,cb],0]]]],function(b,e,d,a,c){return[0,[1,a],b]}],ny],nC=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],nB]],nv];e(k[1][6],dD,0,nC);var
nD=0,nE=0,nF=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],nE],nH=[0,[0,[0,[2,w],nG],function(b,d,a,c){return[0,[0,a],b]}],nF],nK=[0,[0,0,0,[0,[0,[0,[2,w],[0,nJ,[0,nI,[0,[2,dE],0]]]],function(b,e,d,a,c){return[0,[0,a],b]}],nH]],nD];e(k[1][6],cc,0,nK);var
nL=0,nM=0,nO=[0,[0,[0,[2,A],nN],function(b,d,a,c){return[0,[1,a],b]}],nM],nR=[0,[0,[0,[2,A],[0,nQ,[0,nP,[0,[2,cc],0]]]],function(b,e,d,a,c){return[0,[1,a],b]}],nO],nS=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],nR]],nL];e(k[1][6],dE,0,nS);var
nT=0,nU=0,nV=[0,[0,[0,[2,w],0],function(a,b){return[0,[0,a],0]}],nU],nX=[0,[0,[0,[2,w],nW],function(b,d,a,c){return[0,[0,a],b]}],nV];function
nY(b,f,e,d,a,c){return[0,[0,a],b]}var
n1=[0,n0,[0,nZ,[0,[2,ce],0]]],n2=0,n4=[0,[0,n3,function(a,b){return a}],n2],n5=[0,[0,0,0,[0,[0,[0,[2,w],[0,[8,a(cg[2],n4)],n1]],nY],nX]],nT];e(k[1][6],cd,0,n5);var
n6=0,n7=0,n9=[0,[0,[0,[2,aS],n8],function(b,d,a,c){return[0,[1,a],b]}],n7],ob=[0,[0,[0,[2,aS],[0,oa,[0,n$,[0,n_,[0,[2,cd],0]]]]],function(b,f,e,d,a,c){return[0,[1,a],b]}],n9],oc=[0,[0,0,0,[0,[0,[0,[2,aS],0],function(a,b){return[0,[1,a],0]}],ob]],n6];e(k[1][6],ce,0,oc);var
od=0,oe=0,oh=[0,[0,[0,og,[0,of,[0,[2,cd],0]]],function(a,d,c,b){return a}],oe],oi=[0,[0,0,0,[0,[0,[0,[2,ce],0],function(a,b){return a}],oh]],od];e(k[1][6],dF,0,oi);var
oj=0,ok=0,om=[0,[0,[0,ol,[0,[2,b_],0]],function(a,c,b){return[14,a]}],ok];function
on(c,b,a,h,g,f,e){var
d=b2(c);return[15,b2(b),a,d]}var
oo=0,op=0,or=[0,[0,[0,oq,[0,[2,dF],0]],function(a,c,b){return a}],op],os=[0,[8,a(cg[2],or)],oo],ot=0,ow=[0,[0,[0,ov,[0,[7,[2,w],ou,0],0]],function(a,c,b){return a}],ot],ox=[0,[8,a(cg[2],ow)],os],oC=[0,[0,[0,oB,[0,oA,[0,oz,[0,[3,k[15][10],oy],ox]]]],on],om],oE=[0,[0,[0,oD,[0,[2,cb],0]],function(a,c,b){return[7,a]}],oC];function
oF(a,c,b){return[16,a]}var
oI=[0,[0,[0,oH,[0,[7,[2,k[15][1]],oG,0],0]],oF],oE],oK=[0,[0,[0,oJ,[0,[2,b_],0]],function(a,c,b){return[6,a]}],oI],oM=[0,[0,[0,oL,[0,[2,cc],0]],function(a,c,b){return[8,a]}],oK];function
oN(c,f,b,a,e,d){return[12,a,b,c]}var
oQ=[0,[0,[0,oP,[0,[2,k[15][6]],[0,[4,[2,w]],[0,oO,[0,[2,k[15][1]],0]]]]],oN],oM];function
oR(b,e,a,d,c){return[13,[0,a],b]}var
oU=[0,[0,[0,oT,[0,[2,k[15][6]],[0,oS,[0,[2,k[15][1]],0]]]],oR],oQ];function
oV(b,e,a,d,c){return[13,[1,a],b]}e(k[1][6],dG,0,[0,[0,0,0,[0,[0,[0,oX,[0,[2,bo],[0,oW,[0,[2,k[15][1]],0]]]],oV],oU]],oj]);var
oY=0,oZ=0,o0=[0,[0,0,function(a){return 0}],oZ],o2=[0,[0,o1,function(b,a){return 1}],o0],o4=[0,[0,o3,function(b,a){return 2}],o2],o6=[0,[0,0,0,[0,[0,o5,function(b,a){return 3}],o4]],oY];e(k[1][6],dH,0,o6);var
o7=0,o8=0,o9=[0,[0,[0,[2,dA],0],function(a,b){return a}],o8],o_=[0,[0,0,0,[0,[0,[0,[2,dG],0],function(a,b){return a}],o9]],o7];e(k[1][6],dI,0,o_);var
o$=0,pa=0,pc=[0,[0,0,0,[0,[0,[0,[2,dH],[0,[2,dI],pb]],function(d,b,a,c){return[0,a,b]}],pa]],o$];e(k[1][6],dw,0,pc);var
dJ=[0,iM,bZ,ds,iV,dt,du,dv,ao,bm,dw,dx,b2];ay(469,dJ,"Decl_mode_plugin.G_decl_mode");ay(470,[0,y,aI,aP,aR,dJ],"Decl_mode_plugin");return});
