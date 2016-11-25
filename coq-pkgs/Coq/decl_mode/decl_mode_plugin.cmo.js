(function(pg){"use strict";var
dW="*",el='"end claim" expected.',eq=104,aB="that",aF=140,ep="decl_mode_plugin",_=",",ek="be",d$='"end focus" expected.',aE="such",dV="Insufficient justification.",L=250,bI="consider",bz="(",d_="Not inside a proof per cases or induction.",cu="suppose it is",cB=148,Z="Init",co="DeclProof",cp="on",C=246,ct="suppose",T="plugins/decl_mode/decl_proof_instr.ml",dU="    ",ej="escape",d9="assume",aD="Extension: cannot occur",dT='"end cases" expected.',d8="Datatypes",cs="execute_cases ",cw="DeclReturn",s=113,dS="for",bw=122,d7="with",cr="Classic",cv="then",q="",d6="=~",bE="Previous step is not an equality.",eo="~=",d5="suffices",d4='"end induction" expected.',ei="using",cA="ProofInstr",by="given",E="and",bH=121,d3="cases",h="IDENT",dR='"thesis for ..." is not applicable here.',aC="Declarative",d2="plugins/decl_mode/decl_interp.ml",cz="by",d1="induction",eh=".",dQ="take",cq="proof",aW="thesis",eg="impossible",bv="thus",dP="subcase_",O=124,en="of",ef="thesis_for",ee=111,cn=133,dO="Not enough sub-hypotheses to match statements.",d0="hence",cy="we have",bD=")",cm="_tmp",bC="let",aA=":",cl=116,ed="proof_instr",em="show",bG="Logic",bB="we",dZ="define",cx="reconsider",bF="claim",bA="to",dY="This case should already be trapped",bx="Specif",ab=114,ck="focus",aV="as",dN="No pop/stop expected here",ea=146,eb="per",ec="declmode",ag="have",dX="end",J=pg.jsoo_runtime,S=J.caml_check_bound,b=J.caml_new_string,K=J.caml_obj_tag,az=J.caml_register_global,H=J.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):J.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):J.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):J.caml_call_gen(a,[b,c,d])}function
D(a,b,c,d,e){return a.length==4?a(b,c,d,e):J.caml_call_gen(a,[b,c,d,e])}function
N(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):J.caml_call_gen(a,[b,c,d,e,f])}function
cj(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):J.caml_call_gen(a,[b,c,d,e,f,g])}var
l=J.caml_get_global_data(),d=l.Pp,j=l.CErrors,aX=l.Environ,aG=l.Context,w=l.Proof,F=l.Proof_global,g=l.Util,B=l.Goal,t=l.Evd,U=l.Pervasives,cC=l.Pfedit,ah=l.Option,u=l.Loc,y=l.Tacintern,f=l.Term,z=l.Vars,aq=l.Namegen,ac=l.Global,M=l.Assert_failure,V=l.Pretyping,ap=l.Tacinterp,o=l.Names,R=l.Nameops,G=l.Termops,cH=l.Detyping,I=l.CamlinternalLazy,bL=l.Coqlib,cG=l.Universes,P=l.CClosure,$=l.Not_found,a7=l.Reductionops,m=l.Tacmach,p=l.Tactics,n=l.Proofview,i=l.Tacticals,bR=l.Rtree,bS=l.Inductiveops,av=l.Inductive,cT=l.Sigma,ad=l.Printer,cV=l.Locusops,cU=l.Reduction,cS=l.Typing,cR=l.Unification,cW=l.Evarutil,cX=l.CWarnings,b0=l.Ppconstr,bm=l.Pptactic,b4=l.Compat,b3=l.Egramml,bo=l.Vernac_classifier,b1=l.Vernacinterp,k=l.Pcoq,an=l.Genarg,b2=l.Gramext,aa=l.CList,bJ=[0,0],eG=b('"end claim"'),eH=b('"end focus"'),eI=b('"end proof"'),eD=b("lonely suppose"),eF=b('"end induction" or start a new case'),eE=b('"end cases" or start a new case'),eC=b("no previous statement to use"),ev=b("get_info"),eL=b('"(_ | _)" is not allowed here'),eM=b("simple_pattern"),eT=b("Anonymous pattern variable"),eZ=[0,b(d2),302,18],e0=[0,b(d2),314,18],e$=b('"thesis for" is not applicable here.'),e7=b(" does not occur in pattern."),e8=b("Variable "),e9=b(cu),e6=[2,[0,0]],e1=b("No proof per cases/induction/inversion in progress."),e2=b("expected."),e3=b("none"),e4=b("Wrong number of extra arguments: "),e5=b(cu),eX=b("undetected disjunctive pattern"),eY=b("empty pattern list"),eP=b(bE),eQ=b(bE),eO=[0,0],eR=b("__"),gw=b(cm),gA=b(dO),gx=b("Matching hypothesis not found."),gy=b("Last statements do not match a complete hypothesis."),gz=b(cm),gB=b(dO),gD=[0,b(T),811,18],gP=b(dN),gQ=b("Case pattern belongs to wrong inductive type."),gR=b(dN),gS=b("we should pop here"),gT=b("tree is expected to end here"),gU=b("Premature end of branch"),gZ=b('Only "suppose it is" can be used here.'),hd=[0,b(T),1282,2],hb=b("Nothing to skip"),hc=[0,b(cs)],he=[0,b(T),1261,9],hf=b("Nothing to split"),hg=[0,b(cs)],hh=b("End of branch with garbage left"),hi=[0,b(cs)],hj=b("wrong stack size"),hv=[0,b(T),1427,1],hw=[0,b(T),1424,1],hx=[0,b(T),1430,1],hy=[0,b(T),1437,1],hz=b("Not applicable"),hB=b(cr),hC=b(cr),hD=b("hd"),hE=b('"end induction" generated an ill-formed fixpoint'),hu=[0,0],hm=b("_fix"),hn=b("_main_arg"),hl=b(dY),hq=b(el),hr=b(d$),hs=b(dY),hp=b(d4),ho=b(dT),hk=[2,0],g9=b("missing case"),g7=[0,b(T),1218,30],g6=[0,b(T),1220,2],g5=b(eg),g3=b(eg),g2=[0,[0,0,0],0],g0=b(dP),g1=b("wrong place for cases"),gV=b("cannot give an induction hypothesis (wrong inductive type)."),gW=b(ef),gX=b("cannot give an induction hypothesis (wrong parameters)."),gY=b(ef),gO=b(dP),gN=b('Do not mix "suppose" with "suppose it is".'),gM=b("wrong stack state"),gK=[0,b(T),933,5],gL=[0,b(T),924,3],gJ=b("anonymous_matched"),gI=b("Case analysis must be done on an inductive object."),gH=b("map_tree: not a splitting node"),gG=b("map_tree_rp: not a splitting node"),gE=b(dR),gC=b(cm),gu=b("_cofact"),gt=b("_hyp"),gs=b("_claim"),gq=b("No previous equality."),gr=b("_eq"),gn=b(bE),go=b(bE),gl=b("_fact"),gk=b('"then" and "hence" require at least one previous fact'),gj=b(dR),gi=b("I could not relate this statement to the thesis."),gg=b("cannot happen"),gh=[0,b("concl_refiner")],gf=[0,2,1],ge=[0,2,2],fU=b(aC),fT=b(dV),fP=b(dV),fE=b("Weird case occurred ..."),fD=b(d_),fF=b(d_),fz=b(d4),fy=b(dT),fA=b(el),fB=b(d$),fC=b('"end proof" expected.'),fx=b("Lonely suppose on stack."),fv=b('"return" cannot be used outside of Declarative Proof Mode.'),fr=[0,0],fm=b('You are inside a proof per cases/induction.\nPlease "suppose" something or "end" it now.'),fh=b("Cannot clear "),fi=b(q),fk=[0,b("Strict"),[0,b("Proofs"),0]],fl=b("strict proofs"),fG=b("___"),fI=b("No automation registered"),fQ=b(ec),fR=b("declmode-insufficient-justification"),fV=b(E),fW=[0,b(Z),[0,b(bG),0]],fX=b("and_rect"),fY=[0,b(Z),[0,b(bG),0]],fZ=b("prod"),f0=[0,b(Z),[0,b(d8),0]],f1=b("prod_rect"),f2=[0,b(Z),[0,b(d8),0]],f3=b("ex"),f4=[0,b(Z),[0,b(bG),0]],f5=b("ex_ind"),f6=[0,b(Z),[0,b(bG),0]],f7=b("sig"),f8=[0,b(Z),[0,b(bx),0]],f9=b("sig_rect"),f_=[0,b(Z),[0,b(bx),0]],f$=b("sigT"),ga=[0,b(Z),[0,b(bx),0]],gb=b("sigT_rect"),gc=[0,b(Z),[0,b(bx),0]],g_=b(ec),g$=b("declmode-missing-case"),hO=b(E),hP=b("be such that"),hQ=b("such that"),iu=b(bD),iv=b(bz),il=b(bD),im=b(bz),h2=b(ej),h6=b(d0),h5=b(cv),h4=b(bv),h3=b(ag),h7=b(bv),h8=b(dU),h9=b(d5),h_=b(cy),h$=b(d9),ia=b(bC),ib=b(bC),ic=b(by),id=b(by),ie=b("from "),ig=b(bI),ih=b(bI),ii=b(bF),ij=b("focus on"),ik=b(aV),io=b(dZ),ip=b(aV),iq=b(cx),ir=b(cy),is=b(ct),ix=b(cy),iy=b(E),iw=b(d7),it=b(cu),iz=b(dQ),iA=b(eb),iB=b(dX),iM=b(bD),iN=b(aA),iO=b(bz),iI=b(bD),iJ=b(aA),iK=b(bz),iC=b("unknown emphasis"),iD=b(dU),iE=b("*   "),iF=b("**  "),iG=b("*** "),h1=b(eo),h0=b(d6),hY=b(cp),hZ=b(en),hV=b(cq),hW=b(bF),hX=b(ck),hU=b(d1),hT=b(d3),hR=b("to show"),hS=b("to have"),hK=b(dS),hL=b(aW),hM=b(aW),hJ=b(ei),hG=b(_),hH=b(cz),hI=b("by *"),hF=b(aA),kZ=[0,0],jU=b(cw),jR=b(cw),jO=[0,[4,b(aC)],0],jN=b(aD),jL=b(cw),jI=b(aD),jG=b(co),jD=b(co),jA=[0,[4,b(aC)],0],jz=b(aD),jx=b(co),ju=b(aD),jo=[0,[0,1]],jk=b(cA),jf=b(cA),jc=b(aD),ja=b(cA),i9=b(aD),i7=[0,[4,b(cr)],0],i2=b(aC),i0=b("Nothing left to prove here."),i1=b(aC),iX=b(eh),iY=b("Subproof completed, now type "),iT=b("thesis :="),iU=b("============================"),iV=b("  "),iW=b("     *** Declarative Mode ***"),iR=b(ep),iS=b(ep),i3=b(ed),i4=b("vernac:proof_command"),i6=b(ed),jp=[0,1],jr=b(aC),jE=[0,[0,[0,b(cq)],0],0],jS=[0,[0,[0,b("return")],0],0],jV=b(aW),jW=b("statement"),jX=b("constr_or_thesis"),jY=b("statement_or_thesis"),jZ=b("justification_items"),j0=b("justification_method"),j1=b("simple_cut_or_thesis"),j2=b("simple_cut"),j3=b("elim_type"),j4=b("block_type"),j5=b("elim_obj"),j6=b("elim_step"),j7=b("rew_step"),j8=b("cut_step"),j9=b("loc_id"),j_=b("hyp"),j$=b("consider_vars"),ka=b("consider_hyps"),kb=b("assume_vars"),kc=b("assume_hyps"),kd=b("assume_clause"),ke=b("suff_vars"),kf=b("suff_hyps"),kg=b("suff_clause"),kh=b("let_vars"),ki=b("let_hyps"),kj=b("given_vars"),kk=b("given_hyps"),kl=b("suppose_vars"),km=b("suppose_hyps"),kn=b("suppose_clause"),ko=b("intro_step"),kp=b("emphasis"),kq=b("bare_proof_instr"),kt=[0,[10,[0,b(q),b(aW)]],0],kw=[10,[0,b(q),b(dS)]],kx=[10,[0,b(q),b(aW)]],kB=[10,[0,b(q),b(aA)]],kP=[10,[0,b(q),b(aA)]],k2=[10,[0,b(q),b(_)]],k3=[10,[0,b(q),b(cz)]],k5=[0,[10,[0,b(q),b(cz)]],[0,[10,[0,b(q),b(dW)]],0]],k$=[10,[0,b(q),b(ei)]],li=[0,[10,[0,b(h),b(d1)]],0],lk=[0,[10,[0,b(h),b(d3)]],0],lo=[0,[10,[0,b(h),b(bF)]],0],lq=[0,[10,[0,b(h),b(ck)]],0],ls=[0,[10,[0,b(h),b(cq)]],0],ly=[10,[0,b(h),b(cp)]],lA=[10,[0,b(h),b(en)]],lF=[10,[0,b(h),b("from")]],lG=[10,[0,b(h),b(bI)]],lI=[10,[0,b(h),b(eb)]],lK=[10,[0,b(h),b(d5)]],lO=[10,[0,b(q),b(eo)]],lQ=[10,[0,b(q),b(d6)]],lU=[10,[0,b(q),b(cv)]],lW=[10,[0,b(q),b(cv)]],lY=[10,[0,b(h),b(bv)]],l0=[10,[0,b(h),b(bv)]],l2=[10,[0,b(h),b(d0)]],l6=[10,[0,b(h),b(ag)]],l8=[10,[0,b(h),b(bF)]],l_=[10,[0,b(h),b(cp)]],l$=[10,[0,b(h),b(ck)]],mb=[10,[0,b(q),b(dX)]],md=[0,[10,[0,b(h),b(ej)]],0],mm=[10,[0,b(q),b(aA)]],mq=[0,[10,[0,b(q),b(_)]],[0,0,0]],ms=[10,[0,b(h),b(aB)]],mt=[10,[0,b(h),b(aE)]],mx=[0,[10,[0,b(h),b(E)]],[0,0,0]],mz=[10,[0,b(h),b(bI)]],mA=[10,[0,b(h),b(E)]],mG=[0,[10,[0,b(q),b(_)]],[0,0,0]],mI=[10,[0,b(h),b(aB)]],mJ=[10,[0,b(h),b(aE)]],mN=[0,[10,[0,b(h),b(E)]],[0,0,0]],mP=[10,[0,b(h),b(ag)]],mQ=[10,[0,b(h),b(bB)]],mR=[10,[0,b(h),b(E)]],mW=[10,[0,b(h),b(ag)]],mX=[10,[0,b(h),b(bB)]],m2=[10,[0,b(h),b(em)]],m3=[10,[0,b(h),b(bA)]],m5=[0,[10,[0,b(q),b(_)]],[0,0,0]],m7=[10,[0,b(h),b(aB)]],m8=[10,[0,b(h),b(aE)]],na=[0,[10,[0,b(h),b(E)]],[0,0,0]],nc=[10,[0,b(h),b(ag)]],nd=[10,[0,b(h),b(bA)]],ne=[10,[0,b(h),b(E)]],ng=[10,[0,b(h),b(em)]],nh=[10,[0,b(h),b(bA)]],nl=[10,[0,b(h),b(ag)]],nm=[10,[0,b(h),b(bA)]],ns=[0,[10,[0,b(q),b(_)]],[0,0,0]],nu=[10,[0,b(h),b(aB)]],nv=[10,[0,b(h),b(aE)]],nw=[10,[0,b(h),b(ek)]],nA=[0,[10,[0,b(h),b(E)]],[0,0,0]],nC=[10,[0,b(q),b(bC)]],nD=[10,[0,b(h),b(E)]],nJ=[0,[10,[0,b(q),b(_)]],[0,0,0]],nL=[10,[0,b(h),b(aB)]],nM=[10,[0,b(h),b(aE)]],nQ=[0,[10,[0,b(h),b(E)]],[0,0,0]],nS=[10,[0,b(h),b(by)]],nT=[10,[0,b(h),b(E)]],nZ=[0,[10,[0,b(q),b(_)]],[0,0,0]],n2=[10,[0,b(h),b(aB)]],n3=[10,[0,b(h),b(aE)]],n6=[0,[10,[0,b(h),b(ek)]],0],n$=[0,[10,[0,b(h),b(E)]],[0,0,0]],ob=[10,[0,b(h),b(ag)]],oc=[10,[0,b(h),b(bB)]],od=[10,[0,b(h),b(E)]],oi=[10,[0,b(h),b(ag)]],oj=[10,[0,b(h),b(bB)]],oo=[10,[0,b(h),b(ct)]],ot=[10,[0,b(h),b(E)]],ox=[10,[0,b(q),b(_)]],oy=[10,[0,b(q),b(d7)]],oB=b("0"),oC=[10,[0,b(h),b("is")]],oD=[10,[0,b(h),b("it")]],oE=[10,[0,b(h),b(ct)]],oG=[10,[0,b(q),b(bC)]],oJ=[10,[0,b(q),b(_)]],oK=[10,[0,b(h),b(dQ)]],oM=[10,[0,b(h),b(d9)]],oO=[10,[0,b(h),b(by)]],oR=[10,[0,b(q),b(aV)]],oS=[10,[0,b(h),b(dZ)]],oV=[10,[0,b(q),b(aV)]],oW=[10,[0,b(h),b(cx)]],oZ=[10,[0,b(q),b(aV)]],o0=[10,[0,b(h),b(cx)]],o4=[0,[10,[0,b(q),b(dW)]],0],o6=[0,[10,[0,b(q),b("**")]],0],o8=[0,[10,[0,b(q),b("***")]],0],pe=[0,[10,[0,b(q),b(eh)]],0],eJ=l.Constrintern,fb=l.Failure,fd=l.Type_errors,ff=l.Glob_ops,fc=l.Invalid_argument,fg=l.Feedback,fe=l.Goptions,iP=l.Mltop,iQ=l.G_vernac;function
er(a){bJ[1]=1;return 0}function
es(a){bJ[1]=0;return 0}function
et(a){return bJ[1]}var
aY=a(t[3][6],0);function
cD(d){var
b=a(w[33][1],d),e=b[2],f=a(g[17][3],b[1]),h=c(B[4][5],e,f);return c(t[3][3],h,aY)?1:0}function
cE(c){try{var
b=cD(a(cC[9],0));return b}catch(a){a=H(a);if(a===F[11])return 2;throw a}}function
eu(b){return 1===cE(0)?a(j[6],b):0}function
bK(e,d){var
f=c(B[4][5],e,d),b=c(t[3][3],f,aY);return b?b[1]:a(U[1],ev)}function
ew(b,a){var
d=c(B[4][5],b,a);return c(t[3][3],d,aY)}var
aZ=a(w[16],0),ex=c(w[18],0,aZ);function
ey(f){var
b=a(w[33][1],f),c=b[2],d=bK(c,a(g[17][3],b[1]))[1];function
h(a){return e(w[19],ex,d,1)}return a(F[27],h)}function
ez(c){function
b(b,a){return e(w[23],aZ,a,0)}return a(F[27],b)}function
cF(d){try{var
b=c(w[26],aZ,d);return b}catch(b){b=H(b);if(b===w[25]){var
e=a(w[33][3],d);return bK(e[2],e[1])[1]}throw b}}function
eA(a){return c(w[26],aZ,a)}function
eB(c){var
b=a(aX[9],c);return b?a(aG[2][1][1],b[1]):a(j[6],eC)}var
x=[0,er,es,et,cD,cE,eu,aY,bK,ew,eA,cF,eB,function(k){var
b=cF(k);if(b){var
c=b[1];if(typeof
c==="number")switch(c){case
0:var
g=b[2];if(g){var
h=g[1];if(typeof
h==="number")var
f=0;else
var
i=h[1],f=1}else
var
f=0;if(!f){var
l=a(d[1],eD);return e(j[3],0,0,l)}break;case
1:return eG;default:return eH}else
var
i=c[1];return 0===i?eE:eF}return eI},ey,ez];az(414,x,"Decl_mode_plugin.Decl_mode");function
cI(b){var
c=b[3],d=b[2],e=b[1];return a(g[17][47],c)?d:[4,e,d,c]}function
ar(d,b,a){var
e=c(d,b,a[2]);return[0,a[1],e]}function
bM(d,a,b){return[0,a,c(d,a,b)]}function
bN(b,a){return 0===a[0]?[0,c(y[7],b,a[1])]:[1,a[1]]}function
aH(b,a){var
d=a[2];return[0,c(o[1][9][4],b,a[1]),d]}function
cJ(j,b,d){if(0===d[0]){var
e=d[1],f=e[2],g=f[1],k=f[2],l=e[1],m=a(y[7],b),n=[0,[0,l,[0,g,c(ah[15],m,k)]]];return[0,aH(g,b),n]}var
h=d[1],o=[1,ar(j,b,h)],i=h[1],p=i?aH(i[1],b):b;return[0,p,o]}function
as(c,b,a){function
d(a,b){return cJ(c,a,b)}return e(g[17][cl],d,b,a)[2]}function
a0(k,j,b){var
d=c(k,j,b[1]),e=d[1],l=d[2],m=b[3],i=a(y[5],e),n=a(a(ah[15],i),m),o=b[2],f=a(y[7],e),h=a(g[17][12],f);return[0,l,a(a(ah[15],h),o),n]}function
cK(d,b){function
f(d,b){var
e=b[2],f=e[1],g=e[2],h=b[1],i=a(y[7],d),j=[0,h,[0,f,c(ah[15],i,g)]];return[0,aH(f,d),j]}return e(g[17][cl],f,b,d)}function
eK(d,a){var
f=a[2],h=a[1],i=y[7];function
j(a,b){return cJ(i,a,b)}var
b=e(g[17][cl],j,d,h),c=b[1],k=b[2];return[0,c,[0,k,bN(c,f)]]}function
a1(m,l){var
f=m,b=l;for(;;){switch(b[0]){case
0:var
n=b[2],f=aH(b[3],f),b=n;continue;case
1:var
o=b[4],p=b[3],q=a(g[17][15],a1),r=e(ah[17],q,f,p);return e(g[17][15],a1,r,o);case
2:var
h=b[2];if(h){var
i=h[1];if(0!==i[0])return aH(i[1][2],f)}break;case
3:var
s=b[1],t=a(d[1],eL);return c(u[9],s,[0,j[5],eM,t]);case
4:var
k=b[3],v=a(g[17][10],[0,k[1],[0,b[4],k[2]]]);return e(g[17][15],a1,f,v);case
7:var
b=b[3];continue}return f}}function
a2(d,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return[0,a2(d,b[1])];case
1:return[1,a2(d,b[1])];case
2:return[2,a2(d,b[1])];case
3:var
o=b[1],p=function(a,b){return ar(bN,a,b)};return[3,a0(function(a,b){return bM(p,a,b)},d,o)];case
4:var
q=b[2],r=b[1],s=y[7],t=function(a,b){return ar(s,a,b)};return[4,r,a0(function(a,b){return bM(t,a,b)},d,q)];case
5:return[5,a0(eK,d,b[1])];case
6:return[6,as(y[7],d,b[1])];case
7:return[7,as(y[7],d,b[1])];case
8:return[8,as(y[7],d,b[1])];case
9:var
u=b[1],v=as(y[7],d,b[2]);return[9,c(y[7],d,u),v];case
10:return[10,ar(y[7],d,b[1])];case
11:return[11,ar(y[7],d,b[1])];case
12:var
w=b[3],x=b[1],f=cK(b[2],d),n=f[2];return[12,x,n,c(y[7],f[1],w)];case
13:var
z=b[1];return[13,z,c(y[7],d,b[2])];case
14:return[14,as(y[7],d,b[1])];case
15:var
h=b[2],A=b[3],i=cK(b[1],d),B=i[2];return[15,B,h,as(bN,a1(i[1],h),A)];case
16:var
C=b[1],D=a(y[7],d);return[16,c(g[17][12],D,C)];case
17:var
e=b[2],E=b[1];if(0===e[0])var
j=[0,c(y[7],d,e[1])];else
var
k=e[1],l=y[7],m=function(a,b){return ar(l,a,b)},j=[1,a0(function(a,b){return bM(m,a,b)},d,k)];return[17,E,j];default:return[18,b[1]]}}function
eN(b,a){var
c=a2(b,a[2]);return[0,a[1],c]}function
at(d,c,b,a){return d?N(V[11],0,eO,c,b,a[1])[1]:N(V[11],0,0,c,b,a[1])[1]}var
a3=[C,function(b){return a(cG[48],bL[30])}];function
aI(d,c,b,a){var
f=e(d,c,b,a[2]);return[0,a[1],f]}function
cL(k,c,b){function
d(c,d){if(0===c[0]){var
e=c[1],f=e[2],g=f[2],b=f[1],i=e[1];return g?[6,u[4],[0,b],0,g[1][1],d]:[6,u[4],[0,b],0,[13,[0,i,[1,[0,b]],0,0]],d]}var
h=c[1],j=a(k,h[2]);return[6,u[4],h[1],0,j,d]}return e(g[17][16],d,c,b)}var
cM=[12,u[4],0];function
bO(i,b,h,d){if(d){var
j=d[1],m=d[2],e=a(f[34],h),g=e[1],n=e[3],k=[0,g,c(z[12],b,e[2])],o=g?[0,a(f[s],g[1]),b]:[0,a(f[ab],0),b],p=0===j[0]?[0,k]:[1,c(i,k,j[1])],l=bO(i,o,n,m);return[0,[0,p,l[1]],l[2]]}return[0,0,c(z[12],b,h)]}function
cN(f,e,d,c,a,b){var
g=cL(f,a,b);return bO(e,0,N(V[11],0,0,d,c,g)[1],a)}function
au(c,b,a){function
d(a,b){return a}return cN(function(a){return a[1]},d,c,b,a,cM)[1]}var
eS=a(o[1][5],eR);function
cO(b,a){if(0===a[0]){var
f=a[2],d=a[1];if(f){var
h=b[1];b[1]=[0,[0,[0,d,f[1]],h[1]],h[2]];return a}var
i=b[1],j=i[2],k=i[1],e=c(aq[25],eS,j);b[1]=[0,[0,[0,d,e],k],[0,e,j]];return[0,d,[0,e]]}var
l=a[4],m=a[3],n=a[2],o=a[1];function
p(a){return cO(b,a)}return[1,o,n,c(g[17][12],p,m),l]}function
bP(b){if(0===b[0]){var
i=b[2],m=b[1];if(i)return[1,[0,m,i[1]]];var
n=a(d[1],eT);return e(j[3],0,0,n)}var
k=b[2],l=k[1],o=b[3],p=b[1],q=a(ac[26],l)[1],s=c(g[17][12],bP,o),f=q[6],h=s;for(;;){if(0<f){var
r=[0,[13,[0,u[4],[4,l,f],0,0]],h],f=f-1|0,h=r;continue}return cI([0,p,[0,[0,u[4],[3,k],0]],h])}}function
eU(b){var
c=b[2],d=c[2],a=c[1],e=b[1];if(d){var
f=d[1];return function(b){return[6,u[4],[0,a],0,f[1],b]}}return function(b){return[6,u[4],[0,a],0,[13,[0,e,[1,[0,a]],0,0]],b]}}function
eV(a,c){var
b=a[2];return[6,u[4],[0,b],0,[13,[0,a[1],[1,[0,b]],0,0]],c]}function
eW(a,b){var
c=a[1],d=bP(a[2]);return[7,u[4],[0,c],d,b]}function
cP(b,a){if(0===a[0])return b;var
c=a[4],d=a[3],f=c?[0,[0,c[1],a],b]:b;return e(g[17][15],cP,f,d)}function
a4(j,b,i,h){if(h){var
k=h[2],d=a(j,i),e=d[1],l=d[3],m=[0,e,c(z[12],b,d[2])];if(e){var
g=a4(j,[0,a(f[s],e[1]),b],l,k);return[0,[0,m,g[1]],g[2],g[3]]}throw[0,M,eZ]}return[0,0,b,c(z[12],b,i)]}function
cQ(b,i,h){if(h){var
j=h[2],d=a(f[36],i),e=d[1],k=d[4],l=d[2],m=c(z[12],b,d[3]),n=[0,e,[0,c(z[12],b,l),m]];if(e){var
g=cQ([0,a(f[s],e[1]),b],k,j);return[0,[0,n,g[1]],g[2],g[3]]}throw[0,M,e0]}return[0,0,b,c(z[12],b,i)]}function
a5(k,j,d,b){var
f=e(k,j,d,b[1]),l=f[2],m=f[1],n=b[3],o=a(ap[28],0),p=a(ap[2][6],o),q=c(ah[15],p,n),r=b[2];function
h(a){return N(V[11],0,0,m,d,a[1])[1]}var
i=a(g[17][12],h);return[0,l,a(a(ah[15],i),r),q]}function
bQ(d,a,c,b){return[0,a,e(d,a,c,b)]}function
e_(b,i,h){var
d=h[2],k=h[1];if(0===d[0])var
m=d[1][1],n=function(a,b){return a},l=cN(function(a){return a[1]},n,b,i,k,m),f=[0,l[1],[0,l[2]]];else
var
q=d[1]?a(j[6],e$):[0,au(b,i,k),d],f=q;var
o=f[1];function
p(f,e){var
a=f[1],d=a[1];return d?c(aX[30],[0,d[1],a[2]],e):b}return[0,e(g[17][16],p,o,b),f]}function
fa(b){var
c=b[2],d=c[2],a=c[1],e=b[1];if(d){var
f=d[1];return function(b){return[5,u[4],[0,a],0,f[1],b]}}return function(b){return[5,u[4],[0,a],0,[13,[0,e,[1,[0,a]],0,0]],b]}}function
a6(m,h,i,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return[0,a6(m,h,i,b[1])];case
1:return[1,a6(m,h,i,b[1])];case
2:return[2,a6(m,h,i,b[1])];case
3:var
bd=b[1],be=1,bf=function(b,c,a){return 0===a[0]?[0,at(be,b,c,a[1])]:[1,a[1]]},bg=function(a,b,c){return aI(bf,a,b,c)};return[3,a5(function(a,b,c){return bQ(bg,a,b,c)},h,i,bd)];case
4:var
bh=b[2],bi=b[1],ak=a(x[12],h),ah=c(aX[36],ak,h),ae=e(P[42],0,P[9],h),af=a(P[36],ah),ag=c(P[47],ae,af),p=a(f[aF],ag);if(9===p[0]){var
E=p[2],F=K(a3),ai=p[1],aj=L===F?a3[1]:C===F?a(I[2],a3):a3;if(c(f[136],ai,aj))if(3===E.length-1)var
z=S(E,0)[1],B=1;else
var
B=0;else
var
B=0;if(!B)var
z=a(j[6],eQ)}else
var
z=a(j[6],eP);var
bj=function(a,b,c){return N(V[11],0,[0,[0,z]],a,b,c[1])[1]},bk=function(a,b,c){return aI(bj,a,b,c)};return[4,bi,a5(function(a,b,c){return bQ(bk,a,b,c)},h,i,bh)];case
5:return[5,a5(e_,h,i,b[1])];case
6:return[6,au(h,i,b[1])];case
7:return[7,au(h,i,b[1])];case
8:return[8,au(h,i,b[1])];case
9:var
bl=b[1],bm=au(h,i,b[2]);return[9,at(0,h,i,bl),bm];case
10:var
bn=b[1],bo=1;return[10,aI(function(a,b,c){return at(bo,a,b,c)},h,i,bn)];case
11:var
bp=b[1],bq=1;return[11,aI(function(a,b,c){return at(bq,a,b,c)},h,i,bp)];case
12:var
$=b[2],br=b[1],bb=e(g[17][16],fa,$,b[3][1]),bc=N(V[11],0,0,h,i,bb)[1],_=a4(f[35],0,bc,$);return[12,br,_[1],_[3]];case
13:var
bs=b[1];return[13,bs,at(1,h,i,b[2])];case
14:return[14,au(h,i,b[1])];case
15:var
aa=b[3],ab=b[2],n=b[1],O=m[1];if(O){var
Q=O[1];if(typeof
Q==="number")var
D=0;else
var
k=Q[2],D=1}else
var
D=0;if(!D)var
k=a(j[6],e1)[2];var
T=a(ac[26],k[3]),r=T[1][6]-k[7]|0,as=T[2];if(1-(a(g[17][1],n)===r?1:0)){var
av=a(d[1],e2),aw=a(d[16],0),ax=0===r?a(d[1],e3):a(d[19],r),ay=a(d[1],e4),az=c(d[13],ay,ax),aA=c(d[13],az,aw),aB=c(d[13],aA,av);c(j[7],e5,aB)}var
aC=[0,[0,u[4],[2,k[3]],0]],aD=k[6],aE=function(a){return cj(cH[6],0,0,0,h,t[16],a)},aG=c(g[17][12],aE,aD),aH=function(a){return[1,[0,a[1],a[2][1]]]},aJ=c(g[17][12],aH,n),aK=function(a){return[13,[0,u[4],e6,0,0]]},aL=c(g[17][48],as[6],aK),aM=c(g[18],aJ,aL),aN=c(g[18],aG,aM),aO=cI([0,u[4],aC,aN]),H=c(eJ[8],h,ab),q=H[2],ao=H[1];if(q)if(q[2])var
ap=a(d[1],eX),l=e(j[3],0,0,ap);else
var
J=q[1],M=J[2],aq=J[1],an=cP(0,M),al=function(d,b,a){return[0,[0,d,c(o[1][12][3],b,a)],a]},am=e(o[1][10][11],al,aq,an),l=[0,ao,a(g[17][6],am),M];else
var
ar=a(d[1],eY),l=e(j[3],0,0,ar);var
U=l[3],W=l[2],X=l[1],Y=[0,[0,0,X]],aP=cL(function(b){if(0===b[0])return b[1][1];var
e=b[1];if(e){var
f=e[1];if(1-c(o[1][12][2],f,X)){var
g=a(d[1],e7),h=a(R[1],f),i=a(d[1],e8),k=c(d[13],i,h),l=c(d[13],k,g);c(j[7],e9,l)}return[12,u[4],0]}return[12,u[4],0]},aa,cM),aQ=cO(Y,U),Z=a(g[17][6],Y[1][1]),aR=bP(aQ),aS=e(g[17][16],eW,W,[7,u[4],0,[14,u[4],aR,[0,aO]],aP]),aT=e(g[17][16],eV,Z,aS),aU=e(g[17][16],eU,n,aT),aV=N(V[11],0,0,h,i,aU)[1],s=a4(f[34],0,aV,n),aW=s[1],v=a4(f[34],s[2],s[3],Z),aY=v[1],w=cQ(v[2],v[3],W),aZ=w[2],a0=w[1],y=a(f[36],w[3]),a1=y[4],a2=y[3],a7=y[2],a8=function(b,a){var
c=a[2];return 0===c[0]?[0,b[1],[0,b[2]]]:[0,a[1],[1,c[1]]]};return[15,aW,[0,aY,a0,a7,a2,U,ab],bO(a8,aZ,a(G[56],a1),aa)[1]];case
16:var
bt=b[1],bu=function(a){return N(V[11],0,0,h,i,a[1])[1]};return[16,c(g[17][12],bu,bt)];case
17:var
A=b[2],bv=b[1];if(0===A[0])var
ad=[0,N(V[11],0,0,h,i,A[1][1])[1]];else
var
a9=A[1],a_=1,a$=function(a,b,c){return at(a_,a,b,c)},ba=function(a,b,c){return aI(a$,a,b,c)},ad=[1,a5(function(a,b,c){return bQ(ba,a,b,c)},h,i,a9)];return[17,bv,ad];default:return[18,b[1]]}}var
aJ=[0,eN,function(d,c,b,a){var
e=a6(d,c,b,a[2]);return[0,a[1],e]}];az(434,aJ,"Decl_mode_plugin.Decl_interp");function
ai(p,k){var
f=k[2],b=k[1],q=o[1][9][1];function
r(b,a){return c(o[1][9][4],a,b)}var
s=e(g[17][15],r,q,p),u=c(B[4][1],f,b),v=c(B[4][2],f,b),w=c(B[4][4],f,b),l=[0,a(t[98],f)];try{var
F=N(cW[53],u,l,v,w,s),h=F}catch(b){b=H(b);if(b[1]!==cW[51])throw b;var
x=a(R[1],b[2]),y=a(d[1],fh),z=c(d[13],y,x),h=c(j[7],fi,z)}var
m=l[1],A=h[2],C=h[1],E=c(B[4][5],m,b),i=D(B[4][6],m,C,A,E),n=i[1];return[0,[0,n,0],D(B[4][8],i[3],b,n,i[2])]}function
ae(a){return c(x[8],a[2],a[1])}var
cY=[0,0];function
fj(a){cY[1]=a;return 0}function
cZ(a){return cY[1]}c(fe[4],0,[0,1,0,fl,fk,cZ,fj]);function
c0(g,b){var
d=a(m[1],b),h=a(m[7],b),i=a(m[2],b),j=c(B[4][2],i,d),k=a(m[2],b),l=a(g,c(B[4][5],k,d)),n=a(m[2],b),f=D(B[4][6],n,j,h,l),o=f[1];return[0,[0,o,0],e(B[4][7],f[3],d,f[2])]}function
W(b,a){return c0(function(a){return e(t[3][2],a,x[7],b)},a)}function
aK(a){return c0(function(a){return c(t[3][4],a,x[7])},a)}function
aL(b){var
d=a(m[8],b),f=e(P[42],0,P[9],d);return function(b){var
d=a(P[36],b);return c(P[47],f,d)}}function
c1(g,b){var
d=c(av[4],g,b),e=d[2],f=0===e[6]?1:0,h=d[1],i=f?1-a(bS[19],[0,b,h,e]):f;return i}function
c2(b){var
c=1-a(w[7],b);if(c){var
d=a(x[10],b);if(d)if(typeof
d[1]!=="number")return a(j[6],fm);var
e=0}else
var
e=c;return e}function
fn(c,b){var
d=a(t[69],b),f=a(t[98],d);function
h(a,b){return D(t[95],a[1],a[2],0,b)}return e(g[17][16],h,c,f)}function
fo(b){return 95===J.caml_string_get(a(o[1][7],b),0)?1:0}function
fp(d){function
e(a){if(a){var
d=a[1],b=e(a[2]),f=function(e){var
a=[0,d,0];function
b(b){return ai(a,b)}return c(i[21],b,e)};return c(i[5],f,b)}return i[1]}var
b=a(m[9],d);if(b)var
h=a(G[82],b[2]),f=c(g[17][29],fo,h);else
var
f=0;return a(e(f),d)}function
aj(b,a){return c(p[138],[0,b],a)}function
fq(a){return W(fr,a)}function
fs(d){var
b=a(n[66][1],fq);a(cC[21],b);var
c=a(F[12],0);return a(x[14],c)}function
bT(c){var
d=a(ac[2],0);function
b(g,f){var
c=a(n[37],n[54]),b=e(w[29],d,c,f);return[0,b[1],b[2][1]]}return a(F[26],b)?0:e(fg[9],0,0,3)}function
ak(b){return a(x[15],0)}function
ft(a){return ak(0)}function
fu(c){try{bT(0);var
b=ak(0);return b}catch(b){b=H(b);if(b===$)return a(j[6],fv);throw b}}function
fw(a){return fu(0)}function
c3(b){if(a(w[7],b)){var
c=a(x[11],b);if(c){var
g=c[1];if(typeof
g!=="number"){var
l=a(d[1],fE);return e(j[3],0,0,l)}if(0===g){var
h=c[2];if(h)if(typeof
h[1]!=="number")return ak(0)}}return a(j[6],fD)}var
f=a(x[10],b);if(f){var
i=f[1];if(typeof
i!=="number")return 0;if(0===i){var
k=f[2];if(k)if(typeof
k[1]!=="number"){bT(0);return ak(0)}}}return a(j[6],fF)}var
fH=a(o[1][5],fG);function
bU(j,d){var
b=[0,o[1][9][1]];function
g(h,d){var
j=a(f[aF],h);if(1===j[0]){b[1]=c(o[1][9][4],j[1],b[1]);return a(i[1],d)}var
g=c(m[20],fH,d);b[1]=c(o[1][9][4],g,b[1]);var
k=a(p[75],[0,g,0]),l=a(n[66][8],k),q=N(p[ea],0,[0,g],h,0,cV[7]),r=a(n[66][8],q);return e(i[5],r,l,d)}var
h=e(i[32],g,j,d),k=0,l=b[1],q=[0,function(b){function
d(d){var
b=a(aG[2][1][1],d);if(c(o[1][9][3],b,l))return i[1];var
e=[0,b,0];function
f(a){return ai(e,a)}return a(i[21],f)}var
f=a(m[9],b);return e(i[32],d,f,b)},k],r=[0,function(a){return h},q];return c(i[7],r,d)}var
fJ=a(d[1],fI),fK=c(j[2],0,fJ),c4=[0,c(n[18],0,fK)];function
fL(a){c4[1]=a;return 0}function
fM(a){return c4[1]}var
fN=a(n[13],0),a8=c(n[14],fN,fM);function
fO(b){return a(d[25],fP)}var
fS=D(cX[2],fR,fQ,0,fO);function
aM(d,b){function
f(b){return cZ(0)?a(j[6],fT):(c(fS,0,0),a(x[1],0),[0,0,a(t[69],b)])}var
g=a(n[66][8],p[41]),h=[0,c(i[5],d,g),0],k=a(i[20],h);return e(i[4],k,f,b)}function
c5(d,b){var
e=a(n[66][8],a8);function
f(a){return bU(d,a)}return aM(c(i[5],f,e),b)}function
X(b,a){return[C,function(c){return e(bL[4],fU,b,a)}]}var
a9=X(fW,fV),a_=X(fY,fX),a$=X(f0,fZ),ba=X(f2,f1),bb=X(f4,f3),bc=X(f6,f5),bd=X(f8,f7),be=X(f_,f9),bf=X(ga,f$),bg=X(gc,gb);function
bV(d,b,a){if(a){var
e=a[2],f=a[1];return d===f[1]?c(g[18],b,e):[0,f,bV(d,b,e)]}throw $}function
gd(d,b,p,k){var
q=b[2],r=a(aL(k),q),j=a(f[39],r),l=j[2],n=a(f[aF],j[1]);if(11===n[0]){var
h=n[1],i=h[1],s=h[2];if(c1(d,i)){var
o=c(av[4],d,i),u=c(av[17],h,[0,o[1],o[2]]),v=function(q,o){var
r=[0,a(f[131],[0,[0,i,q+1|0],s]),l],u=a(f[59],r),v=c(f[76],o,l),w=c(cU[26],d,v)[1];function
n(i,d,b){if(b){var
g=i+1|0,p=b[2],q=b[1],h=n(g,[0,a(f[ab],g),d],p),r=h[3],s=h[2],t=h[1],u=a(aG[1][1][3],q),v=c(z[12],d,u),j=a(m[8],k),l=e(P[42],0,P[14],j),o=a(P[36],v);return[0,t,s,[0,[0,g,c(P[46],l,o)],r]]}return[0,i,d,0]}var
x=a(g[17][6],w),h=n(b[3],0,x),j=h[3],y=h[1],A=[0,u,a(g[17][6],h[2])],B=a(f[59],A),C=e(t[96],b[1],[0,B,ge],b[5]),E=bV(b[1],j,b[4]);function
F(b,a){return D(t[95],a[1],a[2],0,b)}var
G=e(g[17][15],F,C,j),H=a(g[17][6],j);function
I(a){return c(g[22][3],[0,a[1],a[2],y,E,G],p)}return c(g[17][11],I,H)};return c(g[19][14],v,u)}}return 0}function
c6(b,g){var
a=g;for(;;){if(a){var
d=a[2],e=a[1],f=e[1],h=e[2];if(c(t[88],b,f)){var
a=d;continue}var
i=c6(b,d);return[0,[0,f,c(a7[92],b,h)],i]}return 0}}function
c7(h,W,n){var
i=a(m[7],n),u=a(t[69],n),b=a(m[8],n),k=e(cS[4],b,[0,u],i),X=a(f[ee],k);function
v(w,t,q,o){if(o){var
x=o[2],y=o[1],z=y[1],b=c(G[55],q,y[2]),Y=e(aq[9],w,b,0),h=e(p[14],t,Y,n),A=c(aX[30],[0,h,b],w),Z=e(cS[4],A,[0,u],b),B=a(f[ee],Z),_=[0,[0,z,a(f[s],h)],q];if(a(g[17][47],x)){var
$=c(G[55],_,W);return[0,B,b,e(f[50],h,b,$)]}var
r=v(A,[0,h,t],[0,[0,z,a(f[s],h)],q],x),k=r[2],D=r[1],m=e(f[50],h,b,r[3]),aa=a(f[s],h);if(c(G[54],aa,k)){var
l=e(f[50],h,b,k);if(0===D){if(0===X){var
E=K(bb),ab=[0,b,l],ac=L===E?bb[1]:C===E?a(I[2],bb):bb,F=K(bc),ad=a(f[O],[0,ac,ab]),ae=[0,b,l,i,m],af=L===F?bc[1]:C===F?a(I[2],bc):bc;return[0,0,ad,a(f[O],[0,af,ae])]}var
H=K(bd),ag=[0,b,l],ah=L===H?bd[1]:C===H?a(I[2],bd):bd,J=a(f[O],[0,ah,ag]),M=K(be),ai=[0,b,l,a(f[bw],[0,0,J,i]),m],aj=L===M?be[1]:C===M?a(I[2],be):be;return[0,2,J,a(f[O],[0,aj,ai])]}var
N=K(bf),ak=[0,b,l],al=L===N?bf[1]:C===N?a(I[2],bf):bf,P=a(f[O],[0,al,ak]),Q=K(bg),am=[0,b,l,a(f[bw],[0,0,P,i]),m],an=L===Q?bg[1]:C===Q?a(I[2],bg):bg;return[0,2,P,a(f[O],[0,an,am])]}if(0===B)if(0===D){var
R=K(a9),ao=[0,b,k],ap=L===R?a9[1]:C===R?a(I[2],a9):a9,S=K(a_),ar=a(f[O],[0,ap,ao]),as=[0,b,k,i,m],at=L===S?a_[1]:C===S?a(I[2],a_):a_;return[0,0,ar,a(f[O],[0,at,as])]}var
T=K(a$),au=[0,b,k],av=L===T?a$[1]:C===T?a(I[2],a$):a$,U=a(f[O],[0,av,au]),V=K(ba),aw=[0,b,k,a(f[bw],[0,0,U,i]),m],ax=L===V?ba[1]:C===V?a(I[2],ba):ba;return[0,2,U,a(f[O],[0,ax,aw])]}var
ay=a(d[1],gg);return e(j[3],0,gh,ay)}var
l=v(b,0,0,h)[3],o=[0,l,[0,a(f[ab],1)]];return a(f[O],o)}function
aN(C,B,k,d){try{var
o=a(m[8],d),b=a(m[7],d),u=fn([0,[0,0,b],k],d),h=a(g[22][2],0),F=0,v=0,w=function(b,a){return c(U[5],b,a[1])},x=[0,0,b,e(g[17][15],w,v,k),[0,[0,0,b],0],u];c(g[22][3],x,h);var
q=function(c){for(;;){var
b=a(g[22][9],h);try{var
i=b[2],l=[0,a(cR[5],0)],m=cj(cR[8],o,b[5],1,l,B,i);if(0<c)var
f=q(c-1|0);else
var
n=e(t[96],b[1],[0,C,gf],m),p=bV(b[1],k,b[4]),f=[0,b[1],b[2],b[3],p,n];return f}catch(c){c=H(c);if(a(j[22],c)){gd(o,b,h,d);continue}throw c}}};try{var
i=q(F)}catch(a){a=H(a);if(a===g[22][1])throw $;throw a}var
y=a(f[ab],0),z=c(a7[92],i[5],y),A=[0,c6(i[5],i[4]),z],l=A}catch(b){b=H(b);if(b!==$)throw b;var
l=a(j[6],gi)}var
r=l[2],s=l[1];if(a(g[17][47],s)){var
D=a(p[45],r);return c(n[66][8],D,d)}var
E=c7(s,r,d);return c(m[45],E,d)}function
c8(d,c,b){return 0===b[0]?b[1]:b[1]?a(j[6],gj):a(m[7],c)}function
c9(h,e,r,b){if(h)try{var
o=a(m[8],b),p=a(x[12],o),q=[0,a(f[s],p),0],d=q}catch(b){b=H(b);if(b[1]!==j[5])throw b;var
d=a(j[6],gk)}else
var
d=0;function
k(b){var
f=e[2];return f?bU(c(g[18],d,f[1]),b):a(i[1],b)}function
l(b){var
d=e[3];if(d){var
f=d[1],g=a(ap[28],0),h=c(ap[19],g,f);return c(n[66][8],h,b)}return c(n[66][8],a8,b)}return aM(c(i[5],k,l),b)}function
c_(r,q,p,g,b){var
h=ae(b),j=g[1],k=j[1];if(k)var
d=k[1];else
var
y=a(o[1][5],gl),d=c(m[20],y,b);var
l=e(r,h,b,j[2]),t=[0,function(b){return q?aN(a(f[s],d),l,0,b):a(i[1],b)},0];function
u(a){return c9(p,g,h,a)}var
v=[0,c(i[5],aK,u),t],w=aj(d,l),x=a(n[66][8],w);return e(i[11],x,v,b)}var
Q=[C,function(b){return a(cG[48],bL[30])}];function
gm(h,e){var
i=c(m[19],e,h),k=a(aL(e),i),d=a(f[aF],k);if(9===d[0]){var
b=d[2],g=K(Q),l=d[1],n=L===g?Q[1]:C===g?a(I[2],Q):Q;if(c(G[64],l,n))if(3===b.length-1){var
o=S(b,2)[3],p=S(b,1)[2];return[0,S(b,0)[1],p,o]}return a(j[6],go)}return a(j[6],gn)}function
gp(D,B,d,b){try{var
am=a(m[8],b),an=a(x[12],am),g=an}catch(b){b=H(b);if(b[1]!==j[5])throw b;var
g=a(j[6],gq)}var
k=gm(g,b),l=k[3],q=k[2],r=k[1];function
E(b){var
c=d[2];return c?bU(c[1],b):a(i[1],b)}function
F(b){var
e=d[3];if(e){var
f=e[1],g=a(ap[28],0),h=c(ap[19],g,f);return c(n[66][8],h,b)}return c(n[66][8],a8,b)}function
t(a){return aM(c(i[5],E,F),a)}var
u=d[1][1];if(u)var
h=u[1];else
var
al=a(o[1][5],gr),h=c(m[20],al,b);function
v(c,b){return D?aN(a(f[s],h),c,0,b):a(i[1],b)}if(0===B){var
w=K(Q),G=[0,r,d[1][2],l],J=L===w?Q[1]:C===w?a(I[2],Q):Q,y=a(f[O],[0,J,G]),M=0,N=[0,function(a){return v(y,a)},M],P=a(f[s],g),R=a(p[45],P),S=[0,t,[0,a(n[66][8],R),0]],T=a(p[cn],q),U=a(n[66][8],T),V=c(i[11],U,S),W=[0,c(i[5],aK,V),N],X=aj(h,y),Y=a(n[66][8],X);return e(i[11],Y,W,b)}var
z=K(Q),Z=[0,r,q,d[1][2]],_=L===z?Q[1]:C===z?a(I[2],Q):Q,A=a(f[O],[0,_,Z]),$=0,aa=[0,function(a){return v(A,a)},$],ab=a(f[s],g),ac=a(p[45],ab),ad=[0,a(n[66][8],ac),[0,t,0]],ae=a(p[cn],l),af=a(n[66][8],ae),ag=c(i[11],af,ad),ah=[0,c(i[5],aK,ag),aa],ai=aj(h,A),ak=a(n[66][8],ai);return e(i[11],ak,ah,b)}function
c$(h,d,b){var
k=ae(b),j=d[1];if(j)var
g=j[1];else
var
x=a(o[1][5],gs),g=c(m[20],x,b);function
l(b){if(h){var
c=d[2];return aN(a(f[s],g),c,0,b)}return a(i[1],b)}var
p=k[1],q=h?2:1,r=[0,[0,q,p]],t=0,u=[0,l,[0,function(a){return W(r,a)},t]],v=aj(g,d[2]),w=a(n[66][8],v);return e(i[11],w,u,b)}function
aO(f,e,d){if(e)var
b=e[1];else
var
k=a(o[1][5],gt),b=c(m[20],k,d);var
g=[0,a(f,b),0],h=a(p[23],b),j=[0,a(n[66][8],h),g];return c(i[7],j,d)}function
bh(d,b){var
e=i[1];function
f(d){var
b=d[1],e=b[1];function
f(d){var
e=c(p[4],0,[0,d,b[2]]);return a(n[66][8],e)}function
g(a){return aO(f,e,a)}return a(i[5],g)}return D(g[17][16],f,d,e,b)}function
da(b,a){return a?[0,b,da(b+1|0,a[2])]:0}function
bW(b,e){if(b){var
d=b[1][1],j=bW(b[2],e),g=c(z[8],1,j),h=d[1];if(h)var
k=a(f[s],h[1]),i=c(G[59],k,g);else
var
i=g;return a(f[bH],[0,d[1],d[2],i])}return e}function
db(d,b){if(b){var
e=b[1],h=b[2],i=a(f[34],d)[2],j=[0,a(f[ab],e),0],g=db(c(f[76],d,j),h);return[0,[0,[0,e,i],g[1]],g[2]]}return[0,0,d]}function
gv(n,b){var
o=c(m[19],b,n),p=a(aL(b),o),e=a(f[39],p),q=e[2],r=e[1],d=a(m[8],b),h=a(f[aF],r);if(11===h[0]){var
i=h[1],j=i[1];if(c1(d,j)){var
k=c(av[4],d,j),l=c(av[17],i,[0,k[1],k[2]]);if(1-(1===l.length-1?1:0))throw $;var
s=S(l,0)[1],t=c(f[76],s,q),u=c(cU[26],d,t)[1];return a(g[17][1],u)}}throw $}function
dc(g,f,d,b){if(0<g){var
j=a(o[1][5],gw),h=c(m[20],j,b),k=[0,h,f],l=g-1|0,q=function(a){return dc(l,k,d,a)},r=a(p[23],h),s=a(n[66][8],r);return e(i[5],s,q,b)}return c(d,f,b)}function
af(h,l,k,b,e){if(k){if(b){var
r=b[1][1],t=b[2],q=k[2],d=k[1],y=function(e){try{var
x=gv(d,e),k=x}catch(b){b=H(b);if(b!==$)throw b;var
k=a(j[6],gx)}var
m=0;function
o(a){var
d=c(g[17][8],a,q);return function(a){return af(h,l,d,b,a)}}var
r=0,t=[0,function(a){return dc(k,r,o,a)},m],u=a(f[s],d),v=a(p[eq],u),w=[0,a(n[66][8],v),t];return c(i[7],w,e)},u=r[1];if(u)var
v=u[1],z=0,A=[0,[0,v,1],l],B=[0,function(a){return af(h,A,q,t,a)},z],C=a(p[81],[0,[0,d,v],0]),E=[0,a(n[66][8],C),B],w=a(i[7],E);else
var
I=[0,[0,d,0],l],w=function(a){return af(h,I,q,t,a)};var
F=c(p[4],0,[0,d,r[2]]),G=a(n[66][8],F);return D(i[33],G,w,y,e)}return a(j[6],gy)}if(b){if(h){var
J=a(o[1][5],gz),x=c(m[20],J,e),K=function(b){return a(j[6],gA)},L=[0,x,0],M=0,N=1,O=function(a){return af(N,M,L,b,a)},P=a(p[23],x),Q=a(n[66][8],P);return D(i[33],Q,O,K,e)}return a(j[6],gB)}return a(i[1],e)}function
dd(d,b){if(d){var
f=d[1],g=d[2],h=c(m[15],b,f),j=function(a){return dd(g,a)},k=0,l=function(a){return aN(f,h,k,a)};return e(i[5],l,j,b)}return a(i[1],b)}function
de(b,d){if(b){var
e=b[1],i=de(b[2],d),j=c(z[8],1,i),g=e[1];if(g){var
h=g[1],k=a(f[s],h),l=c(G[59],k,j);return a(f[bw],[0,[0,h],e[2],l])}throw[0,M,gD]}return d}function
gF(c){var
d=c[1],e=c[2];if(d){var
f=d[1],b=a(bR[6],e)[1];if(typeof
b!=="number"&&0===b[0])if(b[1][2]===f)return 1;return 0}return 0}function
df(i,h){var
b=i;for(;;){var
e=b[1],d=a(bR[6],b[2]);if(typeof
d[1]==="number"){var
b=[0,0,a(ac[26],h)[2][12]];continue}var
f=d[2],j=function(b){var
d=a(bR[6],b)[2];function
f(a){return[0,e,a]}return c(g[19][15],f,d)};return c(g[19][15],j,f)}}function
bi(e,a,d,b){var
f=df(d,a);function
h(d,a){var
e=c(b,d,a);return[0,c(g[19][15],gF,a),e]}return[1,e,a,c(g[19][16],h,f)]}function
dg(e,d,b){if(1===b[0]){var
f=b[3],h=b[2],i=b[1],j=function(b,a){var
e=a[1];return[0,e,c(d,b,a[2])]},k=c(g[19][16],j,f);return[1,a(e,i),h,k]}return a(U[2],gH)}function
dh(d,b,a){function
c(b,a){return 0}return bi(o[1][9][1],b,a,c)}function
di(v,d,b){var
l=a(m[7],b),n=a(m[8],b),h=c(m[15],b,d),w=c(G[45],d,l),x=a(aL(b),h),o=a(f[39],x),y=o[2],z=o[1];try{var
I=a(f[43],z),p=I}catch(b){b=H(b);if(b!==f[28])throw b;var
p=a(j[6],gI)}var
i=p[1],q=a(ac[26],i),r=q[1],A=q[2];if(0===v)var
k=r[6],s=0;else
var
k=r[7],s=[0,i[2]];var
t=c(g[17][99],k,y),u=t[2],B=t[1];function
C(a,d){var
e=c(m[15],b,a),f=[0,e,c(G[59],a,d)];return c(aq[17],n,f)}var
D=[0,h,c(G[59],d,l)],E=c(aq[17],n,D),F=e(g[17][16],C,u,E);return[0,w,[0,d,h,i,F,u,B,k,[0,s,A[12]]]]}function
bX(c,b,a){return 0<a?[0,b,bX(c,b,a-1|0)]:[2,c]}function
aP(b,c){var
i=b[1];if(c){var
d=c[2],f=c[1];if(f){var
j=f[2],k=f[1],h=k[1],m=k[2];if(0===h[0]){var
n=aP(b,[0,j,d]);return[0,a(o[1][9][5],i),n]}var
l=h[2],p=h[3],q=l[2],r=l[1],s=function(f,c){if(f===(q-1|0)){var
h=0,k=function(a,b){return[0,b,S(c,a)[a+1]]},l=aP(b,[0,e(g[17][69],k,h,p),[0,j,d]]);return[0,[0,a(o[1][9][5],i),l]]}return 0};return bi(o[1][9][1],r,m,s)}return[2,aP(b,d)]}return[3,b]}function
aw(f,l,b){var
m=f[1];if(l){var
h=l[2],n=l[1];if(n){var
k=n[2],r=n[1],s=r[2],p=r[1];if(0===p[0])switch(b[0]){case
0:var
C=b[1],D=aw(f,[0,k,h],b[2]);return[0,c(o[1][9][4],m,C),D];case
1:var
E=function(b,a){return bj(f,1,[0,k,h],a)};return dg(a(o[1][9][4],m),E,b);default:var
F=a(d[1],gP);return e(j[3],0,0,F)}var
t=p[3],u=p[2],v=u[2],w=u[1];switch(b[0]){case
0:var
x=b[2],i=b[1];return bi(i,w,s,function(b,a){if(b===(v-1|0)){var
d=0,j=function(b,c){return[0,c,S(a,b)[b+1]]},l=e(g[17][69],j,d,t),n=aw(f,[0,l,[0,k,h]],bX(x,i,a.length-1));return[0,[0,c(o[1][9][4],m,i),n]]}return[0,[0,i,bX(x,i,a.length-1)]]});case
1:if(1-c(o[37],w,b[2]))a(j[6],gQ);if(1===b[0]){var
q=b[2],y=b[3],z=b[1],A=df(s,q),B=function(a,b){var
c=b[2],i=b[1],j=S(A,a)[a+1];if(a===(v-1|0))var
l=0,m=function(a,b){return[0,b,S(j,a)[a+1]]},d=bj(f,0,[0,e(g[17][69],m,l,t),[0,k,h]],c);else
var
d=c;return[0,i,d]};return[1,z,q,c(g[19][16],B,y)]}return a(U[2],gG);default:var
G=a(d[1],gR);return e(j[3],0,0,G)}}if(2===b[0])return[2,aw(f,h,b[1])];var
H=a(d[1],gS);return e(j[3],0,0,H)}if(3===b[0])return[3,b[1]];var
I=a(d[1],gT);return e(j[3],0,0,I)}function
bj(b,h,e,d){var
f=b[1];if(d){var
g=d[1],i=g[1],j=bk(b,h,e,g[2]);return[0,[0,c(o[1][9][4],f,i),j]]}var
k=aP(b,e);return[0,[0,a(o[1][9][5],f),k]]}function
bk(f,h,g,b){var
i=f[1];if(0<h)switch(b[0]){case
0:var
k=b[1],l=bk(f,h,g,b[2]);return[0,c(o[1][9][4],i,k),l];case
1:var
m=function(b,a){return bj(f,h+1|0,g,a)};return dg(a(o[1][9][4],i),m,b);case
2:return[2,bk(f,h-1|0,g,b[1])];default:var
n=a(d[1],gU);return e(j[3],0,0,n)}return aw(f,g,b)}function
dj(e,d){var
a=d;for(;;){if(a){var
b=a[1],f=a[2];if(c(o[2][4],b[1],e))return b[2];var
a=f;continue}throw $}}function
bY(h,k,b,i){var
l=a(f[79],k)[1],m=a(f[39],k),p=m[2],q=a(f[43],m[1])[1];if(1-c(o[37],q,b[3])){var
r=a(d[1],gV),s=a(d[16],0),u=e(ad[4],i,t[16],h),v=c(d[13],u,s),w=c(d[13],v,r);c(j[7],gW,w)}var
n=c(g[17][99],b[7],p),x=n[2];if(1-e(g[17][24],G[64],n[1],b[6])){var
y=a(d[1],gX),A=a(d[16],0),B=e(ad[4],i,t[16],h),C=c(d[13],B,A),D=c(d[13],C,y);c(j[7],gY,D)}var
E=c(g[18],x,[0,h,0]),F=b[4],H=a(g[17][1],l),I=[0,c(z[8],H,F),E],J=a(f[59],I),K=c(a7[22],t[16],J);return c(f[64],l,K)}function
bZ(g,j,e,b,d){if(e){var
k=e[1];if(0===k[0])var
o=k[1],p=o[2],h=o[1];else{var
i=k[1],l=i[2],y=i[1];if(0!==l[0]){var
q=l[1],A=bZ(g,j,e[2],b,d),B=c(z[8],1,A),r=i[1],C=r?c(z[20],r[1],B):b;if(q){var
n=q[1],D=a(f[s],n);try{var
E=dj([0,n],g[1]),t=E}catch(a){a=H(a);if(a!==$)throw a;var
t=dj([0,n],g[2])[2]}var
u=bY(D,t,j,a(m[8],d))}else
var
u=a(m[7],d);return a(f[bH],[0,i[1],u,C])}var
p=l[1],h=y}var
v=bZ(g,j,e[2],b,d),w=c(z[8],1,v),x=h?c(z[20],h[1],w):b;return a(f[bH],[0,h,p,x])}return b}function
dk(j,b,h,i,d){var
k=a(m[8],d),l=bZ(b,h,i,bY(b[3],b[4],h,k),d);function
n(b,d){var
g=b[1];if(g){var
h=g[1],i=c(z[8],1,d);return e(f[52],h,b[2],i)}var
j=c(z[8],1,d);return a(f[bH],[0,0,b[2],j])}function
o(b,d){var
e=b[1];if(e){var
g=e[1],h=c(z[8],1,d);return D(f[51],g,b[2][1],b[2][2],h)}var
i=c(z[8],1,d);return a(f[123],[0,0,b[2][1],b[2][2],i])}var
p=e(g[17][16],o,b[2],l),q=c(g[18],j,b[1]);return e(g[17][16],n,q,p)}function
dl(g,f,c,e,d){var
b=d;for(;;){if(typeof
b==="number"){if(0===b)return a(j[6],gZ);var
b=[0,dh(f,c[3],c[8])];continue}return[0,aw(g,[0,[0,[0,e,c[8]],0],0],b[1])]}}function
dm(a){function
b(a){return[0,a,g2]}return c(g[17][12],b,a)}function
dn(i,b){function
f(f){var
b=f[2],k=f[1];if(b)var
c=b[1],g=[0,[0,c[1],[0,i,c[2]]],b[2]];else
var
h=a(d[1],g3),g=e(j[3],0,0,h);return[0,k,g]}return c(g[17][12],f,b)}function
dp(h,f,a){function
b(b){var
a=b[1],d=b[2],e=c(o[1][9][3],a,f)?[0,h]:0;return[0,a,[0,[0,e,0],d]]}return c(g[17][12],b,a)}function
g4(k){var
b=k[2],r=k[1];if(b){var
h=b[1],l=h[1];if(b[2])if(l)var
m=b[2],n=m[1],s=m[2],t=n[2],u=n[1],v=l[1],w=[0,v,a(g[17][6],h[2])],i=[0,[0,u,[0,a(f[59],w),t]],s];else
var
p=b[2],q=p[1],x=p[2],y=q[1],i=[0,[0,y,c(g[18],h[2],q[2])],x];else
var
i=b;var
o=i}else
var
z=a(d[1],g5),o=e(j[3],0,0,z);return[0,r,o]}function
dq(a){return c(g[17][12],g4,a)}function
dr(n,b,h,d){var
p=a(f[s],d),i=c(m[19],h,d),q=a(f[79],i)[1],j=a(f[39],i),r=j[2],t=a(f[43],j[1])[1];if(c(o[37],t,b[3])){var
k=c(g[17][99],b[7],r),u=k[2],v=k[1];try{var
A=e(g[17][24],G[64],v,b[6]),l=A}catch(a){a=H(a);if(a[1]!==fc)throw a;var
l=0}if(l){var
w=c(g[18],u,[0,p,0]),x=[0,a(f[s],n),w],y=a(f[59],x),z=c(a7[22],h[2],y);return c(f[66],q,z)}throw[0,M,g6]}throw[0,M,g7]}function
g8(b){return a(d[25],g9)}var
ha=D(cX[2],g$,g_,0,g8);function
bl(y,x,r,Q,P,w,O,h){var
l=Q,k=P,b=O;for(;;)switch(b[0]){case
0:var
T=b[2];if(k){var
U=k[2],l=dn(k[1],l),k=U,b=T;continue}var
V=a(d[1],hb);return e(j[3],0,hc,V);case
1:var
q=b[2],W=b[3],X=b[1];if(k){var
t=k[1],Y=k[2],z=a(ac[26],q),Z=z[1][6],_=a(m[7],h),u=a(m[8],h),A=c(m[15],h,t),$=a(aL(h),A),B=a(f[39],$),aa=B[2],C=a(f[43],B[1]),ad=C[2];if(c(o[37],C[1],q)){var
D=c(g[17][99],Z,aa),E=D[1],ae=D[2],af=function(a,b){var
d=c(m[15],h,a),e=[0,d,c(G[59],a,b)];return c(aq[17],u,e)},ag=[0,A,c(G[59],t,_)],ah=c(aq[17],u,ag),ai=e(g[17][16],af,ae,ah),aj=e(bS[76],u,q,4),ak=c(av[17],[0,q,ad],z),al=function(b){var
d=c(f[76],b,E),e=a(f[88],d);return c(p[15],e,h)},F=c(g[19][15],al,ak),am=function(b,c){return a(f[ab],b+1|0)},an=[0,aj,ai,t,c(g[19][16],am,F)],ao=a(f[cn],an),ap=function(b,e,v){var
h=e[2],j=e[1],k=S(F,b)[b+1];function
m(b,c){if(c){var
d=c[1],e=m(b+1|0,c[2]),g=e[3],h=e[2],i=e[1];return S(j,b)[b+1]?[0,[0,a(f[s],d),i],[0,d,h],g+1|0]:[0,[0,a(f[s],d),i],h,g]}if(b===j.length-1)return[0,Y,0,w];throw[0,M,hd]}var
d=m(0,k),z=d[3],A=d[2],B=d[1],C=0;if(h)var
t=h[1],D=t[2],G=t[1],H=function(a){return c(o[1][9][3],a[1],G)},I=c(g[17][29],H,l),J=[0,a(f[128],[0,q,b+1|0]),E],K=dp(a(f[59],J),X,I),u=function(a){return bl(y,x,r,K,B,z,D,a)};else{c(ha,0,0);var
u=a(r,a(f[ab],1))}var
L=[0,u,C],N=[0,function(b){function
d(c){return dr(a(R[12],y),x,b,c)}var
e=c(g[17][12],d,A),f=a(p[cB],e);return c(n[66][8],f,b)},L];function
O(b){var
c=a(p[23],b);return a(n[66][8],c)}var
P=[0,c(i[32],O,k),N];return c(i[7],P,v)},ar=c(g[19][16],ap,W),as=a(m[45],ao);return e(i[12],as,ar,h)}throw[0,M,he]}var
at=a(d[1],hf);return e(j[3],0,hg,at);case
2:var
au=b[1],l=dq(l),b=au;continue;default:var
H=b[1],I=H[2],J=I[1],K=H[1],aw=I[2];if(k){var
ax=a(d[1],hh);return e(j[3],0,hi,ax)}var
v=c(o[1][12][3],K,l);if(v){var
L=v[1];if(!L[1])if(!v[2]){var
az=L[2],aA=function(b){return a(f[ab],b+1|0)},aB=c(g[17][48],J+aw|0,aA),N=c(g[17][99],J,aB),aC=N[1],aD=c(g[17][8],az,N[2]),aE=c(g[17][7],aC,aD),aF=[0,a(f[s],K),aE],aG=a(r,a(f[59],aF)),aH=a(n[66][8],p[17]),aI=c(i[26],w,aH);return e(i[5],aI,aG,h)}}var
ay=a(d[1],hj);return e(j[3],0,0,ay)}}function
ds(i,b){var
d=[0,function(j){var
k=a(cT[6],j),l=a(m[7],b),f=a(m[8],b),g=cj(cH[6],0,0,0,f,t[16],i);function
d(a){return 2===a[0]?[13,[0,u[4],hk,0,0]]:c(ff[7],d,a)}var
h=d(g),e=N(V[8],0,f,k,[0,[0,l]],h);return a(cT[21][5],[0,e[2],e[1]])}],e=c(p[160][1],0,d);return c(n[66][8],e,b)}function
ht(a){return W(hu,a)}function
hA(f,m){var
b=m;for(;;)if(typeof
b==="number"){a(x[14],f);return a(F[10],hB)}else
switch(b[0]){case
18:var
h=b[1];if(typeof
h==="number")return 0===h?a(F[10],hC):0;if(0===h[1])return ak(0);var
n=a(w[9],f),o=a(g[17][3],n),k=a(w[33][1],f),p=k[2],q=k[1];try{var
t=a(g[17][3],q),u=c(B[4][1],p,t),l=u}catch(b){b=H(b);if(b[1]===fb)if(J.caml_string_notequal(b[2],hD))var
i=0;else
var
l=a(ac[2],0),i=1;else
var
i=0;if(!i)throw b}try{c(bS[79],l,o);var
s=ft(0);return s}catch(b){b=H(b);if(b[1]===fd[1])if(14===b[3][0]){var
r=a(d[1],hE);return e(j[3],0,0,r)}throw b}case
0:case
1:case
2:var
b=b[1];continue;case
10:case
11:case
14:case
15:case
17:return a(x[14],f);default:return 0}}function
dt(O,r){var
v=O[2];for(;;){if(typeof
v==="number"){c2(r);var
B=1,y=1}else
switch(v[0]){case
18:var
L=v[1];if(typeof
L==="number"){if(a(w[28],r))ak(0);else{var
k=a(w[7],r)?a(x[11],r):a(x[10],r);if(k){var
G=k[1];if(typeof
G==="number")if(0===G){var
U=k[2];if(U){var
V=U[1];if(typeof
V==="number")var
R=1;else
var
X=V[1],E=1,R=0}else
var
R=1;if(R){var
ar=a(d[1],fx);e(j[3],0,0,ar);var
C=1,E=0}}else
var
C=0,E=0;else
var
X=G[1],E=1;if(E)var
C=0===X?(a(j[6],fy),1):(a(j[6],fz),1)}else
var
C=0;if(!C){if(typeof
L==="number"){switch(L){case
0:if(k)var
t=0,l=0;else
var
l=1;break;case
1:if(k){var
Z=k[1];if(typeof
Z==="number")if(2===Z)var
t=0,l=0,S=0;else
var
S=1;else
var
S=1;if(S)var
l=1}else
var
t=0,l=0;break;default:if(k){var
_=k[1];if(typeof
_==="number")if(2===_)var
l=1,T=0;else
var
T=1;else
var
T=1;if(T)var
t=0,l=0}else
var
t=0,l=0}if(l){bT(0);ak(0);var
t=1}}else
var
t=0;if(!t)if(k){var
Y=k[1],aX=typeof
Y==="number"?2===Y?(a(j[6],fB),1):0:0;if(!aX)a(j[6],fA)}else
a(j[6],fC)}}var
B=0,y=1}else
var
y=0;break;case
14:case
15:var
y=0;break;case
0:case
1:case
2:var
v=v[1];continue;default:c2(r);var
B=1,y=1}if(!y){c3(r);var
B=1}var
P=a(F[12],0);if(B){var
an=a(w[33][1],P),ao=an[2],ap=[0,a(g[17][3],an[1]),ao],aq=a(m[8],ap),aR=c(aJ[1],[0,o[1][9][1],aq],O),aS=ae(ap),u=0,q=0,b=D(aJ[2],aS,aq,ao,aR)[2];for(;;){if(typeof
b==="number")var
h=ht;else
switch(b[0]){case
0:var
as=b[1];if(q)throw[0,M,hv];var
q=1,b=as;continue;case
1:var
at=b[1];if(u)throw[0,M,hw];var
u=1,b=at;continue;case
2:var
au=b[1];if(!q)if(!u){var
u=1,q=1,b=au;continue}throw[0,M,hx];case
3:var
av=b[1],h=function(a){return c_(c8,u,q,av,a)};break;case
4:var
aw=b[2],ax=b[1];if(q)throw[0,M,hy];var
h=function(a){return gp(u,ax,aw,a)};break;case
5:var
$=b[1],h=function(b){var
h=ae(b),t=a(o[1][5],gu),j=c(m[20],t,b),k=$[1],d=k[1],l=bW(d,c8(h,b,k[2])),p=da(1,d),r=db(l,p),u=r[2],v=r[1],w=c(g[17][12],f[ab],p),x=[0,a(f[s],j),w],y=a(f[59],x),z=[0,function(a){return aN(y,u,v,a)},0],A=0,B=[0,aK,[0,function(a){return c9(q,$,h,a)},A]],C=[0,function(a){return bh(d,a)},B],D=[0,a(i[7],C),z],E=aj(j,l),F=a(n[66][8],E);return e(i[11],F,D,b)};break;case
6:var
ay=b[1],h=function(a){return bh(ay,a)};break;case
7:var
az=b[1],h=function(a){return bh(az,a)};break;case
8:var
aA=b[1],h=function(a){return af(1,0,0,aA,a)};break;case
9:var
aa=b[2],ad=b[1],h=function(b){var
h=a(f[99],ad),d=a(f[aF],h);if(1===d[0])return af(0,0,[0,d[1],0],aa,b);var
j=a(o[1][5],gC),g=c(m[20],j,b),k=[0,g,0],l=0,q=0;function
r(a){return af(q,l,k,aa,a)}var
s=c(p[143],[0,g],ad),t=a(n[66][8],s);return e(i[5],t,r,b)};break;case
10:var
aB=b[1],aC=0,h=function(a){return c$(aC,aB,a)};break;case
11:var
aD=b[1],aE=1,h=function(a){return c$(aE,aD,a)};break;case
12:var
aH=b[3],aI=b[2],aL=b[1],h=function(d){var
a=de(aI,aH),b=N(p[ea],0,[0,aL],a,0,cV[7]);return c(n[66][8],b,d)};break;case
13:var
ag=b[2],H=b[1],h=function(b){if(0===H[0]){var
d=H[1],f=c(m[18],b,d),g=[0,d,a(aG[2][1][2],f),ag],h=a(aG[2][1][18],g),i=c(p[4],0,h);return c(n[66][8],i,b)}if(H[1])return a(j[6],gE);var
k=e(p[3],0,ag,2);return c(n[66][8],k,b)};break;case
14:var
ah=b[1],h=function(k){var
u=ae(k),w=a(m[7],k),x=a(o[1][5],gO),l=c(m[20],x,k),y=bW(ah,w),p=u[1],z=[0,[0,0,u[1]]];if(p){var
b=p[1];if(typeof
b==="number")var
g=0;else{var
q=p[2],h=b[4],r=b[3],s=b[2],t=b[1];if(typeof
r==="number")if(0===r)var
f=[0,h,[0,[0,t,s,0,[0,l,h]],q]],g=1;else
var
f=[0,h,[0,[0,t,s,0,[0,l,h]],q]],g=1;else
var
f=a(j[6],gN),g=1}}else
var
g=0;if(!g)var
v=a(d[1],gM),f=e(j[3],0,0,v);var
A=f[1],B=[0,f[2]],C=0,D=[0,function(a){return W(B,a)},C],E=0,F=[0,function(a){return ai(A,a)},E],G=[0,function(a){return bh(ah,a)},F],H=[0,function(a){return W(z,a)},G],I=[0,a(i[7],H),D],J=aj(l,y),K=a(n[66][8],J);return e(i[11],K,I,k)};break;case
15:var
I=b[3],z=b[2],J=b[1],h=function(h){var
t=ae(h),x=a(o[1][5],g0),l=c(m[20],x,h),q=t[1];if(q){var
b=q[1];if(typeof
b==="number")var
s=0;else
var
w=b[1],k=b[2],v=b[3],r=b[4],u=q[2],s=1}else
var
s=0;if(!s)var
y=a(d[1],g1),f=e(j[3],0,0,y),w=f[1],k=f[2],v=f[3],r=f[4],u=f[5];var
A=dk(J,z,k,I,h),B=[0,[0,0,t[1]]],C=z[5],E=a(m[8],h),F=a(g[17][1],I),G=[0,[0,[0,w,k,dl([0,l,[0,a(g[17][1],J),F]],E,k,C,v),[0,l,r]],u]],H=0,K=[0,function(a){return W(G,a)},H],L=0,M=[0,function(a){return ai(r,a)},L],N=[0,function(e){var
b=i[1];function
d(b){if(0===b[0])var
d=b[1],f=d[2],e=d[1];else{var
g=b[1],h=g[2],j=g[1];if(0!==h[0]){var
m=function(a){return i[1]},o=function(a){return aO(m,j,a)};return a(i[5],o)}var
f=h[1],e=j}function
k(b){var
d=c(p[4],0,[0,b,f]);return a(n[66][8],d)}function
l(a){return aO(k,e,a)}return a(i[5],l)}return D(g[17][16],d,I,b,e)},M],O=z[2],P=[0,function(e){var
b=i[1];function
d(b){var
d=b[1];function
e(d){var
e=c(p[4],0,[1,d,b[2][1],b[2][2]]);return a(n[66][8],e)}function
f(a){return aO(e,d,a)}return a(i[5],f)}return D(g[17][16],d,O,b,e)},N],Q=c(g[18],J,z[1]),R=[0,function(e){var
b=i[1];function
d(b){var
d=b[1];function
e(d){var
e=c(p[4],0,[0,d,b[2]]);return a(n[66][8],e)}function
f(a){return aO(e,d,a)}return a(i[5],f)}return D(g[17][16],d,Q,b,e)},P],S=[0,function(a){return W(B,a)},R],T=[0,a(i[7],S),K],U=aj(l,A),V=a(n[66][8],U);return e(i[11],V,T,h)};break;case
16:var
aP=b[1],h=function(a){return dd(aP,a)};break;case
17:var
K=b[2],A=b[1],h=function(b){var
l=a(m[8],b),h=ae(b);if(0===K[0]){var
j=di(A,K[1],b),g=j[2],n=j[1]?[0,dh(l,g[3],g[8])]:1;return W([0,[0,[0,A,g,n,0],h[1]]],b)}var
d=K[1];if(0===d[1][1]){var
p=a(o[1][5],gJ),k=c(m[20],p,b),q=a(f[s],k),r=[0,[0,[0,k],d[1][2]],d[2],d[3]],t=function(a){var
b=di(A,q,a),c=b[2];if(b[1])throw[0,M,gK];return W([0,[0,[0,A,c,1,0],h[1]]],a)},u=0,v=0,w=function(c,b,a){return a},x=function(a){return c_(w,v,u,r,a)};return e(i[5],x,t,b)}throw[0,M,gL]};break;default:var
al=b[1];if(typeof
al==="number")var
aQ=a(d[1],hz),h=e(j[3],0,0,aQ);else
var
am=al[1],h=function(y){var
w=ae(y)[1];if(w){var
l=w[1];if(typeof
l==="number"){switch(l){case
0:var
z=a(d[1],hl),q=e(j[3],0,0,z);break;case
1:var
q=a(j[6],hq);break;default:var
q=a(j[6],hr)}var
r=q[1],b=q[2],k=q[3],h=q[4]}else
var
r=l[1],b=l[2],k=l[3],h=l[4]}else
var
$=a(d[1],hs),u=e(j[3],0,0,$),r=u[1],b=u[2],k=u[3],h=u[4];var
A=0===r?0===am?r:a(j[6],ho):0===am?a(j[6],hp):r;if(typeof
k==="number")if(0===k)var
v=0;else
var
Z=a(p[99],b[1]),_=[0,a(n[66][8],Z),0],t=a(i[20],_),v=1;else
var
v=0;if(!v)if(0===A)if(typeof
k==="number")var
B=c(g[17][12],f[s],h),C=function(a){return c5(B,a)},D=a(p[eq],b[1]),E=a(n[66][8],D),t=c(i[5],E,C);else
var
F=k[1],H=[0,b[1],0],G=0,I=dm(h),J=function(b){var
c=0,d=a(n[66][8],p[41]),e=[0,function(a){return aM(d,a)},c],f=[0,function(a){return ai(h,a)},e],g=[0,function(a){return ds(b,a)},f];return a(i[7],g)},K=0,t=function(a){return bl(K,b,J,I,H,G,F,a)};else
if(typeof
k==="number")var
L=0,M=c(g[17][12],f[s],h),N=[0,function(a){return c5(M,a)},L],O=[0,a(g[17][1],b[5])+1|0],P=a(p[101],O),Q=[0,a(n[66][8],P),N],R=c(g[18],b[5],[0,b[1],0]),S=a(p[cB],R),T=[0,a(n[66][8],S),Q],t=a(i[7],T);else
var
U=k[1],x=a(g[17][1],b[5]),V=function(d){var
j=a(o[1][5],hm),e=c(m[20],j,d),k=a(o[1][5],hn),g=c(m[20],k,d),r=[0,a(f[s],g),0],l=0,q=0,t=dm(h);function
u(b){var
c=0,d=a(n[66][8],p[41]),f=[0,function(a){return aM(d,a)},c],g=[0,function(a){return ai(h,a)},f],j=[0,function(a){return ds(b,a)},g],k=[0,e,0],l=[0,function(a){return ai(k,a)},j];return a(i[7],l)}var
v=[0,e],w=[0,function(a){return bl(v,b,u,t,r,q,U,a)},l],y=a(p[23],g),z=[0,a(n[66][8],y),w],A=a(n[66][8],p[17]),B=[0,c(i[26],x,A),z],C=c(p[8],[0,e],x+1|0),D=[0,a(n[66][8],C),B];return c(i[7],D,d)},W=c(g[18],b[5],[0,b[1],0]),X=a(p[cB],W),Y=a(n[66][8],X),t=c(i[5],Y,V);return e(i[5],aK,t,y)}}var
aT=c(i[5],h,fp),aU=a(n[66][1],aT),aV=a(ac[2],0),Q=e(w[29],aV,aU,P)[1];break}}else
var
Q=P;var
aW=function(b,a){return Q};a(F[27],aW);return hA(Q,O[2])}}var
aQ=[0,fs,fw,fL,a8,c7,dt,function(b){return dt(b,a(F[12],0))},W,bl,aP,aw,bj,bk,dk,dl,bY,c3,dq,dp,dn,dr,af,bi];az(458,aQ,"Decl_mode_plugin.Decl_proof_instr");function
Y(n,b){var
o=a(n,b[2]),e=b[1];if(e)var
g=e[1],h=a(d[16],0),i=a(d[1],hF),j=a(d[16],0),k=a(R[1],g),l=c(d[13],k,j),m=c(d[13],l,i),f=c(d[13],m,h);else
var
f=a(d[9],0);return c(d[13],f,o)}function
al(f,b){if(0===b[0])return a(f,b[1]);var
e=b[1];if(e){var
g=a(R[1],e[1]),h=a(d[16],0),i=a(d[1],hK),j=a(d[16],0),k=a(d[1],hL),l=c(d[13],k,j),m=c(d[13],l,i),n=c(d[13],m,h);return c(d[13],n,g)}return a(d[1],hM)}function
am(B,A,z,b){var
h=b[3];if(h)var
t=a(A,h[1]),u=a(d[16],0),v=a(d[1],hJ),w=a(d[16],0),x=c(d[13],w,v),y=c(d[13],x,u),i=c(d[13],y,t);else
var
i=a(d[9],0);var
j=b[2];if(j){var
g=j[1];if(g)var
k=function(b){return a(d[1],hG)},l=e(d[53],k,B,g),m=a(d[16],0),n=a(d[1],hH),o=a(d[16],0),p=c(d[13],o,n),q=c(d[13],p,m),f=c(d[13],q,l);else
var
f=a(d[9],0)}else
var
r=a(d[1],hI),s=a(d[16],0),f=c(d[13],s,r);var
C=a(z,b[1]),D=c(d[29],1,C),E=c(d[13],D,f);return c(d[13],E,i)}function
hN(a){return 0===a[0]?a[1]:f[117]}function
ax(a){return a}function
ay(j,f,i,l,h,e,b){if(l)var
m=a(d[16],0),n=a(d[1],hO),g=c(d[13],n,m);else
var
g=a(d[9],0);if(b){var
k=b[1];if(0===k[0]){var
o=aR(j,f,i,0,h,e,b),p=a(d[1],e),q=a(d[16],0),r=c(d[13],q,g),s=c(d[13],r,p);return c(d[13],s,o)}var
t=k[1],u=ay(j,f,i,1,h,e,b[2]),v=Y(f,t),w=a(d[16],0),x=c(d[13],w,g),y=c(d[13],x,v);return c(d[13],y,u)}return a(d[9],0)}function
aR(f,i,h,k,e,g,b){if(b){var
j=b[1];if(0===j[0]){var
l=b[2],m=j[1],n=k?a(d[43],0):a(d[9],0),o=aR(f,i,h,1,e,g,l),p=a(f,m),q=a(d[16],0),r=c(d[13],q,n),s=c(d[13],r,p);return c(d[13],s,o)}var
t=e?a(d[1],hP):a(d[1],hQ),u=ay(f,i,h,0,e,g,b),v=a(d[16],0),w=c(d[13],v,t);return c(d[13],w,u)}return a(d[9],0)}function
du(b){return 0===b?a(d[1],hT):a(d[1],hU)}function
bn(h,f,bW,i,w){var
o=0,k=0,b=w[2];for(;;){if(typeof
b==="number")var
g=a(d[1],h2);else
switch(b[0]){case
0:var
o=1,b=b[1];continue;case
1:var
k=1,b=b[1];continue;case
2:var
o=1,k=1,b=b[1];continue;case
3:var
m=b[1];if(0===o)if(0===k)var
H=function(a){return al(f,a)},I=am(f,i,function(a){return Y(H,a)},m),J=a(d[16],0),K=a(d[1],h3),L=c(d[13],K,J),g=c(d[13],L,I);else
var
M=function(a){return al(f,a)},N=am(f,i,function(a){return Y(M,a)},m),O=a(d[16],0),P=a(d[1],h4),Q=c(d[13],P,O),g=c(d[13],Q,N);else
if(0===k)var
S=function(a){return al(f,a)},T=am(f,i,function(a){return Y(S,a)},m),U=a(d[16],0),V=a(d[1],h5),W=c(d[13],V,U),g=c(d[13],W,T);else
var
X=function(a){return al(f,a)},Z=am(f,i,function(a){return Y(X,a)},m),_=a(d[16],0),$=a(d[1],h6),aa=c(d[13],$,_),g=c(d[13],aa,Z);break;case
4:var
ab=b[2],ac=b[1],ad=am(f,i,function(a){return Y(f,a)},ab),ae=a(d[16],0),af=0===ac?a(d[1],h0):a(d[1],h1),ag=a(d[16],0),ah=k?a(d[1],h7):a(d[1],h8),ai=c(d[13],ah,ag),aj=c(d[13],ai,af),ak=c(d[13],aj,ae),g=c(d[13],ak,ad);break;case
5:var
an=b[1],ao=am(f,i,function(b){var
e=b[1],g=al(f,b[2]),i=a(d[16],0),j=a(d[1],hR),k=a(d[16],0),l=ay(h,f,ax,0,0,hS,e),m=c(d[13],l,k),n=c(d[13],m,j),o=c(d[13],n,i);return c(d[13],o,g)},an),ap=a(d[1],h9),g=c(d[13],ap,ao);break;case
6:var
aq=ay(h,f,ax,0,0,h_,b[1]),ar=a(d[1],h$),g=c(d[13],ar,aq);break;case
7:var
as=aR(h,f,ax,0,1,ia,b[1]),at=a(d[1],ib),g=c(d[13],at,as);break;case
8:var
au=aR(h,f,ax,0,0,ic,b[1]),av=a(d[1],id),g=c(d[13],av,au);break;case
9:var
aw=b[2],az=a(f,b[1]),aA=a(d[1],ie),aB=a(d[16],0),aC=aR(h,f,ax,0,0,ig,aw),aD=a(d[1],ih),aE=c(d[13],aD,aC),aF=c(d[13],aE,aB),aG=c(d[13],aF,aA),g=c(d[13],aG,az);break;case
10:var
aH=Y(f,b[1]),aI=a(d[16],0),aJ=a(d[1],ii),aK=c(d[13],aJ,aI),g=c(d[13],aK,aH);break;case
11:var
aL=Y(f,b[1]),aM=a(d[16],0),aN=a(d[1],ij),aO=c(d[13],aN,aM),g=c(d[13],aO,aL);break;case
12:var
aP=b[2],aQ=b[1],aS=a(f,b[3]),aT=a(d[1],ik),aU=a(d[16],0),aV=function(b){var
e=a(d[1],il),f=a(h,b),g=a(d[1],im),i=c(d[13],g,f);return c(d[13],i,e)},aW=e(d[53],d[16],aV,aP),aX=a(d[16],0),aY=a(R[1],aQ),aZ=a(d[16],0),a0=a(d[1],io),a1=c(d[13],a0,aZ),a2=c(d[13],a1,aY),a3=c(d[13],a2,aX),a4=c(d[13],a3,aW),a5=c(d[13],a4,aU),a6=c(d[13],a5,aT),g=c(d[13],a6,aS);break;case
13:var
a7=b[1],a8=a(f,b[2]),a9=a(d[16],0),a_=a(d[1],ip),a$=a(d[16],0),ba=al(R[1],a7),bb=a(d[16],0),bc=a(d[1],iq),bd=c(d[13],bc,bb),be=c(d[13],bd,ba),bf=c(d[13],be,a$),bg=c(d[13],bf,a_),bh=c(d[13],bg,a9),g=c(d[13],bh,a8);break;case
14:var
bi=ay(h,f,ax,0,0,ir,b[1]),bj=a(d[1],is),g=c(d[13],bj,bi);break;case
15:var
r=b[3],s=b[1],bk=b[2];if(0===r)var
t=a(d[9],0);else
var
bA=0,bB=0,bC=ay(h,function(a){return al(f,a)},hN,bB,bA,ix,r),bD=a(d[1],iy),bE=a(d[16],0),bF=c(d[13],bE,bD),t=c(d[13],bF,bC);if(0===s)var
u=a(d[9],0);else
var
br=a(d[16],0),bs=function(b){var
e=a(d[1],iu),f=a(h,b),g=a(d[1],iv),i=c(d[13],g,f);return c(d[13],i,e)},bt=e(d[53],d[16],bs,s),bu=a(d[16],0),bv=a(d[1],iw),bw=a(d[16],0),bx=c(d[13],bw,bv),by=c(d[13],bx,bu),bz=c(d[13],by,bt),u=c(d[13],bz,br);var
bl=a(bW,bk),bm=a(d[16],0),bn=a(d[1],it),bo=c(d[13],bn,bm),bp=c(d[13],bo,bl),bq=c(d[13],bp,u),g=c(d[13],bq,t);break;case
16:var
bG=e(d[53],d[43],f,b[1]),bH=a(d[16],0),bI=a(d[1],iz),bJ=c(d[13],bI,bH),g=c(d[13],bJ,bG);break;case
17:var
p=b[2],bK=b[1];if(0===p[0])var
y=a(f,p[1]),z=a(d[16],0),A=a(d[1],hY),B=c(d[13],A,z),v=c(d[13],B,y);else
var
C=p[1],D=am(f,i,function(a){return Y(f,a)},C),E=a(d[16],0),F=a(d[1],hZ),G=c(d[13],F,E),v=c(d[13],G,D);var
bL=a(d[16],0),bM=du(bK),bN=a(d[16],0),bO=a(d[1],iA),bP=c(d[13],bO,bN),bQ=c(d[13],bP,bM),bR=c(d[13],bQ,bL),g=c(d[13],bR,v);break;default:var
q=b[1];if(typeof
q==="number")switch(q){case
0:var
n=a(d[1],hV);break;case
1:var
n=a(d[1],hW);break;default:var
n=a(d[1],hX)}else
var
n=du(q[1]);var
bS=a(d[16],0),bT=a(d[1],iB),bU=c(d[13],bT,bS),g=c(d[13],bU,n)}var
bX=a(d[16],0),x=w[1];if(3<x>>>0)var
bV=a(d[1],iC),l=e(j[3],0,0,bV);else
switch(x){case
0:var
l=a(d[1],iD);break;case
1:var
l=a(d[1],iE);break;case
2:var
l=a(d[1],iF);break;default:var
l=a(d[1],iG)}var
bY=c(d[13],l,bX);return c(d[13],bY,g)}}function
iH(g,f,e,b){var
h=a(e,bm[30]),i=b0[25];return bn(function(h){var
b=h[2],e=b[2],f=b[1];if(e){var
i=e[1],j=a(d[1],iI),k=a(g,i),l=a(d[1],iJ),m=a(R[1],f),n=a(d[1],iK),o=c(d[13],n,m),p=c(d[13],o,l),q=c(d[13],p,k);return c(d[13],q,j)}return a(R[1],f)},f,i,h,b)}function
iL(g,f,e,b){var
h=a(e,bm[30]),i=b0[25];return bn(function(h){var
b=h[2],e=b[2],f=b[1];if(e){var
i=e[1],j=a(d[1],iM),k=a(g,i),l=a(d[1],iN),m=a(R[1],f),n=a(d[1],iO),o=c(d[13],n,m),p=c(d[13],o,l),q=c(d[13],p,k);return c(d[13],q,j)}return a(R[1],f)},f,i,h,b)}var
aS=[0,bn,iH,iL,function(e,d,c,b){var
f=a(c,bm[30]);function
g(b){return a(b0[25],b[6])}return bn(function(a){return Y(e,a)},d,g,f,b)}];az(461,aS,"Decl_mode_plugin.Ppdecl_proof");a(iP[12],iS);function
b5(f){var
j=a(t[68],f),k=a(m[2],f),g=c(B[4][13],k,j),b=g[2],h=g[1],i=c(B[4][1],b,h),l=c(B[4][4],b,h),n=e(ad[19],i,b,l),o=a(d[17],0),p=a(d[1],iT),q=a(d[17],0),r=a(d[1],iU),s=a(d[17],0),u=c(ad[61],i,b),v=c(d[13],u,s),w=c(d[13],v,r),x=c(d[13],w,q),y=c(d[13],x,p),z=c(d[13],y,o),A=c(d[13],z,n),C=c(d[27],0,A),D=a(d[1],iV),E=a(d[6],0),F=a(d[6],0),G=a(d[1],iW),H=c(d[13],G,F),I=c(d[13],H,E),J=c(d[13],I,D);return c(d[13],J,C)}function
dv(e,r,f,q,p,o,b){var
g=e?e[1]:1;if(b)if(!b[2]){var
n=b[1];if(g)return b5([0,n,f])}var
h=a(F[12],0),i=a(x[13],h),j=a(d[1],iX),k=a(d[1],i),l=a(d[1],iY),m=c(d[13],l,k);return c(d[13],m,j)}function
iZ(h,b){var
a=b[2],d=b[1],f=c(B[4][1],a,d),g=c(x[8],a,d);return e(aJ[2],g,f,a)}function
dw(c){var
b=a(F[12],0);return a(w[7],b)?a(j[6],i0):(a(aQ[1],0),a(F[10],i1))}function
dx(b){a(aQ[2],0);return a(F[10],i2)}function
dy(b){return a(aQ[7],b)}var
ao=a(an[2],i3),bp=a(k[1][10],i4),i5=a(an[4],ao),dz=e(k[13],k[9],i6,i5);D(bm[1],ao,aS[2],aS[3],aS[4]);function
dA(d){var
a=d[2];if(typeof
a==="number")var
b=1;else
if(18===a[0])var
c=a[1],b=typeof
c==="number"?0===c?1:0:0;else
var
b=0;return b?i7:bo[7]}var
i8=0,i_=[0,[0,0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(an[4],ao),f=c(an[8],e,d);return function(a){return dy(f)}}return a(U[2],i9)}],i8];function
i$(b,a){return e(b1[1],a[1],[0,ja,b],a[2])}c(aa[80],i$,i_);var
jb=0,jd=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(an[4],ao),f=c(an[8],e,d);return function(a){return dA(f)}}return a(U[2],jc)},jb];function
je(b,a){return c(bo[3],[0,jf,b],a)}c(aa[80],je,jd);var
jg=[6,a(k[12],ao)],jh=a(an[4],ao),ji=[0,[0,[1,u[4],jh,jg],0],0];function
jj(b,a){return e(b3[1],[0,jk,b],[0,bp],a)}c(aa[80],jj,ji);var
jl=0,jm=0;function
jn(b,c){return a(b,jo)}e(k[1][6],bp,jp,[0,[0,0,0,[0,[0,[0,[2,iQ[10]],0],jn],jm]],jl]);function
jq(b){a(k[21],k[18][8]);return a(ad[84],ad[85])}var
js=[0,jr,function(b){a(k[21],bp);return a(ad[84],[0,dv,ad[85][2],b5])},jq];a(F[1],js);var
jt=0,jv=[0,[0,0,function(b){return b?a(U[2],ju):function(a){return dw(0)}}],jt];function
jw(b,a){return e(b1[1],a[1],[0,jx,b],a[2])}c(aa[80],jw,jv);var
jy=0,jB=[0,function(b){return b?a(U[2],jz):function(a){return jA}},jy];function
jC(b,a){return c(bo[3],[0,jD,b],a)}c(aa[80],jC,jB);function
jF(b,a){return e(b3[1],[0,jG,b],0,a)}c(aa[80],jF,jE);var
jH=0,jJ=[0,[0,0,function(b){return b?a(U[2],jI):function(a){return dx(0)}}],jH];function
jK(b,a){return e(b1[1],a[1],[0,jL,b],a[2])}c(aa[80],jK,jJ);var
jM=0,jP=[0,function(b){return b?a(U[2],jN):function(a){return jO}},jM];function
jQ(b,a){return c(bo[3],[0,jR,b],a)}c(aa[80],jQ,jP);function
jT(b,a){return e(b3[1],[0,jU,b],0,a)}c(aa[80],jT,jS);function
b6(a){return a?a[1]:0}var
r=k[1][4][1],bq=a(r,jV),A=a(r,jW),br=a(r,jX),aT=a(r,jY),bs=a(r,jZ),bt=a(r,j0),aU=a(r,j1),bu=a(r,j2),b7=a(r,j3),dB=a(r,j4),dC=a(r,j5),b8=a(r,j6),b9=a(r,j7),dD=a(r,j8),b_=a(r,j9),v=a(r,j_),b$=a(r,j$),dE=a(r,ka),ca=a(r,kb),cb=a(r,kc),cc=a(r,kd),cd=a(r,ke),ce=a(r,kf),dF=a(r,kg),cf=a(r,kh),dG=a(r,ki),cg=a(r,kj),dH=a(r,kk),ch=a(r,kl),ci=a(r,km),dI=a(r,kn),dJ=a(r,ko),dK=a(r,kp),dL=a(r,kq),kr=0,ks=0,ku=[0,[0,kt,function(b,a){return 0}],ks];function
kv(a,d,c,b){return[0,a]}e(k[1][6],bq,0,[0,[0,0,0,[0,[0,[0,kx,[0,kw,[0,[2,k[15][6]],0]]],kv],ku]],kr]);var
ky=0,kz=0;function
kA(b,d,a,c){return[0,[0,a],b]}var
kC=[0,[0,[0,[2,k[15][6]],[0,kB,[0,[2,k[15][1]],0]]],kA],kz];function
kD(c,b){return[0,0,[0,[1,[0,a(b4[11],b),c]],0]]}var
kE=[0,[0,[0,[2,k[15][6]],0],kD],kC];function
kF(a,b){return[0,0,a]}e(k[1][6],A,0,[0,[0,0,0,[0,[0,[0,[2,k[15][1]],0],kF],kE]],ky]);var
kG=0,kH=0;function
kI(a,b){return[0,a]}var
kJ=[0,[0,0,0,[0,[0,[0,[2,k[15][1]],0],kI],kH]],kG],kK=0,kL=[0,[0,0,0,[0,[0,[0,[2,bq],0],function(a,b){return[1,a]}],kK]],kJ];e(k[1][6],br,0,kL);var
kM=0,kN=0;function
kO(b,d,a,c){return[0,[0,a],b]}var
kQ=[0,[0,[0,[2,k[15][6]],[0,kP,[0,[2,br],0]]],kO],kN];function
kR(c,b){return[0,0,[0,[0,[1,[0,a(b4[11],b),c]],0]]]}var
kS=[0,[0,[0,[2,k[15][6]],0],kR],kQ];function
kT(a,b){return[0,0,[0,a]]}var
kU=[0,[0,0,0,[0,[0,[0,[2,k[15][1]],0],kT],kS]],kM],kV=0,kW=[0,[0,0,0,[0,[0,[0,[2,bq],0],function(a,b){return[0,0,[1,a]]}],kV]],kU];e(k[1][6],aT,0,kW);var
kX=0,kY=0,k0=[0,[0,0,function(a){return kZ}],kY];function
k1(a,c,b){return[0,a]}var
k4=[0,[0,[0,k3,[0,[7,[2,k[15][1]],k2,0],0]],k1],k0],k6=[0,[0,0,0,[0,[0,k5,function(c,b,a){return 0}],k4]],kX];e(k[1][6],bs,0,k6);var
k7=0,k8=0,k9=[0,[0,0,function(a){return 0}],k8];function
k_(a,c,b){return[0,a]}e(k[1][6],bt,0,[0,[0,0,0,[0,[0,[0,k$,[0,[2,k[17][19]],0]],k_],k9]],k7]);var
la=0,lb=0,lc=[0,[0,0,0,[0,[0,[0,[2,aT],[0,[2,bs],[0,[2,bt],0]]],function(c,b,a,d){return[0,a,b,c]}],lb]],la];e(k[1][6],aU,0,lc);var
ld=0,le=0,lf=[0,[0,0,0,[0,[0,[0,[2,A],[0,[2,bs],[0,[2,bt],0]]],function(c,b,a,d){return[0,a,b,c]}],le]],ld];e(k[1][6],bu,0,lf);var
lg=0,lh=0,lj=[0,[0,li,function(b,a){return 1}],lh],ll=[0,[0,0,0,[0,[0,lk,function(b,a){return 0}],lj]],lg];e(k[1][6],b7,0,ll);var
lm=0,ln=0,lp=[0,[0,lo,function(b,a){return 1}],ln],lr=[0,[0,lq,function(b,a){return 2}],lp],lt=[0,[0,ls,function(b,a){return 0}],lr],lu=[0,[0,0,0,[0,[0,[0,[2,b7],0],function(a,b){return[0,a]}],lt]],lm];e(k[1][6],dB,0,lu);var
lv=0,lw=0;function
lx(a,c,b){return[0,a]}var
lz=[0,[0,[0,ly,[0,[2,k[15][1]],0]],lx],lw],lB=[0,[0,0,0,[0,[0,[0,lA,[0,[2,bu],0]],function(a,c,b){return[1,a]}],lz]],lv];e(k[1][6],dC,0,lB);var
lC=0,lD=0;function
lE(b,e,a,d,c){return[9,b,a]}var
lH=[0,[0,[0,lG,[0,[2,b$],[0,lF,[0,[2,k[15][1]],0]]]],lE],lD],lJ=[0,[0,[0,lI,[0,[2,b7],[0,[2,dC],0]]],function(b,a,d,c){return[17,a,b]}],lH],lL=[0,[0,0,0,[0,[0,[0,lK,[0,[2,dF],[0,[2,bs],[0,[2,bt],0]]]],function(c,b,a,e,d){return[5,[0,a,b,c]]}],lJ]],lC];e(k[1][6],b8,0,lL);var
lM=0,lN=0,lP=[0,[0,[0,lO,[0,[2,bu],0]],function(a,c,b){return[0,1,a]}],lN],lR=[0,[0,0,0,[0,[0,[0,lQ,[0,[2,bu],0]],function(a,c,b){return[0,0,a]}],lP]],lM];e(k[1][6],b9,0,lR);var
lS=0,lT=0,lV=[0,[0,[0,lU,[0,[2,b8],0]],function(a,c,b){return[0,a]}],lT],lX=[0,[0,[0,lW,[0,[2,aU],0]],function(a,c,b){return[0,[3,a]]}],lV],lZ=[0,[0,[0,lY,[0,[2,b9],0]],function(a,c,b){return[1,[4,a[1],a[2]]]}],lX],l1=[0,[0,[0,l0,[0,[2,aU],0]],function(a,c,b){return[1,[3,a]]}],lZ],l3=[0,[0,[0,l2,[0,[2,aU],0]],function(a,c,b){return[2,[3,a]]}],l1],l4=[0,[0,[0,[2,b8],0],function(a,b){return a}],l3],l5=[0,[0,[0,[2,b9],0],function(a,b){return[4,a[1],a[2]]}],l4],l7=[0,[0,[0,l6,[0,[2,aU],0]],function(a,c,b){return[3,a]}],l5],l9=[0,[0,[0,l8,[0,[2,A],0]],function(a,c,b){return[10,a]}],l7],ma=[0,[0,[0,l$,[0,l_,[0,[2,A],0]]],function(a,d,c,b){return[11,a]}],l9],mc=[0,[0,[0,mb,[0,[2,dB],0]],function(a,c,b){return[18,a]}],ma],me=[0,[0,0,0,[0,[0,md,function(b,a){return 0}],mc]],lS];e(k[1][6],dD,0,me);var
mf=0,mg=0;function
mh(d,c,b){return[0,a(b4[11],c),[0,d,b]]}e(k[1][6],b_,0,[0,[0,0,0,[0,[0,[0,[2,k[15][6]],0],mh],mg]],mf]);var
mi=0,mj=0,mk=[0,[0,[0,[2,b_],0],function(b,c){return a(b,0)}],mj];function
ml(c,e,b,d){return a(b,[0,c])}e(k[1][6],v,0,[0,[0,0,0,[0,[0,[0,[2,b_],[0,mm,[0,[2,k[15][1]],0]]],ml],mk]],mi]);var
mn=0,mo=0,mp=[0,[0,[0,[2,v],0],function(a,b){return[0,[0,a],0]}],mo],mr=[0,[0,[0,[2,v],mq],function(b,d,a,c){return[0,[0,a],b]}],mp],mu=[0,[0,0,0,[0,[0,[0,[2,v],[0,mt,[0,ms,[0,[2,dE],0]]]],function(b,e,d,a,c){return[0,[0,a],b]}],mr]],mn];e(k[1][6],b$,0,mu);var
mv=0,mw=0,my=[0,[0,[0,[2,A],mx],function(b,d,a,c){return[0,[1,a],b]}],mw],mB=[0,[0,[0,[2,A],[0,mA,[0,mz,[0,[2,b$],0]]]],function(b,e,d,a,c){return[0,[1,a],b]}],my],mC=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],mB]],mv];e(k[1][6],dE,0,mC);var
mD=0,mE=0,mF=[0,[0,[0,[2,v],0],function(a,b){return[0,[0,a],0]}],mE],mH=[0,[0,[0,[2,v],mG],function(b,d,a,c){return[0,[0,a],b]}],mF],mK=[0,[0,0,0,[0,[0,[0,[2,v],[0,mJ,[0,mI,[0,[2,cb],0]]]],function(b,e,d,a,c){return[0,[0,a],b]}],mH]],mD];e(k[1][6],ca,0,mK);var
mL=0,mM=0,mO=[0,[0,[0,[2,A],mN],function(b,d,a,c){return[0,[1,a],b]}],mM],mS=[0,[0,[0,[2,A],[0,mR,[0,mQ,[0,mP,[0,[2,ca],0]]]]],function(b,f,e,d,a,c){return[0,[1,a],b]}],mO],mT=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],mS]],mL];e(k[1][6],cb,0,mT);var
mU=0,mV=0,mY=[0,[0,[0,mX,[0,mW,[0,[2,ca],0]]],function(a,d,c,b){return a}],mV],mZ=[0,[0,0,0,[0,[0,[0,[2,cb],0],function(a,b){return a}],mY]],mU];e(k[1][6],cc,0,mZ);var
m0=0,m1=0,m4=[0,[0,[0,[2,v],[0,m3,[0,m2,[0,[2,br],0]]]],function(b,e,d,a,c){return[0,[0,[0,a],0],b]}],m1],m6=[0,[0,[0,[2,v],m5],function(a,d,b,c){return[0,[0,[0,b],a[1]],a[2]]}],m4],m9=[0,[0,0,0,[0,[0,[0,[2,v],[0,m8,[0,m7,[0,[2,ce],0]]]],function(a,e,d,b,c){return[0,[0,[0,b],a[1]],a[2]]}],m6]],m0];e(k[1][6],cd,0,m9);var
m_=0,m$=0,nb=[0,[0,[0,[2,A],na],function(a,d,b,c){return[0,[0,[1,b],a[1]],a[2]]}],m$],nf=[0,[0,[0,[2,A],[0,ne,[0,nd,[0,nc,[0,[2,cd],0]]]]],function(a,f,e,d,b,c){return[0,[0,[1,b],a[1]],a[2]]}],nb],ni=[0,[0,0,0,[0,[0,[0,[2,A],[0,nh,[0,ng,[0,[2,br],0]]]],function(b,e,d,a,c){return[0,[0,[1,a],0],b]}],nf]],m_];e(k[1][6],ce,0,ni);var
nj=0,nk=0,nn=[0,[0,[0,nm,[0,nl,[0,[2,cd],0]]],function(a,d,c,b){return a}],nk],no=[0,[0,0,0,[0,[0,[0,[2,ce],0],function(a,b){return a}],nn]],nj];e(k[1][6],dF,0,no);var
np=0,nq=0,nr=[0,[0,[0,[2,v],0],function(a,b){return[0,[0,a],0]}],nq],nt=[0,[0,[0,[2,v],ns],function(b,d,a,c){return[0,[0,a],b]}],nr],nx=[0,[0,0,0,[0,[0,[0,[2,v],[0,nw,[0,nv,[0,nu,[0,[2,dG],0]]]]],function(b,f,e,d,a,c){return[0,[0,a],b]}],nt]],np];e(k[1][6],cf,0,nx);var
ny=0,nz=0,nB=[0,[0,[0,[2,A],nA],function(b,d,a,c){return[0,[1,a],b]}],nz],nE=[0,[0,[0,[2,A],[0,nD,[0,nC,[0,[2,cf],0]]]],function(b,e,d,a,c){return[0,[1,a],b]}],nB],nF=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],nE]],ny];e(k[1][6],dG,0,nF);var
nG=0,nH=0,nI=[0,[0,[0,[2,v],0],function(a,b){return[0,[0,a],0]}],nH],nK=[0,[0,[0,[2,v],nJ],function(b,d,a,c){return[0,[0,a],b]}],nI],nN=[0,[0,0,0,[0,[0,[0,[2,v],[0,nM,[0,nL,[0,[2,dH],0]]]],function(b,e,d,a,c){return[0,[0,a],b]}],nK]],nG];e(k[1][6],cg,0,nN);var
nO=0,nP=0,nR=[0,[0,[0,[2,A],nQ],function(b,d,a,c){return[0,[1,a],b]}],nP],nU=[0,[0,[0,[2,A],[0,nT,[0,nS,[0,[2,cg],0]]]],function(b,e,d,a,c){return[0,[1,a],b]}],nR],nV=[0,[0,0,0,[0,[0,[0,[2,A],0],function(a,b){return[0,[1,a],0]}],nU]],nO];e(k[1][6],dH,0,nV);var
nW=0,nX=0,nY=[0,[0,[0,[2,v],0],function(a,b){return[0,[0,a],0]}],nX],n0=[0,[0,[0,[2,v],nZ],function(b,d,a,c){return[0,[0,a],b]}],nY];function
n1(b,f,e,d,a,c){return[0,[0,a],b]}var
n4=[0,n3,[0,n2,[0,[2,ci],0]]],n5=0,n7=[0,[0,n6,function(a,b){return a}],n5],n8=[0,[0,0,0,[0,[0,[0,[2,v],[0,[8,a(b2[2],n7)],n4]],n1],n0]],nW];e(k[1][6],ch,0,n8);var
n9=0,n_=0,oa=[0,[0,[0,[2,aT],n$],function(b,d,a,c){return[0,[1,a],b]}],n_],oe=[0,[0,[0,[2,aT],[0,od,[0,oc,[0,ob,[0,[2,ch],0]]]]],function(b,f,e,d,a,c){return[0,[1,a],b]}],oa],of=[0,[0,0,0,[0,[0,[0,[2,aT],0],function(a,b){return[0,[1,a],0]}],oe]],n9];e(k[1][6],ci,0,of);var
og=0,oh=0,ok=[0,[0,[0,oj,[0,oi,[0,[2,ch],0]]],function(a,d,c,b){return a}],oh],ol=[0,[0,0,0,[0,[0,[0,[2,ci],0],function(a,b){return a}],ok]],og];e(k[1][6],dI,0,ol);var
om=0,on=0,op=[0,[0,[0,oo,[0,[2,cc],0]],function(a,c,b){return[14,a]}],on];function
oq(c,b,a,h,g,f,e){var
d=b6(c);return[15,b6(b),a,d]}var
or=0,os=0,ou=[0,[0,[0,ot,[0,[2,dI],0]],function(a,c,b){return a}],os],ov=[0,[8,a(b2[2],ou)],or],ow=0,oz=[0,[0,[0,oy,[0,[7,[2,v],ox,0],0]],function(a,c,b){return a}],ow],oA=[0,[8,a(b2[2],oz)],ov],oF=[0,[0,[0,oE,[0,oD,[0,oC,[0,[3,k[15][10],oB],oA]]]],oq],op],oH=[0,[0,[0,oG,[0,[2,cf],0]],function(a,c,b){return[7,a]}],oF];function
oI(a,c,b){return[16,a]}var
oL=[0,[0,[0,oK,[0,[7,[2,k[15][1]],oJ,0],0]],oI],oH],oN=[0,[0,[0,oM,[0,[2,cc],0]],function(a,c,b){return[6,a]}],oL],oP=[0,[0,[0,oO,[0,[2,cg],0]],function(a,c,b){return[8,a]}],oN];function
oQ(c,f,b,a,e,d){return[12,a,b,c]}var
oT=[0,[0,[0,oS,[0,[2,k[15][6]],[0,[4,[2,v]],[0,oR,[0,[2,k[15][1]],0]]]]],oQ],oP];function
oU(b,e,a,d,c){return[13,[0,a],b]}var
oX=[0,[0,[0,oW,[0,[2,k[15][6]],[0,oV,[0,[2,k[15][1]],0]]]],oU],oT];function
oY(b,e,a,d,c){return[13,[1,a],b]}e(k[1][6],dJ,0,[0,[0,0,0,[0,[0,[0,o0,[0,[2,bq],[0,oZ,[0,[2,k[15][1]],0]]]],oY],oX]],om]);var
o1=0,o2=0,o3=[0,[0,0,function(a){return 0}],o2],o5=[0,[0,o4,function(b,a){return 1}],o3],o7=[0,[0,o6,function(b,a){return 2}],o5],o9=[0,[0,0,0,[0,[0,o8,function(b,a){return 3}],o7]],o1];e(k[1][6],dK,0,o9);var
o_=0,o$=0,pa=[0,[0,[0,[2,dD],0],function(a,b){return a}],o$],pb=[0,[0,0,0,[0,[0,[0,[2,dJ],0],function(a,b){return a}],pa]],o_];e(k[1][6],dL,0,pb);var
pc=0,pd=0,pf=[0,[0,0,0,[0,[0,[0,[2,dK],[0,[2,dL],pe]],function(d,b,a,c){return[0,a,b]}],pd]],pc];e(k[1][6],dz,0,pf);var
dM=[0,iR,b5,dv,iZ,dw,dx,dy,ao,bp,dz,dA,b6];az(472,dM,"Decl_mode_plugin.G_decl_mode");az(473,[0,x,aJ,aQ,aS,dM],"Decl_mode_plugin");return});
