(function(lq){"use strict";var
cE=",",bk="AddSetoidRing",R='"',cD="field_mod",v=250,cC="(",bg="AddSetoidField",cB="constants",Q="plugins/setoid_ring/g_newring.ml4",aT="Field_tac",cm="with carrier ",cz="postprocess tactic",cA="Init",aS="field",cy="InitialRing",p=246,cl="decidable",cv="Pphi_pow",cw='Using setoid "',cx="tactic recognizing constants",ck=102,L="Extension: cannot occur",cu="postprocess",$=120,bj="gen_phiZ",aY="setoid",ct="ring_mod",cj="$map",E="]",ci="preprocess",bf="power_tac",cs="Print",aX="closed_term",bi="power",at=135,cr="Ring_polynom",ad=136,aW="protect_fv",ch="Ring_tac",as="Ring",cg='and "',cf="$lH",cd="morphism",ce="plugins/setoid_ring/newring.ml",bh="field_lookup",aV="vernac argument needs not wit printer",_="Coq",cb="closed",cc="F",aU="x",be="ring_lookup",bd="ring",ca="preprocess tactic",D="[",cq='and morphisms "',b$="and equivalence relation ",b_=")",b7="field_mods",b8=":",b9="t",b5="abstract",b6="$f",aR="vernac argument needs not globwit printer",aQ="sign",b4="PEeval",aP="div",aO="newring_plugin",co="Add",cp="completeness",bc="IDphi",cn="ring_mods",b3="Pphi_dev",F=lq.jsoo_runtime,K=F.caml_check_bound,b=F.caml_new_string,u=F.caml_obj_tag,bb=F.caml_register_global,J=F.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):F.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):F.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):F.caml_call_gen(a,[b,c,d])}function
P(a,b,c,d,e){return a.length==4?a(b,c,d,e):F.caml_call_gen(a,[b,c,d,e])}function
U(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):F.caml_call_gen(a,[b,c,d,e,f])}function
ln(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):F.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
lo(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):F.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
lp(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):F.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}var
g=F.caml_get_global_data(),bv=[0,b(aO),b("get_res")],bx=[0,[0,b(_),[0,b("Setoids"),[0,b("Setoid"),0]]],[0,[0,b(_),[0,b("Lists"),[0,b("List"),0]]],[0,[0,b(_),[0,b(cA),[0,b("Datatypes"),0]]],[0,[0,b(_),[0,b(cA),[0,b("Logic"),0]]],0]]]],aq=b("setoid_ring"),I=b(aO),q=g.Term,M=g.CClosure,j=g.Util,bU=g.Tacmach,A=g.Proofview,V=g.Coqlib,af=g.Global,i=g.Names,n=g.Pervasives,f=g.Ltac_plugin,bQ=g.Lib,t=g.CamlinternalLazy,av=g.Universes,N=g.Not_found,r=g.CErrors,B=g.Mod_subst,T=g.Libnames,ac=g.Retyping,bD=g.Reductionops,d=g.Pp,G=g.Printer,H=g.Assert_failure,ae=g.Globnames,aG=g.Typing,ab=g.Evarutil,bK=g.Smartlocate,k=g.Loc,ai=g.Feedback,bG=g.Flags,l=g.Stdarg,h=g.Genarg,aw=g.Evd,br=g.Univ,bs=g.Declare,bq=g.Constrintern,a2=g.Quote_plugin,bo=g.Tactics,aD=g.Summary,aE=g.Libobject,bZ=g.Egramml,al=g.Vernac_classifier,bY=g.Vernacinterp,O=g.Ppconstr,ar=g.Mltop,o=g.Pcoq,m=g.CLexer,ak=g.CList,gN=[0,0,0],gO=[0,0,0],gD=b("field kind"),gE=b(cx),gF=b(ca),gG=b(cz),gH=b(aY),gI=b(bi),gJ=b(aQ),gK=b(aP),gL=b("infinite property"),gs=[0,b(aT),0],gt=[0,0,0],gu=b("field_lemmas"),gv=b("_field_lemma1"),gw=b("_field_lemma2"),gx=b("_field_lemma3"),gy=b("_field_lemma4"),gz=b("_lemma5"),gB=[22,0],gA=[22,0],gq=b("field inverse should be declared as a morphism"),gb=b("arguments of field_simplify do not have all the same type"),gc=[0,b(aS)],ga=[0,b(aT),0],gd=b(R),ge=b(R),gf=b("cannot find a declared field structure over"),gg=[0,b(aS)],gh=[0,b(ce),846,12],f7=[0,1],f8=[0,0],f6=b("bad field structure"),e9=[0,0,0],e_=[0,0,0],e0=b("ring kind"),e1=b(cx),e2=b(ca),e3=b(cz),e4=b(aY),e5=b(bi),e6=b(aQ),e7=b(aP),eY=b(" cannot be set twice"),eR=[0,b("Ring_base"),0],eS=b("ring_lemmas"),eT=b("_ring_lemma1"),eU=b("_ring_lemma2"),eW=[22,0],eV=[22,0],eP=[0,1],eQ=[0,0],eO=b("bad ring structure"),ex=b("ring addition should be declared as a morphism"),ey=b("ring multiplication should be declared as a morphism"),ez=b("ring opposite should be declared as a morphism"),eA=b(R),eB=b(cg),eC=b(R),eD=b(R),eE=b('",'),eF=b(cq),eG=b(R),eH=b(cw),eI=b(R),eJ=b(cg),eK=b(R),eL=b(cq),eM=b(R),eN=b(cw),et=b("cannot find setoid relation"),ef=b("arguments of ring_simplify do not have all the same type"),eg=[0,b(bd)],eh=b(R),ei=b(R),ej=b("cannot find a declared ring structure over"),ek=[0,b(bd)],el=[0,b(ce),363,12],dx=[0,b(cr),0],dw=b("newring"),dn=b(as),dm=b(as),di=b("ring: cannot find relation (not closed)"),dg=b("ring: cannot find relation"),c_=b(as),c9=b(as),c7=b(aU),c6=b(aU),c4=b(aU),c2=b("Ring.exec_tactic: anomaly"),cX=b(aU),cY=b(cc),cZ=b(cc),cV=[2,1],cW=[0,1],cR=[0,b(aO),b(aX)],cS=b(b9),cT=b(b9),cJ=b("not found"),cK=b("map "),cL=[0,b("lookup_map")],cH=b("dummy"),cG=b("global_head_of_constr"),c$=b("Build_Setoid_Theory"),db=b("None"),dc=b("Some"),dd=b("eq"),de=b("cons"),df=b("nil"),dj=b(_),dk=[0,[0,b("Ring_theory"),0],[0,[0,b(cr),0],[0,[0,b(ch),0],[0,[0,b(cy),0],[0,[0,b(aT),0],[0,[0,b("Field_theory"),0],0]]]]]],dp=[0,b(_),0],dq=b(ch),ds=[0,b(_),0],dt=b(cy),dy=b("almost_ring_theory"),dz=b("Eqsth"),dB=b("Eq_ext"),dD=b("Eq_s_ext"),dF=b("ring_theory"),dG=b("mk_reqe"),dI=b("semi_ring_theory"),dJ=b("mk_seqe"),dL=b("Abstract"),dM=b("Computational"),dO=b("Morphism"),dQ=b("inv_morph_nothing"),dR=b("mkhypo"),dT=b("hypo"),dW=b(b4),dZ=b(cv),d2=b(b3),d5=b(bj),d8=b(bc),ea=b(bd),ec=b("ring-tac-carrier-table"),ed=b("ring-tac-name-table"),en=b("tactic-new-ring-theory"),fa=[0,b(_),0],fb=b(aT),fg=b("FEeval"),fj=b(b4),fm=b(cv),fp=b(b3),fs=b("display_pow_linear"),fv=b("display_linear"),fy=b(bj),fB=b(bc),fF=b(aS),fI=b("PCond"),fL=b(bj),fO=b(bc),fS=b("field_cond"),fT=b(aS),fV=b("simpl_field_expr"),fX=b("almost_field_theory"),fY=b("field_theory"),fZ=b("semi_field_theory"),f0=b("AF_AR"),f2=b("F_R"),f4=b("SF_SR"),f9=b("field-tac-carrier-table"),f_=b("field-tac-name-table"),gj=b("tactic-new-field-theory"),lk=[0,b(Q),1,0],lh=[0,b(Q),1,0],le=[0,b(Q),1,0],ld=b("$lt"),lf=[0,b(E)],lg=b(cf),li=[0,b(D)],lj=b(b6),ll=[0,b(bh)],lm=b(bh),k_=b(L),k8=b(bg),kS=b(bg),kP=b(L),kN=b(L),kL=b(bg),kI=b(L),kF=b(b$),kG=b(cm),kE=b("The following field structures have been declared:"),kD=b(L),kA=b(aR),ky=b(aV),kj=b(aR),kh=b(aV),j7=b(cp),j4=[0,b(Q),1,0],j1=[0,b(Q),1,0],jY=[0,b(Q),1,0],jX=b("$lrt"),jZ=[0,b(E)],j0=b(cf),j2=[0,b(D)],j3=b(b6),j5=[0,b(be)],j6=b(be),jS=b(L),jQ=b(bk),jA=b(bk),jx=b(L),jv=b(L),jt=b(bk),jq=b(L),jn=b(b$),jo=b(cm),jm=b("The following ring structures have been declared:"),jl=b(L),ji=b(aR),jg=b(aV),i3=b(aR),i1=b(aV),hS=[0,0],hk=b(b5),hl=b(cl),hm=b(cd),hn=b(E),ho=b(D),hp=b(cB),hq=b(E),hr=b(D),hs=b(cb),ht=b(E),hu=b(D),hv=b(ci),hw=b(E),hx=b(D),hy=b(cu),hz=b(aY),hA=b(E),hB=b(D),hC=b(bf),hD=b(E),hE=b(D),hF=b(bf),hG=b(aQ),hH=b(aP),hh=[0,b(Q),1,0],he=[0,b(Q),1,0],hc=[0,[0,b(E)],0],hd=b("$l"),hf=[0,b(D)],hg=b("$t"),hi=[0,b(aX)],hj=b(aX),g9=b(L),g5=[0,b(Q),1,0],g2=[0,b(Q),1,0],gZ=[0,b(Q),1,0],gY=b(cj),g0=[0,b(aW)],g1=b("$id"),g3=[0,b("in")],g4=b(cj),g6=[0,b(aW)],g7=b(aW),gT=b(L),gR=b(L),gP=b(aO),gW=b(aW),ha=b(aX),hI=b(ct),hK=b(ct),hP=b(cl),hT=b(b5),hX=b(cd),h0=b(E),h3=b(D),h5=b(cB),h8=b(E),h$=b(D),ib=b(cb),ie=b(E),ii=b(D),ik=b(ci),io=b(E),ir=b(D),it=b(cu),iy=b(aY),iC=b(aQ),iF=b(E),iI=b(D),iL=b(bi),iO=b(E),iR=b(D),iU=b(bf),iY=b(aP),i5=b(cn),i7=b(cn),i$=b(b_),jb=b(cE),jd=b(cC),jB=[0,[0,[0,b(cs)],[0,[0,b("Rings")],0]],0],jI=[0,b(b8)],jM=[0,b(as)],jN=[0,b(co)],jV=b(be),j8=b(cD),j_=b(cD),ke=b(cp),kl=b(b7),kn=b(b7),kr=b(b_),kt=b(cE),kv=b(cC),kT=[0,[0,[0,b(cs)],[0,[0,b("Fields")],0]],0],k0=[0,b(b8)],k4=[0,b("Field")],k5=[0,b(co)],lb=b(bh),eX=g.Option,dh=g.Vars,c8=g.Refiner,c0=g.Environ,c1=g.Goal,cQ=g.Tacticals,cM=g.Esubst,eb=g.Constr,fW=g.Redexpr;function
bl(g,f,e,d,b){switch(a(f,d)){case
0:return c(M[54],e,b);case
1:return a(M[37],b);default:return-1===d?c(M[54],e,b):a(g,b)}}function
cF(b){var
c=a(q[39],b)[1];try{var
g=a(ae[16],c);return g}catch(b){b=J(b);if(b===N){var
f=a(d[3],cG);return e(r[3],0,0,f)}throw b}}function
bm(b){try{var
c=a(ae[16],b);return c}catch(b){b=J(b);if(b===N)return[0,a(i[1][6],cH)];throw b}}function
au(f,d,b){var
k=a(f,bm(b));if(k){var
p=k[1],r=-1;return bl(function(a){return au(f,d,a)},p,d,r,b)}var
h=a(q[ad],b);switch(h[0]){case
6:var
s=function(a,b){return au(f,a,b)};return e(M[56],s,d,b);case
9:var
g=h[2],i=h[1],t=0;if(g.length-1<=0){var
u=a(q[$],[0,i,g]);return a(M[37],u)}var
l=c(j[19][50],t,g),v=l[2],m=a(q[$],[0,i,l[1]]),n=a(f,bm(m));if(n){var
w=n[1],o=function(b,a){return bl(function(a){return au(f,d,a)},w,d,b,a)},x=c(j[19][16],o,v),y=[6,o(-1,m),x];return a(M[38],y)}var
z=a(q[$],[0,i,g]);return a(M[37],z);default:return a(M[37],b)}}function
bn(b,a){try{var
c=[0,e(j[17][119],ae[5],a,b)];return c}catch(a){a=J(a);if(a===N)return 0;throw a}}var
aZ=[0,j[15][45][1]];function
a0(b,a){aZ[1]=e(j[15][45][4],b,a,aZ[1]);return 0}function
cI(f){try{var
b=c(j[15][45][22],f,aZ[1]);return b}catch(b){b=J(b);if(b===N){var
g=a(d[3],cJ),h=a(d[20],f),i=a(d[3],cK),k=c(d[12],i,h),l=c(d[12],k,g);return e(r[6],0,cL,l)}throw b}}function
a1(f,d,j,b){var
g=a(cM[1],0),h=au(a(cI(f),b),g,b),i=e(M[42],0,M[9],d);return c(M[59],i,h)}function
cN(a){var
b=0,c=2,d=[0,function(b,c,d){return a1(a,b,c,d)},c];return e(bo[49],0,d,b)}function
cO(b,a){var
c=[0,[0,a,0]],d=2,f=[0,function(a,c,d){return a1(b,a,c,d)},d];return e(bo[49],0,f,c)}function
cP(f,b){var
g=c(j[17][12],av[46],b),h=e(j[17][16],a2[1][8][4],g,a2[1][8][1]);if(c(a2[1][16],h,f))return a(A[13],0);var
i=a(d[7],0);return c(cQ[70][4],0,i)}function
bp(b){var
d=[0,cR,0];function
e(a){return[0,[0,k[4],a]]}var
f=c(j[17][12],e,b),g=a(h[17],l[11]),m=a(h[5],g),n=[0,[0,c(h[7],m,f)],0],o=a(i[1][6],cS),p=[0,[1,[0,k[4],o]],0],q=a(h[5],l[13]),r=[0,[0,c(h[7],q,p)],n],s=[31,k[4],d,r];return[28,[0,[0,[0,a(i[1][6],cT)],0],s]]}function
cU(c){var
b=a(af[2],0),d=a(aw[17],b);return e(bq[13],b,d,c)}function
S(c){var
b=a(af[2],0),d=a(aw[17],b);return P(bq[10],b,d,0,c)[1]}function
aa(e,d,b){var
f=a(av[52],b),g=a(br[35][14],d),h=c(av[53],g,f),j=[0,a(br[35][13],h)],k=[0,[0,ln(bs[2],0,cW,0,0,0,j,0,b)],cV],l=a(i[1][6],e),m=U(bs[3],0,0,l,0,k);return a(q[121],m)}function
bt(h,g){function
l(g,b){var
d=b[1],h=b[3],j=b[2],l=a(n[20],d),m=c(n[16],cX,l),f=a(i[1][6],m),o=[2,[1,[0,k[4],f]]];return[0,d+1|0,[0,o,j],e(i[1][11][4],f,g,h)]}var
b=e(j[17][16],l,g,[0,0,0,i[1][11][1]]),m=b[3],o=b[2],p=a(i[1][6],cY),q=e(i[1][11][4],p,h,m),r=[0,q,a(f[12][28],0)[2]],d=a(i[1][6],cZ);return c(f[12][18],r,[29,[0,k[4],[3,k[4],[1,[0,k[4],d]],o]]])}var
bu=[0,[0]],c3=[0,bv,0],c5=[0,function(d,b){var
e=a(j[17][3],d),g=a(h[6],l[4]),k=c(f[12][2][7],g,e);function
m(d){var
e=b[1],f=a(n[20],d),g=c(n[16],c4,f),h=a(i[1][6],g);return c(i[1][11][22],h,e)}bu[1]=c(j[19][2],k,m);return a(A[13],0)}];e(f[6][9],0,bv,c5);function
bw(D,C,m,b,B){function
E(h,b){var
d=b[1],j=b[3],l=b[2],m=a(n[20],d),o=c(n[16],c6,m),g=a(i[1][6],o),p=[2,[1,[0,k[4],g]]],q=a(f[12][2][1],h);return[0,d+1|0,[0,p,l],e(i[1][11][4],g,q,j)]}var
o=e(j[17][16],E,B,[0,0,0,i[1][11][1]]),F=o[3],G=o[2],H=[0,F,a(f[12][28],0)[2]];function
I(b){var
d=a(n[20],b),e=c(n[16],c7,d);return a(i[1][6],e)}var
J=c(j[17][48],m,I),K=a(h[5],l[4]),L=[0,[0,c(h[7],K,m)],0],M=[31,k[4],c3,L];function
N(a){return[0,a]}var
O=[5,[28,[0,c(j[17][12],N,J),M]]],w=aw[3][1],x=q[113],y=a(c0[10],D),g=P(c1[4][6],C,y,x,w),z=[0,g[1],g[3]],d=u(b),Q=c(j[18],G,[0,O,0]),s=v===d?b[1]:p===d?a(t[2],b):b,R=c(f[12][18],H,[29,[0,k[4],[3,k[4],[0,[0,k[4],s]],Q]]]),S=c(A[67][8],R,z),T=a(c8[2],S),r=a(ab[44],T),U=r[2],V=c(aw[143],0,r[1])[2],W=bu[1];function
X(c){var
b=a(f[12][2][2],c),d=b?b[1]:a(n[2],c2);return a(U,d)}return[0,c(j[19][15],X,W),V]}function
by(a){return[p,function(b){return e(V[6],c9,bx,a)}]}function
ax(a){return[p,function(b){return e(V[7],c_,bx,a)}]}var
da=by(c$),a3=ax(db),a4=ax(dc),W=by(dd),an=ax(de),ao=ax(df);function
ap(b,d){var
c=u(b),e=v===c?b[1]:p===c?a(t[2],b):b;return a(q[$],[0,e,d])}function
w(f,b,e){var
d=u(b),g=v===d?b[1]:p===d?a(t[2],b):b,h=[0,c(ab[13],f,g),e];return a(q[$],h)}function
bz(h){var
c=a(q[ad],h);if(9===c[0]){var
b=c[2],i=c[1];if(2<=b.length-1){var
k=[0,i,e(j[19][7],b,0,b.length-1-2|0)],d=a(q[$],k);if(a(dh[2],d)){var
f=b.length-1-1|0,g=b.length-1-2|0,l=K(b,f)[f+1];return[0,d,K(b,g)[g+1],l]}return a(r[7],di)}}return a(r[7],dg)}var
ay=[0,dj,[0,aq,0]];function
dl(a){return c(j[18],ay,a)}var
bA=c(j[17][12],dl,dk);function
X(a){return[p,function(b){return e(V[6],dm,bA,a)}]}function
s(a){return[p,function(b){return e(V[7],dn,bA,a)}]}var
dr=c(j[17][12],i[1][6],[0,dq,[0,aq,dp]]);a(i[5][4],dr);var
du=c(j[17][12],i[1][6],[0,dt,[0,aq,ds]]),dv=a(i[5][4],du);function
bB(b){return[p,function(d){var
c=a(i[6][4],b);return e(i[ck],[0,dv],i[5][6],c)}]}function
ag(a){var
b=[0,aq,dx];return[p,function(c){return e(V[5],dw,b,a)}]}var
az=X(dy),dA=s(dz),dC=s(dB),dE=s(dD),aA=X(dF),dH=X(dG),aB=X(dI),dK=X(dJ),aC=X(dL),dN=X(dM),dP=X(dO),Y=bB(dQ),dS=s(dR),z=s(dT);function
bC(f,e){var
b=e;for(;;){var
d=a(q[ad],b);if(6===d[0]){var
b=d[3];continue}var
g=bz(b)[1],h=function(c){var
b=c[1],d=u(b),e=c[2],f=v===d?b[1]:p===d?a(t[2],b):b;return[0,f,e]},i=c(j[17][12],h,f),k=function(a){return-1===a?1:2},l=[0,[0,cF(g),k],i];return function(a){return bn(l,a)}}}var
dU=0;function
dV(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
13:return 2;case
0:case
9:case
11:case
14:return 0}return 1}var
dX=[0,[0,ag(dW),dV],dU];function
dY(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
d0=[0,[0,ag(dZ),dY],dX];function
d1(b){var
a=b-8|0;if(6<a>>>0){if(-9!==a)return 1}else
if(5===a)return 2;return 0}var
d3=[0,[0,ag(d2),d1],d0];function
d4(a){return 0}var
d6=[0,[0,s(d5),d4],d3];function
d7(a){return 0}var
d9=[0,[0,s(d8),d7],d6],d_=[0,[0,ao,function(a){return-1===a?0:1}],d9],d$=[0,[0,an,function(a){return-1===a?0:2===a?2:1}],d_];a0(ea,function(a){return bC(d$,a)});var
ah=a(j[21][1],[0,eb[33]]),a5=e(aD[2],0,ec,ah[1]),a6=e(aD[2],0,ed,T[24][1]);function
ee(a){return c(ah[22],a,a5[1])}function
em(d){var
a=d[2],b=d[1],e=c(B[45],b,a[1]),g=c(B[45],b,a[2]),h=c(B[45],b,a[3]),i=c(B[45],b,a[4]),j=c(B[45],b,a[5]),k=c(B[45],b,a[6]),l=c(B[45],b,a[9]),m=c(B[45],b,a[10]),n=c(f[5][1],b,a[7]),o=c(f[5][1],b,a[8]),p=c(f[5][1],b,a[11]),r=c(f[5][1],b,a[12]);if(e===a[1])if(g===a[2])if(c(q[132],h,a[3]))if(i===a[4])if(j===a[5])if(k===a[6])if(l===a[9])if(m===a[10])if(n===a[7])if(o===a[8])if(p===a[11])if(r===a[12])return a;return[0,e,g,h,i,j,k,n,o,l,m,p,r]}function
bE(b){var
a=b[2],c=b[1][1];a5[1]=e(ah[4],a[1],a,a5[1]);a6[1]=e(T[24][4],c,a,a6[1]);return 0}var
aF=a(aE[1],en),eo=aF[8],ep=aF[7];function
eq(a){return[0,a]}function
er(c,b){var
a=1===c?1:0;return a?bE(b):a}var
es=a(aE[4],[0,aF[1],bE,aF[3],er,eq,em,ep,eo]);function
bF(d,e,c,b){try{var
g=P(f[22][11],d,e[1],c,b),j=g[2],h=P(f[22][12],d,g[1],c,b),k=h[2],i=P(f[22][13],d,h[1],c,b),l=i[2];e[1]=i[1];var
m=ap(da,[0,c,b,j,k,l]);return m}catch(b){b=J(b);if(b===N)return a(r[7],et);throw b}}function
eu(h,g,f,e,d,c,b,a){return ap(dH,[0,h,g,f,e,d,c,b,a])}function
ev(f,e,d,c,b,a){return ap(dK,[0,f,e,d,c,b,a])}function
ew(s,h,i){var
b=i[5],l=i[4],j=i[3],k=i[2],g=i[1],o=a(q[ad],b);if(9===o[0])if(1===o[2].length-1){var
E=u(W),aR=o[1],aS=v===E?W[1]:p===E?a(t[2],W):W;if(c(q[at],aR,aS)){var
aT=w(h,dA,[0,g]),aU=l?w(h,dC,[0,g,k,j,l[1]]):w(h,dE,[0,g,k,j]),aV=e(aG[7],s,h,aT);return[0,aV,e(aG[7],s,h,aU)]}}var
x=[0,[0,[0,[0,g,[0,b]]],[0,[0,[0,g,[0,b]]],0]],[0,[0,g,[0,b]]]],F=bF(a(af[2],0),h,g,b);try{var
aQ=c(f[22][14],x,k),y=aQ}catch(b){b=J(b);if(b!==N)throw b;var
y=a(r[7],ex)}var
m=y[2];try{var
aP=c(f[22][14],x,j),z=aP}catch(b){b=J(b);if(b!==N)throw b;var
z=a(r[7],ey)}var
n=z[2];if(l){var
A=l[1];try{var
aq=c(f[22][14],[0,[0,[0,[0,g,[0,b]]],0],[0,[0,g,[0,b]]]],A),B=aq}catch(b){b=J(b);if(b!==N)throw b;var
B=a(r[7],ez)}var
C=B[2],H=eu(g,k,j,A,b,m,n,C),I=a(d[3],eA),K=a(G[5],C),L=a(d[3],eB),M=a(d[13],0),O=a(d[3],eC),P=a(G[5],n),Q=a(d[3],eD),R=a(d[13],0),S=a(d[3],eE),T=a(G[5],m),U=a(d[3],eF),V=a(d[13],0),X=a(d[3],eG),Y=a(G[5],b),Z=a(d[3],eH),_=c(d[12],Z,Y),$=c(d[12],_,X),aa=c(d[12],$,V),ab=c(d[12],aa,U),ac=c(d[12],ab,T),ae=c(d[12],ac,S),ag=c(d[12],ae,R),ah=c(d[12],ag,Q),aj=c(d[12],ah,P),ak=c(d[12],aj,O),al=c(d[12],ak,M),am=c(d[12],al,L),an=c(d[12],am,K),ao=c(d[12],an,I),ap=function(a){return c(ai[6],0,a)};c(bG[49],ap,ao);var
D=H}else{var
ar=a(d[3],eI),as=a(G[5],n),au=a(d[3],eJ),av=a(d[13],0),aw=a(d[3],eK),ax=a(G[5],m),ay=a(d[3],eL),az=a(d[13],0),aA=a(d[3],eM),aB=a(G[5],b),aC=a(d[3],eN),aD=c(d[12],aC,aB),aE=c(d[12],aD,aA),aF=c(d[12],aE,az),aH=c(d[12],aF,ay),aI=c(d[12],aH,ax),aJ=c(d[12],aI,aw),aK=c(d[12],aJ,av),aL=c(d[12],aK,au),aM=c(d[12],aL,as),aN=c(d[12],aM,ar),aO=function(a){return c(ai[6],0,a)};c(bG[49],aO,aN);var
D=ev(g,k,j,b,m,n)}return[0,F,D]}function
bH(h,g,f,e,d,c,b,a){return a?a[1]:ew(h,g,[0,f,e,d,c,b])}function
bI(b){if(typeof
b==="number"){var
c=u(aC);return v===c?aC[1]:p===c?a(t[2],aC):aC}else
return 0===b[0]?ap(dN,[0,b[1]]):ap(dP,[0,b[1]])}function
bJ(q,o,n,m,l,d){if(d){var
b=d[1];if(0===b[0])return a(f[8][3],b[1]);var
g=b[1],h=function(a){return c(bK[3],0,a)};return bp(c(j[17][12],h,g))}var
e=u(Y),i=v===e?Y[1]:p===e?a(t[2],Y):Y;return[29,[0,k[4],[3,k[4],[0,[0,k[4],i]],0]]]}function
aH(c,b,a){return w(b,dS,[0,U(ac[2],0,0,c,b[1],a),a])}function
bL(d,b,h){var
f=u(z),i=v===f?z[1]:p===f?a(t[2],z):z,g=c(ab[13],b,i),k=w(b,ao,[0,g]);function
l(c,a){return w(b,an,[0,g,aH(d,b,c),a])}var
m=e(j[17][16],l,h,k),n=e(aG[7],d,b,m);return c(ab[43],b[1],n)}function
bM(n,b,e){var
g=u(z),o=v===g?z[1]:p===g?a(t[2],z):z,h=c(ab[13],b,o);if(e){var
i=e[1],d=i[1],q=i[2];if(0===d[0])var
l=a(f[8][3],d[1]);else
var
r=d[1],s=function(a){return c(bK[3],0,a)},l=bp(c(j[17][12],s,r));return[0,l,w(b,a4,[0,h,aH(n,b,S(q))])]}var
m=u(Y),x=v===m?Y[1]:p===m?a(t[2],Y):Y,y=[0,[0,k[4],x]],A=w(b,a3,[0,h]);return[0,[29,[0,k[4],[3,k[4],y,0]]],A]}function
bN(g,b,d){var
e=u(z),h=v===e?z[1]:p===e?a(t[2],z):z,f=c(ab[13],b,h);return d?w(b,a4,[0,f,aH(g,b,S(d[1]))]):w(b,a3,[0,f])}function
bO(g,b,d){var
e=u(z),h=v===e?z[1]:p===e?a(t[2],z):z,f=c(ab[13],b,h);return d?w(b,a4,[0,f,aH(g,b,S(d[1]))]):w(b,a3,[0,f])}function
bP(o,L,ar,J,aq,I,ap,ao,an){var
M=I[2],N=I[1],O=L[2],s=L[1],as=c(j[18],ay,eR);a(V[11],as);var
g=a(af[2],0),$=U(ac[2],0,0,g,s,O),l=a(q[ad],$);if(9===l[0]){var
b=l[2],w=b.length-1-6|0;if(2<w>>>0)var
e=0;else{var
m=l[1];switch(w){case
0:var
x=u(aB),ab=b[1],ae=b[2],ag=b[3],ah=b[4],ai=b[5],aj=b[6],ak=v===x?aB[1]:p===x?a(t[2],aB):aB;if(c(q[at],m,ak))var
d=[0,eP,ab,ae,ag,ah,ai,0,0,aj],e=1;else
var
e=0;break;case
1:var
e=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],F=b[8],G=u(az),al=v===G?az[1]:p===G?a(t[2],az):az;if(c(q[at],m,al))var
d=[0,0,y,z,A,B,C,[0,D],[0,E],F],e=1;else{var
H=u(aA),am=v===H?aA[1]:p===H?a(t[2],aA):aA;if(c(q[at],m,am))var
d=[0,eQ,y,z,A,B,C,[0,D],[0,E],F],e=1;else
var
e=0}}}}else
var
e=0;if(!e)var
d=a(r[7],eO);var
P=d[9],Q=d[8],R=d[6],S=d[5],T=d[2],h=[0,s],au=d[4],av=d[3],aw=d[1],W=bH(g,h,T,S,R,Q,P,ar),X=W[1],ax=W[2],Y=bM(g,h,ap),aC=Y[2],aD=Y[1],aE=bN(g,h,ao),aF=bO(g,h,an),aG=[0,X,[0,ax,[0,O,[0,aC,[0,aE,[0,aF,[0,bI(J),0]]]]]]],aH=bB(eS),Z=bw(g,h[1],5,aH,aG),_=Z[2],k=Z[1],aI=K(k,3)[4],aJ=K(k,4)[5],aK=a(i[1][8],o),aL=aa(c(n[16],aK,eT),_,aI),aM=a(i[1][8],o),aN=aa(c(n[16],aM,eU),_,aJ),aO=bJ(g,s,J,aw,[0,av,au,S,R,Q],aq),aP=N?a(f[8][3],N[1]):eW,aQ=M?a(f[8][3],M[1]):eV,aR=K(k,0)[1],aS=K(k,2)[3],aT=a(es,[0,T,P,X,K(k,1)[2],aS,aR,aO,aD,aL,aN,aP,aQ]);c(bQ[6],o,aT);return 0}function
bR(a){return typeof
a==="number"?0:0===a[0]?[0,S(a[1])]:[1,S(a[1])]}function
x(e,b,d){if(a(eX[3],b[1])){b[1]=[0,d];return 0}var
f=c(n[16],e,eY);return a(r[7],f)}function
eZ(l){var
b=[0,0],d=[0,0],e=[0,0],f=[0,0],g=[0,0],h=[0,0],i=[0,0],k=[0,0];function
m(a){switch(a[0]){case
0:return x(e0,b,bR(a[1]));case
1:return x(e1,e,a[1]);case
2:return x(e2,f,a[1]);case
3:return x(e3,g,a[1]);case
4:var
c=a[1],j=S(a[2]);return x(e4,d,[0,S(c),j]);case
5:return x(e5,i,[0,a[1],a[2]]);case
6:return x(e6,h,a[1]);default:return x(e7,k,a[1])}}c(j[17][11],m,l);var
a=b[1],n=a?a[1]:0;return[0,n,d[1],e[1],f[1],g[1],i[1],h[1],k[1]]}function
bS(a,c){if(a)return a;var
b=bz(c);return[0,b[2],[0,b[3],0]]}function
bT(d,a,b,c){var
f=w(a,ao,[0,b]);function
g(d,c){return w(a,an,[0,b,d,c])}var
h=e(j[17][16],g,c,f);return e(aG[7],d,a,h)}var
y=f[12][2][1];function
Z(b){var
d=a(f[12][28],0);return c(f[12][2][6],d,b)}function
e8(b){var
c=a(y,b[2]),d=a(y,b[3]),e=a(y,b[4]),f=a(y,b[5]),g=a(y,b[6]),h=Z(b[7]),i=Z(b[8]),j=a(y,b[9]),k=a(y,b[10]),l=Z([28,[0,e9,b[11]]]);return[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,l,[0,Z([28,[0,e_,b[12]]]),0]]]]]]]]]]]}function
e$(E,D,C,B){var
b=[0,function(l){var
f=a(bU[48][4],l),b=a(A[63][5],l);try{var
i=[0,f],g=bS(C,B);if(g){var
m=g[2],h=U(ac[2],0,0,b,f,g[1]),n=function(g){var
i=U(ac[2],0,0,b,f,g),c=1-U(bD[77],0,b,f,h,i);if(c){var
j=a(d[3],ef);return e(r[6],0,eg,j)}return c};c(j[17][11],n,m);try{var
z=ee(h),k=z}catch(b){b=J(b);if(b!==N)throw b;var
o=a(d[3],eh),p=a(G[5],h),q=a(d[3],ei),s=a(d[13],0),t=a(d[3],ej),u=c(d[12],t,s),v=c(d[12],u,q),w=c(d[12],v,p),x=c(d[12],w,o),k=e(r[6],0,ek,x)}var
F=a(y,bT(b,i,k[1],g)),I=a(y,bL(b,i,D)),K=e8(k),L=bt(E,c(j[18],K,[0,I,[0,F,0]])),M=a(A[61][1],i[1]),O=c(A[15],M,L);return O}throw[0,H,el]}catch(b){b=J(b);if(a(A[67][10],b))return c(A[18],0,b);throw b}}];return a(A[63][10],b)}var
fc=c(j[17][12],i[1][6],[0,fb,[0,aq,fa]]),fd=a(i[5][4],fc),fe=0;function
ff(b){var
a=b+1|0;if(!(16<a>>>0))switch(a){case
15:return 2;case
0:case
11:case
13:case
16:return 0}return 1}var
fh=[0,[0,s(fg),ff],fe];function
fi(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
13:return 2;case
0:case
9:case
11:case
14:return 0}return 1}var
fk=[0,[0,ag(fj),fi],fh];function
fl(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
17:return 2;case
0:case
9:case
10:case
11:case
12:case
14:case
16:case
18:return 0}return 1}var
fn=[0,[0,ag(fm),fl],fk];function
fo(b){var
a=b-8|0;if(6<a>>>0){if(-9!==a)return 1}else
if(5===a)return 2;return 0}var
fq=[0,[0,ag(fp),fo],fn];function
fr(b){var
a=b+1|0;if(!(20<a>>>0))switch(a){case
13:case
18:return 2;case
0:case
10:case
11:case
12:case
15:case
17:case
19:case
20:return 0}return 1}var
ft=[0,[0,s(fs),fr],fq];function
fu(b){var
a=b-9|0;if(7<a>>>0){if(-10!==a)return 1}else
if(5===a)return 2;return 0}var
fw=[0,[0,s(fv),fu],ft];function
fx(a){return 0}var
fz=[0,[0,s(fy),fx],fw];function
fA(a){return 0}var
fC=[0,[0,s(fB),fA],fz],fD=[0,[0,ao,function(a){return-1===a?0:1}],fC],fE=[0,[0,an,function(a){return-1===a?0:2===a?2:1}],fD];a0(fF,function(a){return bC(fE,a)});var
fG=0;function
fH(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
10:case
14:return 2;case
0:case
12:case
15:return 0}return 1}var
fJ=[0,[0,s(fI),fH],fG];function
fK(a){return 0}var
fM=[0,[0,s(fL),fK],fJ];function
fN(a){return 0}var
fP=[0,[0,s(fO),fN],fM],fQ=[0,[0,ao,function(a){return-1===a?0:1}],fP],fR=[0,[0,an,function(a){return-1===a?0:2===a?2:1}],fQ];a0(fS,function(e){function
b(c){var
b=c[1],d=u(b),e=c[2],f=v===d?b[1]:p===d?a(t[2],b):b;return[0,f,e]}var
d=c(j[17][12],b,fR);return function(a){return bn(d,a)}});function
fU(a,b,c){return a1(fT,a,b,c)}c(fW[3],fV,fU);var
aI=s(fX),aJ=s(fY),aK=s(fZ),f1=s(f0),f3=s(f2),f5=s(f4),a7=e(aD[2],0,f9,ah[1]),a8=e(aD[2],0,f_,T[24][1]);function
f$(a){return c(ah[22],a,a7[1])}function
gi(d){var
a=d[2],b=d[1],e=c(B[45],b,a[1]),g=c(B[45],b,a[2]),h=c(B[45],b,a[5]),i=c(B[45],b,a[6]),j=c(B[45],b,a[7]),k=c(B[45],b,a[8]),l=c(B[45],b,a[9]),m=c(f[5][1],b,a[3]),n=c(f[5][1],b,a[4]),o=c(f[5][1],b,a[10]),p=c(f[5][1],b,a[11]);if(e===a[1])if(g===a[2])if(h===a[5])if(i===a[6])if(j===a[7])if(k===a[8])if(l===a[9])if(m===a[3])if(n===a[4])if(o===a[10])if(p===a[11])return a;return[0,e,g,m,n,h,i,j,k,l,o,p]}function
bV(b){var
a=b[2],c=b[1][1];a7[1]=e(ah[4],a[1],a,a7[1]);a8[1]=e(T[24][4],c,a,a8[1]);return 0}var
aL=a(aE[1],gj),gk=aL[8],gl=aL[7];function
gm(a){return[0,a]}function
gn(c,b){var
a=1===c?1:0;return a?bV(b):a}var
go=a(aE[4],[0,aL[1],bV,aL[3],gn,gm,gi,gl,gk]);function
gp(j,b,g,d){var
e=a(q[ad],d);if(9===e[0])if(1===e[2].length-1){var
i=u(W),m=e[1],n=v===i?W[1]:p===i?a(t[2],W):W;if(c(q[at],m,n)){var
o=a(V[38],0)[6],s=[0,a(av[46],o),[0,b,b,g]];return a(q[$],s)}}bF(a(af[2],0),j,b,d);var
k=[0,[0,[0,[0,b,[0,d]]],0],[0,[0,b,[0,d]]]];try{var
l=c(f[22][14],k,g),h=l}catch(b){b=J(b);if(b!==N)throw b;var
h=a(r[7],gq)}return h[2]}function
gr(k,al,aE,J,ak,aj,ai,ah,ag,ab){var
am=ai[2],an=ai[1],o=al[2],ao=al[1],aF=c(j[18],ay,gs);a(V[11],aF);var
l=a(af[2],0),g=[0,ao],aA=U(ac[2],0,0,l,g[1],o),H=a(q[ad],aA);if(9===H[0]){var
b=H[2],N=b.length-1-8|0;if(2<N>>>0)var
h=0;else{var
I=H[1];switch(N){case
0:var
O=b[1],P=b[2],Q=b[3],R=b[4],S=b[5],T=b[6],W=b[7],X=b[8],Y=u(aK),aB=v===Y?aK[1]:p===Y?a(t[2],aK):aK;if(c(ae[11],aB,I))var
d=[0,f7,O,P,Q,R,S,0,0,T,W,X,w(g,f5,[0,O,P,Q,R,S,T,W,X,o])],h=1;else
var
h=0;break;case
1:var
h=0;break;default:var
x=b[1],y=b[2],z=b[3],A=b[4],B=b[5],C=b[6],D=b[7],E=b[8],F=b[9],G=b[10],Z=u(aI),aC=v===Z?aI[1]:p===Z?a(t[2],aI):aI;if(c(ae[11],aC,I))var
d=[0,0,x,y,z,A,B,[0,C],[0,D],E,F,G,w(g,f1,[0,x,y,z,A,B,C,D,E,F,G,o])],h=1;else{var
_=u(aJ),aD=v===_?aJ[1]:p===_?a(t[2],aJ):aJ;if(c(ae[11],aD,I))var
d=[0,f8,x,y,z,A,B,[0,C],[0,D],E,F,G,w(g,f3,[0,x,y,z,A,B,C,D,E,F,G,o])],h=1;else
var
h=0}}}}else
var
h=0;if(!h)var
d=a(r[7],f6);var
L=d[11],ap=d[8],aq=d[6],ar=d[5],M=d[2],aG=d[12],aH=d[10],aL=d[4],aM=d[3],aN=d[1],as=bH(l,g,M,ar,aq,ap,L,aE),at=as[2],au=as[1];bP(k,[0,g[1],aG],[0,[0,au,at]],J,ak,gt,ah,ag,ab);var
av=bM(l,g,ah),aO=av[2],aP=av[1],aQ=bN(l,g,ag),aR=bO(l,g,ab),aS=gp(g,M,aH,L),aT=[0,au,[0,at,[0,aS,[0,o,[0,aO,[0,aQ,[0,aR,[0,bI(J),0]]]]]]]],az=[p,function(c){var
b=a(i[6][4],gu);return e(i[ck],[0,fd],i[5][6],b)}],aw=bw(l,g[1],9,az,aT),s=aw[2],m=aw[1],aU=K(m,3)[4],aV=K(m,4)[5],aW=K(m,5)[6],aX=K(m,6)[7];if(aj)var
aY=[0,aj[1]],aZ=[0,K(m,8)[9],aY],ax=a(q[$],aZ);else
var
ax=K(m,7)[8];var
a0=a(i[1][8],k),a1=aa(c(n[16],a0,gv),s,aU),a2=a(i[1][8],k),a3=aa(c(n[16],a2,gw),s,aV),a4=a(i[1][8],k),a5=aa(c(n[16],a4,gx),s,aW),a6=a(i[1][8],k),a7=aa(c(n[16],a6,gy),s,aX),a8=a(i[1][8],k),a9=aa(c(n[16],a8,gz),s,ax),a_=bJ(l,ao,J,aN,[0,aM,aL,ar,aq,ap],ak),a$=an?a(f[8][3],an[1]):gB,ba=am?a(f[8][3],am[1]):gA,bb=a(go,[0,M,L,a_,aP,a1,a3,a5,a7,a9,a$,ba]);c(bQ[6],k,bb);return 0}function
gC(b){var
d=[0,0],e=[0,0],f=[0,0],g=[0,0],h=[0,0],i=[0,0],k=[0,0],l=[0,0],m=[0,0];function
n(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return x(gD,d,bR(a[1]));case
1:return x(gE,f,a[1]);case
2:return x(gF,g,a[1]);case
3:return x(gG,h,a[1]);case
4:var
c=a[1],j=S(a[2]);return x(gH,e,[0,S(c),j]);case
5:return x(gI,l,[0,a[1],a[2]]);case
6:return x(gJ,k,a[1]);default:return x(gK,m,a[1])}}return x(gL,i,S(b[1]))}c(j[17][11],n,b);var
a=d[1],o=a?a[1]:0;return[0,o,e[1],i[1],f[1],g[1],h[1],l[1],k[1],m[1]]}function
gM(b){var
c=a(y,b[2]),d=Z(b[3]),e=Z(b[4]),f=a(y,b[5]),g=a(y,b[7]),h=a(y,b[6]),i=a(y,b[8]),j=a(y,b[9]),k=Z([28,[0,gN,b[10]]]);return[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,Z([28,[0,gO,b[11]]]),0]]]]]]]]]]}var
C=[0,cO,cN,cP,eZ,bP,cU,a6,e$,gC,gr,a8,function(F,E,D,C){var
b=[0,function(l){var
f=a(bU[48][4],l),b=a(A[63][5],l);try{var
i=[0,f],g=bS(D,C),m=c(j[18],ay,ga);a(V[11],m);if(g){var
n=g[2],h=U(ac[2],0,0,b,f,g[1]),o=function(g){var
i=U(ac[2],0,0,b,f,g),c=1-U(bD[77],0,b,f,h,i);if(c){var
j=a(d[3],gb);return e(r[6],0,gc,j)}return c};c(j[17][11],o,n);try{var
B=f$(h),k=B}catch(b){b=J(b);if(b!==N)throw b;var
p=a(d[3],gd),q=a(G[5],h),s=a(d[3],ge),t=a(d[13],0),u=a(d[3],gf),v=c(d[12],u,t),w=c(d[12],v,s),x=c(d[12],w,q),z=c(d[12],x,p),k=e(r[6],0,gg,z)}var
I=a(y,bT(b,i,k[1],g)),K=a(y,bL(b,i,E)),L=gM(k),M=bt(F,c(j[18],L,[0,K,[0,I,0]])),O=a(A[61][1],i[1]),P=c(A[15],O,M);return P}throw[0,H,gh]}catch(b){b=J(b);if(a(A[67][10],b))return c(A[18],0,b);throw b}}];return a(A[63][10],b)}];bb(367,C,"Newring_plugin.Newring");a(ar[12],gP);var
gQ=0,gS=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(h[6],l[5]),g=c(f[12][2][7],e,d);return function(b){return a(C[2],g)}}return a(n[2],gR)},gQ],gU=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],g=b[1],i=a(h[6],l[5]),j=c(f[12][2][7],i,g),k=a(h[6],l[9]),m=c(f[12][2][7],k,e);return function(a){return c(C[1],j,m)}}}return a(n[2],gT)},gS],gV=a(j[19][12],gU);e(f[6][9],0,[0,I,gW],gV);function
gX(r){var
j=a(i[1][7],gY),b=l[5],g=0,h=0;if(0===b[0]){var
m=[0,[0,g0,[0,[1,k[4],[5,[0,b[1]]],j],h]],g],o=a(i[1][7],g1),c=l[9],n=0;if(0===c[0]){var
p=[0,g3,[0,[1,k[4],[5,[0,c[1]]],o],n]],q=a(i[1][7],g4),d=l[5];if(0===d[0])return e(f[9][4],[0,I,g7],0,[0,[0,g6,[0,[1,k[4],[5,[0,d[1]]],q],p]],m]);throw[0,H,g5]}throw[0,H,g2]}throw[0,H,gZ]}c(ar[19],gX,I);var
g8=0,g_=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],g=b[1],i=a(h[6],l[13]),j=c(f[12][2][7],i,g),k=a(h[17],l[22]),m=a(h[6],k),o=c(f[12][2][7],m,e);return function(a){return c(C[3],j,o)}}}return a(n[2],g9)},g8],g$=a(j[19][12],g_);e(f[6][9],0,[0,I,ha],g$);function
hb(m){var
g=a(i[1][7],hd),b=l[22],d=0;if(0===b[0]){var
h=[0,hf,[0,[1,k[4],[0,[5,[0,b[1]]]],g],hc]],j=a(i[1][7],hg),c=l[13];if(0===c[0])return e(f[9][4],[0,I,hj],0,[0,[0,hi,[0,[1,k[4],[5,[0,c[1]]],j],h]],d]);throw[0,H,hh]}throw[0,H,he]}c(ar[19],hb,I);function
aM(b){switch(b[0]){case
0:var
g=b[1];if(typeof
g==="number")return a(d[3],hk);else{if(0===g[0]){var
j=c(d[31],O[23],g[1]),k=a(d[3],hl);return c(d[12],k,j)}var
l=c(d[31],O[23],g[1]),m=a(d[3],hm);return c(d[12],m,l)}case
1:var
h=b[1];if(0===h[0]){var
n=h[1],o=a(d[3],hn),p=a(f[2][18],n),q=a(d[3],ho),r=a(d[13],0),s=a(d[3],hp),t=c(d[12],s,r),u=c(d[12],t,q),v=c(d[12],u,p);return c(d[12],v,o)}var
w=h[1],x=a(d[3],hq),y=e(d[38],d[13],T[41],w),z=a(d[3],hr),A=a(d[13],0),B=a(d[3],hs),C=c(d[12],B,A),D=c(d[12],C,z),E=c(d[12],D,y);return c(d[12],E,x);case
2:var
F=b[1],G=a(d[3],ht),H=a(f[2][18],F),I=a(d[3],hu),J=a(d[13],0),K=a(d[3],hv),L=c(d[12],K,J),M=c(d[12],L,I),N=c(d[12],M,H);return c(d[12],N,G);case
3:var
P=b[1],Q=a(d[3],hw),R=a(f[2][18],P),S=a(d[3],hx),U=a(d[13],0),V=a(d[3],hy),W=c(d[12],V,U),X=c(d[12],W,S),Y=c(d[12],X,R);return c(d[12],Y,Q);case
4:var
Z=b[1],_=c(d[31],O[23],b[2]),$=c(d[31],O[23],Z),aa=a(d[3],hz),ab=c(d[12],aa,$);return c(d[12],ab,_);case
5:var
i=b[1];if(0===i[0]){var
ac=b[2],ad=i[1],ae=a(d[3],hA),af=a(f[2][18],ad),ag=a(d[3],hB),ah=a(d[13],0),ai=c(d[31],O[23],ac),aj=a(d[3],hC),ak=c(d[12],aj,ai),al=c(d[12],ak,ah),am=c(d[12],al,ag),an=c(d[12],am,af);return c(d[12],an,ae)}var
ao=b[2],ap=i[1],aq=a(d[3],hD),ar=e(d[38],d[13],T[41],ap),as=a(d[3],hE),at=a(d[13],0),au=c(d[31],O[23],ao),av=a(d[3],hF),aw=c(d[12],av,au),ax=c(d[12],aw,at),ay=c(d[12],ax,as),az=c(d[12],ay,ar);return c(d[12],az,aq);case
6:var
aA=c(d[31],O[23],b[1]),aB=a(d[3],hG);return c(d[12],aB,aA);default:var
aC=c(d[31],O[23],b[1]),aD=a(d[3],hH);return c(d[12],aD,aC)}}var
a9=a(h[3],hI),hJ=a(h[4],a9),aN=e(o[13],o[9],hK,hJ),hL=0,hM=0;function
hN(a,c,b){return[0,[0,a]]}var
hO=[6,o[15][1]],hQ=[0,[0,[0,[0,0,[0,a(m[12],hP)]],hO],hN],hM];function
hR(b,a){return hS}var
hU=[0,[0,[0,0,[0,a(m[12],hT)]],hR],hQ];function
hV(a,c,b){return[0,[1,a]]}var
hW=[6,o[15][1]],hY=[0,[0,[0,[0,0,[0,a(m[12],hX)]],hW],hV],hU];function
hZ(e,a,d,c,b){return[1,[0,a]]}var
h1=[0,a(m[12],h0)],h2=[6,f[3][18]],h4=[0,a(m[12],h3)],h6=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],h5)]],h4],h2],h1],hZ],hY];function
h7(e,a,d,c,b){return[1,[1,a]]}var
h9=[0,a(m[12],h8)],h_=[1,[6,o[15][7]]],ia=[0,a(m[12],h$)],ic=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],ib)]],ia],h_],h9],h7],h6];function
id(e,a,d,c,b){return[2,a]}var
ig=[0,a(m[12],ie)],ih=[6,f[3][18]],ij=[0,a(m[12],ii)],il=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],ik)]],ij],ih],ig],id],ic];function
im(e,a,d,c,b){return[3,a]}var
ip=[0,a(m[12],io)],iq=[6,f[3][18]],is=[0,a(m[12],ir)],iu=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],it)]],is],iq],ip],im],il];function
iv(b,a,d,c){return[4,a,b]}var
iw=[6,o[15][1]],ix=[6,o[15][1]],iz=[0,[0,[0,[0,[0,0,[0,a(m[12],iy)]],ix],iw],iv],iu];function
iA(a,c,b){return[6,a]}var
iB=[6,o[15][1]],iD=[0,[0,[0,[0,0,[0,a(m[12],iC)]],iB],iA],iz];function
iE(f,b,e,a,d,c){return[5,[1,b],a]}var
iG=[0,a(m[12],iF)],iH=[1,[6,o[15][7]]],iJ=[0,a(m[12],iI)],iK=[6,o[15][1]],iM=[0,[0,[0,[0,[0,[0,[0,0,[0,a(m[12],iL)]],iK],iJ],iH],iG],iE],iD];function
iN(f,b,e,a,d,c){return[5,[0,b],a]}var
iP=[0,a(m[12],iO)],iQ=[6,f[3][18]],iS=[0,a(m[12],iR)],iT=[6,o[15][1]],iV=[0,[0,[0,[0,[0,[0,[0,0,[0,a(m[12],iU)]],iT],iS],iQ],iP],iN],iM];function
iW(a,c,b){return[7,a]}var
iX=[6,o[15][1]],iZ=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(m[12],iY)]],iX],iW],iV]],hL]];e(o[22],aN,0,iZ);function
i0(h,g,f,c){var
b=a(d[3],i1);return e(r[3],0,0,b)}function
i2(h,g,f,c){var
b=a(d[3],i3);return e(r[3],0,0,b)}function
i4(c,b,a){return aM}P(f[2][1],a9,i4,i2,i0);function
bW(b){var
c=e(d[38],d[28],aM,b);return a(d[45],c)}var
aj=a(h[3],i5),i6=a(h[4],aj),bX=e(o[13],o[9],i7,i6),i8=0,i9=0;function
i_(d,a,c,b){return a}var
ja=[0,a(m[12],i$)],jc=[2,[6,aN],[0,a(m[12],jb)]],je=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(m[12],jd)]],jc],ja],i_],i9]],i8]];e(o[22],bX,0,je);function
jf(h,g,f,c){var
b=a(d[3],jg);return e(r[3],0,0,b)}function
jh(h,g,f,c){var
b=a(d[3],ji);return e(r[3],0,0,b)}function
jj(c,b,a){return bW}P(f[2][1],aj,jj,jh,jf);var
jk=0,jp=[0,[0,0,function(b){return b?a(n[2],jl):function(g){var
b=a(d[22],jm);c(ai[7],0,b);var
e=C[7][1];function
f(e,b){var
f=a(G[5],b[2]),g=a(d[3],jn),h=a(d[13],0),i=a(G[5],b[1]),j=a(d[3],jo),k=a(d[13],0),l=a(T[20],e),m=a(O[12],l),n=c(d[12],m,k),o=c(d[12],n,j),p=c(d[12],o,i),q=c(d[12],p,h),r=c(d[12],q,g),s=c(d[12],r,f),t=c(d[26],2,s);return c(ai[7],0,t)}return c(T[24][10],f,e)}}],jk],jr=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],i=d[1],j=b[1],k=a(h[4],l[9]),m=c(h[8],k,j),o=a(h[4],l[13]),p=c(h[8],o,i),q=a(h[18],aj),r=a(h[4],q),f=c(h[8],r,g);return function(n){var
c=f?f[1]:0,b=a(C[4],c),d=b[8],e=b[7],g=b[6],h=b[3],i=b[2],j=b[1],k=[0,b[4],b[5]],l=a(C[6],p);return lo(C[5],m,l,i,j,h,k,g,e,d)}}}}return a(n[2],jq)}],jp];function
js(b,a){return e(bY[1],a[1],[0,jt,b],a[2])}c(ak[80],js,jr);var
ju=0,jw=[0,function(b){return b?a(n[2],jv):function(a){return al[5]}},ju],jy=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return al[6]}}}return a(n[2],jx)},jw];function
jz(b,a){return c(al[3],[0,jA,b],a)}c(ak[80],jz,jy);var
jC=[5,[6,a(o[12],aj)]],jD=a(h[18],aj),jE=a(h[4],jD),jF=[0,[1,k[4],jE,jC],0],jG=[6,a(o[12],l[13])],jH=a(h[4],l[13]),jJ=[0,jI,[0,[1,k[4],jH,jG],jF]],jK=[6,a(o[12],l[9])],jL=a(h[4],l[9]),jO=[0,[0,jN,[0,jM,[0,[1,k[4],jL,jK],jJ]]],jB];function
jP(b,a){return e(bZ[1],[0,jQ,b],0,a)}c(ak[80],jP,jO);var
jR=0,jT=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],i=d[1],k=b[1],m=a(h[6],f[1][1]),o=c(f[12][2][7],m,k),p=a(h[17],l[13]),q=a(h[6],p),r=c(f[12][2][7],q,i),s=a(h[17],l[13]),t=a(h[6],s),u=c(f[12][2][7],t,g);return function(c){var
b=a(j[17][93],u);return P(C[8],o,r,b[2],b[1])}}}}return a(n[2],jS)},jR],jU=a(j[19][12],jT);e(f[6][9],0,[0,I,jV],jU);function
jW(r){var
j=a(i[1][7],jX),b=l[13],g=0,h=0;if(0===b[0]){var
m=[0,jZ,[0,[1,k[4],[0,[5,[0,b[1]]]],j],h]],n=a(i[1][7],j0),c=l[13];if(0===c[0]){var
o=[0,j2,[0,[1,k[4],[2,[5,[0,c[1]]]],n],m]],p=a(i[1][7],j3),d=f[1][1],q=0;if(0===d[0])return e(f[9][4],[0,I,j6],0,[0,[0,j5,[0,[1,k[4],[6,[0,d[1]],q],p],o]],g]);throw[0,H,j4]}throw[0,H,j1]}throw[0,H,jY]}c(ar[19],jW,I);function
a_(b){if(0===b[0])return aM(b[1]);var
e=c(d[31],O[23],b[1]),f=a(d[3],j7);return c(d[12],f,e)}var
a$=a(h[3],j8),j9=a(h[4],a$),ba=e(o[13],o[9],j_,j9),j$=0,ka=0,kb=[0,[0,[0,0,[6,aN]],function(a,b){return[0,a]}],ka];function
kc(a,c,b){return[1,a]}var
kd=[6,o[15][1]],kf=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(m[12],ke)]],kd],kc],kb]],j$]];e(o[22],ba,0,kf);function
kg(h,g,f,c){var
b=a(d[3],kh);return e(r[3],0,0,b)}function
ki(h,g,f,c){var
b=a(d[3],kj);return e(r[3],0,0,b)}function
kk(c,b,a){return a_}P(f[2][1],a$,kk,ki,kg);function
b0(b){var
c=e(d[38],d[28],a_,b);return a(d[45],c)}var
am=a(h[3],kl),km=a(h[4],am),b1=e(o[13],o[9],kn,km),ko=0,kp=0;function
kq(d,a,c,b){return a}var
ks=[0,a(m[12],kr)],ku=[2,[6,ba],[0,a(m[12],kt)]],kw=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(m[12],kv)]],ku],ks],kq],kp]],ko]];e(o[22],b1,0,kw);function
kx(h,g,f,c){var
b=a(d[3],ky);return e(r[3],0,0,b)}function
kz(h,g,f,c){var
b=a(d[3],kA);return e(r[3],0,0,b)}function
kB(c,b,a){return b0}P(f[2][1],am,kB,kz,kx);var
kC=0,kH=[0,[0,0,function(b){return b?a(n[2],kD):function(g){var
b=a(d[22],kE);c(ai[7],0,b);var
e=C[11][1];function
f(e,b){var
f=a(G[5],b[2]),g=a(d[3],kF),h=a(d[13],0),i=a(G[5],b[1]),j=a(d[3],kG),k=a(d[13],0),l=a(T[20],e),m=a(O[12],l),n=c(d[12],m,k),o=c(d[12],n,j),p=c(d[12],o,i),q=c(d[12],p,h),r=c(d[12],q,g),s=c(d[12],r,f),t=c(d[26],2,s);return c(ai[7],0,t)}return c(T[24][10],f,e)}}],kC],kJ=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],i=d[1],j=b[1],k=a(h[4],l[9]),m=c(h[8],k,j),o=a(h[4],l[13]),p=c(h[8],o,i),q=a(h[18],am),r=a(h[4],q),f=c(h[8],r,g);return function(o){var
c=f?f[1]:0,b=a(C[9],c),d=b[9],e=b[8],g=b[7],h=b[4],i=b[3],j=b[2],k=b[1],l=[0,b[5],b[6]],n=a(C[6],p);return lp(C[10],m,n,j,k,h,i,l,g,e,d)}}}}return a(n[2],kI)}],kH];function
kK(b,a){return e(bY[1],a[1],[0,kL,b],a[2])}c(ak[80],kK,kJ);var
kM=0,kO=[0,function(b){return b?a(n[2],kN):function(a){return al[5]}},kM],kQ=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return al[6]}}}return a(n[2],kP)},kO];function
kR(b,a){return c(al[3],[0,kS,b],a)}c(ak[80],kR,kQ);var
kU=[5,[6,a(o[12],am)]],kV=a(h[18],am),kW=a(h[4],kV),kX=[0,[1,k[4],kW,kU],0],kY=[6,a(o[12],l[13])],kZ=a(h[4],l[13]),k1=[0,k0,[0,[1,k[4],kZ,kY],kX]],k2=[6,a(o[12],l[9])],k3=a(h[4],l[9]),k6=[0,[0,k5,[0,k4,[0,[1,k[4],k3,k2],k1]]],kT];function
k7(b,a){return e(bZ[1],[0,k8,b],0,a)}c(ak[80],k7,k6);var
k9=0,k$=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],i=d[1],k=b[1],m=a(h[6],f[1][1]),o=c(f[12][2][7],m,k),p=a(h[17],l[13]),q=a(h[6],p),r=c(f[12][2][7],q,i),s=a(h[17],l[13]),t=a(h[6],s),u=c(f[12][2][7],t,g);return function(c){var
b=a(j[17][93],u);return P(C[12],o,r,b[2],b[1])}}}}return a(n[2],k_)},k9],la=a(j[19][12],k$);e(f[6][9],0,[0,I,lb],la);function
lc(q){var
j=a(i[1][7],ld),b=l[13],g=0,h=0;if(0===b[0]){var
m=[0,lf,[0,[1,k[4],[0,[5,[0,b[1]]]],j],h]],n=a(i[1][7],lg),c=l[13];if(0===c[0]){var
o=[0,li,[0,[1,k[4],[2,[5,[0,c[1]]]],n],m]],p=a(i[1][7],lj),d=f[1][1];if(0===d[0])return e(f[9][4],[0,I,lm],0,[0,[0,ll,[0,[1,k[4],[5,[0,d[1]]],p],o]],g]);throw[0,H,lk]}throw[0,H,lh]}throw[0,H,le]}c(ar[19],lc,I);var
b2=[0,I,aM,a9,aN,bW,aj,bX,a_,a$,ba,b0,am,b1];bb(376,b2,"Newring_plugin.G_newring");bb(377,[0,C,b2],"Newring_plugin");return});
