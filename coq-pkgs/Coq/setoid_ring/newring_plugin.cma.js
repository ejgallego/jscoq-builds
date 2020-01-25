function(jA){"use strict";var
b8="closed",a5="x",ct=115,b7="ring_lookup",al=105,cs=",",J='"',a2="ring",k=246,cr="(",cq="constants",b6="preprocess tactic",aL="Field_tac",a1="_vendor+v8.11+32bit/coq/plugins/setoid_ring/newring.ml",cc="with carrier ",cp="postprocess tactic",aK="field",co="InitialRing",cb="decidable",A="[",cg='and morphisms "',cl="Pphi_pow",cm='Using setoid "',cn="tactic recognizing constants",ck="postprocess",a_="gen_phiZ",aM="setoid",b5="and equivalence relation ",b4=")",cf=103,ca=142,B="]",b2=":",b3="t",b$="preprocess",a3="power_tac",cj="Print",a9="power",b1="abstract",ci="Ring_polynom",aJ="sign",a8="protect_fv",b0="PEeval",a7="Ring",aI="div",b_='and "',a0="newring_plugin",cd="Add",ce="completeness",aZ="IDphi",b9="morphism",ch="field_lookup",a4=100,bZ="Pphi_dev",n=250,a6="Coq",G=jA.jsoo_runtime,D=G.caml_check_bound,c=G.caml_new_string,p=G.caml_obj_tag,aH=G.caml_register_global,z=G.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):G.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):G.caml_call_gen(a,[b,c])}function
f(a,b,c,d){return a.length==3?a(b,c,d):G.caml_call_gen(a,[b,c,d])}function
X(a,b,c,d,e){return a.length==4?a(b,c,d,e):G.caml_call_gen(a,[b,c,d,e])}function
H(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):G.caml_call_gen(a,[b,c,d,e,f])}function
jz(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):G.caml_call_gen(a,[b,c,d,e,f,g,h])}var
g=G.caml_get_global_data(),be=[0,c(a0),c("closed_term")],bk=[0,c(a0),c("get_res")],ap=c("setoid_ring"),ak=c(a0),Z=g.Constr,e=g.Util,Q=g.CClosure,ag=g.Termops,j=g.Names,E=g.Not_found,h=g.EConstr,bL=g.Tacmach,q=g.Proofview,v=g.Ltac_plugin__Tacinterp,P=g.Coqlib,L=g.Global,w=g.Stdlib,af=g.Ltac_plugin__Tacintern,bF=g.Lib,o=g.CamlinternalLazy,aT=g.UnivGen,ac=g.Ltac_plugin__Rewrite,x=g.Mod_subst,V=g.Ltac_plugin__Tacsubst,ab=g.Retyping,bt=g.Reductionops,d=g.Pp,Y=g.CErrors,C=g.Printer,aR=g.Assert_failure,$=g.Evd,ae=g.Feedback,ax=g.Typing,am=g.Option,_=g.Evarutil,bz=g.Smartlocate,an=g.Loc,O=g.CAst,bv=g.Flags,F=g.Ppconstr,t=g.Stdarg,l=g.Genarg,aS=g.Declare,bi=g.Constrintern,bc=g.Tactics,bf=g.Ltac_plugin__Tacenv,br=g.Summary,aw=g.Libobject,N=g.Vernacextend,aG=g.Attributes,aE=g.Ltac_plugin__Pptactic,aW=g.Ltac_plugin__Tacentries,I=g.Pcoq,i=g.CLexer,aF=g.Ltac_plugin__Pltac,bT=g.Ltac_plugin__Tacarg,cz=g.Esubst,cZ=g.Refiner,cR=g.Environ,cS=g.Goal,cL=g.Vars,cM=g.UState,cN=g.Typeops,cO=g.Univ,cJ=g.DAst,cF=g.Tacticals,cy=g.Globnames,fD=g.Redexpr,gv=g.Mltop;aH(249,[0,0,0,0],"Newring_plugin");aH(n,[0],"Newring_plugin__Newring_ast");var
gt=[0,0,0],gu=[0,0,0],gj=c("field kind"),gk=c(cn),gl=c(b6),gm=c(cp),gn=c(aM),go=c(a9),gp=c(aJ),gq=c(aI),gr=c("infinite property"),f$=[0,c(aL),0],ga=[0,0,0],gb=c("field_lemmas"),gc=c("_field_lemma1"),gd=c("_field_lemma2"),ge=c("_field_lemma3"),gf=c("_field_lemma4"),gg=c("_lemma5"),gi=[22,0],gh=[22,0],f_=c("core.eq.congr"),f9=c("field inverse should be declared as a morphism"),fW=c("arguments of field_simplify do not have all the same type"),fX=[0,c(aK)],fV=[0,c(aL),0],fY=c(J),fZ=c(J),f0=c("cannot find a declared field structure over"),f1=[0,c(aK)],f2=[0,c(a1),815,12],fS=c(b5),fT=c(cc),fR=c("The following field structures have been declared:"),fO=[0,1],fP=[0,0],fN=c("bad field structure"),eR=[0,0,0],eS=[0,0,0],eI=c("ring kind"),eJ=c(cn),eK=c(b6),eL=c(cp),eM=c(aM),eN=c(a9),eO=c(aJ),eP=c(aI),eH=c(" cannot be set twice"),eB=[0,c("Ring_base"),0],eC=c("ring_lemmas"),eD=c("_ring_lemma1"),eE=c("_ring_lemma2"),eG=[22,0],eF=[22,0],ez=[0,1],eA=[0,0],ey=c("bad ring structure"),eh=c("ring addition should be declared as a morphism"),ei=c("ring multiplication should be declared as a morphism"),ej=c("ring opposite should be declared as a morphism"),ek=c(J),el=c(b_),em=c(J),en=c(J),eo=c('",'),ep=c(cg),eq=c(J),er=c(cm),es=c(J),et=c(b_),eu=c(J),ev=c(cg),ew=c(J),ex=c(cm),ed=c("cannot find setoid relation"),d3=c("arguments of ring_simplify do not have all the same type"),d4=[0,c(a2)],d5=c(J),d6=c(J),d7=c("cannot find a declared ring structure over"),d8=[0,c(a2)],d9=[0,c(a1),359,12],d0=c(b5),d1=c(cc),dZ=c("The following ring structures have been declared:"),dh=[0,c(ci),0],dg=c("newring"),db=c(a7),da=c(a7),c8=c("ring: cannot find relation (not closed)"),c7=c("ring: cannot find relation"),cY=c(a5),cX=c(a5),cV=c(a5),cT=c("Ring.exec_tactic: anomaly"),cP=[0,1],cQ=[2,1],cI=c(b3),cK=c(b3),cE=[0,c(a1),ct,7],cB=c("not found"),cC=c("map "),cD=[0,c("lookup_map")],cx=c("dummy"),cv=c("global_head_of_constr."),c0=c("plugins.setoid_ring.Build_Setoid_Theory"),c2=c("core.option.None"),c3=c("core.option.Some"),c4=c("core.eq.type"),c5=c("core.list.cons"),c6=c("core.list.nil"),c9=c(a6),c_=[0,[0,c("Ring_theory"),0],[0,[0,c(ci),0],[0,[0,c("Ring_tac"),0],[0,[0,c(co),0],[0,[0,c(aL),0],[0,[0,c("Field_theory"),0],0]]]]]],dc=[0,c(a6),0],dd=c(co),di=c("almost_ring_theory"),dj=c("Eqsth"),dl=c("Eq_ext"),dn=c("Eq_s_ext"),dq=c("ring_theory"),dr=c("mk_reqe"),dt=c("semi_ring_theory"),du=c("mk_seqe"),dw=c("Abstract"),dx=c("Computational"),dz=c("Morphism"),dB=c("inv_morph_nothing"),dC=c("mkhypo"),dE=c("hypo"),dH=c(b0),dK=c(cl),dN=c(bZ),dQ=c(a_),dT=c(aZ),dX=c(a2),dY=c("ring-tac-carrier-table"),ea=c("tactic-new-ring-theory"),eT=[0,c(a6),0],eU=c(aL),eZ=c("FEeval"),e2=c(b0),e5=c(cl),e8=c(bZ),e$=c("display_pow_linear"),fc=c("display_linear"),ff=c(a_),fi=c(aZ),fm=c(aK),fp=c("PCond"),fs=c(a_),fv=c(aZ),fz=c("field_cond"),fA=c(aK),fC=c("simpl_field_expr"),fE=c("almost_field_theory"),fF=c("field_theory"),fG=c("semi_field_theory"),fH=c("AF_AR"),fJ=c("F_R"),fL=c("SF_SR"),fQ=c("field-tac-carrier-table"),f5=c("tactic-new-field-theory"),iS=c(ce),g_=[0,0],gG=c(b1),gH=c(cb),gI=c(b9),gJ=c(B),gK=c(A),gL=c(cq),gM=c(B),gN=c(A),gO=c(b8),gP=c(B),gQ=c(A),gR=c(b$),gS=c(B),gT=c(A),gU=c(ck),gV=c(aM),gW=c(B),gX=c(A),gY=c(a3),gZ=c(B),g0=c(A),g1=c(a3),g2=c(aJ),g3=c(aI),gy=c(a8),gB=c("in"),gD=c(a8),gF=c(a8),g7=c(cb),g$=c(b1),hd=c(b9),hg=c(B),hj=c(A),hl=c(cq),ho=c(B),hr=c(A),ht=c(b8),hw=c(B),hz=c(A),hB=c(b$),hE=c(B),hH=c(A),hJ=c(ck),hO=c(aM),hS=c(aJ),hV=c(B),hY=c(A),h1=c(a9),h4=c(B),h7=c(A),h_=c(a3),ic=c(aI),ig=c("ring_mod"),ik=c(b4),im=c(cs),ip=c(cr),is=c("ring_mods"),iw=[0,c(cj),[0,c("Rings"),0]],iB=c(b2),iD=c(a7),iE=c(cd),iI=c("AddSetoidRing"),iL=c(B),iN=c(A),iP=c(b7),iR=c(b7),iX=c(ce),i0=c("field_mod"),i4=c(b4),i6=c(cs),i8=c(cr),i$=c("field_mods"),jd=[0,c(cj),[0,c("Fields"),0]],ji=c(b2),jk=c("Field"),jl=c(cd),jp=c("AddSetoidField"),js=c(B),ju=c(A),jw=c(ch),jy=c(ch);function
K(b){var
c=a(d[3],b);return f(Y[5],0,0,c)}function
cu(c,e){var
g=b(h[92],c,e)[1];try{var
j=b(ag[99],c,g)[1];return j}catch(b){b=z(b);if(b===E){var
i=a(d[3],cv);return f(Y[2],0,0,i)}throw b}}function
cw(b){try{var
c=a(cy[15],b);return c}catch(b){b=z(b);if(b===E)return[0,a(j[1][6],cx)];throw b}}function
a$(h,g,f){var
i=a(Z[71],f),j=i[2],c=i[1],k=a(h,cw(c));if(k){var
l=k[1],d=function(d,c){switch(a(l,d)){case
0:var
e=a(cz[1],g);return b(Q[43],e,c);case
1:return a(Q[22],c);default:return a$(h,g,c)}};if(a(e[24][35],j))return d(-1,c);var
m=b(e[24][16],d,j),n=[5,d(-1,c),m];return a(Q[23],n)}return a(Q[22],f)}function
ba(b,a){try{var
c=[0,f(e[22][ct],j[69][1],a,b)];return c}catch(a){a=z(a);if(a===E)return 0;throw a}}var
aN=[0,e[20][52][1]];function
aO(c,b){var
d=a(e[3],aN);aN[1]=f(e[20][52][4],c,b,d);return 0}function
cA(g){try{var
c=a(e[3],aN),m=b(e[20][52][23],g,c);return m}catch(c){c=z(c);if(c===E){var
h=a(d[3],cB),i=a(d[20],g),j=a(d[3],cC),k=b(d[12],j,i),l=b(d[12],k,h);return f(Y[5],0,cD,l)}throw c}}function
aP(j,i,g,c){function
k(a){return b(_[12],g,a)}var
l=a(h[ca][1],c),m=a(Q[31],0),n=f(Q[29],[0,k],Q[4],i),o=b(cA(j),g,c);function
d(g,h){var
c=a(Z[30],h);if(6===c[0]){var
j=c[3],k=c[2],l=c[1],p=d(b(e[4],g,1),j),q=[0,l,d(g,k),p];return a(Z[13],q)}var
i=a$(o,g,h);return f(Q[47],n,m,i)}var
p=d(0,l);return a(h[9],p)}function
bb(a){var
b=0,c=2,d=[0,function(b,c,d){return aP(a,b,c,d)},c];return f(bc[50],0,d,b)}function
bd(b,a){var
c=[0,[0,a,0]],d=2,e=[0,function(a,c,d){return aP(b,a,c,d)},d];return f(bc[50],0,e,c)}function
aQ(a,f,c){var
g=c;for(;;)try{var
m=b(ag[99],a,g)[1],n=b(j[69][4][3],m,f);return n}catch(c){c=z(c);if(c===E){var
d=b(h[3],a,g);switch(d[0]){case
5:var
g=d[1];continue;case
9:var
k=d[2],i=aQ(a,f,d[1]);if(i){var
l=function(b){return aQ(a,f,b)};return b(e[24][21],l,k)}return i;default:return 0}}throw c}}var
cG=[0,function(c,s){if(c){var
g=c[2];if(g)if(!g[2]){var
h=g[1],i=a(v[2][2],c[1]),k=a(am[7],i),m=a(v[2][5],h),n=a(am[7],m),o=function(c){var
d=a(l[6],t[9]);return b(v[2][7],d,c)},p=b(e[22][68],o,n),r=function(c){if(aQ(c,f(e[22][16],j[69][4][4],p,j[69][4][1]),k))return a(q[16],0);var
g=a(d[7],0);return b(cF[57][4],0,g)};return b(q[72][1],q[54],r)}}throw[0,aR,cE]}];f(bf[16],0,be,cG);var
cH=[0,be,0];function
bg(c){function
d(a){return[0,b(an[12],0,a)]}var
f=b(e[22][68],d,c),g=a(l[18],t[9]),h=a(l[5],g),i=[0,[0,b(l[7],h,f)],0],k=[1,a(j[1][6],cI)],m=[0,b(cJ[3],0,k),0],n=a(l[5],t[11]),o=[0,cH,[0,[0,b(l[7],n,m)],i]],p=[31,b(O[1],0,o)];return[28,[0,[0,[0,a(j[1][6],cK)],0],p]]}function
bh(d){var
b=a(L[2],0),e=a($[17],b),c=X(bi[10],b,e,0,d),f=c[1];return[0,a($[18],c[2]),f]}function
M(c){var
b=a(L[2],0),d=a($[17],b);return X(bi[10],b,d,0,c)[1]}function
aa(e,d,c){var
g=a(cL[27],c),h=a(L[5],0),i=f(cM[22],h,d,g);b(aS[1],0,i);var
k=a(L[2],0),l=b(cN[1],k,c)[2],m=[0,jz(aS[3],0,cP,0,[0,l],[0,[0,cO[39][1]]],0,c)],n=a(j[1][6],e),o=X(aS[6],0,n,cQ,m);return a(Z[18],o)}var
bj=[0,[0]],cU=[0,bk,0],cW=[0,function(d,c){var
f=a(e[22][5],d),g=a(l[6],t[3]),h=b(v[2][7],g,f);function
i(d){var
e=c[1],f=a(w[22],d),g=b(w[17],cV,f),h=a(j[1][6],g);return b(j[1][11][23],h,e)}bj[1]=b(e[24][2],h,i);return a(q[16],0)}];f(bf[16],0,bk,cW);function
bl(F,E,i,c,D){function
G(g,c){var
d=c[1],h=c[3],i=c[2],k=a(w[22],d),l=b(w[17],cX,k),e=a(j[1][6],l),m=[2,[1,b(O[1],0,e)]],n=a(v[2][1],g);return[0,d+1|0,[0,m,i],f(j[1][11][4],e,n,h)]}var
m=f(e[22][16],G,D,[0,0,0,j[1][11][1]]),H=m[3],I=m[2],r=a(v[31],0),J=[0,H,r[2],r[3]];function
K(c){var
d=a(w[22],c),e=b(w[17],cY,d);return a(j[1][6],e)}var
L=b(e[22][56],i,K),M=a(l[5],t[3]),N=[0,cU,[0,[0,b(l[7],M,i)],0]],P=[31,b(O[1],0,N)];function
Q(a){return[0,a]}var
R=[5,[28,[0,b(e[22][68],Q,L),P]]],A=h[16],B=a(cR[14],F),g=f(cS[3][5],E,B,A),C=[0,g[1],g[3]],S=b(e[23],I,[0,R,0]),d=p(c),u=n===d?c[1]:k===d?a(o[2],c):c,x=[0,[0,b(an[12],0,u)],S],y=[3,b(O[1],0,x)],z=[29,b(O[1],0,y)],T=b(v[23],J,z),U=b(q[71][7],T,C),V=a(cZ[2],U),s=a($[167],V);function
W(c){var
b=a(v[2][2],c);return b?f(h[5],0,s,b[1]):a(w[3],cT)}var
X=a($[153],s),Y=a(e[3],bj);return[0,b(e[24][15],W,Y),X]}function
bm(b){return[k,function(e){var
c=a(P[2],b),d=a(aT[15],c);return a(h[9],d)}]}function
ao(b){return[k,function(c){return a(P[2],b)}]}var
c1=bm(c0),aU=ao(c2),aV=ao(c3),R=bm(c4),ah=ao(c5),ai=ao(c6);function
aj(b,d){var
c=p(b),e=n===c?b[1]:k===c?a(o[2],b):b;return a(h[23],[0,e,d])}function
r(d,c,i){var
f=p(c),j=n===f?c[1]:k===f?a(o[2],c):c,l=a(e[3],d),g=b(_[9],l,j),m=g[2];d[1]=g[1];return a(h[23],[0,m,i])}function
bn(g,l){var
d=b(h[3],g,l);if(9===d[0]){var
c=d[2],m=d[1];if(2<=c.length-1){var
n=b(e[5],c.length-1,2),o=[0,m,f(e[24][7],c,0,n)],i=a(h[23],o);if(b(h[121][16],g,i)){var
j=b(e[5],c.length-1,1),p=D(c,j)[1+j],k=b(e[5],c.length-1,2);return[0,i,D(c,k)[1+k],p]}return K(c8)}}return K(c7)}var
aq=[0,c9,[0,ap,0]];function
c$(a){return b(e[23],aq,a)}var
bo=b(e[22][68],c$,c_);function
S(b){return[k,function(e){var
c=f(P[18],da,bo,b),d=a(aT[15],c);return a(h[9],d)}]}function
m(a){return[k,function(b){return f(P[18],db,bo,a)}]}var
de=b(e[22][68],j[1][6],[0,dd,[0,ap,dc]]),df=a(j[5][4],de);function
bp(c){return[k,function(e){var
d=a(j[8][4],c);return b(j[15][1],[0,df],d)}]}function
ad(a){var
b=[0,ap,dh];return[k,function(c){return f(P[17],dg,b,a)}]}var
ar=S(di),dk=m(dj),dm=m(dl),dp=m(dn),as=S(dq),ds=S(dr),at=S(dt),dv=S(du),au=S(dw),dy=S(dx),dA=S(dz),T=bp(dB),dD=m(dC),u=m(dE);function
bq(i,d,g){var
c=g;for(;;){var
f=b(h[3],d,c);if(6===f[0]){var
c=f[3];continue}var
j=bn(d,c)[1],l=function(c){var
b=c[1],e=c[2],d=p(b),f=n===d?b[1]:k===d?a(o[2],b):b;return[0,f,e]},m=b(e[22][68],l,i),q=function(a){return-1===a?1:2},r=[0,[0,cu(d,j),q],m];return function(a){return ba(r,a)}}}var
dF=0;function
dG(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
dI=[0,[0,ad(dH),dG],dF];function
dJ(b){var
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
dL=[0,[0,ad(dK),dJ],dI];function
dM(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
dO=[0,[0,ad(dN),dM],dL];function
dP(a){return 0}var
dR=[0,[0,m(dQ),dP],dO];function
dS(a){return 0}var
dU=[0,[0,m(dT),dS],dR],dV=[0,[0,ai,function(a){return-1===a?0:1}],dU],dW=[0,[0,ah,function(a){return-1===a?0:2===a?2:1}],dV];aO(dX,function(a,b){return bq(dW,a,b)});var
U=a(e[26][1],[0,Z[87]]),av=f(br[4],0,dY,U[1]);function
bs(i){var
c=a(d[22],dZ);b(ae[7],0,c);var
g=a(e[3],av);function
h(v,c){var
e=a(L[2],0),g=a($[17],e),h=f(C[6],e,g,c[3]),i=a(d[3],d0),j=a(d[13],0),k=f(C[6],e,g,c[2]),l=a(d[3],d1),m=a(d[13],0),n=a(F[6],c[1]),o=b(d[12],n,m),p=b(d[12],o,l),q=b(d[12],p,k),r=b(d[12],q,j),s=b(d[12],r,i),t=b(d[12],s,h),u=b(d[26],2,t);return b(ae[7],0,u)}return b(U[11],h,g)}function
d2(c){var
d=a(e[3],av);return b(U[23],c,d)}function
d_(d){var
a=d[2],c=d[1],e=b(x[46],c,a[2]),f=b(x[46],c,a[3]),g=b(x[46],c,a[4]),h=b(x[46],c,a[5]),i=b(x[46],c,a[6]),j=b(x[46],c,a[7]),k=b(x[46],c,a[10]),l=b(x[46],c,a[11]),m=b(V[1],c,a[8]),n=b(V[1],c,a[9]),o=b(V[1],c,a[12]),p=b(V[1],c,a[13]);if(e===a[2])if(f===a[3])if(b(Z[81],g,a[4]))if(h===a[5])if(i===a[6])if(j===a[7])if(k===a[10])if(l===a[11])if(m===a[8])if(n===a[9])if(o===a[12])if(p===a[13])return a;return[0,a[1],e,f,g,h,i,j,m,n,k,l,o,p]}function
d$(d){var
b=d[2],c=a(e[3],av);av[1]=f(U[4],b[2],b,c);return 0}var
eb=f(aw[16],ea,d$,[0,d_]),ec=a(aw[4],eb);function
bu(d,f,c,b){try{var
j=a(e[3],f),g=X(ac[13],d,j,c,b),k=g[2],h=X(ac[14],d,g[1],c,b),l=h[2],i=X(ac[15],d,h[1],c,b),m=i[2];f[1]=i[1];var
n=aj(c1,[0,c,b,k,l,m]);return n}catch(a){a=z(a);if(a===E)return K(ed);throw a}}function
ee(h,g,f,e,d,c,b,a){return aj(ds,[0,h,g,f,e,d,c,b,a])}function
ef(f,e,d,c,b,a){return aj(dv,[0,f,e,d,c,b,a])}function
eg(j,c,l){var
g=l[5],s=l[4],m=l[3],q=l[2],i=l[1],J=a(e[3],c),v=b(h[3],J,g);if(9===v[0])if(1===v[2].length-1){var
a3=v[1],G=p(R),a4=n===G?R[1]:k===G?a(o[2],R):R,a5=a(e[3],c);if(f(h[al],a5,a3,a4)){var
a6=r(c,dk,[0,i]),a7=s?r(c,dm,[0,i,q,m,s[1]]):r(c,dp,[0,i,q,m]),a8=a(e[3],c),H=f(ax[6],j,a8,a6),a9=H[2],I=f(ax[6],j,H[1],a7),a_=I[2];c[1]=I[1];return[0,a9,a_]}}var
w=[0,[0,[0,[0,i,[0,g]]],[0,[0,[0,i,[0,g]]],0]],[0,[0,i,[0,g]]]],M=bu(a(L[2],0),c,i,g);try{var
a2=b(ac[16],w,q),x=a2}catch(a){a=z(a);if(a!==E)throw a;var
x=K(eh)}var
t=x[2];try{var
a1=b(ac[16],w,m),y=a1}catch(a){a=z(a);if(a!==E)throw a;var
y=K(ei)}var
u=y[2];if(s){var
A=s[1];try{var
aA=b(ac[16],[0,[0,[0,[0,i,[0,g]]],0],[0,[0,i,[0,g]]]],A),B=aA}catch(a){a=z(a);if(a!==E)throw a;var
B=K(ej)}var
D=B[2],N=ee(i,q,m,A,g,t,u,D),O=a(d[3],ek),P=a(e[3],c),Q=f(C[11],j,P,D),S=a(d[3],el),T=a(d[13],0),U=a(d[3],em),V=a(e[3],c),W=f(C[11],j,V,u),X=a(d[3],en),Y=a(d[13],0),Z=a(d[3],eo),_=a(e[3],c),$=f(C[11],j,_,t),aa=a(d[3],ep),ab=a(d[13],0),ad=a(d[3],eq),af=a(e[3],c),ag=f(C[11],j,af,g),ah=a(d[3],er),ai=b(d[12],ah,ag),aj=b(d[12],ai,ad),ak=b(d[12],aj,ab),am=b(d[12],ak,aa),an=b(d[12],am,$),ao=b(d[12],an,Z),ap=b(d[12],ao,Y),aq=b(d[12],ap,X),ar=b(d[12],aq,W),as=b(d[12],ar,U),at=b(d[12],as,T),au=b(d[12],at,S),av=b(d[12],au,Q),aw=b(d[12],av,O),ay=ae[6],az=function(a){return b(ay,0,a)};b(bv[21],az,aw);var
F=N}else{var
aB=a(d[3],es),aC=a(e[3],c),aD=f(C[11],j,aC,u),aE=a(d[3],et),aF=a(d[13],0),aG=a(d[3],eu),aH=a(e[3],c),aI=f(C[11],j,aH,t),aJ=a(d[3],ev),aK=a(d[13],0),aL=a(d[3],ew),aM=a(e[3],c),aN=f(C[11],j,aM,g),aO=a(d[3],ex),aP=b(d[12],aO,aN),aQ=b(d[12],aP,aL),aR=b(d[12],aQ,aK),aS=b(d[12],aR,aJ),aT=b(d[12],aS,aI),aU=b(d[12],aT,aG),aV=b(d[12],aU,aF),aW=b(d[12],aV,aE),aX=b(d[12],aW,aD),aY=b(d[12],aX,aB),aZ=ae[6],a0=function(a){return b(aZ,0,a)};b(bv[21],a0,aY);var
F=ef(i,q,m,g,t,u)}return[0,M,F]}function
bw(h,g,f,e,d,c,b,a){return a?a[1]:eg(h,g,[0,f,e,d,c,b])}function
bx(b){if(typeof
b==="number"){var
c=p(au);return n===c?au[1]:k===c?a(o[2],au):au}else
return 0===b[0]?aj(dy,[0,b[1]]):aj(dA,[0,b[1]])}function
by(u,t,s,r,q,d){if(d){var
c=d[1];if(0===c[0])return a(af[2],c[1]);var
g=c[1],h=bz[3],i=function(a){return b(h,0,a)};return bg(b(e[22][68],i,g))}var
f=p(T),j=n===f?T[1]:k===f?a(o[2],T):T,l=[0,[0,b(an[12],0,j)],0],m=[3,b(O[1],0,l)];return[29,b(O[1],0,m)]}function
ay(d,c,b){var
f=a(e[3],c);return r(c,dD,[0,H(ab[2],0,0,d,f,b),b])}function
bA(d,c,m){var
g=p(u),q=n===g?u[1]:k===g?a(o[2],u):u,s=a(e[3],c),i=b(_[9],s,q),j=i[2];c[1]=i[1];var
t=r(c,ai,[0,j]);function
v(b,a){return r(c,ah,[0,j,ay(d,c,b),a])}var
w=f(e[22][16],v,m,t),x=a(e[3],c),l=f(ax[6],d,x,w),y=l[2];c[1]=l[1];var
z=a(h[ca][1],y),A=a(e[3],c);return b(_[41],A,z)}function
bB(q,c,f){var
g=p(u),s=n===g?u[1]:k===g?a(o[2],u):u,t=a(e[3],c),h=b(_[9],t,s),i=h[2];c[1]=h[1];if(f){var
j=f[1],d=j[1],v=j[2];if(0===d[0])var
l=a(af[2],d[1]);else
var
w=d[1],x=bz[3],y=function(a){return b(x,0,a)},l=bg(b(e[22][68],y,w));return[0,l,r(c,aV,[0,i,ay(q,c,M(v))])]}var
m=p(T),z=n===m?T[1]:k===m?a(o[2],T):T,A=[0,b(an[12],0,z)],B=r(c,aU,[0,i]),C=[3,b(O[1],0,[0,A,0])];return[0,[29,b(O[1],0,C)],B]}function
bC(i,c,d){var
f=p(u),j=n===f?u[1]:k===f?a(o[2],u):u,l=a(e[3],c),g=b(_[9],l,j),h=g[2];c[1]=g[1];return d?r(c,aV,[0,h,ay(i,c,M(d[1]))]):r(c,aU,[0,h])}function
bD(i,c,d){var
f=p(u),j=n===f?u[1]:k===f?a(o[2],u):u,l=a(e[3],c),g=b(_[9],l,j),h=g[2];c[1]=g[1];return d?r(c,aV,[0,h,ay(i,c,M(d[1]))]):r(c,aU,[0,h])}function
bE(r,N,aw,M,av,J,au,ap,ao){var
O=J[2],Q=J[1],R=N[2],g=N[1],ax=b(e[23],aq,eB);a(P[12],ax);var
l=a(L[2],0),ac=H(ab[2],0,0,l,g,R),s=b(h[3],g,ac);if(9===s[0]){var
c=s[2],u=c.length-1-6|0;if(2<u>>>0)var
i=0;else{var
t=s[1];switch(u){case
0:var
ad=c[1],ae=c[2],ag=c[3],ah=c[4],ai=c[5],aj=c[6],v=p(at),ak=n===v?at[1]:k===v?a(o[2],at):at;if(f(h[al],g,t,ak))var
d=[0,ez,ad,ae,ag,ah,ai,0,0,aj],i=1;else
var
i=0;break;case
1:var
i=0;break;default:var
x=c[1],y=c[2],z=c[3],A=c[4],B=c[5],C=c[6],E=c[7],F=c[8],G=p(ar),am=n===G?ar[1]:k===G?a(o[2],ar):ar;if(f(h[al],g,t,am))var
d=[0,0,x,y,z,A,B,[0,C],[0,E],F],i=1;else{var
I=p(as),an=n===I?as[1]:k===I?a(o[2],as):as;if(f(h[al],g,t,an))var
d=[0,eA,x,y,z,A,B,[0,C],[0,E],F],i=1;else
var
i=0}}}}else
var
i=0;if(!i)var
d=K(ey);var
S=d[9],T=d[8],U=d[6],V=d[5],W=d[2],m=[0,g],ay=d[4],az=d[3],aA=d[1],X=bw(l,m,W,V,U,T,S,aw),Y=X[1],aB=X[2],Z=bB(l,m,au),aC=Z[2],aD=Z[1],aE=bC(l,m,ap),aF=bD(l,m,ao),aG=[0,Y,[0,aB,[0,R,[0,aC,[0,aE,[0,aF,[0,bx(M),0]]]]]]],aH=bp(eC),_=bl(l,a(e[3],m),5,aH,aG),$=_[2],q=_[1],aI=D(q,3)[4],aJ=D(q,4)[5],aK=a(j[1][8],r),aL=aa(b(w[17],aK,eD),$,aI),aM=a(j[1][8],r),aN=aa(b(w[17],aM,eE),$,aJ),aO=by(l,g,M,aA,[0,az,ay,V,U,T],av),aP=Q?a(af[2],Q[1]):eG,aQ=O?a(af[2],O[1]):eF,aR=f(h[5],0,g,W),aS=f(h[5],0,g,S),aT=f(h[5],0,g,Y),aU=D(q,0)[1],aV=D(q,2)[3],aW=a(ec,[0,r,aR,aS,aT,D(q,1)[2],aV,aU,aO,aD,aL,aN,aP,aQ]);b(bF[10],r,aW);return 0}function
bG(a){return typeof
a==="number"?0:0===a[0]?[0,M(a[1])]:[1,M(a[1])]}function
s(f,c,d){var
g=a(e[3],c);return a(am[3],g)?(c[1]=[0,d],0):K(b(w[17],f,eH))}function
bH(y,x,w){var
m=bh(x),c=[0,0],d=[0,0],f=[0,0],g=[0,0],h=[0,0],i=[0,0],j=[0,0],k=[0,0],z=m[2],A=m[1];function
n(a){switch(a[0]){case
0:return s(eI,c,bG(a[1]));case
1:return s(eJ,f,a[1]);case
2:return s(eK,g,a[1]);case
3:return s(eL,h,a[1]);case
4:var
b=a[1],e=M(a[2]);return s(eM,d,[0,M(b),e]);case
5:return s(eN,j,[0,a[1],a[2]]);case
6:return s(eO,i,a[1]);default:return s(eP,k,a[1])}}b(e[22][11],n,w);var
l=a(e[3],c),o=l?l[1]:0,p=a(e[3],k),q=a(e[3],i),r=a(e[3],j),t=a(e[3],h),u=a(e[3],g),v=a(e[3],f);return bE(y,[0,A,z],a(e[3],d),o,v,[0,u,t],r,q,p)}function
bI(d,a,c){if(a)return a;var
b=bn(d,c);return[0,b[2],[0,b[3],0]]}function
bJ(h,b,c,g){var
i=r(b,ai,[0,c]);function
j(d,a){return r(b,ah,[0,c,d,a])}var
k=f(e[22][16],j,g,i),l=a(e[3],b),d=f(ax[6],h,l,k),m=d[2];b[1]=d[1];return m}function
y(b){var
c=a(h[9],b);return a(v[2][1],c)}function
W(c){var
d=a(v[31],0);return b(v[2][6],d,c)}function
eQ(a){var
b=y(a[3]),c=y(a[4]),d=y(a[5]),e=y(a[6]),f=y(a[7]),g=W(a[8]),h=W(a[9]),i=y(a[10]),j=y(a[11]),k=W([28,[0,eR,a[12]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,W([28,[0,eS,a[13]]]),0]]]]]]]]]]]}function
bK(J,I,G,F){function
c(m){var
c=a(bL[32][3],m),g=a(q[67][3],m);try{var
i=bI(c,G,F),k=[0,c];if(i){var
n=i[2],j=H(ab[2],0,0,g,c,i[1]),o=function(e){var
h=H(ab[2],0,0,g,c,e),b=1-H(bt[81],0,g,c,j,h);if(b){var
i=a(d[3],d3);return f(Y[5],0,d4,i)}return b};b(e[22][11],o,n);try{var
D=d2(f(h[5],0,c,j)),l=D}catch(e){e=z(e);if(e!==E)throw e;var
p=a(d[3],d5),r=f(C[11],g,c,j),s=a(d[3],d6),t=a(d[13],0),u=a(d[3],d7),w=b(d[12],u,t),x=b(d[12],w,s),A=b(d[12],x,r),B=b(d[12],A,p),l=f(Y[5],0,d8,B)}var
K=bJ(g,k,a(h[9],l[2]),i),L=a(v[2][1],K),M=y(bA(g,k,I)),N=eQ(l),O=b(e[23],N,[0,M,[0,L,0]]),P=b(v[2][8],J,O),Q=a(e[3],k),R=a(q[65][1],Q),S=b(q[18],R,P);return S}throw[0,aR,d9]}catch(c){c=z(c);if(a(q[71][9],c))return b(q[21],0,c);throw c}}return a(q[67][7],c)}var
eV=b(e[22][68],j[1][6],[0,eU,[0,ap,eT]]),eW=a(j[5][4],eV),eX=0;function
eY(b){var
a=b+1|0;if(!(16<a>>>0))switch(a){case
11:case
15:return 2;case
0:case
13:case
16:return 0}return 1}var
e0=[0,[0,m(eZ),eY],eX];function
e1(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
e3=[0,[0,ad(e2),e1],e0];function
e4(b){var
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
e6=[0,[0,ad(e5),e4],e3];function
e7(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
e9=[0,[0,ad(e8),e7],e6];function
e_(b){var
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
fa=[0,[0,m(e$),e_],e9];function
fb(a){if(12<=a)if(15<=a)var
b=17<=a?0:1;else{if(13!==a)return 2;var
b=1}else
var
b=-1===a?1:9<=a?1:0;return b?0:1}var
fd=[0,[0,m(fc),fb],fa];function
fe(a){return 0}var
fg=[0,[0,m(ff),fe],fd];function
fh(a){return 0}var
fj=[0,[0,m(fi),fh],fg],fk=[0,[0,ai,function(a){return-1===a?0:1}],fj],fl=[0,[0,ah,function(a){return-1===a?0:2===a?2:1}],fk];aO(fm,function(a,b){return bq(fl,a,b)});var
fn=0;function
fo(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
10:case
14:return 2;case
0:case
12:case
15:return 0}return 1}var
fq=[0,[0,m(fp),fo],fn];function
fr(a){return 0}var
ft=[0,[0,m(fs),fr],fq];function
fu(a){return 0}var
fw=[0,[0,m(fv),fu],ft],fx=[0,[0,ai,function(a){return-1===a?0:1}],fw],fy=[0,[0,ah,function(a){return-1===a?0:2===a?2:1}],fx];aO(fz,function(f,g){function
c(c){var
b=c[1],e=c[2],d=p(b),f=n===d?b[1]:k===d?a(o[2],b):b;return[0,f,e]}var
d=b(e[22][68],c,fy);return function(a){return ba(d,a)}});function
fB(a,b,c){return aP(fA,a,b,c)}b(fD[3],fC,fB);var
az=m(fE),aA=m(fF),aB=m(fG),fI=m(fH),fK=m(fJ),fM=m(fL),aC=f(br[4],0,fQ,U[1]);function
bM(i){var
c=a(d[22],fR);b(ae[7],0,c);var
g=a(e[3],aC);function
h(w,c){var
e=a(L[2],0),g=a($[17],e),h=f(C[6],e,g,c[3]),i=a(d[3],fS),k=a(d[13],0),l=f(C[6],e,g,c[2]),m=a(d[3],fT),n=a(d[13],0),o=a(j[1][9],c[1]),p=b(d[12],o,n),q=b(d[12],p,m),r=b(d[12],q,l),s=b(d[12],r,k),t=b(d[12],s,i),u=b(d[12],t,h),v=b(d[26],2,u);return b(ae[7],0,v)}return b(U[11],h,g)}function
fU(c){var
d=a(e[3],aC);return b(U[23],c,d)}function
f3(d){var
a=d[2],c=d[1],e=b(x[46],c,a[2]),f=b(x[46],c,a[3]),g=b(x[46],c,a[6]),h=b(x[46],c,a[7]),i=b(x[46],c,a[8]),j=b(x[46],c,a[9]),k=b(x[46],c,a[10]),l=b(V[1],c,a[4]),m=b(V[1],c,a[5]),n=b(V[1],c,a[11]),o=b(V[1],c,a[12]);if(e===a[2])if(f===a[3])if(g===a[6])if(h===a[7])if(i===a[8])if(j===a[9])if(k===a[10])if(l===a[4])if(m===a[5])if(n===a[11])if(o===a[12])return a;return[0,a[1],e,f,l,m,g,h,i,j,k,n,o]}function
f4(d){var
b=d[2],c=a(e[3],aC);aC[1]=f(U[4],b[2],b,c);return 0}var
f6=f(aw[16],f5,f4,[0,f3]),f7=a(aw[4],f6);function
f8(g,c,j,d){var
q=a(e[3],g),i=b(h[3],q,d);if(9===i[0])if(1===i[2].length-1){var
t=i[1],m=p(R),u=n===m?R[1]:k===m?a(o[2],R):R,v=a(e[3],g);if(f(h[al],v,t,u)){var
w=a(P[2],f_),x=a(aT[15],w),y=[0,a(h[9],x),[0,c,c,j]];return a(h[23],y)}}bu(a(L[2],0),g,c,d);var
r=[0,[0,[0,[0,c,[0,d]]],0],[0,[0,c,[0,d]]]];try{var
s=b(ac[16],r,j),l=s}catch(a){a=z(a);if(a!==E)throw a;var
l=K(f9)}return l[2]}function
bN(i,bA,bz){var
as=[0,0],at=[0,0],au=[0,0],av=[0,0],aw=[0,0],ax=[0,0],ay=[0,0],aC=[0,0],aD=[0,0];function
bu(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return s(gj,as,bG(a[1]));case
1:return s(gk,au,a[1]);case
2:return s(gl,av,a[1]);case
3:return s(gm,aw,a[1]);case
4:var
c=a[1],d=M(a[2]);return s(gn,at,[0,M(c),d]);case
5:return s(go,aC,[0,a[1],a[2]]);case
6:return s(gp,ay,a[1]);default:return s(gq,aD,a[1])}}return s(gr,ax,M(b[1]))}b(e[22][11],bu,bz);var
aE=a(e[3],as),R=aE?aE[1]:0,aF=a(e[3],aD),aG=a(e[3],ay),aH=a(e[3],aC),aI=a(e[3],aw),aJ=a(e[3],av),aK=a(e[3],au),aL=a(e[3],ax),bv=a(e[3],at),aW=b(e[23],aq,f$);a(P[12],aW);var
ah=bh(bA),t=ah[2],u=ah[1],m=a(L[2],0),d=[0,u],aN=a(e[3],d),aO=H(ab[2],0,0,m,aN,t),aP=a(e[3],d),J=b(h[3],aP,aO);if(9===J[0]){var
c=J[2],S=c.length-1-8|0;if(2<S>>>0)var
l=0;else{var
N=J[1];switch(S){case
0:var
T=c[1],U=c[2],V=c[3],W=c[4],X=c[5],Y=c[6],_=c[7],$=c[8],ac=p(aB),aQ=n===ac?aB[1]:k===ac?a(o[2],aB):aB,aR=a(e[3],d);if(f(ag[a4],aR,aQ,N))var
g=[0,fO,T,U,V,W,X,0,0,Y,_,$,r(d,fM,[0,T,U,V,W,X,Y,_,$,t])],l=1;else
var
l=0;break;case
1:var
l=0;break;default:var
x=c[1],y=c[2],z=c[3],A=c[4],B=c[5],C=c[6],E=c[7],F=c[8],G=c[9],I=c[10],ad=p(az),aS=n===ad?az[1]:k===ad?a(o[2],az):az,aT=a(e[3],d);if(f(ag[a4],aT,aS,N))var
g=[0,0,x,y,z,A,B,[0,C],[0,E],F,G,I,r(d,fI,[0,x,y,z,A,B,C,E,F,G,I,t])],l=1;else{var
ae=p(aA),aU=n===ae?aA[1]:k===ae?a(o[2],aA):aA,aV=a(e[3],d);if(f(ag[a4],aV,aU,N))var
g=[0,fP,x,y,z,A,B,[0,C],[0,E],F,G,I,r(d,fK,[0,x,y,z,A,B,C,E,F,G,I,t])],l=1;else
var
l=0}}}}else
var
l=0;if(!l)var
g=K(fN);var
O=g[11],ai=g[8],aj=g[6],ak=g[5],Q=g[2],aX=g[12],aY=g[10],aZ=g[4],a0=g[3],a1=g[1],al=bw(m,d,Q,ak,aj,ai,O,bv),am=al[2],an=al[1];bE(i,[0,a(e[3],d),aX],[0,[0,an,am]],R,aK,ga,aH,aG,aF);var
ao=bB(m,d,aH),a2=ao[2],a3=ao[1],a5=bC(m,d,aG),a6=bD(m,d,aF),a7=f8(d,Q,aY,O),a8=[0,an,[0,am,[0,a7,[0,t,[0,a2,[0,a5,[0,a6,[0,bx(R),0]]]]]]]],aM=[k,function(d){var
c=a(j[8][4],gb);return b(j[15][1],[0,eW],c)}],ap=bl(m,a(e[3],d),9,aM,a8),v=ap[2],q=ap[1],a9=D(q,3)[4],a_=D(q,4)[5],a$=D(q,5)[6],ba=D(q,6)[7];if(aL)var
bb=[0,f(h[5],0,u,aL[1])],bc=[0,D(q,8)[9],bb],ar=a(Z[16],bc);else
var
ar=D(q,7)[8];var
bd=a(j[1][8],i),be=aa(b(w[17],bd,gc),v,a9),bf=a(j[1][8],i),bg=aa(b(w[17],bf,gd),v,a_),bi=a(j[1][8],i),bj=aa(b(w[17],bi,ge),v,a$),bk=a(j[1][8],i),bm=aa(b(w[17],bk,gf),v,ba),bn=a(j[1][8],i),bo=aa(b(w[17],bn,gg),v,ar),bp=by(m,u,R,a1,[0,a0,aZ,ak,aj,ai],aK),bq=aJ?a(af[2],aJ[1]):gi,br=aI?a(af[2],aI[1]):gh,bs=f(h[5],0,u,Q),bt=a(f7,[0,i,bs,f(h[5],0,u,O),bp,a3,be,bg,bj,bm,bo,bq,br]);b(bF[10],i,bt);return 0}function
gs(a){var
b=y(a[3]),c=W(a[4]),d=W(a[5]),e=y(a[6]),f=y(a[8]),g=y(a[7]),h=y(a[9]),i=y(a[10]),j=W([28,[0,gt,a[11]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,W([28,[0,gu,a[12]]]),0]]]]]]]]]]}function
bO(K,J,I,G){function
c(m){var
c=a(bL[32][3],m),g=a(q[67][3],m);try{var
i=bI(c,I,G),k=[0,c],n=b(e[23],aq,fV);a(P[12],n);if(i){var
o=i[2],j=H(ab[2],0,0,g,c,i[1]),p=function(e){var
h=H(ab[2],0,0,g,c,e),b=1-H(bt[81],0,g,c,j,h);if(b){var
i=a(d[3],fW);return f(Y[5],0,fX,i)}return b};b(e[22][11],p,o);try{var
F=fU(f(h[5],0,c,j)),l=F}catch(e){e=z(e);if(e!==E)throw e;var
r=a(d[3],fY),s=f(C[11],g,c,j),t=a(d[3],fZ),u=a(d[13],0),w=a(d[3],f0),x=b(d[12],w,u),A=b(d[12],x,t),B=b(d[12],A,s),D=b(d[12],B,r),l=f(Y[5],0,f1,D)}var
L=bJ(g,k,a(h[9],l[2]),i),M=a(v[2][1],L),N=y(bA(g,k,J)),O=gs(l),Q=b(e[23],O,[0,N,[0,M,0]]),R=b(v[2][8],K,Q),S=a(e[3],k),T=a(q[65][1],S),U=b(q[18],T,R);return U}throw[0,aR,f2]}catch(c){c=z(c);if(a(q[71][9],c))return b(q[21],0,c);throw c}}return a(q[67][7],c)}aH(307,[0,bd,bb,bH,bs,bK,bN,bM,bO],"Newring_plugin__Newring");a(gv[9],ak);var
gw=0;function
gx(a,b){return bb(a)}var
gz=[0,[0,[0,gy,[1,[5,a(l[16],t[4])],0]],gx],gw];function
gA(b,a,c){return bd(b,a)}var
gC=[0,gB,[1,[5,a(l[16],t[7])],0]],gE=[0,[0,[0,gD,[1,[5,a(l[16],t[4])],gC]],gA],gz];H(aW[8],ak,gF,0,0,gE);function
aD(g,e,c){switch(c[0]){case
0:var
h=c[1];if(typeof
h==="number")return a(d[3],gG);else{if(0===h[0]){var
k=h[1],l=b(F[17],g,e),m=b(d[32],l,k),n=a(d[3],gH);return b(d[12],n,m)}var
o=h[1],p=b(F[17],g,e),q=b(d[32],p,o),r=a(d[3],gI);return b(d[12],r,q)}case
1:var
i=c[1];if(0===i[0]){var
s=i[1],t=a(d[3],gJ),u=f(aE[22],g,e,s),v=a(d[3],gK),w=a(d[13],0),x=a(d[3],gL),y=b(d[12],x,w),z=b(d[12],y,v),A=b(d[12],z,u);return b(d[12],A,t)}var
B=i[1],C=a(d[3],gM),D=f(d[39],d[13],F[7],B),E=a(d[3],gN),G=a(d[13],0),H=a(d[3],gO),I=b(d[12],H,G),J=b(d[12],I,E),K=b(d[12],J,D);return b(d[12],K,C);case
2:var
L=c[1],M=a(d[3],gP),N=f(aE[22],g,e,L),O=a(d[3],gQ),P=a(d[13],0),Q=a(d[3],gR),R=b(d[12],Q,P),S=b(d[12],R,O),T=b(d[12],S,N);return b(d[12],T,M);case
3:var
U=c[1],V=a(d[3],gS),W=f(aE[22],g,e,U),X=a(d[3],gT),Y=a(d[13],0),Z=a(d[3],gU),_=b(d[12],Z,Y),$=b(d[12],_,X),aa=b(d[12],$,W);return b(d[12],aa,V);case
4:var
ab=c[2],ac=c[1],ad=b(F[17],g,e),ae=b(d[32],ad,ab),af=b(F[17],g,e),ag=b(d[32],af,ac),ah=a(d[3],gV),ai=b(d[12],ah,ag);return b(d[12],ai,ae);case
5:var
j=c[1];if(0===j[0]){var
aj=c[2],ak=j[1],al=a(d[3],gW),am=f(aE[22],g,e,ak),an=a(d[3],gX),ao=a(d[13],0),ap=b(F[17],g,e),aq=b(d[32],ap,aj),ar=a(d[3],gY),as=b(d[12],ar,aq),at=b(d[12],as,ao),au=b(d[12],at,an),av=b(d[12],au,am);return b(d[12],av,al)}var
aw=c[2],ax=j[1],ay=a(d[3],gZ),az=f(d[39],d[13],F[7],ax),aA=a(d[3],g0),aB=a(d[13],0),aC=b(F[17],g,e),aD=b(d[32],aC,aw),aF=a(d[3],g1),aG=b(d[12],aF,aD),aH=b(d[12],aG,aB),aI=b(d[12],aH,aA),aJ=b(d[12],aI,az);return b(d[12],aJ,ay);case
6:var
aK=c[1],aL=b(F[17],g,e),aM=b(d[32],aL,aK),aN=a(d[3],g2);return b(d[12],aN,aM);default:var
aO=c[1],aP=b(F[17],g,e),aQ=b(d[32],aP,aO),aR=a(d[3],g3);return b(d[12],aR,aQ)}}var
g4=0;function
g5(a,c,b){return[0,[0,a]]}var
g6=[6,I[16][1]],g8=[0,[0,[0,[0,0,[0,a(i[10],g7)]],g6],g5],g4];function
g9(b,a){return g_}var
ha=[0,[0,[0,0,[0,a(i[10],g$)]],g9],g8];function
hb(a,c,b){return[0,[1,a]]}var
hc=[6,I[16][1]],he=[0,[0,[0,[0,0,[0,a(i[10],hd)]],hc],hb],ha];function
hf(e,a,d,c,b){return[1,[0,a]]}var
hh=[0,a(i[10],hg)],hi=[6,aF[18]],hk=[0,a(i[10],hj)],hm=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hl)]],hk],hi],hh],hf],he];function
hn(e,a,d,c,b){return[1,[1,a]]}var
hp=[0,a(i[10],ho)],hq=[1,[6,I[16][7]]],hs=[0,a(i[10],hr)],hu=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],ht)]],hs],hq],hp],hn],hm];function
hv(e,a,d,c,b){return[2,a]}var
hx=[0,a(i[10],hw)],hy=[6,aF[18]],hA=[0,a(i[10],hz)],hC=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hB)]],hA],hy],hx],hv],hu];function
hD(e,a,d,c,b){return[3,a]}var
hF=[0,a(i[10],hE)],hG=[6,aF[18]],hI=[0,a(i[10],hH)],hK=[0,[0,[0,[0,[0,[0,0,[0,a(i[10],hJ)]],hI],hG],hF],hD],hC];function
hL(b,a,d,c){return[4,a,b]}var
hM=[6,I[16][1]],hN=[6,I[16][1]],hP=[0,[0,[0,[0,[0,0,[0,a(i[10],hO)]],hN],hM],hL],hK];function
hQ(a,c,b){return[6,a]}var
hR=[6,I[16][1]],hT=[0,[0,[0,[0,0,[0,a(i[10],hS)]],hR],hQ],hP];function
hU(f,b,e,a,d,c){return[5,[1,b],a]}var
hW=[0,a(i[10],hV)],hX=[1,[6,I[16][7]]],hZ=[0,a(i[10],hY)],h0=[6,I[16][1]],h2=[0,[0,[0,[0,[0,[0,[0,0,[0,a(i[10],h1)]],h0],hZ],hX],hW],hU],hT];function
h3(f,b,e,a,d,c){return[5,[0,b],a]}var
h5=[0,a(i[10],h4)],h6=[6,aF[18]],h8=[0,a(i[10],h7)],h9=[6,I[16][1]],h$=[0,[0,[0,[0,[0,[0,[0,0,[0,a(i[10],h_)]],h9],h8],h6],h5],h3],h2];function
ia(a,c,b){return[7,a]}var
ib=[6,I[16][1]],id=[1,[0,[0,[0,[0,0,[0,a(i[10],ic)]],ib],ia],h$]],ie=[0,function(b,a){return function(c){return aD(b,a,c)}},id],bP=b(N[3],ig,ie),aX=bP[2],ih=bP[1];function
bQ(e,c,b){function
g(a){return aD(e,c,a)}var
h=f(d[39],d[28],g,b);return a(d[46],h)}var
ii=0;function
ij(d,a,c,b){return a}var
il=[0,a(i[10],ik)],io=[2,[6,aX],[0,a(i[10],im)]],iq=[1,[0,[0,[0,[0,[0,0,[0,a(i[10],ip)]],io],il],ij],ii]],ir=[0,function(b,a){return function(c){return bQ(b,a,c)}},iq],bR=b(N[3],is,ir),bS=bR[1],it=bR[2],iu=0,iv=[0,N[5]],ix=[0,[0,0,iw,function(b){a(aG[2],b);return[0,function(a){return bs(0)}]},iv],iu],iy=0;function
iz(f,e,d,c){a(aG[2],c);return[0,function(a){return bH(f,e,b(am[23],0,d))}]}var
iA=[1,[4,[5,a(l[16],bS)]],0],iC=[0,iB,[1,[5,a(l[16],t[11])],iA]],iF=[0,[0,0,[0,iE,[0,iD,[1,[5,a(l[16],t[7])],iC]]],iz,iy],ix],iG=0,iH=[0,function(a){return N[6]}];X(N[2],iI,iH,iG,iF);var
iJ=0;function
iK(f,d,c,g){var
b=a(e[22][cf],c);return bK(f,d,b[2],b[1])}var
iM=[0,iL,[1,[0,[5,a(l[16],t[11])]],0]],iO=[0,iN,[1,[2,[5,a(l[16],t[11])]],iM]],iQ=[0,[0,[0,iP,[1,[6,a(l[16],bT[9]),0],iO]],iK],iJ];H(aW[8],ak,iR,0,0,iQ);function
aY(f,e,c){if(0===c[0])return aD(f,e,c[1]);var
g=c[1],h=b(F[17],f,e),i=b(d[32],h,g),j=a(d[3],iS);return b(d[12],j,i)}var
iT=0,iU=[0,[0,[0,0,[6,aX]],function(a,b){return[0,a]}],iT];function
iV(a,c,b){return[1,a]}var
iW=[6,I[16][1]],iY=[1,[0,[0,[0,[0,0,[0,a(i[10],iX)]],iW],iV],iU]],iZ=[0,function(b,a){return function(c){return aY(b,a,c)}},iY],bU=b(N[3],i0,iZ),bV=bU[2],i1=bU[1];function
bW(e,c,b){function
g(a){return aY(e,c,a)}var
h=f(d[39],d[28],g,b);return a(d[46],h)}var
i2=0;function
i3(d,a,c,b){return a}var
i5=[0,a(i[10],i4)],i7=[2,[6,bV],[0,a(i[10],i6)]],i9=[1,[0,[0,[0,[0,[0,0,[0,a(i[10],i8)]],i7],i5],i3],i2]],i_=[0,function(b,a){return function(c){return bW(b,a,c)}},i9],bX=b(N[3],i$,i_),bY=bX[1],ja=bX[2],jb=0,jc=[0,N[5]],je=[0,[0,0,jd,function(b){a(aG[2],b);return[0,function(a){return bM(0)}]},jc],jb],jf=0;function
jg(e,d,b,c){a(aG[2],c);return[0,function(c){var
a=b?b[1]:0;return bN(e,d,a)}]}var
jh=[1,[4,[5,a(l[16],bY)]],0],jj=[0,ji,[1,[5,a(l[16],t[11])],jh]],jm=[0,[0,0,[0,jl,[0,jk,[1,[5,a(l[16],t[7])],jj]]],jg,jf],je],jn=0,jo=[0,function(a){return N[6]}];X(N[2],jp,jo,jn,jm);var
jq=0;function
jr(f,d,c,g){var
b=a(e[22][cf],c);return bO(f,d,b[2],b[1])}var
jt=[0,js,[1,[0,[5,a(l[16],t[11])]],0]],jv=[0,ju,[1,[2,[5,a(l[16],t[11])]],jt]],jx=[0,[0,[0,jw,[1,[5,a(l[16],bT[9])],jv]],jr],jq];H(aW[8],ak,jy,0,0,jx);aH(317,[0,ak,aD,ih,aX,bQ,bS,it,aY,i1,bV,bW,bY,ja],"Newring_plugin__G_newring");return}
