(function(le){"use strict";var
ag=140,cK=",",bp="AddSetoidRing",R='"',cJ="field_mod",u=250,cI="(",bm="AddSetoidField",cH="constants",Q="plugins/setoid_ring/g_newring.ml4",a0="Field_tac",cs="with carrier ",cF="postprocess tactic",cG="Init",aZ="field",cE="InitialRing",o=246,cr="decidable",cB="Pphi_pow",cC='Using setoid "',cD="tactic recognizing constants",cq=102,L="Extension: cannot occur",cA="postprocess",a5="setoid",cp=-10,cz="ring_mod",co="$map",E="]",cn="preprocess",bl="power_tac",cy="Print",a4="closed_term",bo="power",cx="Ring_polynom",a3="protect_fv",cm="Ring_tac",az="Ring",cl='and "',ck="$lH",ci="morphism",cj="plugins/setoid_ring/newring.ml",bn="field_lookup",a2="vernac argument needs not wit printer",ab="Coq",cg="closed",ch="F",a1="x",bk="ring_lookup",bj="ring",cf="preprocess tactic",aa=124,D="[",cw='and morphisms "',ce="and equivalence relation ",cd=")",ca="field_mods",cb=":",cc="t",ay=139,b_="abstract",b$="$f",aY="vernac argument needs not globwit printer",aX="sign",b9="PEeval",aW="div",aV="newring_plugin",cu="Add",cv="completeness",ct="ring_mods",b8="Pphi_dev",F=le.jsoo_runtime,K=F.caml_check_bound,b=F.caml_new_string,t=F.caml_obj_tag,bi=F.caml_register_global,J=F.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):F.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):F.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):F.caml_call_gen(a,[b,c,d])}function
P(a,b,c,d,e){return a.length==4?a(b,c,d,e):F.caml_call_gen(a,[b,c,d,e])}function
U(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):F.caml_call_gen(a,[b,c,d,e,f])}function
lb(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):F.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
lc(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):F.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
ld(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):F.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}var
f=F.caml_get_global_data(),bA=[0,b(aV),b("get_res")],bC=[0,[0,b(ab),[0,b("Setoids"),[0,b("Setoid"),0]]],[0,[0,b(ab),[0,b("Lists"),[0,b("List"),0]]],[0,[0,b(ab),[0,b(cG),[0,b("Datatypes"),0]]],[0,[0,b(ab),[0,b(cG),[0,b("Logic"),0]]],0]]]],aw=b("setoid_ring"),I=b(aV),p=f.Term,M=f.CClosure,i=f.Util,bZ=f.Tacmach,z=f.Proofview,V=f.Coqlib,ai=f.Global,h=f.Names,n=f.Pervasives,an=f.Tacintern,bV=f.Lib,s=f.CamlinternalLazy,aB=f.Universes,af=f.Rewrite,N=f.Not_found,q=f.CErrors,B=f.Mod_subst,Z=f.Tacsubst,T=f.Libnames,ae=f.Retyping,bI=f.Reductionops,d=f.Pp,G=f.Printer,H=f.Assert_failure,ah=f.Globnames,r=f.Tacinterp,aM=f.Typing,ad=f.Evarutil,bP=f.Smartlocate,j=f.Loc,am=f.Feedback,bL=f.Flags,aj=f.Stdarg,g=f.Genarg,aC=f.Evd,bw=f.Univ,bx=f.Declare,bv=f.Constrintern,l=f.Constrarg,a9=f.Quote_plugin,bt=f.Tactics,as=f.Tacenv,aJ=f.Summary,aK=f.Libobject,aS=f.Tacentries,b4=f.Egramml,aq=f.Vernac_classifier,b3=f.Vernacinterp,O=f.Ppconstr,$=f.Pptactic,ax=f.Mltop,k=f.Pcoq,m=f.CLexer,ap=f.CList,gB=[0,0,0],gC=[0,0,0],gr=b("field kind"),gs=b(cD),gt=b(cf),gu=b(cF),gv=b(a5),gw=b(bo),gx=b(aX),gy=b(aW),gz=b("infinite property"),gg=[0,b(a0),0],gh=[0,0,0],gi=b("field_lemmas"),gj=b("_field_lemma1"),gk=b("_field_lemma2"),gl=b("_field_lemma3"),gm=b("_field_lemma4"),gn=b("_lemma5"),gp=[22,0],go=[22,0],ge=b("field inverse should be declared as a morphism"),f1=b("arguments of field_simplify do not have all the same type"),f2=b(aZ),f0=[0,b(a0),0],f3=b(R),f4=b(R),f5=b("cannot find a declared field structure over"),f6=b(aZ),f7=[0,b(cj),840,12],fV=[0,1],fW=[0,0],fU=b("bad field structure"),e9=[0,0,0],e_=[0,0,0],e0=b("ring kind"),e1=b(cD),e2=b(cf),e3=b(cF),e4=b(a5),e5=b(bo),e6=b(aX),e7=b(aW),eY=b(" cannot be set twice"),eR=[0,b("Ring_base"),0],eS=b("ring_lemmas"),eT=b("_ring_lemma1"),eU=b("_ring_lemma2"),eW=[22,0],eV=[22,0],eP=[0,1],eQ=[0,0],eO=b("bad ring structure"),ex=b("ring addition should be declared as a morphism"),ey=b("ring multiplication should be declared as a morphism"),ez=b("ring opposite should be declared as a morphism"),eA=b(R),eB=b(cl),eC=b(R),eD=b(R),eE=b('",'),eF=b(cw),eG=b(R),eH=b(cC),eI=b(R),eJ=b(cl),eK=b(R),eL=b(cw),eM=b(R),eN=b(cC),et=b("cannot find setoid relation"),ef=b("arguments of ring_simplify do not have all the same type"),eg=b(bj),eh=b(R),ei=b(R),ej=b("cannot find a declared ring structure over"),ek=b(bj),el=[0,b(cj),360,12],dD=[0,b(cx),0],dC=b("newring"),du=b(az),dt=b(az),dp=b("ring: cannot find relation (not closed)"),dm=b("ring: cannot find relation"),de=b(az),dd=b(az),db=b(a1),da=b(a1),c_=b(a1),c8=b("Ring.exec_tactic: anomaly"),c3=b(a1),c4=b(ch),c5=b(ch),c1=[2,1],c2=[0,1],cX=[0,b(aV),b(a4)],cY=b(cc),cZ=b(cc),cP=b("not found"),cQ=b("map "),cR=b("lookup_map"),cN=b("dummy"),cM=b("global_head_of_constr"),df=b("Build_Setoid_Theory"),dh=b("None"),di=b("Some"),dj=b("eq"),dk=b("cons"),dl=b("nil"),dq=b(ab),dr=[0,[0,b("Ring_theory"),0],[0,[0,b(cx),0],[0,[0,b(cm),0],[0,[0,b(cE),0],[0,[0,b(a0),0],[0,[0,b("Field_theory"),0],0]]]]]],dv=[0,b(ab),0],dw=b(cm),dy=[0,b(ab),0],dz=b(cE),dE=b("almost_ring_theory"),dF=b("Eqsth"),dH=b("Eq_ext"),dJ=b("Eq_s_ext"),dL=b("ring_theory"),dM=b("mk_reqe"),dO=b("semi_ring_theory"),dP=b("mk_seqe"),dR=b("Abstract"),dS=b("Computational"),dU=b("Morphism"),dW=b("inv_morph_nothing"),dX=b("mkhypo"),dZ=b("hypo"),d2=b(b9),d5=b(cB),d8=b(b8),ea=b(bj),ec=b("ring-tac-carrier-table"),ed=b("ring-tac-name-table"),en=b("tactic-new-ring-theory"),fa=[0,b(ab),0],fb=b(a0),fg=b("FEeval"),fj=b(b9),fm=b(cB),fp=b(b8),fs=b("display_pow_linear"),fv=b("display_linear"),fz=b(aZ),fC=b("PCond"),fG=b("field_cond"),fH=b(aZ),fJ=b("simpl_field_expr"),fL=b("almost_field_theory"),fM=b("field_theory"),fN=b("semi_field_theory"),fO=b("AF_AR"),fQ=b("F_R"),fS=b("SF_SR"),fX=b("field-tac-carrier-table"),fY=b("field-tac-name-table"),f9=b("tactic-new-field-theory"),k_=[0,b(Q),1,0],k7=[0,b(Q),1,0],k4=[0,b(Q),1,0],k3=b("$lt"),k5=[0,b(E)],k6=b(ck),k8=[0,b(D)],k9=b(b$),k$=[0,b(bn)],la=b(bn),kY=b(L),kW=b(bm),kG=b(bm),kD=b(L),kB=b(L),kz=b(bm),kw=b(L),kt=b(ce),ku=b(cs),ks=b("The following field structures have been declared:"),kr=b(L),ko=b(aY),km=b(a2),j9=b(aY),j7=b(a2),jV=b(cv),jS=[0,b(Q),1,0],jP=[0,b(Q),1,0],jM=[0,b(Q),1,0],jL=b("$lrt"),jN=[0,b(E)],jO=b(ck),jQ=[0,b(D)],jR=b(b$),jT=[0,b(bk)],jU=b(bk),jG=b(L),jE=b(bp),jo=b(bp),jl=b(L),jj=b(L),jh=b(bp),je=b(L),jb=b(ce),jc=b(cs),ja=b("The following ring structures have been declared:"),i$=b(L),i8=b(aY),i6=b(a2),iR=b(aY),iP=b(a2),hG=[0,0],g_=b(b_),g$=b(cr),ha=b(ci),hb=b(E),hc=b(D),hd=b(cH),he=b(E),hf=b(D),hg=b(cg),hh=b(E),hi=b(D),hj=b(cn),hk=b(E),hl=b(D),hm=b(cA),hn=b(a5),ho=b(E),hp=b(D),hq=b(bl),hr=b(E),hs=b(D),ht=b(bl),hu=b(aX),hv=b(aW),g7=[0,b(Q),1,0],g4=[0,b(Q),1,0],g2=[0,[0,b(E)],0],g3=b("$l"),g5=[0,b(D)],g6=b("$t"),g8=[0,b(a4)],g9=b(a4),gX=b(L),gT=[0,b(Q),1,0],gQ=[0,b(Q),1,0],gN=[0,b(Q),1,0],gM=b(co),gO=[0,b(a3)],gP=b("$id"),gR=[0,b("in")],gS=b(co),gU=[0,b(a3)],gV=b(a3),gH=b(L),gF=b(L),gD=b(aV),gK=b(a3),g0=b(a4),hw=b(cz),hy=b(cz),hD=b(cr),hH=b(b_),hL=b(ci),hO=b(E),hR=b(D),hT=b(cH),hW=b(E),hZ=b(D),h1=b(cg),h4=b(E),h7=b(D),h9=b(cn),ia=b(E),id=b(D),ig=b(cA),il=b(a5),iq=b(aX),it=b(E),iw=b(D),iz=b(bo),iC=b(E),iF=b(D),iI=b(bl),iM=b(aW),iT=b(ct),iV=b(ct),iZ=b(cd),i1=b(cK),i3=b(cI),jp=[0,[0,[0,b(cy)],[0,[0,b("Rings")],0]],0],jw=[0,b(cb)],jA=[0,b(az)],jB=[0,b(cu)],jJ=b(bk),jW=b(cJ),jY=b(cJ),j4=b(cv),j$=b(ca),kb=b(ca),kf=b(cd),kh=b(cK),kj=b(cI),kH=[0,[0,[0,b(cy)],[0,[0,b("Fields")],0]],0],kO=[0,b(cb)],kS=[0,b("Field")],kT=[0,b(cu)],k1=b(bn),eX=f.Option,dn=f.Vars,dc=f.Refiner,c6=f.Environ,c7=f.Goal,cW=f.Tacticals,cS=f.Esubst,eb=f.Constr,fK=f.Redexpr;function
bq(g,f,e,d,b){switch(a(f,d)){case
0:return c(M[54],e,b);case
1:return a(M[37],b);default:return-1===d?c(M[54],e,b):a(g,b)}}function
cL(b){var
c=a(p[39],b)[1];try{var
g=a(ah[16],c);return g}catch(b){b=J(b);if(b===N){var
f=a(d[1],cM);return e(q[3],0,0,f)}throw b}}function
br(b){try{var
c=a(ah[16],b);return c}catch(b){b=J(b);if(b===N)return[0,a(h[1][5],cN)];throw b}}function
aA(f,d,b){var
k=a(f,br(b));if(k){var
q=k[1],r=-1;return bq(function(a){return aA(f,d,a)},q,d,r,b)}var
h=a(p[ag],b);switch(h[0]){case
6:var
s=function(a,b){return aA(f,a,b)};return e(M[56],s,d,b);case
9:var
g=h[2],j=h[1],t=0;if(g.length-1<=0){var
u=a(p[aa],[0,j,g]);return a(M[37],u)}var
l=c(i[19][50],t,g),v=l[2],m=a(p[aa],[0,j,l[1]]),n=a(f,br(m));if(n){var
w=n[1],o=function(b,a){return bq(function(a){return aA(f,d,a)},w,d,b,a)},x=c(i[19][16],o,v),y=[6,o(-1,m),x];return a(M[38],y)}var
z=a(p[aa],[0,j,g]);return a(M[37],z);default:return a(M[37],b)}}function
bs(b,a){try{var
c=[0,e(i[17][119],ah[5],a,b)];return c}catch(a){a=J(a);if(a===N)return 0;throw a}}var
a6=[0,i[15][45][1]];function
a7(b,a){a6[1]=e(i[15][45][4],b,a,a6[1]);return 0}function
cO(e){try{var
b=c(i[15][45][22],e,a6[1]);return b}catch(b){b=J(b);if(b===N){var
f=a(d[1],cP),g=a(d[23],e),h=a(d[1],cQ),j=c(d[13],h,g),k=c(d[13],j,f);return c(q[7],cR,k)}throw b}}function
a8(f,d,j,b){var
g=a(cS[1],0),h=aA(a(cO(f),b),g,b),i=e(M[42],0,M[9],d);return c(M[59],i,h)}function
cT(a){var
b=0,c=2,d=[0,function(b,c,d){return a8(a,b,c,d)},c];return e(bt[49],0,d,b)}function
cU(b,a){var
c=[0,[0,a,0]],d=2,f=[0,function(a,c,d){return a8(b,a,c,d)},d];return e(bt[49],0,f,c)}function
cV(f,b){var
g=c(i[17][12],aB[48],b),h=e(i[17][16],a9[1][8][4],g,a9[1][8][1]);if(c(a9[1][16],h,f))return a(z[13],0);var
j=a(d[9],0);return c(cW[70][4],0,j)}function
bu(b){var
d=[0,cX,0];function
e(a){return[0,[0,j[4],a]]}var
f=c(i[17][12],e,b),k=a(g[17],l[6]),m=a(g[5],k),n=[0,[0,c(g[7],m,f)],0],o=a(h[1][5],cY),p=[0,[1,[0,j[4],o]],0],q=a(g[5],l[8]),r=[0,[0,c(g[7],q,p)],n],s=[31,j[4],d,r];return[28,[0,[0,[0,a(h[1][5],cZ)],0],s]]}function
c0(c){var
b=a(ai[2],0),d=a(aC[17],b);return e(bv[13],b,d,c)}function
S(c){var
b=a(ai[2],0),d=a(aC[17],b);return P(bv[10],b,d,0,c)[1]}function
ac(e,d,b){var
f=a(aB[54],b),g=a(bw[35][14],d),i=c(aB[55],g,f),j=[0,a(bw[35][13],i)],k=[0,[0,lb(bx[2],0,c2,0,0,0,j,0,b)],c1],l=a(h[1][5],e),m=U(bx[3],0,0,l,0,k);return a(p[125],m)}function
by(g,f){function
k(g,b){var
d=b[1],i=b[3],k=b[2],l=a(n[20],d),m=c(n[16],c3,l),f=a(h[1][5],m),o=[2,[1,[0,j[4],f]]];return[0,d+1|0,[0,o,k],e(h[1][10][4],f,g,i)]}var
b=e(i[17][16],k,f,[0,0,0,h[1][10][1]]),l=b[3],m=b[2],o=a(h[1][5],c4),p=e(h[1][10][4],o,g,l),q=[0,p,a(r[28],0)[2]],d=a(h[1][5],c5);return c(r[18],q,[29,[0,j[4],[3,j[4],[1,[0,j[4],d]],m]]])}var
bz=[0,[0]],c9=[0,bA,0],c$=[0,function(d,b){var
e=a(i[17][3],d),f=a(g[6],aj[3]),j=c(r[2][7],f,e);function
k(d){var
e=b[1],f=a(n[20],d),g=c(n[16],c_,f),i=a(h[1][5],g);return c(h[1][10][22],i,e)}bz[1]=c(i[19][2],j,k);return a(z[13],0)}];e(as[9],0,bA,c$);function
bB(C,B,k,b,A){function
D(g,b){var
d=b[1],i=b[3],k=b[2],l=a(n[20],d),m=c(n[16],da,l),f=a(h[1][5],m),o=[2,[1,[0,j[4],f]]],p=a(r[2][1],g);return[0,d+1|0,[0,o,k],e(h[1][10][4],f,p,i)]}var
l=e(i[17][16],D,A,[0,0,0,h[1][10][1]]),E=l[3],F=l[2],G=[0,E,a(r[28],0)[2]];function
H(b){var
d=a(n[20],b),e=c(n[16],db,d);return a(h[1][5],e)}var
I=c(i[17][48],k,H),J=a(g[5],aj[3]),K=[0,[0,c(g[7],J,k)],0],L=[31,j[4],c9,K];function
M(a){return[0,a]}var
N=[5,[28,[0,c(i[17][12],M,I),L]]],v=aC[3][1],w=p[117],x=a(c6[10],C),f=P(c7[4][6],B,x,w,v),y=[0,f[1],f[3]],d=t(b),O=c(i[18],F,[0,N,0]),q=u===d?b[1]:o===d?a(s[2],b):b,Q=c(r[18],G,[29,[0,j[4],[3,j[4],[0,[0,j[4],q]],O]]]),R=c(z[66][8],Q,y),S=a(dc[2],R),m=a(ad[44],S),T=m[2],U=c(aC[143],0,m[1])[2],V=bz[1];function
W(c){var
b=a(r[2][2],c),d=b?b[1]:a(n[2],c8);return a(T,d)}return[0,c(i[19][15],W,V),U]}function
bD(a){return[o,function(b){return e(V[6],dd,bC,a)}]}function
aD(a){return[o,function(b){return e(V[7],de,bC,a)}]}var
dg=bD(df),a_=aD(dh),a$=aD(di),W=bD(dj),at=aD(dk),au=aD(dl);function
av(b,d){var
c=t(b),e=u===c?b[1]:o===c?a(s[2],b):b;return a(p[aa],[0,e,d])}function
v(f,b,e){var
d=t(b),g=u===d?b[1]:o===d?a(s[2],b):b,h=[0,c(ad[13],f,g),e];return a(p[aa],h)}function
bE(h){var
c=a(p[ag],h);if(9===c[0]){var
b=c[2],j=c[1];if(2<=b.length-1){var
k=[0,j,e(i[19][7],b,0,b.length-1-2|0)],d=a(p[aa],k);if(a(dn[2],d)){var
f=b.length-1-1|0,g=b.length-1-2|0,l=K(b,f)[f+1];return[0,d,K(b,g)[g+1],l]}return a(q[6],dp)}}return a(q[6],dm)}var
aE=[0,dq,[0,aw,0]];function
ds(a){return c(i[18],aE,a)}var
bF=c(i[17][12],ds,dr);function
X(a){return[o,function(b){return e(V[6],dt,bF,a)}]}function
A(a){return[o,function(b){return e(V[7],du,bF,a)}]}var
dx=c(i[17][12],h[1][5],[0,dw,[0,aw,dv]]);a(h[5][4],dx);var
dA=c(i[17][12],h[1][5],[0,dz,[0,aw,dy]]),dB=a(h[5][4],dA);function
bG(b){return[o,function(d){var
c=a(h[6][4],b);return e(h[cq],[0,dB],h[5][6],c)}]}function
ak(a){var
b=[0,aw,dD];return[o,function(c){return e(V[5],dC,b,a)}]}var
aF=X(dE),dG=A(dF),dI=A(dH),dK=A(dJ),aG=X(dL),dN=X(dM),aH=X(dO),dQ=X(dP),aI=X(dR),dT=X(dS),dV=X(dU),Y=bG(dW),dY=A(dX),y=A(dZ);function
bH(f,e){var
b=e;for(;;){var
d=a(p[ag],b);if(6===d[0]){var
b=d[3];continue}var
g=bE(b)[1],h=function(c){var
b=c[1],d=t(b),e=c[2],f=u===d?b[1]:o===d?a(s[2],b):b;return[0,f,e]},j=c(i[17][12],h,f),k=function(a){return-1===a?1:2},l=[0,[0,cL(g),k],j];return function(a){return bs(l,a)}}}var
d0=0;function
d1(b){var
a=b+1|0;if(!(13<a>>>0))switch(a){case
12:return 2;case
0:case
8:case
10:case
13:return 0}return 1}var
d3=[0,[0,ak(d2),d1],d0];function
d4(b){var
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
d6=[0,[0,ak(d5),d4],d3];function
d7(b){var
a=b-8|0;if(6<a>>>0){if(-9!==a)return 1}else
if(5===a)return 2;return 0}var
d9=[0,[0,ak(d8),d7],d6],d_=[0,[0,au,function(a){return-1===a?0:1}],d9],d$=[0,[0,at,function(a){return-1===a?0:2===a?2:1}],d_];a7(ea,function(a){return bH(d$,a)});var
al=a(i[21][1],[0,eb[33]]),ba=e(aJ[2],0,ec,al[1]),bb=e(aJ[2],0,ed,T[24][1]);function
ee(a){return c(al[22],a,ba[1])}function
em(d){var
a=d[2],b=d[1],e=c(B[45],b,a[1]),f=c(B[45],b,a[2]),g=c(B[45],b,a[3]),h=c(B[45],b,a[4]),i=c(B[45],b,a[5]),j=c(B[45],b,a[6]),k=c(B[45],b,a[9]),l=c(B[45],b,a[10]),m=c(Z[1],b,a[7]),n=c(Z[1],b,a[8]),o=c(Z[1],b,a[11]),q=c(Z[1],b,a[12]);if(e===a[1])if(f===a[2])if(c(p[136],g,a[3]))if(h===a[4])if(i===a[5])if(j===a[6])if(k===a[9])if(l===a[10])if(m===a[7])if(n===a[8])if(o===a[11])if(q===a[12])return a;return[0,e,f,g,h,i,j,m,n,k,l,o,q]}function
bJ(b){var
a=b[2],c=b[1][1];ba[1]=e(al[4],a[1],a,ba[1]);bb[1]=e(T[24][4],c,a,bb[1]);return 0}var
aL=a(aK[1],en),eo=aL[8],ep=aL[7];function
eq(a){return[0,a]}function
er(c,b){var
a=1===c?1:0;return a?bJ(b):a}var
es=a(aK[4],[0,aL[1],bJ,aL[3],er,eq,em,ep,eo]);function
bK(d,e,c,b){try{var
f=P(af[11],d,e[1],c,b),i=f[2],g=P(af[12],d,f[1],c,b),j=g[2],h=P(af[13],d,g[1],c,b),k=h[2];e[1]=h[1];var
l=av(dg,[0,c,b,i,j,k]);return l}catch(b){b=J(b);if(b===N)return a(q[6],et);throw b}}function
eu(h,g,f,e,d,c,b,a){return av(dN,[0,h,g,f,e,d,c,b,a])}function
ev(f,e,d,c,b,a){return av(dQ,[0,f,e,d,c,b,a])}function
ew(r,g,h){var
b=h[5],k=h[4],i=h[3],j=h[2],f=h[1],n=a(p[ag],b);if(9===n[0])if(1===n[2].length-1){var
D=t(W),aR=n[1],aS=u===D?W[1]:o===D?a(s[2],W):W;if(c(p[ay],aR,aS)){var
aT=v(g,dG,[0,f]),aU=k?v(g,dI,[0,f,j,i,k[1]]):v(g,dK,[0,f,j,i]),aV=e(aM[7],r,g,aT);return[0,aV,e(aM[7],r,g,aU)]}}var
w=[0,[0,[0,[0,f,[0,b]]],[0,[0,[0,f,[0,b]]],0]],[0,[0,f,[0,b]]]],E=bK(a(ai[2],0),g,f,b);try{var
aQ=c(af[14],w,j),x=aQ}catch(b){b=J(b);if(b!==N)throw b;var
x=a(q[6],ex)}var
l=x[2];try{var
aP=c(af[14],w,i),y=aP}catch(b){b=J(b);if(b!==N)throw b;var
y=a(q[6],ey)}var
m=y[2];if(k){var
z=k[1];try{var
aq=c(af[14],[0,[0,[0,[0,f,[0,b]]],0],[0,[0,f,[0,b]]]],z),A=aq}catch(b){b=J(b);if(b!==N)throw b;var
A=a(q[6],ez)}var
B=A[2],F=eu(f,j,i,z,b,l,m,B),H=a(d[1],eA),I=a(G[5],B),K=a(d[1],eB),L=a(d[16],0),M=a(d[1],eC),O=a(G[5],m),P=a(d[1],eD),Q=a(d[16],0),R=a(d[1],eE),S=a(G[5],l),T=a(d[1],eF),U=a(d[16],0),V=a(d[1],eG),X=a(G[5],b),Y=a(d[1],eH),Z=c(d[13],Y,X),_=c(d[13],Z,V),$=c(d[13],_,U),aa=c(d[13],$,T),ab=c(d[13],aa,S),ac=c(d[13],ab,R),ad=c(d[13],ac,Q),ae=c(d[13],ad,P),ah=c(d[13],ae,O),aj=c(d[13],ah,M),ak=c(d[13],aj,L),al=c(d[13],ak,K),an=c(d[13],al,I),ao=c(d[13],an,H),ap=function(a){return c(am[12],0,a)};c(bL[51],ap,ao);var
C=F}else{var
ar=a(d[1],eI),as=a(G[5],m),at=a(d[1],eJ),au=a(d[16],0),av=a(d[1],eK),aw=a(G[5],l),ax=a(d[1],eL),az=a(d[16],0),aA=a(d[1],eM),aB=a(G[5],b),aC=a(d[1],eN),aD=c(d[13],aC,aB),aE=c(d[13],aD,aA),aF=c(d[13],aE,az),aG=c(d[13],aF,ax),aH=c(d[13],aG,aw),aI=c(d[13],aH,av),aJ=c(d[13],aI,au),aK=c(d[13],aJ,at),aL=c(d[13],aK,as),aN=c(d[13],aL,ar),aO=function(a){return c(am[12],0,a)};c(bL[51],aO,aN);var
C=ev(f,j,i,b,l,m)}return[0,E,C]}function
bM(h,g,f,e,d,c,b,a){return a?a[1]:ew(h,g,[0,f,e,d,c,b])}function
bN(b){if(typeof
b==="number"){var
c=t(aI);return u===c?aI[1]:o===c?a(s[2],aI):aI}else
return 0===b[0]?av(dT,[0,b[1]]):av(dV,[0,b[1]])}function
bO(p,n,m,l,k,d){if(d){var
b=d[1];if(0===b[0])return a(an[3],b[1]);var
f=b[1],g=function(a){return c(bP[3],0,a)};return bu(c(i[17][12],g,f))}var
e=t(Y),h=u===e?Y[1]:o===e?a(s[2],Y):Y;return[29,[0,j[4],[3,j[4],[0,[0,j[4],h]],0]]]}function
aN(c,b,a){return v(b,dY,[0,U(ae[2],0,0,c,b[1],a),a])}function
bQ(d,b,h){var
f=t(y),j=u===f?y[1]:o===f?a(s[2],y):y,g=c(ad[13],b,j),k=v(b,au,[0,g]);function
l(c,a){return v(b,at,[0,g,aN(d,b,c),a])}var
m=e(i[17][16],l,h,k),n=e(aM[7],d,b,m);return c(ad[43],b[1],n)}function
bR(m,b,e){var
f=t(y),n=u===f?y[1]:o===f?a(s[2],y):y,g=c(ad[13],b,n);if(e){var
h=e[1],d=h[1],p=h[2];if(0===d[0])var
k=a(an[3],d[1]);else
var
q=d[1],r=function(a){return c(bP[3],0,a)},k=bu(c(i[17][12],r,q));return[0,k,v(b,a$,[0,g,aN(m,b,S(p))])]}var
l=t(Y),w=u===l?Y[1]:o===l?a(s[2],Y):Y,x=[0,[0,j[4],w]],z=v(b,a_,[0,g]);return[0,[29,[0,j[4],[3,j[4],x,0]]],z]}function
bS(g,b,d){var
e=t(y),h=u===e?y[1]:o===e?a(s[2],y):y,f=c(ad[13],b,h);return d?v(b,a$,[0,f,aN(g,b,S(d[1]))]):v(b,a_,[0,f])}function
bT(g,b,d){var
e=t(y),h=u===e?y[1]:o===e?a(s[2],y):y,f=c(ad[13],b,h);return d?v(b,a$,[0,f,aN(g,b,S(d[1]))]):v(b,a_,[0,f])}function
bU(m,J,ar,I,aq,H,ap,ao,am){var
L=H[2],M=H[1],N=J[2],r=J[1],as=c(i[18],aE,eR);a(V[11],as);var
f=a(ai[2],0),_=U(ae[2],0,0,f,r,N),k=a(p[ag],_);if(9===k[0]){var
b=k[2],v=b.length-1-6|0;if(2<v>>>0)var
e=0;else{var
l=k[1];switch(v){case
0:var
w=t(aH),$=b[1],aa=b[2],ab=b[3],ad=b[4],af=b[5],ah=b[6],aj=u===w?aH[1]:o===w?a(s[2],aH):aH;if(c(p[ay],l,aj))var
d=[0,eP,$,aa,ab,ad,af,0,0,ah],e=1;else
var
e=0;break;case
1:var
e=0;break;default:var
x=b[1],y=b[2],z=b[3],A=b[4],B=b[5],C=b[6],D=b[7],E=b[8],F=t(aF),ak=u===F?aF[1]:o===F?a(s[2],aF):aF;if(c(p[ay],l,ak))var
d=[0,0,x,y,z,A,B,[0,C],[0,D],E],e=1;else{var
G=t(aG),al=u===G?aG[1]:o===G?a(s[2],aG):aG;if(c(p[ay],l,al))var
d=[0,eQ,x,y,z,A,B,[0,C],[0,D],E],e=1;else
var
e=0}}}}else
var
e=0;if(!e)var
d=a(q[6],eO);var
O=d[9],P=d[8],Q=d[6],R=d[5],S=d[2],g=[0,r],at=d[4],au=d[3],av=d[1],T=bM(f,g,S,R,Q,P,O,ar),W=T[1],aw=T[2],X=bR(f,g,ap),ax=X[2],az=X[1],aA=bS(f,g,ao),aB=bT(f,g,am),aC=[0,W,[0,aw,[0,N,[0,ax,[0,aA,[0,aB,[0,bN(I),0]]]]]]],aD=bG(eS),Y=bB(f,g[1],5,aD,aC),Z=Y[2],j=Y[1],aI=K(j,3)[4],aJ=K(j,4)[5],aK=a(h[1][7],m),aL=ac(c(n[16],aK,eT),Z,aI),aM=a(h[1][7],m),aN=ac(c(n[16],aM,eU),Z,aJ),aO=bO(f,r,I,av,[0,au,at,R,Q,P],aq),aP=M?a(an[3],M[1]):eW,aQ=L?a(an[3],L[1]):eV,aR=K(j,0)[1],aS=K(j,2)[3],aT=a(es,[0,S,O,W,K(j,1)[2],aS,aR,aO,az,aL,aN,aP,aQ]);c(bV[6],m,aT);return 0}function
bW(a){return typeof
a==="number"?0:0===a[0]?[0,S(a[1])]:[1,S(a[1])]}function
w(e,b,d){if(a(eX[3],b[1])){b[1]=[0,d];return 0}var
f=c(n[16],e,eY);return a(q[6],f)}function
eZ(l){var
b=[0,0],d=[0,0],e=[0,0],f=[0,0],g=[0,0],h=[0,0],j=[0,0],k=[0,0];function
m(a){switch(a[0]){case
0:return w(e0,b,bW(a[1]));case
1:return w(e1,e,a[1]);case
2:return w(e2,f,a[1]);case
3:return w(e3,g,a[1]);case
4:var
c=a[1],i=S(a[2]);return w(e4,d,[0,S(c),i]);case
5:return w(e5,j,[0,a[1],a[2]]);case
6:return w(e6,h,a[1]);default:return w(e7,k,a[1])}}c(i[17][11],m,l);var
a=b[1],n=a?a[1]:0;return[0,n,d[1],e[1],f[1],g[1],j[1],h[1],k[1]]}function
bX(a,c){if(a)return a;var
b=bE(c);return[0,b[2],[0,b[3],0]]}function
bY(d,a,b,c){var
f=v(a,au,[0,b]);function
g(d,c){return v(a,at,[0,b,d,c])}var
h=e(i[17][16],g,c,f);return e(aM[7],d,a,h)}var
x=r[2][1];function
_(b){var
d=a(r[28],0);return c(r[2][6],d,b)}function
e8(b){var
c=a(x,b[2]),d=a(x,b[3]),e=a(x,b[4]),f=a(x,b[5]),g=a(x,b[6]),h=_(b[7]),i=_(b[8]),j=a(x,b[9]),k=a(x,b[10]),l=_([28,[0,e9,b[11]]]);return[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,l,[0,_([28,[0,e_,b[12]]]),0]]]]]]]]]]]}function
e$(D,C,B,A){var
b=[0,function(k){var
e=a(bZ[48][4],k),b=a(z[62][5],k);try{var
h=[0,e],f=bX(B,A);if(f){var
l=f[2],g=U(ae[2],0,0,b,e,f[1]),m=function(h){var
i=U(ae[2],0,0,b,e,h),f=1-U(bI[77],0,b,e,g,i);if(f){var
j=a(d[1],ef);return c(q[7],eg,j)}return f};c(i[17][11],m,l);try{var
y=ee(g),j=y}catch(b){b=J(b);if(b!==N)throw b;var
n=a(d[1],eh),o=a(G[5],g),p=a(d[1],ei),r=a(d[16],0),s=a(d[1],ej),t=c(d[13],s,r),u=c(d[13],t,p),v=c(d[13],u,o),w=c(d[13],v,n),j=c(q[7],ek,w)}var
E=a(x,bY(b,h,j[1],f)),F=a(x,bQ(b,h,C)),I=e8(j),K=by(D,c(i[18],I,[0,F,[0,E,0]])),L=a(z[60][1],h[1]),M=c(z[15],L,K);return M}throw[0,H,el]}catch(b){b=J(b);if(a(z[66][10],b))return c(z[18],0,b);throw b}}];return a(z[62][10],b)}var
fc=c(i[17][12],h[1][5],[0,fb,[0,aw,fa]]),fd=a(h[5][4],fc),fe=0;function
ff(c){var
a=c-8|0;if(6<a>>>0)var
b=-9===a?1:0;else
if(4<=a)switch(a-4|0){case
0:var
b=0;break;case
1:return 2;default:var
b=1}else
var
b=1;return b?0:1}var
fh=[0,[0,A(fg),ff],fe];function
fi(b){var
a=b+1|0;if(!(13<a>>>0))switch(a){case
12:return 2;case
0:case
8:case
10:case
13:return 0}return 1}var
fk=[0,[0,ak(fj),fi],fh];function
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
fn=[0,[0,ak(fm),fl],fk];function
fo(b){var
a=b-8|0;if(6<a>>>0){if(-9!==a)return 1}else
if(5===a)return 2;return 0}var
fq=[0,[0,ak(fp),fo],fn];function
fr(d){var
b=d-9|0;if(10<b>>>0)var
a=cp===b?1:0;else{var
c=b-6|0;if(2<c>>>0)var
a=1;else
switch(c){case
0:var
a=0;break;case
1:var
a=1;break;default:return 2}}return a?0:1}var
ft=[0,[0,A(fs),fr],fq];function
fu(b){var
a=b-9|0;if(7<a>>>0){if(cp!==a)return 1}else
if(5===a)return 2;return 0}var
fw=[0,[0,A(fv),fu],ft],fx=[0,[0,au,function(a){return-1===a?0:1}],fw],fy=[0,[0,at,function(a){return-1===a?0:2===a?2:1}],fx];a7(fz,function(a){return bH(fy,a)});var
fA=0;function
fB(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
14:return 2;case
0:case
10:case
12:case
15:return 0}return 1}var
fD=[0,[0,A(fC),fB],fA],fE=[0,[0,au,function(a){return-1===a?0:1}],fD],fF=[0,[0,at,function(a){return-1===a?0:2===a?2:1}],fE];a7(fG,function(e){function
b(c){var
b=c[1],d=t(b),e=c[2],f=u===d?b[1]:o===d?a(s[2],b):b;return[0,f,e]}var
d=c(i[17][12],b,fF);return function(a){return bs(d,a)}});function
fI(a,b,c){return a8(fH,a,b,c)}c(fK[3],fJ,fI);var
aO=A(fL),aP=A(fM),aQ=A(fN),fP=A(fO),fR=A(fQ),fT=A(fS),bc=e(aJ[2],0,fX,al[1]),bd=e(aJ[2],0,fY,T[24][1]);function
fZ(a){return c(al[22],a,bc[1])}function
f8(d){var
a=d[2],b=d[1],e=c(B[45],b,a[1]),f=c(B[45],b,a[2]),g=c(B[45],b,a[5]),h=c(B[45],b,a[6]),i=c(B[45],b,a[7]),j=c(B[45],b,a[8]),k=c(B[45],b,a[9]),l=c(Z[1],b,a[3]),m=c(Z[1],b,a[4]),n=c(Z[1],b,a[10]),o=c(Z[1],b,a[11]);if(e===a[1])if(f===a[2])if(g===a[5])if(h===a[6])if(i===a[7])if(j===a[8])if(k===a[9])if(l===a[3])if(m===a[4])if(n===a[10])if(o===a[11])return a;return[0,e,f,l,m,g,h,i,j,k,n,o]}function
b0(b){var
a=b[2],c=b[1][1];bc[1]=e(al[4],a[1],a,bc[1]);bd[1]=e(T[24][4],c,a,bd[1]);return 0}var
aR=a(aK[1],f9),f_=aR[8],f$=aR[7];function
ga(a){return[0,a]}function
gb(c,b){var
a=1===c?1:0;return a?b0(b):a}var
gc=a(aK[4],[0,aR[1],b0,aR[3],gb,ga,f8,f$,f_]);function
gd(i,b,f,d){var
e=a(p[ag],d);if(9===e[0])if(1===e[2].length-1){var
h=t(W),l=e[1],m=u===h?W[1]:o===h?a(s[2],W):W;if(c(p[ay],l,m)){var
n=a(V[38],0)[6],r=[0,a(aB[48],n),[0,b,b,f]];return a(p[aa],r)}}bK(a(ai[2],0),i,b,d);var
j=[0,[0,[0,[0,b,[0,d]]],0],[0,[0,b,[0,d]]]];try{var
k=c(af[14],j,f),g=k}catch(b){b=J(b);if(b!==N)throw b;var
g=a(q[6],ge)}return g[2]}function
gf(j,ak,aD,I,aj,af,ad,ab,$,_){var
al=ad[2],am=ad[1],m=ak[2],ao=ak[1],aF=c(i[18],aE,gg);a(V[11],aF);var
k=a(ai[2],0),f=[0,ao],az=U(ae[2],0,0,k,f[1],m),G=a(p[ag],az);if(9===G[0]){var
b=G[2],M=b.length-1-8|0;if(2<M>>>0)var
g=0;else{var
H=G[1];switch(M){case
0:var
N=b[1],O=b[2],P=b[3],Q=b[4],R=b[5],S=b[6],T=b[7],W=b[8],X=t(aQ),aA=u===X?aQ[1]:o===X?a(s[2],aQ):aQ;if(c(ah[11],aA,H))var
d=[0,fV,N,O,P,Q,R,0,0,S,T,W,v(f,fT,[0,N,O,P,Q,R,S,T,W,m])],g=1;else
var
g=0;break;case
1:var
g=0;break;default:var
w=b[1],x=b[2],y=b[3],z=b[4],A=b[5],B=b[6],C=b[7],D=b[8],E=b[9],F=b[10],Y=t(aO),aB=u===Y?aO[1]:o===Y?a(s[2],aO):aO;if(c(ah[11],aB,H))var
d=[0,0,w,x,y,z,A,[0,B],[0,C],D,E,F,v(f,fP,[0,w,x,y,z,A,B,C,D,E,F,m])],g=1;else{var
Z=t(aP),aC=u===Z?aP[1]:o===Z?a(s[2],aP):aP;if(c(ah[11],aC,H))var
d=[0,fW,w,x,y,z,A,[0,B],[0,C],D,E,F,v(f,fR,[0,w,x,y,z,A,B,C,D,E,F,m])],g=1;else
var
g=0}}}}else
var
g=0;if(!g)var
d=a(q[6],fU);var
J=d[11],ap=d[8],aq=d[6],ar=d[5],L=d[2],aG=d[12],aH=d[10],aI=d[4],aJ=d[3],aK=d[1],as=bM(k,f,L,ar,aq,ap,J,aD),at=as[2],au=as[1];bU(j,[0,f[1],aG],[0,[0,au,at]],I,aj,gh,ab,$,_);var
av=bR(k,f,ab),aL=av[2],aM=av[1],aN=bS(k,f,$),aR=bT(k,f,_),aS=gd(f,L,aH,J),aT=[0,au,[0,at,[0,aS,[0,m,[0,aL,[0,aN,[0,aR,[0,bN(I),0]]]]]]]],ay=[o,function(c){var
b=a(h[6][4],gi);return e(h[cq],[0,fd],h[5][6],b)}],aw=bB(k,f[1],9,ay,aT),r=aw[2],l=aw[1],aU=K(l,3)[4],aV=K(l,4)[5],aW=K(l,5)[6],aX=K(l,6)[7];if(af)var
aY=[0,af[1]],aZ=[0,K(l,8)[9],aY],ax=a(p[aa],aZ);else
var
ax=K(l,7)[8];var
a0=a(h[1][7],j),a1=ac(c(n[16],a0,gj),r,aU),a2=a(h[1][7],j),a3=ac(c(n[16],a2,gk),r,aV),a4=a(h[1][7],j),a5=ac(c(n[16],a4,gl),r,aW),a6=a(h[1][7],j),a7=ac(c(n[16],a6,gm),r,aX),a8=a(h[1][7],j),a9=ac(c(n[16],a8,gn),r,ax),a_=bO(k,ao,I,aK,[0,aJ,aI,ar,aq,ap],aj),a$=am?a(an[3],am[1]):gp,ba=al?a(an[3],al[1]):go,bb=a(gc,[0,L,J,a_,aM,a1,a3,a5,a7,a9,a$,ba]);c(bV[6],j,bb);return 0}function
gq(b){var
d=[0,0],e=[0,0],f=[0,0],g=[0,0],h=[0,0],j=[0,0],k=[0,0],l=[0,0],m=[0,0];function
n(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return w(gr,d,bW(a[1]));case
1:return w(gs,f,a[1]);case
2:return w(gt,g,a[1]);case
3:return w(gu,h,a[1]);case
4:var
c=a[1],i=S(a[2]);return w(gv,e,[0,S(c),i]);case
5:return w(gw,l,[0,a[1],a[2]]);case
6:return w(gx,k,a[1]);default:return w(gy,m,a[1])}}return w(gz,j,S(b[1]))}c(i[17][11],n,b);var
a=d[1],o=a?a[1]:0;return[0,o,e[1],j[1],f[1],g[1],h[1],l[1],k[1],m[1]]}function
gA(b){var
c=a(x,b[2]),d=_(b[3]),e=_(b[4]),f=a(x,b[5]),g=a(x,b[7]),h=a(x,b[6]),i=a(x,b[8]),j=a(x,b[9]),k=_([28,[0,gB,b[10]]]);return[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,_([28,[0,gC,b[11]]]),0]]]]]]]]]]}var
C=[0,cU,cT,cV,eZ,bU,c0,bb,e$,gq,gf,bd,function(E,D,C,B){var
b=[0,function(k){var
e=a(bZ[48][4],k),b=a(z[62][5],k);try{var
h=[0,e],f=bX(C,B),l=c(i[18],aE,f0);a(V[11],l);if(f){var
m=f[2],g=U(ae[2],0,0,b,e,f[1]),n=function(h){var
i=U(ae[2],0,0,b,e,h),f=1-U(bI[77],0,b,e,g,i);if(f){var
j=a(d[1],f1);return c(q[7],f2,j)}return f};c(i[17][11],n,m);try{var
A=fZ(g),j=A}catch(b){b=J(b);if(b!==N)throw b;var
o=a(d[1],f3),p=a(G[5],g),r=a(d[1],f4),s=a(d[16],0),t=a(d[1],f5),u=c(d[13],t,s),v=c(d[13],u,r),w=c(d[13],v,p),y=c(d[13],w,o),j=c(q[7],f6,y)}var
F=a(x,bY(b,h,j[1],f)),I=a(x,bQ(b,h,D)),K=gA(j),L=by(E,c(i[18],K,[0,I,[0,F,0]])),M=a(z[60][1],h[1]),O=c(z[15],M,L);return O}throw[0,H,f7]}catch(b){b=J(b);if(a(z[66][10],b))return c(z[18],0,b);throw b}}];return a(z[62][10],b)}];bi(366,C,"Newring_plugin.Newring");a(ax[12],gD);var
gE=0,gG=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(g[6],aj[4]),f=c(r[2][7],e,d);return function(b){return a(C[2],f)}}return a(n[2],gF)},gE],gI=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],h=a(g[6],aj[4]),i=c(r[2][7],h,f),j=a(g[6],l[4]),k=c(r[2][7],j,e);return function(a){return c(C[1],i,k)}}}return a(n[2],gH)},gG],gJ=a(i[19][12],gI);e(as[9],0,[0,I,gK],gJ);function
gL(q){var
i=a(h[1][6],gM),b=aj[4],f=0,g=0;if(0===b[0]){var
k=[0,[0,gO,[0,[1,j[4],[5,[0,b[1]]],i],g]],f],n=a(h[1][6],gP),d=l[4],m=0;if(0===d[0]){var
o=[0,gR,[0,[1,j[4],[5,[0,d[1]]],n],m]],p=a(h[1][6],gS),e=aj[4];if(0===e[0])return c(aS[4],[0,I,gV],[0,[0,gU,[0,[1,j[4],[5,[0,e[1]]],p],o]],k]);throw[0,H,gT]}throw[0,H,gQ]}throw[0,H,gN]}c(ax[19],gL,I);var
gW=0,gY=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],h=a(g[6],l[8]),i=c(r[2][7],h,f),j=a(g[17],l[18]),k=a(g[6],j),m=c(r[2][7],k,e);return function(a){return c(C[3],i,m)}}}return a(n[2],gX)},gW],gZ=a(i[19][12],gY);e(as[9],0,[0,I,g0],gZ);function
g1(k){var
f=a(h[1][6],g3),b=l[18],e=0;if(0===b[0]){var
g=[0,g5,[0,[1,j[4],[0,[5,[0,b[1]]]],f],g2]],i=a(h[1][6],g6),d=l[8];if(0===d[0])return c(aS[4],[0,I,g9],[0,[0,g8,[0,[1,j[4],[5,[0,d[1]]],i],g]],e]);throw[0,H,g7]}throw[0,H,g4]}c(ax[19],g1,I);function
aT(b){switch(b[0]){case
0:var
f=b[1];if(typeof
f==="number")return a(d[1],g_);else{if(0===f[0]){var
i=c(d[46],O[23],f[1]),j=a(d[1],g$);return c(d[13],j,i)}var
k=c(d[46],O[23],f[1]),l=a(d[1],ha);return c(d[13],l,k)}case
1:var
g=b[1];if(0===g[0]){var
m=g[1],n=a(d[1],hb),o=a($[19],m),p=a(d[1],hc),q=a(d[16],0),r=a(d[1],hd),s=c(d[13],r,q),t=c(d[13],s,p),u=c(d[13],t,o);return c(d[13],u,n)}var
v=g[1],w=a(d[1],he),x=e(d[53],d[16],T[41],v),y=a(d[1],hf),z=a(d[16],0),A=a(d[1],hg),B=c(d[13],A,z),C=c(d[13],B,y),D=c(d[13],C,x);return c(d[13],D,w);case
2:var
E=b[1],F=a(d[1],hh),G=a($[19],E),H=a(d[1],hi),I=a(d[16],0),J=a(d[1],hj),K=c(d[13],J,I),L=c(d[13],K,H),M=c(d[13],L,G);return c(d[13],M,F);case
3:var
N=b[1],P=a(d[1],hk),Q=a($[19],N),R=a(d[1],hl),S=a(d[16],0),U=a(d[1],hm),V=c(d[13],U,S),W=c(d[13],V,R),X=c(d[13],W,Q);return c(d[13],X,P);case
4:var
Y=b[1],Z=c(d[46],O[23],b[2]),_=c(d[46],O[23],Y),aa=a(d[1],hn),ab=c(d[13],aa,_);return c(d[13],ab,Z);case
5:var
h=b[1];if(0===h[0]){var
ac=b[2],ad=h[1],ae=a(d[1],ho),af=a($[19],ad),ag=a(d[1],hp),ah=a(d[16],0),ai=c(d[46],O[23],ac),aj=a(d[1],hq),ak=c(d[13],aj,ai),al=c(d[13],ak,ah),am=c(d[13],al,ag),an=c(d[13],am,af);return c(d[13],an,ae)}var
ao=b[2],ap=h[1],aq=a(d[1],hr),ar=e(d[53],d[16],T[41],ap),as=a(d[1],hs),at=a(d[16],0),au=c(d[46],O[23],ao),av=a(d[1],ht),aw=c(d[13],av,au),ax=c(d[13],aw,at),ay=c(d[13],ax,as),az=c(d[13],ay,ar);return c(d[13],az,aq);case
6:var
aA=c(d[46],O[23],b[1]),aB=a(d[1],hu);return c(d[13],aB,aA);default:var
aC=c(d[46],O[23],b[1]),aD=a(d[1],hv);return c(d[13],aD,aC)}}var
be=a(g[3],hw),hx=a(g[4],be),aU=e(k[13],k[9],hy,hx),hz=0,hA=0;function
hB(a,c,b){return[0,[0,a]]}var
hC=[6,k[15][1]],hE=[0,[0,[0,[0,0,[0,a(m[12],hD)]],hC],hB],hA];function
hF(b,a){return hG}var
hI=[0,[0,[0,0,[0,a(m[12],hH)]],hF],hE];function
hJ(a,c,b){return[0,[1,a]]}var
hK=[6,k[15][1]],hM=[0,[0,[0,[0,0,[0,a(m[12],hL)]],hK],hJ],hI];function
hN(e,a,d,c,b){return[1,[0,a]]}var
hP=[0,a(m[12],hO)],hQ=[6,k[17][19]],hS=[0,a(m[12],hR)],hU=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],hT)]],hS],hQ],hP],hN],hM];function
hV(e,a,d,c,b){return[1,[1,a]]}var
hX=[0,a(m[12],hW)],hY=[1,[6,k[15][7]]],h0=[0,a(m[12],hZ)],h2=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],h1)]],h0],hY],hX],hV],hU];function
h3(e,a,d,c,b){return[2,a]}var
h5=[0,a(m[12],h4)],h6=[6,k[17][19]],h8=[0,a(m[12],h7)],h_=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],h9)]],h8],h6],h5],h3],h2];function
h$(e,a,d,c,b){return[3,a]}var
ib=[0,a(m[12],ia)],ic=[6,k[17][19]],ie=[0,a(m[12],id)],ih=[0,[0,[0,[0,[0,[0,0,[0,a(m[12],ig)]],ie],ic],ib],h$],h_];function
ii(b,a,d,c){return[4,a,b]}var
ij=[6,k[15][1]],ik=[6,k[15][1]],im=[0,[0,[0,[0,[0,0,[0,a(m[12],il)]],ik],ij],ii],ih];function
io(a,c,b){return[6,a]}var
ip=[6,k[15][1]],ir=[0,[0,[0,[0,0,[0,a(m[12],iq)]],ip],io],im];function
is(f,b,e,a,d,c){return[5,[1,b],a]}var
iu=[0,a(m[12],it)],iv=[1,[6,k[15][7]]],ix=[0,a(m[12],iw)],iy=[6,k[15][1]],iA=[0,[0,[0,[0,[0,[0,[0,0,[0,a(m[12],iz)]],iy],ix],iv],iu],is],ir];function
iB(f,b,e,a,d,c){return[5,[0,b],a]}var
iD=[0,a(m[12],iC)],iE=[6,k[17][19]],iG=[0,a(m[12],iF)],iH=[6,k[15][1]],iJ=[0,[0,[0,[0,[0,[0,[0,0,[0,a(m[12],iI)]],iH],iG],iE],iD],iB],iA];function
iK(a,c,b){return[7,a]}var
iL=[6,k[15][1]],iN=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(m[12],iM)]],iL],iK],iJ]],hz]];e(k[23],aU,0,iN);function
iO(h,g,f,c){var
b=a(d[1],iP);return e(q[3],0,0,b)}function
iQ(h,g,f,c){var
b=a(d[1],iR);return e(q[3],0,0,b)}function
iS(c,b,a){return aT}P($[1],be,iS,iQ,iO);function
b1(b){var
c=e(d[53],d[43],aT,b);return a(d[60],c)}var
ao=a(g[3],iT),iU=a(g[4],ao),b2=e(k[13],k[9],iV,iU),iW=0,iX=0;function
iY(d,a,c,b){return a}var
i0=[0,a(m[12],iZ)],i2=[2,[6,aU],[0,a(m[12],i1)]],i4=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(m[12],i3)]],i2],i0],iY],iX]],iW]];e(k[23],b2,0,i4);function
i5(h,g,f,c){var
b=a(d[1],i6);return e(q[3],0,0,b)}function
i7(h,g,f,c){var
b=a(d[1],i8);return e(q[3],0,0,b)}function
i9(c,b,a){return b1}P($[1],ao,i9,i7,i5);var
i_=0,jd=[0,[0,0,function(b){return b?a(n[2],i$):function(g){var
b=a(d[25],ja);c(am[13],0,b);var
e=C[7][1];function
f(e,b){var
f=a(G[5],b[2]),g=a(d[1],jb),h=a(d[16],0),i=a(G[5],b[1]),j=a(d[1],jc),k=a(d[16],0),l=a(T[20],e),m=a(O[12],l),n=c(d[13],m,k),o=c(d[13],n,j),p=c(d[13],o,i),q=c(d[13],p,h),r=c(d[13],q,g),s=c(d[13],r,f),t=c(d[29],2,s);return c(am[13],0,t)}return c(T[24][10],f,e)}}],i_],jf=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
h=e[1],i=d[1],j=b[1],k=a(g[4],l[4]),m=c(g[8],k,j),o=a(g[4],l[8]),p=c(g[8],o,i),q=a(g[18],ao),r=a(g[4],q),f=c(g[8],r,h);return function(n){var
c=f?f[1]:0,b=a(C[4],c),d=b[8],e=b[7],g=b[6],h=b[3],i=b[2],j=b[1],k=[0,b[4],b[5]],l=a(C[6],p);return lc(C[5],m,l,i,j,h,k,g,e,d)}}}}return a(n[2],je)}],jd];function
jg(b,a){return e(b3[1],a[1],[0,jh,b],a[2])}c(ap[80],jg,jf);var
ji=0,jk=[0,function(b){return b?a(n[2],jj):function(a){return aq[5]}},ji],jm=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return aq[6]}}}return a(n[2],jl)},jk];function
jn(b,a){return c(aq[3],[0,jo,b],a)}c(ap[80],jn,jm);var
jq=[5,[6,a(k[12],ao)]],jr=a(g[18],ao),js=a(g[4],jr),jt=[0,[1,j[4],js,jq],0],ju=[6,a(k[12],l[8])],jv=a(g[4],l[8]),jx=[0,jw,[0,[1,j[4],jv,ju],jt]],jy=[6,a(k[12],l[4])],jz=a(g[4],l[4]),jC=[0,[0,jB,[0,jA,[0,[1,j[4],jz,jy],jx]]],jp];function
jD(b,a){return e(b4[1],[0,jE,b],0,a)}c(ap[80],jD,jC);var
jF=0,jH=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],j=b[1],k=a(g[6],l[14]),m=c(r[2][7],k,j),o=a(g[17],l[8]),p=a(g[6],o),q=c(r[2][7],p,h),s=a(g[17],l[8]),t=a(g[6],s),u=c(r[2][7],t,f);return function(c){var
b=a(i[17][93],u);return P(C[8],m,q,b[2],b[1])}}}}return a(n[2],jG)},jF],jI=a(i[19][12],jH);e(as[9],0,[0,I,jJ],jI);function
jK(q){var
i=a(h[1][6],jL),b=l[8],f=0,g=0;if(0===b[0]){var
k=[0,jN,[0,[1,j[4],[0,[5,[0,b[1]]]],i],g]],m=a(h[1][6],jO),d=l[8];if(0===d[0]){var
n=[0,jQ,[0,[1,j[4],[2,[5,[0,d[1]]]],m],k]],o=a(h[1][6],jR),e=l[14],p=0;if(0===e[0])return c(aS[4],[0,I,jU],[0,[0,jT,[0,[1,j[4],[6,[0,e[1]],p],o],n]],f]);throw[0,H,jS]}throw[0,H,jP]}throw[0,H,jM]}c(ax[19],jK,I);function
bf(b){if(0===b[0])return aT(b[1]);var
e=c(d[46],O[23],b[1]),f=a(d[1],jV);return c(d[13],f,e)}var
bg=a(g[3],jW),jX=a(g[4],bg),bh=e(k[13],k[9],jY,jX),jZ=0,j0=0,j1=[0,[0,[0,0,[6,aU]],function(a,b){return[0,a]}],j0];function
j2(a,c,b){return[1,a]}var
j3=[6,k[15][1]],j5=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(m[12],j4)]],j3],j2],j1]],jZ]];e(k[23],bh,0,j5);function
j6(h,g,f,c){var
b=a(d[1],j7);return e(q[3],0,0,b)}function
j8(h,g,f,c){var
b=a(d[1],j9);return e(q[3],0,0,b)}function
j_(c,b,a){return bf}P($[1],bg,j_,j8,j6);function
b5(b){var
c=e(d[53],d[43],bf,b);return a(d[60],c)}var
ar=a(g[3],j$),ka=a(g[4],ar),b6=e(k[13],k[9],kb,ka),kc=0,kd=0;function
ke(d,a,c,b){return a}var
kg=[0,a(m[12],kf)],ki=[2,[6,bh],[0,a(m[12],kh)]],kk=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(m[12],kj)]],ki],kg],ke],kd]],kc]];e(k[23],b6,0,kk);function
kl(h,g,f,c){var
b=a(d[1],km);return e(q[3],0,0,b)}function
kn(h,g,f,c){var
b=a(d[1],ko);return e(q[3],0,0,b)}function
kp(c,b,a){return b5}P($[1],ar,kp,kn,kl);var
kq=0,kv=[0,[0,0,function(b){return b?a(n[2],kr):function(g){var
b=a(d[25],ks);c(am[13],0,b);var
e=C[11][1];function
f(e,b){var
f=a(G[5],b[2]),g=a(d[1],kt),h=a(d[16],0),i=a(G[5],b[1]),j=a(d[1],ku),k=a(d[16],0),l=a(T[20],e),m=a(O[12],l),n=c(d[13],m,k),o=c(d[13],n,j),p=c(d[13],o,i),q=c(d[13],p,h),r=c(d[13],q,g),s=c(d[13],r,f),t=c(d[29],2,s);return c(am[13],0,t)}return c(T[24][10],f,e)}}],kq],kx=[0,[0,0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
h=e[1],i=d[1],j=b[1],k=a(g[4],l[4]),m=c(g[8],k,j),o=a(g[4],l[8]),p=c(g[8],o,i),q=a(g[18],ar),r=a(g[4],q),f=c(g[8],r,h);return function(o){var
c=f?f[1]:0,b=a(C[9],c),d=b[9],e=b[8],g=b[7],h=b[4],i=b[3],j=b[2],k=b[1],l=[0,b[5],b[6]],n=a(C[6],p);return ld(C[10],m,n,j,k,h,i,l,g,e,d)}}}}return a(n[2],kw)}],kv];function
ky(b,a){return e(b3[1],a[1],[0,kz,b],a[2])}c(ap[80],ky,kx);var
kA=0,kC=[0,function(b){return b?a(n[2],kB):function(a){return aq[5]}},kA],kE=[0,function(b){if(b){var
c=b[2];if(c){var
d=c[2];if(d)if(!d[2])return function(a){return aq[6]}}}return a(n[2],kD)},kC];function
kF(b,a){return c(aq[3],[0,kG,b],a)}c(ap[80],kF,kE);var
kI=[5,[6,a(k[12],ar)]],kJ=a(g[18],ar),kK=a(g[4],kJ),kL=[0,[1,j[4],kK,kI],0],kM=[6,a(k[12],l[8])],kN=a(g[4],l[8]),kP=[0,kO,[0,[1,j[4],kN,kM],kL]],kQ=[6,a(k[12],l[4])],kR=a(g[4],l[4]),kU=[0,[0,kT,[0,kS,[0,[1,j[4],kR,kQ],kP]]],kH];function
kV(b,a){return e(b4[1],[0,kW,b],0,a)}c(ap[80],kV,kU);var
kX=0,kZ=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],j=b[1],k=a(g[6],l[14]),m=c(r[2][7],k,j),o=a(g[17],l[8]),p=a(g[6],o),q=c(r[2][7],p,h),s=a(g[17],l[8]),t=a(g[6],s),u=c(r[2][7],t,f);return function(c){var
b=a(i[17][93],u);return P(C[12],m,q,b[2],b[1])}}}}return a(n[2],kY)},kX],k0=a(i[19][12],kZ);e(as[9],0,[0,I,k1],k0);function
k2(p){var
i=a(h[1][6],k3),b=l[8],f=0,g=0;if(0===b[0]){var
k=[0,k5,[0,[1,j[4],[0,[5,[0,b[1]]]],i],g]],m=a(h[1][6],k6),d=l[8];if(0===d[0]){var
n=[0,k8,[0,[1,j[4],[2,[5,[0,d[1]]]],m],k]],o=a(h[1][6],k9),e=l[14];if(0===e[0])return c(aS[4],[0,I,la],[0,[0,k$,[0,[1,j[4],[5,[0,e[1]]],o],n]],f]);throw[0,H,k_]}throw[0,H,k7]}throw[0,H,k4]}c(ax[19],k2,I);var
b7=[0,I,aT,be,aU,b1,ao,b2,bf,bg,bh,b5,ar,b6];bi(377,b7,"Newring_plugin.G_newring");bi(378,[0,C,b7],"Newring_plugin");return});
