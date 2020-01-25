function(eT){"use strict";var
br="mkapp2",aJ="UnOp",bA="mkrel",bB="target_prop",h=246,bz="micromega",P=104,bo=" ",N="Zify",bq="mkapp",aH=142,bv="Spec",O="Show",aG="",aF="BinRel",bp="InjTyp",aI="BinOp",bu="injprop_ok",bs="ZifyClasses.",bt="mkprop_op",bn="Saturate",y="Add",aE="CstOp",by="mkapp0",m=250,bw="mkuprop_op",bx="Coq",q=eT.jsoo_runtime,f=q.caml_check_bound,eR=q.caml_list_of_js_array,b=q.caml_new_string,l=q.caml_obj_tag,aD=q.caml_register_global,H=q.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):q.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):q.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):q.caml_call_gen(a,[b,c,d])}function
M(a,b,c,d,e){return a.length==4?a(b,c,d,e):q.caml_call_gen(a,[b,c,d,e])}function
ap(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):q.caml_call_gen(a,[b,c,d,e,f])}function
bm(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):q.caml_call_gen(a,[b,c,d,e,f,g])}function
eS(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):q.caml_call_gen(a,[b,c,d,e,f,g,h,i])}var
g=q.caml_get_global_data(),an=b("zify_plugin"),v=g.Not_found,c=g.EConstr,k=g.CamlinternalLazy,a$=g.Stdlib__array,p=g.Stdlib__list,j=g.Tacmach,n=g.Tacticals,K=g.Tacred,A=g.Names,t=g.Tactics,G=g.Proofview,bf=g.Ltac_plugin__Tacinterp,$=g.Coqlib,ax=g.Equality,w=g.Global,i=g.Pp,T=g.CErrors,L=g.Stdlib,B=g.CClosure,aL=g.Printer,aP=g.Reductionops,aK=g.Retyping,a8=g.Context,aq=g.Constr,V=g.Evd,aV=g.Feedback,aT=g.Constrintern,aU=g.Lib,aS=g.Mod_subst,Z=g.Libobject,ar=g.Summary,ao=g.Vernacextend,o=g.Attributes,x=g.Stdarg,u=g.Genarg,bl=g.Ltac_plugin__Tacentries,db=g.Sorts,cC=g.UnivGen,bD=g.Stdlib__map,cW=g.Stdlib__hashtbl,dm=g.Mltop,el=g.Ltac_plugin__Tacarg;aD(88,[0,0,0],"Zify_plugin");var
dk=b("__sat"),di=[0,b(bx),[0,b(bz),[0,b("ZifyClasses"),0]]],dj=[0,b(bx),[0,b(bz),[0,b("ZifyInst"),0]]],dg=b("is_progress_rewrite: not a rewrite"),df=b("is_progress_rewrite: not even an application"),dd=b("__zify"),de=[0,0,2],dc=b("Missing injection for type "),cX=b("cstr"),cT=eR([b("source_prop"),b(bB),b("uop_iff"),b("op_iff"),b(bw),b("TUOp"),b("inj_ok"),b("TRInj"),b("inj"),b("source"),b(bu),b("TR"),b("TBOp"),b("TCst"),b("target"),b(bA),b(br),b(bq),b(by),b(bt)]),cS=b("iff"),cR=b(bu),cQ=b(bB),cP=b("iff_refl"),cO=b("mkinjprop"),cN=b(bw),cM=b(bt),cL=b(bA),cK=b("eq_refl"),cJ=b("mkinjterm"),cI=b(by),cH=b(bq),cG=b(br),cE=b(bs),cF=b(" should be a constant"),cB=b(bs),cA=b(bo),cy=b(aG),cz=b(aG),cl=b(bo),ck=b(aG),cg=b(" which should be of the form  [F X1 .. Xn]"),ch=b(". It has type "),ci=b(": Cannot register term "),cf=b("Cannot register term "),cj=b("register-zify-"),bE=b(" should be an application i.e. (c a1 ... an)"),bF=b("The hnf of term "),bG=b("zify_table"),bH=b("zify_saturate"),bI=b("EInj"),bN=b(aI),bS=b(aE),bX=b(aJ),b2=b(aF),b7=b("PropBinOp"),ca=b("PropUnOp"),cm=b(bn),cq=b("zify_Spec"),ct=b("register-zify-Spec"),c6=b("y"),c_=b("x"),dr=b(bn),ds=b(y),dw=b("UnOpSpec"),dx=b(y),dB=b("BinOpSpec"),dC=b(y),dG=b(bv),dH=b(y),dL=b("PropUOp"),dM=b(y),dQ=b("PropOp"),dR=b(y),dV=b(aF),dW=b(y),d0=b(aE),d1=b(y),d5=b(aJ),d6=b(y),d_=b(aI),d$=b(y),ed=b(bp),ee=b(y),ei=b("DECLAREINJECTION"),em=b("iter_specs"),eo=b("ITER"),eq=[0,b("saturate"),0],es=[0,b("zify_op"),0],eu=b("TRANS"),ex=[0,b(O),[0,b(N),[0,b(bv),0]]],eA=[0,b(O),[0,b(N),[0,b(aF),0]]],eD=[0,b(O),[0,b(N),[0,b(aE),0]]],eG=[0,b(O),[0,b(N),[0,b(aJ),0]]],eJ=[0,b(O),[0,b(N),[0,b(aI),0]]],eM=[0,b(O),[0,b(N),[0,b(bp),0]]],eQ=b("ZifyPrint");function
Q(b,a,c){var
d=ap(aK[2],0,0,b,a,c);return e(K[16],b,a,d)}var
R=c[aH][1];function
I(c,b,a){return e(aL[11],c,b,a)}function
aM(e,d){var
b=d;for(;;){if(b){var
f=b[2],c=a(e,b[1]);if(c)return c[1];var
b=f;continue}throw v}}var
bC=[0,function(c,b){var
e=a(R,b),f=a(R,c);return d(aq[87],f,e)}],X=a(bD[1],bC);function
S(b,a){try{var
c=d(X[27],b,a);return c}catch(a){a=H(a);if(a===v)return 0;throw a}}function
aN(b,c,a){var
d=[0,c,S(b,a)];return e(X[4],b,d,a)}var
Y=X[1];function
aO(c,b,a){function
d(d,b,a){function
f(b,a){return e(c,d,a,b)}return e(p[20],f,a,b)}return e(X[13],d,b,a)}var
C=e(ar[4],0,bG,Y),aQ=e(ar[4],0,bH,Y),U=[0,Y],aR=[0,Y];function
bJ(a){return[2,a]}function
bK(a){return 2===a[0]?[0,a[1]]:0}function
bL(d,m,a){var
g=f(a,1)[2],h=f(a,0)[1],b=e(c[P],d,h,g),i=b?0:[0,f(a,4)[5]],j=f(a,3)[4],k=f(a,2)[3],l=f(a,1)[2];return[0,b,f(a,0)[1],l,k,j,i]}var
bM=0;function
bO(k,j,a){var
b=f(a,9)[10],c=f(a,7)[8],d=f(a,6)[7],e=f(a,5)[6],g=f(a,3)[4],h=f(a,2)[3],i=f(a,1)[2];return[0,f(a,0)[1],i,h,g,e,d,c,b]}var
bP=4;function
bQ(a){return[4,a]}function
bR(a){return 4===a[0]?[0,a[1]]:0}function
bT(a){return[6,a]}function
bU(a){return 6===a[0]?[0,a[1]]:0}function
bV(e,d,a){var
b=f(a,3)[4],c=f(a,1)[2];return[0,f(a,0)[1],c,b]}var
bW=2;function
bY(a){return[5,a]}function
bZ(a){return 5===a[0]?[0,a[1]]:0}function
b0(i,h,a){var
b=f(a,6)[7],c=f(a,5)[6],d=f(a,4)[5],e=f(a,2)[3],g=f(a,1)[2];return[0,f(a,0)[1],g,e,d,c,b]}var
b1=3;function
b3(a){return[3,a]}function
b4(a){return 3===a[0]?[0,a[1]]:0}function
b5(g,e,a){var
b=f(a,4)[5],c=f(a,3)[4],d=f(a,1)[2];return[0,f(a,0)[1],d,c,b]}var
b6=2;function
b8(a){return[0,a]}function
b9(a){return 0===a[0]?[0,a[1]]:0}function
b_(c,a,b){return a}var
b$=0;function
cb(a){return[1,a]}function
cc(a){return 1===a[0]?[0,a[1]]:0}function
cd(c,a,b){return a}var
ce=0;function
D(b){function
j(k){var
g=k[2],f=k[1],l=a(w[2],0),m=M(aP[17],B[4],l,f,g),h=d(c[3],f,m);if(9===h[0]){var
j=h[2];if([0,j])return e(b[6],f,g,j);var
s=I(a(w[2],0),f,g),t=a(i[49],s),u=d(L[17],cf,t);return a(L[3],u)}var
n=a(i[3],bE),o=I(a(w[2],0),f,g),p=a(i[3],bF),q=d(i[12],p,o),r=d(i[12],q,n);throw e(T[5],0,0,r)}function
k(h,e,f){var
g=d(c[3],h,e);if(9===g[0]){var
k=g[1],l=b[2][1],m=aN(k,[0,[0,e],a(b[3],f)],l);b[2][1]=m;return 0}var
i=b[2][1],j=aN(e,[0,[1,e],a(b[3],f)],i);b[2][1]=j;return 0}function
g(q,g,p){var
h=a(c[9],p),l=Q(q,g,h),m=d(c[3],g,l);if(9===m[0]){var
o=b[5],B=f(m[2],o)[1+o];return k(g,B,[0,h,j([0,g,h])])}var
n=a(w[2],0),r=a(i[3],cg),s=I(n,g,l),t=a(i[3],ch),u=I(n,g,h),v=a(i[3],ci),x=d(i[12],v,u),y=d(i[12],x,t),z=d(i[12],y,s),A=d(i[12],z,r);throw e(T[5],0,0,A)}function
m(c){var
d=c[2],b=a(w[2],0);return g(b,a(V[17],b),d)}var
n=[0,function(a){return d(aS[46],a[1],a[2])}],o=d(L[17],cj,b[1]),p=e(Z[18],o,m,n),h=a(Z[4],p);function
q(g){var
b=a(w[2],0),i=a(V[17],b),f=e(aT[13],b,i,g),j=a(h,e(c[5],0,f[1],f[2]));d(aU[11],0,j);return 0}function
l(g){var
e=a(w[2],0),h=a(V[17],e),c=a(i[3],ck),f=b[2][1];return aO(function(n,g,f){var
c=g[1];if(a(b[4],g[2])){var
j=a(i[3],cl),k=0===c[0]?c[1]:c[1],l=I(e,h,k),m=d(i[12],l,j);return d(i[12],m,f)}return f},f,c)}return[0,j,k,g,h,q,l,function(b){var
a=l(0);return d(aV[6],0,a)}]}var
aW=D([0,bI,C,bJ,bK,bM,bL]);function
cn(a){return[7,a]}function
co(a){return 7===a[0]?[0,a[1]]:0}function
cp(e,d,a){var
b=f(a,5)[6],c=f(a,3)[4];return[0,f(a,2)[3],c,b]}var
aX=D([0,bN,C,bQ,bR,bP,bO]),aY=D([0,bX,C,bY,bZ,b1,b0]),aZ=D([0,bS,C,bT,bU,bW,bV]),a0=D([0,b2,C,b3,b4,b6,b5]),a1=D([0,b7,C,b8,b9,b$,b_]),a2=D([0,ca,C,cb,cc,ce,cd]),a3=D([0,cm,aQ,cn,co,1,cp]);function
a4(a){U[1]=C[1];aR[1]=aQ[1];return 0}var
_=e(ar[4],0,cq,0);function
cr(b){var
d=_[1];_[1]=[0,a(c[9],b[2]),d];return 0}var
cs=[0,function(a){return d(aS[46],a[1],a[2])}],cu=e(Z[18],ct,cr,cs),cv=a(Z[4],cu);function
cw(g){var
b=a(w[2],0),f=a(V[17],b),h=e(aT[13],b,f,g)[2],i=a(cv,e(c[5],0,f,h));d(aU[11],0,i);return 0}function
cx(m){var
b=a(w[2],0),g=a(V[17],b),h=_[1],j=a(i[3],cz);function
k(k,j){var
l=a(i[3],cA),h=Q(b,g,j),e=d(c[3],g,h),m=9===e[0]?I(b,g,f(e[2],2)[3]):a(i[3],cy),n=d(i[12],m,l);return d(i[12],n,k)}var
l=e(p[20],k,j,h);return d(aV[7],0,l)}function
r(b){var
e=d(L[17],cB,b),f=a($[2],e),g=a(cC[15],f);return a(c[9],g)}function
cD(f){var
b=d(L[17],cE,f),c=a($[2],b);if(1===c[0])return c[1];var
g=a(i[3],cF),h=a(i[3],b),j=d(i[12],h,g);return e(T[2],0,0,j)}var
aa=[h,function(a){return r(cG)}],E=[h,function(a){return r(cH)}],ab=[h,function(a){return r(cI)}],ac=[h,function(a){return r(cJ)}],ad=[h,function(a){return r(cK)}],F=[h,function(a){return r(cL)}],ae=[h,function(a){return r(cM)}],af=[h,function(a){return r(cN)}],ag=[h,function(a){return r(cO)}],ah=[h,function(a){return r(cP)}],ai=[h,function(a){return r(cQ)}],s=[h,function(a){return r(cR)}],aj=[h,function(a){return r(cS)}],ak=[h,function(a){return d(p[19],cD,cT)}];function
cU(b){var
c=a(R,b);return a(aq[114],c)}var
cV=[0,function(c,b){var
e=a(R,b),f=a(R,c);return d(aq[81],f,e)},cU],z=a(cW[25],cV),as=a(z[1],10);function
a5(g,p,b,d){if(1-g){var
i=b[6];if(i)e(z[5],as,d,i[1])}var
j=a(c[23],[0,b[4],[0,d]]);if(g)var
q=a(w[2],0),f=e(K[19],q,p,j);else
var
f=j;var
r=[0,b[3],f],n=l(ad),s=m===n?ad[1]:h===n?a(k[2],ad):ad,t=a(c[23],[0,s,r]),u=[0,b[2],b[3],b[4],d,f,t],o=l(ac),v=m===o?ac[1]:h===o?a(k[2],ac):ac;return a(c[23],[0,v,u])}function
at(a){return 1===a[0]?1:0}function
a6(a){if(2===a[0])return 0;var
b=a[2];return a[1][1]?[0,b]:0}function
J(b,a){switch(a[0]){case
0:return a5(0,b,a[1],a[2]);case
1:return a5(1,b,a[1],a[2]);default:return a[1]}}function
cY(j,t,i,g,q,b,f,d){function
n(r){var
i=J(j,f),n=J(j,d),o=[0,b[1],b[2],b[3],b[4],g,b[5],b[6],b[7],q,i,n],e=l(aa),p=m===e?aa[1]:h===e?a(k[2],aa):aa;return[2,a(c[23],[0,p,o])]}if(i[1]){switch(f[0]){case
0:var
r=f[2];if(2===d[0])var
e=0;else
var
p=d[2],o=r,e=1;break;case
1:var
s=f[2];if(2===d[0])var
e=0;else
var
p=d[2],o=s,e=1;break;default:var
e=0}return e?[0,i,a(c[23],[0,g,[0,o,p]])]:n(0)}return n(0)}function
cZ(i,v,p,g,f,d){var
b=g[2],j=g[1];if(e(c[P],i,b[6],f)){if(2===d[0]){var
q=[0,b[1],b[2],b[3],f,b[4],b[5],j,d[1]],n=l(E),r=m===n?E[1]:h===n?a(k[2],E):E;return[2,a(c[23],[0,r,q])]}return[0,p,a(c[23],[0,f,[0,d[2]]])]}var
s=J(i,d),t=[0,b[1],b[2],b[3],f,b[4],b[5],j,s],o=l(E),u=m===o?E[1]:h===o?a(k[2],E):E;return[2,a(c[23],[0,u,t])]}function
c0(e,d,c){var
a=S(c,U[1]);if(a){var
b=a[1][2];if(2===b[0])return b[1];throw v}throw v}function
a7(b){var
c=a(A[1][6],b),e=a(A[2][1],c);return d(a8[4],e,0)}var
c1=a(c[10],2),c2=a(c[10],2),c3=[0,d(a8[4],0,0),c2,c1],c4=a(c[20],c3),c5=c[16],c7=[0,a7(c6),c5,c4],c8=a(c[21],c7),c9=c[16],c$=[0,a7(c_),c9,c8],da=a(c[21],c$);function
au(d,c,b){var
e=M(aK[3],0,d,c,b);return a(db[10],e)}function
a9(k,b,j){var
a=d(c[3],b,j);switch(a[0]){case
6:var
h=a[3],i=a[2],o=a[1],l=au(k,b,i);if(l){var
m=au(d(c[122],[0,o,i],k),b,h);if(m){var
n=0===o[1]?1:0;if(n)var
g=n,f=0;else
var
p=e(c[121][13],b,1,h),f=1}else
var
g=m,f=0}else
var
g=l,f=0;if(!f)var
p=g;if(p)return[0,da,[0,i,h]];break;case
9:return[0,a[1],a[2]]}return[0,j,[0]]}function
a_(l,k,j,i,h){var
f=h[2],g=h[1];function
b(b,d){var
g=[0,j,e(a$[7],i,0,i.length-1-d|0)],h=a(c[23],g);return bm(aP[84],0,0,l,k,h,b)?[0,[0,f,b]]:0}if(0===g[0]){var
d=g[1];switch(f[0]){case
0:return b(d,2);case
1:return b(d,1);case
3:return b(d,2);case
4:return b(d,2);case
5:return b(d,1);case
6:return b(d,0);default:return 0}}return[0,[0,f,g[1]]]}function
al(i,b,w){var
x=c0(i,b,w[2]),e=x[2],r=x[1],g=w[1];if(d(c[64],b,g))return[1,e,g];var
y=a9(i,b,g),j=y[2],z=y[1];try{var
O=S(z,U[1]),A=aM(function(a){return a_(i,b,z,j,a)},O),n=A[2],o=A[1],s=j.length-1;switch(o[0]){case
4:var
B=o[1],t=B[2],C=s-2|0,P=B[1],Q=t[1],D=al(i,b,[0,f(j,C)[1+C],Q]),E=s-1|0,R=t[2],F=al(i,b,[0,f(j,E)[1+E],R]);if(at(D))if(at(F))if(d(c[64],b,n))var
G=[1,e,g],q=1;else
var
q=0;else
var
q=0;else
var
q=0;if(!q)var
G=cY(b,r,e,n,P,t,D,F);var
p=G;break;case
5:var
I=o[1],J=I[2],K=s-1|0,T=I[1],V=J[1],L=al(i,b,[0,f(j,K)[1+K],V]);if(at(L))if(d(c[64],b,n))var
M=[1,e,g],u=1;else
var
u=0;else
var
u=0;if(!u)var
M=cZ(b,r,e,[0,T,J],n,L);var
p=M;break;case
6:var
W=[0,e[2],e[3],g,r,o[1][1]],N=l(ab),X=m===N?ab[1]:h===N?a(k[2],ab):ab,p=[2,a(c[23],[0,X,W])];break;default:var
p=[0,e,g]}return p}catch(a){a=H(a);if(a===v)return[0,e,g];throw a}}function
ba(g,f,c){try{var
b=al(g,f,c);return b}catch(b){b=H(b);if(b===v){var
h=e(aL[12],g,f,c[2]),j=a(i[3],dc),k=d(i[12],j,h);throw e(T[5],0,0,k)}throw b}}function
av(b){if(0===b[0])return b[1];var
d=b[1],e=l(ah),g=[0,d],i=m===e?ah[1]:h===e?a(k[2],ah):ah,j=[0,d,d,a(c[23],[0,i,g])],f=l(ag),n=m===f?ag[1]:h===f?a(k[2],ag):ag;return a(c[23],[0,n,j])}function
am(i,b,g){var
w=a9(i,b,g),j=w[2],x=w[1];try{var
aa=S(x,U[1]),y=aM(function(a){return a_(i,b,x,j,a)},aa),n=y[2],p=y[1],o=j.length-1;switch(p[0]){case
0:var
ab=p[1][1];try{var
A=o-2|0,B=am(i,b,f(j,A)[1+A]),C=o-1|0,D=am(i,b,f(j,C)[1+C]);if(1===B[0])if(1===D[0])var
G=[1,g],t=1;else
var
t=0;else
var
t=0;if(!t)var
ac=av(B),ad=[0,n,ab,ac,av(D)],E=l(ae),ag=m===E?ae[1]:h===E?a(k[2],ae):ae,G=[0,a(c[23],[0,ag,ad])];var
z=G}catch(a){a=H(a);if(a!==v)throw a;var
z=[1,g]}var
q=z;break;case
1:var
ah=p[1][1];try{var
K=o-1|0,L=am(i,b,f(j,K)[1+K]);if(0===L[0])var
ai=[0,n,ah,av(L)],M=l(af),aj=m===M?af[1]:h===M?a(k[2],af):af,N=[0,a(c[23],[0,aj,ai])];else
var
N=[1,g];var
I=N}catch(a){a=H(a);if(a!==v)throw a;var
I=[1,g]}var
q=I;break;case
3:var
O=p[1],d=O[2],Q=O[1];try{var
T=o-2|0,ak=d[1],r=ba(i,b,[0,f(j,T)[1+T],ak]),V=o-1|0,al=d[1],s=ba(i,b,[0,f(j,V)[1+V],al]);if(e(c[P],b,n,d[4])){var
W=a6(r),X=a6(s);if(W)if(X)var
Y=[1,a(c[23],[0,n,[0,W[1],X[1]]])],u=1;else
var
u=0;else
var
u=0;if(!u)var
an=J(b,r),ao=J(b,s),ap=[0,d[1],d[2],n,d[3],Q,an,ao],_=l(F),aq=m===_?F[1]:h===_?a(k[2],F):F,Y=[0,a(c[23],[0,aq,ap])];var
Z=Y}else
var
ar=J(b,r),as=J(b,s),at=[0,d[1],d[2],n,d[3],Q,ar,as],$=l(F),au=m===$?F[1]:h===$?a(k[2],F):F,Z=[0,a(c[23],[0,au,at])];var
R=Z}catch(a){a=H(a);if(a!==v)throw a;var
R=[1,g]}var
q=R;break;default:var
q=[1,g]}return q}catch(a){a=H(a);if(a===v)return[1,g];throw a}}function
aw(i,g,f,b){var
q=0,n=U[1],o=aO(function(h,g,e){var
a=g[2];switch(a[0]){case
0:var
b=a[1][1];break;case
1:var
b=a[1][1];break;case
2:var
b=a[1][1];break;case
3:var
b=a[1][1];break;case
4:var
b=a[1][1];break;case
5:var
b=a[1][1];break;case
6:var
b=a[1][1];break;default:var
b=a[1][1]}return[0,d(c[83],f,b)[1],e]},n,q),t=i?M(K[12],[0,[0,0,[0,i[1]]],0],g,f,b):b,j=l(ak),u=m===j?ak[1]:h===j?a(k[2],ak):ak,v=d(p[12],u,o),r=d(p[19],B[3][8],v),s=a(B[3][15],[0,B[3][1],[0,B[3][4],[0,B[3][5],[0,B[3][6],[0,B[3][7],r]]]]]);return e(a(K[15],s),g,f,t)}function
bb(c,b,a){if(au(c,b,a)){var
d=am(c,b,a);return 0===d[0]?[0,d[1]]:0}return 0}function
bc(b,v,j){var
f=d(c[3],b,j);if(9===f[0]){var
g=f[2];if(2===g.length-1){var
o=f[1],p=g[1],q=g[2],n=l(aj),r=m===n?aj[1]:h===n?a(k[2],aj):aj;if(e(c[P],b,r,o))return 1-e(c[P],b,p,q);var
s=I(a(w[2],0),b,j),t=a(i[3],dg),u=d(i[12],t,s);return e(T[2],0,0,u)}}return a(L[3],df)}function
dh(b){a($[12],di);a($[12],dj);a4(0);var
f=a(j[32][3],b),g=a(j[32][4],b),i=bb(g,f,a(j[32][5],b)),H=a(j[32][13],b),x=a(p[9],H),y=0;function
B(b,a){var
c=a[2],e=a[1],d=bb(g,f,c);return d?[0,[0,e,c,d[1]],b]:b}var
C=e(p[20],B,y,x),q=0;function
r(c,b,a){return[0,[0,c,b],a]}var
u=e(z[14],r,as,q);a(z[2],as);function
v(b){var
h=a(j[32][3],b),f=a(z[1],20),g=a(j[32][13],b);function
i(a){return e(z[5],f,a[2],0)}d(p[15],i,g);var
k=n[57][2];function
l(k,b){var
o=b[2],p=b[1];function
g(i){var
b=a(j[32][4],i),g=a(c[23],[0,o,[0,p]]),k=Q(b,h,g);if(d(z[11],f,k))return n[57][2];var
l=a(A[1][6],cX),m=[0,e(t[13],A[1][10][1],l,b)];return d(t[aH],m,g)}var
i=a(G[67][7],g);return d(n[57][3],i,k)}return e(p[20],l,k,u)}var
w=a(G[67][7],v);function
I(b){var
q=b[3],f=b[1],z=b[2];function
g(g){var
i=a(j[32][4],g),b=a(j[32][3],g),o=l(s),u=[0,q],v=m===o?s[1]:h===o?a(k[2],s):s,p=aw(0,i,b,a(c[23],[0,v,u]));if(bc(b,z,Q(i,b,p))){var
w=0,r=function(r){var
g=a(j[32][4],r),u=a(A[1][6],dd),b=e(t[13],A[1][10][1],u,g),i=e(t[13],A[1][10][1],f,g),v=a(t[76],[0,f,0]),w=[0,a(n[57][24],v),0],x=[0,a(t[76],[0,b,0]),w],y=[0,i,0],z=[0,b];function
B(a,b,c){return aw(z,a,b,c)}var
C=[0,M(t[49],1,0,B,y),x],D=a(c[11],f),E=a(t[46],D),F=[0,a(c[11],b)],o=l(s),G=m===o?s[1]:h===o?a(k[2],s):s,H=a(c[23],[0,G,F]),I=d(ax[5],0,H),J=d(n[57][3],I,E),K=[0,a(c[11],b)],p=l(ai),L=m===p?ai[1]:h===p?a(k[2],ai):ai,N=a(c[23],[0,L,K]),O=[0,e(t[140],[0,i],N,J),C],P=[0,ap(t[146],0,[0,b],q,0,de),O];return a(n[57][22],P)},x=[0,a(G[67][7],r),w],y=[0,eS(ax[9],1,0,1,0,0,f,p,0),x];return a(n[57][26],y)}return n[57][2]}return a(G[67][7],g)}var
J=d(p[19],I,C),K=a(n[57][22],J),o=d(n[57][3],K,w);if(i){var
F=i[1],D=function(b){var
i=a(j[32][5],b),e=a(j[32][4],b),d=a(j[32][3],b),f=l(s),o=[0,F],p=m===f?s[1]:h===f?a(k[2],s):s,g=aw(0,e,d,a(c[23],[0,p,o]));return bc(d,i,Q(e,d,g))?bm(ax[3],1,0,1,0,0,g):n[57][2]},E=a(G[67][7],D);return d(n[57][3],E,o)}return o}var
bd=a(G[67][7],dh);function
be(h){var
b=_[1],c=0;function
f(c,b){var
e=[0,a(bf[2][1],b),0];return[0,d(bf[2][8],h,e),c]}var
g=e(p[20],f,c,b);return a(n[57][22],g)}function
bg(f,b,a){try{var
g=function(a){return e(c[P],f,b,a[2])},h=[0,d(p[33],g,a)[1]];return h}catch(a){a=H(a);if(a===v)return 0;throw a}}function
dl(g){a4(0);var
h=a(z[1],20),i=a(j[32][5],g),k=a(j[32][13],g),o=a(j[32][3],g);a(j[32][4],g);function
b(n){var
f=n;for(;;){var
a=d(c[3],o,f);switch(a[0]){case
6:var
q=a[3],r=a[2];if(0===a[1][1]){b(r);var
f=q;continue}break;case
9:var
i=a[2],j=a[1];b(j);d(a$[13],b,i);if(2===i.length-1){var
k=S(j,aR[1]),l=0,m=function(a,c){var
b=c[2];return 7===b[0]?[0,b[1],a]:a},g=e(p[20],m,l,k);if(0===g)return 0;var
s=function(a){return e(z[5],h,f,a[2])};return d(p[15],s,g)}return 0}return 0}}b(i);function
l(a){return b(a[2])}d(p[15],l,k);var
m=0;function
q(H,k,g){function
b(h){var
b=a(j[32][3],h),i=a(j[32][4],h),l=a(j[32][13],h),m=d(c[3],b,H);if(9===m[0]){var
g=m[2];if(2===g.length-1){var
q=[0,f(g,0)[1]],r=a(c[23],[0,k[1],q]),s=e(K[16],i,b,r),u=[0,f(g,1)[2]],v=a(c[23],[0,k[2],u]),w=e(K[16],i,b,v),o=bg(b,s,l),p=bg(b,w,l);if(o)if(p){var
x=p[1],y=o[1],z=a(A[1][6],dk),B=e(t[13],A[1][10][1],z,i),C=a(c[11],x),D=a(c[11],y),E=f(g,1)[2],F=[0,f(g,0)[1],E,D,C],G=a(c[23],[0,k[3],F]);return d(t[aH],[0,B],G)}return n[57][2]}return n[57][2]}return n[57][2]}return[0,a(G[67][7],b),g]}var
r=e(z[14],q,h,m);return a(n[57][22],r)}var
bh=a(G[67][7],dl),bi=[0,a3[5],a3[7]],W=[0,cw,cx],bj=[0,a2[5],a2[7]],bk=[0,a1[5],a1[7]],ay=[0,a0[5],a0[7]],az=[0,aZ[5],aZ[7]],aA=[0,aX[5],aX[7]],aB=[0,aY[5],aY[7]],aC=[0,aW[5],aW[7]];aD(124,[0,aC,aB,aA,az,ay,bk,bj,W,bi,bd,bh,be],"Zify_plugin__Zify");a(dm[9],an);var
dn=0,dp=0;function
dq(c,b){a(o[2],b);return[0,function(b){return a(bi[1],c)}]}var
dt=[0,[0,0,[0,ds,[0,dr,[1,[5,a(u[16],x[11])],0]]],dq,dp],dn],du=0;function
dv(c,b){a(o[2],b);return[0,function(b){return a(W[1],c)}]}var
dy=[0,[0,0,[0,dx,[0,dw,[1,[5,a(u[16],x[11])],0]]],dv,du],dt],dz=0;function
dA(c,b){a(o[2],b);return[0,function(b){return a(W[1],c)}]}var
dD=[0,[0,0,[0,dC,[0,dB,[1,[5,a(u[16],x[11])],0]]],dA,dz],dy],dE=0;function
dF(c,b){a(o[2],b);return[0,function(b){return a(W[1],c)}]}var
dI=[0,[0,0,[0,dH,[0,dG,[1,[5,a(u[16],x[11])],0]]],dF,dE],dD],dJ=0;function
dK(c,b){a(o[2],b);return[0,function(b){return a(bj[1],c)}]}var
dN=[0,[0,0,[0,dM,[0,dL,[1,[5,a(u[16],x[11])],0]]],dK,dJ],dI],dO=0;function
dP(c,b){a(o[2],b);return[0,function(b){return a(bk[1],c)}]}var
dS=[0,[0,0,[0,dR,[0,dQ,[1,[5,a(u[16],x[11])],0]]],dP,dO],dN],dT=0;function
dU(c,b){a(o[2],b);return[0,function(b){return a(ay[1],c)}]}var
dX=[0,[0,0,[0,dW,[0,dV,[1,[5,a(u[16],x[11])],0]]],dU,dT],dS],dY=0;function
dZ(c,b){a(o[2],b);return[0,function(b){return a(az[1],c)}]}var
d2=[0,[0,0,[0,d1,[0,d0,[1,[5,a(u[16],x[11])],0]]],dZ,dY],dX],d3=0;function
d4(c,b){a(o[2],b);return[0,function(b){return a(aB[1],c)}]}var
d7=[0,[0,0,[0,d6,[0,d5,[1,[5,a(u[16],x[11])],0]]],d4,d3],d2],d8=0;function
d9(c,b){a(o[2],b);return[0,function(b){return a(aA[1],c)}]}var
ea=[0,[0,0,[0,d$,[0,d_,[1,[5,a(u[16],x[11])],0]]],d9,d8],d7],eb=0;function
ec(c,b){a(o[2],b);return[0,function(b){return a(aC[1],c)}]}var
ef=[0,[0,0,[0,ee,[0,ed,[1,[5,a(u[16],x[11])],0]]],ec,eb],ea],eg=0,eh=[0,function(a){return ao[6]}];M(ao[2],ei,eh,eg,ef);var
ej=0;function
ek(a,b){return be(a)}var
en=[0,[0,[0,em,[1,[5,a(u[16],el[9])],0]],ek],ej];ap(bl[8],an,eo,0,0,en);var
ep=0,er=[0,[0,eq,function(a){return bh}],ep],et=[0,[0,es,function(a){return bd}],er];ap(bl[8],an,eu,0,0,et);var
ev=0,ew=0,ey=[0,[0,0,ex,function(b){a(o[2],b);return[0,function(b){return a(W[2],0)}]},ew],ev],ez=0,eB=[0,[0,0,eA,function(b){a(o[2],b);return[0,function(b){return a(ay[2],0)}]},ez],ey],eC=0,eE=[0,[0,0,eD,function(b){a(o[2],b);return[0,function(b){return a(az[2],0)}]},eC],eB],eF=0,eH=[0,[0,0,eG,function(b){a(o[2],b);return[0,function(b){return a(aB[2],0)}]},eF],eE],eI=0,eK=[0,[0,0,eJ,function(b){a(o[2],b);return[0,function(b){return a(aA[2],0)}]},eI],eH],eL=0,eN=[0,[0,0,eM,function(b){a(o[2],b);return[0,function(b){return a(aC[2],0)}]},eL],eK],eO=0,eP=[0,function(a){return ao[6]}];M(ao[2],eQ,eP,eO,eN);aD(132,[0,an],"Zify_plugin__G_zify");return}
