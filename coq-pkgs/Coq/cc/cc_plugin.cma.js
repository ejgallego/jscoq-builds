function(eN){"use strict";var
aD=".",bL="  [",bG=" : ",bP=915186972,A=246,bK=3901498,aE=121,aF="congruence",bO="[",bN=-431191102,bF="A",aV=1000,bJ=142,bS="with",aH="]",aG=15500,bI=-318868643,bR=122,bM=" and ",bE=114,bH=888453194,aW=-912009552,bQ="Heq",bD="f_equal",_=100,V=250,y=eN.jsoo_runtime,q=y.caml_check_bound,am=y.caml_int_compare,aA=y.caml_make_vect,d=y.caml_new_string,U=y.caml_obj_tag,al=y.caml_register_global,r=y.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):y.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):y.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):y.caml_call_gen(a,[b,c,d])}function
an(a,b,c,d,e){return a.length==4?a(b,c,d,e):y.caml_call_gen(a,[b,c,d,e])}function
aB(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):y.caml_call_gen(a,[b,c,d,e,f])}function
aC(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):y.caml_call_gen(a,[b,c,d,e,f,g,h])}function
eM(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):y.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
h=y.caml_get_global_data(),ax=d("cc_plugin"),n=h.Constr,m=h.Names,$=h.Hashset,aY=h.Sorts,e=h.Util,t=h.Not_found,a8=h.Term,f=h.EConstr,Q=h.Typing,g=h.Int,w=h.Stdlib__queue,c=h.Pp,aO=h.Control,H=h.CErrors,aa=h.Stdlib,bc=h.Environ,L=h.Context,ba=h.Assert_failure,ah=h.Printer,o=h.Tacmach,ae=h.Stdlib__hashtbl,P=h.Termops,T=h.CamlinternalLazy,p=h.Tacticals,j=h.Proofview,G=h.Tactics,bw=h.Evarsolve,bz=h.DAst,O=h.Coqlib,bu=h.Equality,bq=h.CClosure,br=h.Reductionops,ay=h.Stdarg,az=h.Genarg,bC=h.Ltac_plugin__Tacentries,dc=h.Vars,da=h.Namegen,bT=h.Feedback,bY=h.Goptions,dN=h.Global,dO=h.Inductiveops,dP=h.Retyping,dU=h.Evarutil,ep=h.Pretype_errors,eq=h.Type_errors,eb=h.Detyping,dY=h.Evd,dV=h.Refine,er=h.Mltop;al(120,[0,0,0,0,0],"Cc_plugin");var
aI=[0,0],di=d("Out of depth ... "),dh=d("Out of instances ... "),dj=d("First run was incomplete, completing ... "),dg=d("Executing ... "),df=d("Running E-matching algorithm ... "),de=d("paf_of_patt: pattern is trivial"),db=d("wrong incomplete class."),c6=d(" ... "),c7=d(" = "),c8=d("Checking if "),c5=d("Yes"),c9=d("No"),c2=d(aD),c3=d("Processing mark for term "),cZ=d("weird error in injection subterms merge."),c0=[0,d("add_pacs")],cX=d(aD),cY=d("Updating term "),cU=d(aD),cV=d(bM),cW=d("Merging "),cQ=d(aD),cR=d(bM),cS=d("Linking "),cP=[0,d("_vendor+v8.11+32bit/coq/plugins/cc/ccalgo.ml"),635,2],cJ=d(aH),cK=d(" <> "),cL=d(bG),cM=d(bL),cN=d("Adding new disequality, depth="),cE=d(aH),cF=d(" == "),cG=d(bG),cH=d(bL),cI=d("Adding new equality, depth="),cD=d("discarding redundant (dis)equality"),cA=d(aH),cB=d(bO),cx=d(aH),cy=d(":="),cz=d(bO),cw=d("incomplete matching."),cl=d("not a node."),cm=[0,d("subterms")],cd=d("not a constructor."),ce=[0,d("get_constructor")],cb=d("not a representative."),cc=[0,d("get_representative")],b4=d("signature already entered."),b5=[0,d("enter")],bV=[0,d("Congruence"),[0,d("Verbose"),0]],bW=d("Congruence Verbose"),cj=d("Cc_plugin.Ccalgo.Discriminable"),co=d(bF),cq=d(bF),c_=d("_eps_"),dk=d("invalid cc transitivity."),dl=d("not enough args."),dm=[0,d("nth_arg")],dn=[0,1,20],dp=d("equal_proof "),dq=[0,1,20],dr=d("edge_proof "),ds=[0,1,20],dt=d("constr_proof "),dv=d(","),du=d("}"),dw=d("{"),dx=[0,1,20],dy=d("path_proof "),dz=[0,1,20],dA=d("congr_proof "),dB=[0,1,20],dC=d("ind_proof "),dM=[0,0],dT=[0,d("_vendor+v8.11+32bit/coq/plugins/cc/cctac.ml"),251,9],dZ=d("f"),d0=d("I don't know how to handle dependent equality"),en=[0,0],ee=d("("),ef=d(")"),ea=[0,1],d_=d("Goal solved, generating proof ..."),d9=d("Computation completed."),d8=d("Problem built, solving ..."),d7=d("Reading subgoal ..."),d$=[13,0,0,0],ec=d("  replacing metavariables by arbitrary terms."),ed=d(')",'),eg=d('"congruence with ('),eh=d("  Try "),ei=d("Goal is solvable by congruence but some arguments are missing."),ej=d("congruence failed"),d6=d(bQ),d5=d("H"),d3=d("e"),d4=d("X"),d1=d(bQ),dW=[0,0],dX=[0,1],dS=d("t"),dL=d("core.False.type"),dK=d("core.eq.type"),dJ=d("core.eq.trans"),dH=d("core.eq.sym"),dG=d("core.eq.refl"),dE=d("core.eq.rect"),dD=d("core.eq.congr"),ek=d("congruence failed."),eu=d(bS),ew=d(aF),ez=d(bS),eA=d(aF),eD=d(aF),eF=[0,d(aF),0],eH=d("cc"),eJ=[0,d(bD),0],eL=d(bD),W=5;function
s(d){var
c=a(e[3],aI);if(c){var
f=a(d,0);return b(bT[9],0,f)}return c}function
bU(a){aI[1]=a;return 0}var
bX=[0,0,bW,bV,function(b){return a(e[3],aI)},bU];b(bY[4],0,bX);var
bZ=g[1],b0=[0,function(b,a){return b===a?1:0},bZ],ao=a(ae[25],b0);function
b1(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
b2=[0,b1,function(c){var
d=c[1],e=a(g[1],c[2]),f=a(g[1],d);return b($[2][1],f,e)}],af=a(ae[25],b2);function
b3(f,e,d){if(b(af[11],d[1],e)){var
g=a(c[3],b4);i(H[2],0,b5,g)}else
i(af[10],d[1],e,f);return i(ao[10],d[2],f,e)}function
b6(c,a){return b(af[7],a[1],c)}function
b7(a,c){try{var
d=b(ao[7],a[2],c);b(af[6],a[1],d);var
e=b(ao[6],a[2],c);return e}catch(a){a=r(a);if(a===t)return 0;throw a}}function
b8(c,a){function
d(a){return b7(c,a)}return b(g[2][13],d,a)}var
b9=[0,function(b,a){var
c=am(b[1],a[1]),f=a[3],g=a[2],h=b[3],j=b[2];if(0===c){var
d=am(j,g);return 0===d?i(e[22][46],am,h,f):d}return c}],b_=[0,function(b,a){var
c=am(b[1],a[1]),d=a[2],e=b[2];return 0===c?am(e,d):c}],l=a(e[26][1],b9),k=a(e[26][1],b_);function
aX(c,a){if(typeof
c==="number")switch(c){case
1:var
b=typeof
a==="number"?1===a?1:0:0;break;case
2:var
b=typeof
a==="number"?2<=a?1:0:0;break;default:var
b=0}else
var
b=typeof
a==="number"?0:1;return b?1:0}function
ap(l,k){var
c=l,a=k;for(;;){switch(c[0]){case
0:if(0===a[0])return b(n[86],c[1],a[1]);break;case
1:if(1===a[0]){var
o=a[2],p=c[2],f=aX(c[1],a[1]);return f?aX(p,o):f}break;case
2:if(2===a[0])return b(m[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],g=ap(c[1],a[1]);if(g){var
c=r,a=q;continue}return g}break;default:if(4===a[0]){var
d=a[1],e=c[1],h=e[2]===d[2]?1:0,s=d[3],t=d[1][1],u=e[3],v=e[1][1];if(h){var
i=u===s?1:0;if(i)return b(m[52],v,t);var
j=i}else
var
j=h;return j}}return 0}}function
aJ(c){switch(c[0]){case
0:var
e=a(n[bE],c[1]);return b($[2][1],1,e);case
1:var
f=c[1],g=a(aY[7],c[2]),h=a(aY[7],f);return i($[2][3],2,h,g);case
2:var
j=a(m[1][3],c[1]);return b($[2][1],3,j);case
3:var
k=c[1],l=aJ(c[2]),o=aJ(k);return i($[2][3],4,o,l);default:var
d=c[1],p=d[3],q=d[2],r=a(m[56],d[1][1]);return an($[2][4],5,r,q,p)}}var
C=a(ae[25],[0,n[86],n[bE]]),X=a(ae[25],[0,ap,aJ]),aK=a(ae[25],[0,m[1][1],m[1][3]]),b$=[0,a(n[1],aa[9])],aZ=[0,[1,aa[9],[0,aa[9],aa[9],0]],aa[9],l[1],0,b$];function
a0(c){var
b=a(X[1],W);return[0,W,0,aA(5,aZ),a(C[1],W),0,b]}function
a1(e,b){var
f=a(o[2],b),h=a(o[5],b),i=a(C[1],W),j=a(aK[1],W),k=g[2][1],l=a(w[2],0),m=a(w[2],0),n=g[2][1],c=a(ao[1],W),d=[0,a(af[1],W),c];return[0,a0(0),d,n,m,l,0,0,k,j,e,0,i,h,f]}function
ca(a){return a[1]}function
z(f,h){var
c=0,a=h;for(;;){var
d=q(f[3],a)[1+a][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
g=function(b){q(f[3],b)[1+b][2]=a;return 0};b(e[22][11],g,c);return a}}function
K(e,b){var
d=q(e[3],b)[1+b][1];if(0===d[0])return d[1];var
f=a(c[3],cb);return i(H[2],0,cc,f)}function
a2(b,a){return q(b[3],a)[1+a][3]}function
a3(c,f,e){var
a=f;for(;;)try{var
g=a2(c,a),h=b(l[23],e,g);return h}catch(b){b=r(b);if(b===t){var
d=q(c[3],a)[1+a][1];if(0===d[0])throw t;var
a=d[1];continue}throw b}}function
ag(e,b){var
d=q(e[3],b)[1+b][5];if(4===d[0])return d[1];var
f=a(c[3],cd);return i(H[2],0,ce,f)}function
a4(b,a){return K(b,a)[1]}function
cf(a){return a[4]}function
cg(a){return a[5]}function
ch(f,d,c){var
a=K(f,d);a[1]=b(e[4],a[1],1);a[2]=b(g[2][4],c,a[2]);a[3]=b(g[2][4],c,a[3]);return 0}function
ci(f,d,c){var
a=K(f,d);a[1]=b(e[4],a[1],1);a[3]=b(g[2][4],c,a[3]);return 0}var
a5=[248,cj,y.caml_fresh_oo_id(0)];function
a6(b){var
c=a(e[22][6],b[3]);return[0,b[1],b[2]+1|0,c]}function
ck(a,c,e){try{var
j=b(k[23],c,a[6]),d=j}catch(a){a=r(a);if(a!==t)throw a;var
d=g[2][1]}var
f=a[6],h=b(g[2][4],e,d);a[6]=i(k[4],c,h,f);return 0}function
D(b,a){return q(b[3],a)[1+a][5]}function
ab(f,b){var
d=q(f[3],b)[1+b][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[3],cl);return i(H[2],0,cm,g)}function
a7(a,c){var
b=ab(a,c),d=b[1],e=z(a,b[2]);return[0,z(a,d),e]}function
cn(a){var
c=a[2],d=c+1|0;if(d===a[1]){var
f=b(e[4],(a[1]*3|0)/2|0,1),g=aA(f,aZ);a[1]=f;aB(e[24][10],a[3],0,g,0,c);a[3]=g}a[2]=d;return c}function
aq(a){return[0,0,g[2][1],g[2][1],0,a,k[1]]}var
cp=[0,a(m[1][6],co)],cr=[0,a(m[1][6],cq)],cs=a(n[1],2),ct=a(n[1],2),cu=[0,b(L[4],0,0),ct,cs],cv=a(n[13],cu);function
F(c){switch(c[0]){case
0:return c[1];case
1:var
k=c[1],f=a(n[7],c[2]),g=[0,b(L[4],cr,0),f,cv],h=a(n[14],g),i=a(n[7],k),j=[0,b(L[4],cp,0),i,h];return a(n[14],j);case
2:return a(n[2],c[1]);case
3:var
l=c[1],e=[0,F(c[2]),0],d=l;for(;;){if(3===d[0]){var
o=d[1],e=[0,F(d[2]),e],d=o;continue}var
m=[0,F(d),e];return a(a8[12],m)}default:return a(n[24],c[1][1])}}function
a9(q,p){var
g=a(f[bJ][1],p);function
d(b){return a9(q,a(f[9],b))}var
c=a(n[30],g);switch(c[0]){case
6:var
r=c[2],s=c[1],t=d(c[3]),u=[0,s,d(r),t];return a(n[13],u);case
7:var
v=c[2],w=c[1],x=d(c[3]),y=[0,w,d(v),x];return a(n[14],y);case
8:var
z=c[3],A=c[2],B=c[1],C=d(c[4]),D=d(z),E=[0,B,d(A),D,C];return a(n[15],E);case
9:var
F=c[1],G=b(e[24][73][1],d,c[2]),H=[0,d(F),G];return a(n[16],H);case
10:var
h=c[1],I=h[2],J=a(m[19][5],h[1]),K=[0,a(m[19][2],J),I];return a(n[19],K);case
11:var
i=c[1],j=i[1],L=i[2],M=j[2],N=a(m[25][5],j[1]),O=[0,[0,a(m[25][2],N),M],L];return a(n[22],O);case
12:var
k=c[1],l=k[1],o=l[1],P=k[2],Q=l[2],R=o[2],S=a(m[25][5],o[1]),T=[0,[0,[0,a(m[25][2],S),R],Q],P];return a(n[24],T);case
16:var
U=c[2],V=c[1],W=function(b){var
c=a(m[25][5],b);return a(m[25][2],c)},X=b(m[68][19],W,V),Y=[0,X,d(U)];return a(n[20],Y);default:return g}}function
aL(b,a){if(0===a[0]){var
d=a[2],f=a[1],g=function(c,a){return[3,a,aL(b,c)]};return i(e[22][16],g,d,f)}var
c=a[1]-1|0;return q(b,c)[1+c]}function
v(h,g,e,d){var
j=a(c[3],cx),k=F(D(e,d)),l=a(f[9],k),m=i(ah[11],h,g,l),n=a(c[3],cy),o=a(c[16],d),p=a(c[3],cz),q=b(c[12],p,o),r=b(c[12],q,n),s=b(c[12],r,m);return b(c[12],s,j)}function
ar(g,e,d){var
h=a(c[3],cA),j=F(d),k=a(f[9],j),l=i(ah[11],g,e,k),m=a(c[3],cB),n=b(c[12],m,l);return b(c[12],n,h)}function
M(d,e){var
h=d[1];try{var
k=b(X[7],h[6],e);return k}catch(k){k=r(k);if(k===t){var
c=cn(h),s=F(e),u=a(f[9],s),v=i(Q[1],d[13],d[14],u),j=a9(d[14],v);switch(e[0]){case
2:var
B=l[1],m=[0,[0,aq(j)],-1,B,0,e];break;case
3:var
D=e[2],o=M(d,e[1]),p=M(d,D);ch(h,z(h,o),c);ci(h,z(h,p),c);d[3]=b(g[2][4],c,d[3]);var
E=l[1],m=[0,[0,aq(j)],-1,E,[0,[0,o,p]],e];break;case
4:var
G=e[1];b(w[3],[0,c,[0,[0,c,0]]],d[5]);b(w[3],[0,c,[1,[0,c,G[2],0]]],d[5]);var
H=l[1],m=[0,[0,aq(j)],-1,H,0,e];break;default:b(w[3],[0,c,[0,[0,c,0]]],d[5]);var
x=l[1],m=[0,[0,aq(j)],-1,x,0,e]}q(h[3],c)[1+c]=m;i(X[5],h[6],e,c);try{var
A=b(C[7],d[12],j),n=A}catch(a){a=r(a);if(a!==t)throw a;var
n=g[2][1]}var
y=b(g[2][4],c,n);i(C[10],d[12],j,y);return c}throw k}}function
aM(a,e,d,c){var
f=M(a,d),g=M(a,c);b(w[3],[0,f,g,[0,e,0]],a[4]);return i(C[5],a[1][4],e,[0,d,c])}function
Y(a,d,c,b){var
e=M(a,c),f=M(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
aN(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
cC(a,d,c){try{var
f=a[1],g=function(a){return z(f,a)},h=b(e[24][15],g,c),j=b(aK[9],a[9],d),k=function(b){function
c(c,b){return c===z(a[1],b)?1:0}return i(e[24][37],c,h,b)},l=b(e[22][22],k,j);return l}catch(a){a=r(a);if(a===t)return 0;throw a}}function
cO(e,b,a,d){var
c=q(e[3],b)[1+b];c[1]=[1,a,d];c[2]=a;return 0}function
a_(g,f,e){var
a=f,b=e;for(;;){var
c=q(g[3],a)[1+a][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
a$(c,i,h){var
o=z(c,h);if(z(c,i)===o){var
p=a_(c,h,0),a=[0,a_(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,ba,cP]}function
bb(d,h,m,y){s(function(o){var
e=a(c[3],cQ),f=v(d[13],d[14],d[1],m),g=a(c[3],cR),i=v(d[13],d[14],d[1],h),j=a(c[3],cS),k=b(c[12],j,i),l=b(c[12],k,g),n=b(c[12],l,f);return b(c[12],n,e)});var
j=K(d[1],h),e=K(d[1],m);cO(d[1],h,m,y);try{var
I=b(C[7],d[12],j[5]),p=I}catch(a){a=r(a);if(a!==t)throw a;var
p=g[2][1]}var
z=b(g[2][6],h,p);i(C[10],d[12],j[5],z);var
u=b(g[2][7],j[3],e[3]);e[1]=a(g[2][20],u);e[3]=u;e[2]=b(g[2][7],j[2],e[2]);b8(d[2],j[3]);d[3]=b(g[2][7],d[3],j[3]);var
A=q(d[1][3],h)[1+h][3];function
B(c,a){return b(w[3],[0,a,[1,c]],d[5])}b(l[11],B,A);var
D=j[6];function
E(c){function
e(a){return b(w[3],[0,a,[0,c]],d[5])}return a(g[2][13],e)}b(k[11],E,D);var
n=j[4],f=e[4];if(typeof
n==="number"){if(0===n)return 0;if(typeof
f==="number"){if(0===f){e[4]=1;return 0}}else
if(0===f[0]){d[8]=b(g[2][6],m,d[8]);e[4]=1;return 0}}else
if(0===n[0]){var
F=n[1];if(typeof
f==="number"){if(0===f){e[4]=[0,F];d[8]=b(g[2][6],h,d[8]);d[8]=b(g[2][4],m,d[8]);return 0}var
x=0}else
var
x=1===f[0]?1:0;if(!x){d[8]=b(g[2][6],h,d[8]);return 0}}else{var
o=n[1],G=o[2],H=o[1];if(typeof
f==="number"){if(0===f){e[4]=[1,o];return 0}}else
if(0!==f[0])return b(w[3],[0,H,[1,G]],d[5])}return 0}function
cT(f,e){s(function(n){var
d=a(c[3],cU),g=v(e[13],e[14],e[1],f[2]),h=a(c[3],cV),i=v(e[13],e[14],e[1],f[1]),j=a(c[3],cW),k=b(c[12],j,i),l=b(c[12],k,h),m=b(c[12],l,g);return b(c[12],m,d)});var
g=e[1],h=z(g,f[1]),i=z(g,f[2]),j=1-(h===i?1:0);if(j){var
l=a4(g,i);if(a4(g,h)<l)return bb(e,h,i,f);var
d=f[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bb(e,i,h,[0,f[2],f[1],k])}return j}function
c1(h,u,d){s(function(j){var
e=a(c[3],c2),f=v(d[13],d[14],d[1],h),g=a(c[3],c3),i=b(c[12],g,f);return b(c[12],i,e)});var
r=z(d[1],h),j=K(d[1],r);if(0===u[0]){ck(j,u[1],h);d[3]=b(g[2][7],j[2],d[3]);return 0}var
f=u[1],t=q(d[1][3],r)[1+r];if(1-b(l[3],f,t[3]))t[3]=i(l[4],f,h,t[3]);var
k=j[4];if(typeof
k==="number"){if(0===k)return 0===f[2]?(j[4]=[1,[0,h,f]],0):(d[3]=b(g[2][7],j[2],d[3]),j[4]=[0,f],d[8]=b(g[2][4],r,d[8]),0)}else
if(1===k[0]){var
x=k[1],m=x[2],y=x[1];if(f[1]===m[1]){var
B=ag(d[1],f[1]),p=B[3],o=m[3],n=f[3];for(;;){var
A=0<p?1:0;if(A){if(o)if(n){var
C=n[2],D=o[2];b(w[3],[0,o[1],n[1],[1,y,m,h,f,p]],d[4]);var
p=b(e[5],p,1),o=D,n=C;continue}var
E=a(c[3],cZ);return i(H[2],0,c0,E)}return A}}throw[0,a5,y,m,h,f]}d[3]=b(g[2][7],j[2],d[3]);return 0}function
c4(d){var
g=d[1];function
h(f){if(f){var
e=f[1],k=f[2],l=z(g,e[2]);if(z(g,e[1])===l)var
j=a(c[3],c5),i=[0,e];else
var
m=h(k),j=a(c[3],c9),i=m;s(function(p){var
f=a(c[3],c6),g=v(d[13],d[14],d[1],e[2]),h=a(c[3],c7),i=v(d[13],d[14],d[1],e[1]),k=a(c[3],c8),l=b(c[12],k,i),m=b(c[12],l,h),n=b(c[12],m,g),o=b(c[12],n,f);return b(c[12],o,j)});return i}return 0}return h(d[6])}var
c$=a(m[1][6],c_);function
dd(d){var
h=d[8];function
j(p){var
j=K(d[1],p)[4];if(typeof
j!=="number"&&0===j[0]){var
g=j[1],z=F(D(d[1],g[1])),A=a(f[9],z),B=i(Q[1],d[13],d[14],A),C=a(f[bJ][1],B),E=g[3],G=function(a){return F(D(d[1],a))},I=b(e[22][68],G,E),J=a(e[22][9],I),N=b(a8[29],C,J),O=g[2],l=D(d[1],p),m=N,k=O;for(;;){if(0<k){var
o=a(n[66],m),v=o[3],w=a(f[9],o[2]),q=a(bc[14],d[13]),r=a(bc[39],q),h=b(da[26],c$,r),s=d[13],t=[0,b(L[4],h,0),w];d[13]=b(f[125],t,s);var
x=b(e[5],k,1),y=[0,a(n[2],h),0],l=[3,l,[2,h]],m=b(dc[13],y,v),k=x;continue}d[1][5]=[0,g,d[1][5]];M(d,l);return 0}}var
u=a(c[3],db);return i(H[2],0,0,u)}return b(g[2][13],j,h)}function
bd(d){var
c=[0,k[1]],j=d[1],f=d[1][3];function
h(d,l){var
f=d<j[2]?1:0;if(f){var
h=l[1];if(0===h[0]){var
m=h[1][6],n=function(f,o){try{var
m=a(e[3],c),n=b(k[23],f,m),h=n}catch(a){a=r(a);if(a!==t)throw a;var
h=g[2][1]}var
j=a(e[3],c),l=b(g[2][4],d,h);c[1]=i(k[4],f,l,j);return 0};return b(k[11],n,m)}return 0}return f}b(e[24][14],h,f);return a(e[3],c)}function
be(s,p,d){var
c=a(e[27][9],d),m=c[3];if(m){var
f=m[2],u=m[1],h=u[2],i=u[1],j=s[1];if(0===i[0]){var
l=i[2],n=i[1];if(l){var
A=l[2],B=l[1];try{var
C=b(X[7],j[6],n),D=[0,C,a(e[22][1],l)],E=K(j,h)[6],F=b(k[23],D,E),G=function(h){var
g=a7(s[1],h),i=[0,[0,[0,n,A],g[1]],[0,[0,B,g[2]],f]],j=c[2],k=[0,a(e[24][8],c[1]),j,i];return b(e[27][3],k,d)},H=b(g[2][13],G,F);return H}catch(a){a=r(a);if(a===t)return 0;throw a}}try{var
v=z(j,b(X[7],j[6],n))===h?1:0,I=v?b(e[27][3],[0,c[1],c[2],f],d):v;return I}catch(a){a=r(a);if(a===t)return 0;throw a}}var
o=i[1],w=o-1|0;if(0<=q(c[1],w)[1+w]){var
x=o-1|0;return q(c[1],x)[1+x]===h?b(e[27][3],[0,c[1],c[2],f],d):0}var
y=o-1|0;q(c[1],y)[1+y]=h;return b(e[27][3],[0,c[1],c[2],f],d)}var
J=a(e[3],p);p[1]=[0,[0,c[2],c[1]],J];return 0}function
aP(d,c){if(0===c[0]){var
f=c[1],g=a(e[22][1],c[2]);return[0,b(X[7],d,f),g]}return a(aa[2],de)}function
bf(c){var
l=c[1][6],h=a(e[27][2],0),m=bd(c);function
d(a){var
i=a[5];if(typeof
i==="number")if(0===i)try{var
x=aP(l,a[4]),y=b(k[23],x,m),d=y}catch(a){a=r(a);if(a!==t)throw a;var
d=g[2][1]}else
var
d=g[2][1];else{var
z=i[1];try{var
A=b(C[7],c[12],z),o=A}catch(a){a=r(a);if(a!==t)throw a;var
o=g[2][1]}var
d=o}function
p(c){return b(e[27][3],[0,aA(a[3],-1),a,[0,[0,a[4],c],0]],h)}b(g[2][13],p,d);var
j=a[7];if(typeof
j==="number")if(0===j)try{var
s=aP(l,a[6]),u=b(k[23],s,m),f=u}catch(a){a=r(a);if(a!==t)throw a;var
f=g[2][1]}else
var
f=g[2][1];else{var
v=j[1];try{var
w=b(C[7],c[12],v),n=w}catch(a){a=r(a);if(a!==t)throw a;var
n=g[2][1]}var
f=n}function
q(c){return b(e[27][3],[0,aA(a[3],-1),a,[0,[0,a[6],c],0]],h)}return b(g[2][13],q,f)}b(e[22][11],d,c[7]);return h}function
bg(b){var
d=[0,0],f=bf(b);s(function(b){return a(c[3],df)});try{for(;;){a(aO[3],0);be(b,d,f);continue}}catch(b){b=r(b);if(b===e[27][1])return a(e[3],d);throw b}}function
as(y,d){s(function(b){return a(c[3],dg)});try{for(;;){a(aO[3],0);try{cT(a(w[5],d[4]),d);var
M=1,j=M}catch(e){e=r(e);if(e!==w[1])throw e;try{var
x=a(w[5],d[5]);c1(x[1],x[2],d);var
L=1,j=L}catch(e){e=r(e);if(e!==w[1])throw e;try{var
h=a(g[2][26],d[3]);d[3]=b(g[2][6],h,d[3]);s(function(i){return function(j){var
e=a(c[3],cX),f=v(d[13],d[14],d[1],i),g=a(c[3],cY),h=b(c[12],g,f);return b(c[12],h,e)}}(h));var
o=a7(d[1],h),p=o[1],A=ab(d[1],h)[2],q=K(d[1],p),u=q[4],X=typeof
u==="number"?0:0===u[0]?(q[4]=1,d[8]=b(g[2][6],p,d[8]),1):0,B=a2(d[1],p),C=function(c,e){return function(a,f){return b(w[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(A,h);b(l[11],C,B);var
E=q[6],G=function(c){return function(a,e){return b(w[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(h);b(k[11],G,E);try{var
I=b6(o,d[2]);b(w[3],[0,h,I,0],d[4])}catch(a){a=r(a);if(a!==t)throw a;b3(h,o,d[2]);var
Z=a}var
J=1,j=J}catch(a){a=r(a);if(a!==t)throw a;var
j=0,_=a}var
$=e}var
aa=e}if(j)continue;var
z=c4(d);if(z)var
S=z[1],T=y?[1,S]:0,m=[0,T];else
if(a(g[2][2],d[8]))if(0<d[10]){var
U=bg(d),V=function(q){var
m=q[2],g=q[1];a(aO[3],0);var
o=0<d[10]?1:0;if(o){if(cC(d,g[1],m))return s(function(b){return a(c[3],cD)});i(aK[5],d[9],g[1],m);var
u=d[1],t=function(b){try{var
e=D(u,b);return e}catch(b){b=r(b);if(a(H[13],b)){var
d=a(c[3],cw);return i(H[2],0,0,d)}throw b}},l=b(e[24][15],t,m),v=a(n[2],g[1]),p=b(e[24][15],F,l);a(e[24][46],p);var
h=a(n[16],[0,v,p]),j=aL(l,g[4]),k=aL(l,g[6]);d[11]=1;d[10]=d[10]-1|0;return g[2]?(s(function(C){var
e=a(c[3],cE),g=ar(d[13],d[14],k),l=a(c[3],cF),m=ar(d[13],d[14],j),n=a(c[3],cG),o=a(f[9],h),p=i(ah[11],d[13],d[14],o),q=a(c[3],cH),r=b(c[12],q,p),s=b(c[12],r,n),t=b(c[12],s,m),u=b(c[12],t,l),v=b(c[12],u,g),w=b(c[12],v,e),x=a(c[5],0),y=a(c[16],d[10]),z=a(c[3],cI),A=b(c[12],z,y),B=b(c[12],A,x);return b(c[12],B,w)}),aM(d,h,j,k)):(s(function(C){var
e=a(c[3],cJ),g=ar(d[13],d[14],k),l=a(c[3],cK),m=ar(d[13],d[14],j),n=a(c[3],cL),o=a(f[9],h),p=i(ah[11],d[13],d[14],o),q=a(c[3],cM),r=b(c[12],q,p),s=b(c[12],r,n),t=b(c[12],s,m),u=b(c[12],t,l),v=b(c[12],u,g),w=b(c[12],v,e),x=a(c[5],0),y=a(c[16],d[10]),z=a(c[3],cN),A=b(c[12],z,y),B=b(c[12],A,x);return b(c[12],B,w)}),Y(d,[0,h],j,k))}return o};b(e[22][11],V,U);var
W=d[11]?(d[11]=0,as(1,d)):(s(function(b){return a(c[3],dh)}),0),m=W}else{s(function(b){return a(c[3],di)});var
m=0}else{s(function(b){return a(c[3],dj)});dd(d);var
m=as(0,d)}return m}}catch(a){a=r(a);if(a[1]===a5){var
N=a[5],O=a[4],P=a[3],Q=a[2],R=y?[0,[0,Q,P,O,N]]:0;return[0,R]}throw a}}al(146,[0,[0,k[1],k[2],k[3],k[4],k[5],k[6],k[7],k[8],k[9],k[10],k[11],k[12],k[13],k[14],k[15],k[16],k[17],k[18],k[19],k[20],k[21],k[22],k[23],k[24],k[25],k[26]],[0,l[1],l[2],l[3],l[4],l[5],l[6],l[7],l[8],l[9],l[10],l[11],l[12],l[13],l[14],l[15],l[16],l[17],l[18],l[19],l[20],l[21],l[22],l[23],l[24],l[25],l[26]],C,X,ap,F,s,ca,cf,cg,a1,M,aM,Y,aN,a6,z,a3,D,ag,ab,a$,bd,be,bf,aP,bg,as,v,a0],"Cc_plugin__Ccalgo");function
ai(a){return[0,a,a,[2,a]]}function
ac(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return ai([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
B(m,l){var
d=m,b=l;for(;;){var
e=d[3],f=b[3];switch(e[0]){case
2:return b;case
4:var
j=e[2],k=e[1];switch(f[0]){case
2:var
g=0;break;case
3:var
h=f[1][3];if(4===h[0]){var
p=f[2],q=h[1],r=B(j,h[2]),d=ac(B(k,q),r),b=p;continue}var
g=1;break;case
4:var
s=f[1],t=B(j,f[2]);return ac(B(k,s),t);default:var
g=1}break;default:var
g=0}if(!g){if(2===f[0])return d;if(3===e[0]){var
o=e[1],d=o,b=B(e[2],b);continue}}if(ap(d[2],b[1]))return[0,d[1],b[2],[3,d,b]];var
n=a(c[3],dk);return i(H[2],0,0,n)}}function
N(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=N(a[1]);return B(N(c),d);case
4:var
e=a[1],f=N(a[2]);return ac(N(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,N(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
bh(d,a){var
c=b(C[7],d,a);return[0,c[1],c[2],[0,a]]}function
bi(d,a){var
c=b(C[7],d,a);return[0,c[2],c[1],[1,a]]}function
bj(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
h=b[2],j=b[1];if(0<d){var
b=j,d=d-1|0;continue}return h}var
g=a(c[3],dl);return i(H[2],0,dm,g)}}function
bk(c,d,b,a){var
e=bj(c[2],b-a|0);return[0,bj(c[1],b-a|0),e,[5,c,d,b,a]]}function
R(h,g,d,e,f){s(function(o){var
i=v(h,g,d,f),j=a(c[4],dn),k=v(h,g,d,e),l=a(c[3],dp),m=b(c[12],l,k),n=b(c[12],m,j);return b(c[12],n,i)});if(e===f)return ai(D(d,e));var
i=a$(d,e,f),j=i[1],k=N(at(h,g,d,f,i[2]));return B(at(h,g,d,e,j),k)}function
bl(g,f,d,j){var
h=j[2],k=j[1],l=k[2],m=k[1];s(function(o){var
e=v(g,f,d,l),h=a(c[4],dq),i=v(g,f,d,m),j=a(c[3],dr),k=b(c[12],j,i),n=b(c[12],k,h);return b(c[12],n,e)});var
q=R(g,f,d,m,h[1]),r=N(R(g,f,d,l,h[2])),e=h[3];if(typeof
e==="number")var
i=bm(g,f,d,h[1],h[2]);else
if(0===e[0])var
n=e[1],t=e[2]?bi(d[4],n):bh(d[4],n),i=t;else
var
o=e[2],u=e[5],w=aR(g,f,d,e[1],o,e[3],e[4]),p=ag(d,o[1]),i=bk(w,p[1],p[3],u);return B(B(q,i),r)}function
aQ(h,g,d,f,e){s(function(l){var
e=a(c[4],ds),i=v(h,g,d,f),j=a(c[3],dt),k=b(c[12],j,i);return b(c[12],k,e)});var
i=a3(d,f,e),j=R(h,g,d,f,i);if(0===e[3])return j;var
l=a6(e),k=ab(d,i),m=k[1],n=D(d,k[2]),o=aQ(h,g,d,m,l);return B(j,ac(o,ai(n)))}function
at(g,f,e,h,d){s(function(w){var
j=a(c[3],du);function
k(b){return a(c[16],b[1][2])}function
l(b){return a(c[3],dv)}var
m=i(c[39],l,k,d),n=a(c[3],dw),o=a(c[4],dx),p=v(g,f,e,h),q=a(c[3],dy),r=b(c[12],q,p),s=b(c[12],r,o),t=b(c[12],s,n),u=b(c[12],t,m);return b(c[12],u,j)});if(d){var
j=d[1],k=d[2],l=bl(g,f,e,j);return B(at(g,f,e,j[1][2],k),l)}return ai(D(e,h))}function
bm(f,e,d,h,g){s(function(o){var
i=v(f,e,d,g),j=a(c[4],dz),k=v(f,e,d,h),l=a(c[3],dA),m=b(c[12],l,k),n=b(c[12],m,j);return b(c[12],n,i)});var
i=ab(d,h),k=i[2],l=i[1],j=ab(d,g),m=j[1],n=R(f,e,d,k,j[2]);return ac(R(f,e,d,l,m),n)}function
aR(f,e,d,h,j,g,i){s(function(o){var
i=v(f,e,d,g),j=a(c[4],dB),k=v(f,e,d,h),l=a(c[3],dC),m=b(c[12],l,k),n=b(c[12],m,j);return b(c[12],n,i)});var
k=R(f,e,d,h,g),l=aQ(f,e,d,h,j),m=B(k,aQ(f,e,d,g,i));return B(N(l),m)}function
aS(e,d,c,b){if(bI<=b[1]){var
a=b[2];return aR(e,d,c,a[1],a[2],a[3],a[4])}var
f=b[2];return R(e,d,c,f[1],f[2])}al(147,[0,ai,ac,B,N,bh,bi,bk,R,bl,at,bm,aR,aS],"Cc_plugin__Ccproof");var
aT=[A,function(b){return a(O[2],dD)}],dF=[A,function(b){return a(O[2],dE)}],bn=[A,function(b){return a(O[2],dG)}],dI=[A,function(b){return a(O[2],dH)}],bo=[A,function(b){return a(O[2],dJ)}],x=[A,function(b){return a(O[2],dK)}],S=[A,function(b){return a(O[2],dL)}];function
bp(c,b,a){return an(br[17],bq[9],c,b,a)}function
au(c,b,a){return an(br[17],bq[4],c,b,a)}function
av(c,b,a){return i(Q[3],c,b,a)[2]}function
E(g,c,z){var
h=z;for(;;){var
A=bp(g,c,h),d=b(f[3],c,A);switch(d[0]){case
6:var
l=d[3],o=d[2];if(i(f[aE][13],c,1,l)){var
p=a(P[43],l),B=av(g,c,p),C=av(g,c,o),D=E(g,c,p);return[3,[3,[1,C,B],E(g,c,o)],D]}break;case
9:var
F=d[2],G=E(g,c,d[1]),H=function(a){return E(g,c,a)},I=b(e[24][15],H,F),J=function(b,a){return[3,b,a]};return i(e[24][17],J,G,I);case
10:var
q=d[1],K=q[1],L=b(f[2][2],c,q[2]),M=a(m[19][5],K),N=[0,a(m[19][2],M),L];return[0,a(n[19],N)];case
11:var
r=d[1],s=r[1],O=s[2],Q=s[1],R=b(f[2][2],c,r[2]),S=a(m[25][5],Q),T=[0,[0,a(m[25][2],S),O],R];return[0,a(n[22],T)];case
12:var
u=d[1],v=u[1],w=v[2],x=v[1],U=x[2],V=x[1],W=b(f[2][2],c,u[2]),X=a(m[25][5],V),j=[0,a(m[25][2],X),U],Y=a(dN[40],j)[1],y=b(dO[45],g,[0,j,w]);return[4,[0,[0,[0,j,w],W],y,b(e[5],y,Y[6])]];case
16:var
Z=d[2],_=d[1],$=function(b){var
c=a(m[25][5],b);return a(m[25][2],c)},aa=b(m[68][19],$,_),h=aB(dP[9],g,c,aa,Z,0);continue}var
k=b(P[56],c,h);if(b(f[aE][16],c,k))return[0,i(f[5],dM,c,k)];throw t}}function
aU(d,c,g){var
k=au(d,c,g),h=b(f[3],c,k);if(9===h[0]){var
e=h[2],l=h[1],j=U(x),m=V===j?x[1]:A===j?a(T[2],x):x;if(i(P[_],c,m,l))if(3===e.length-1){var
n=E(d,c,q(e,2)[3]),o=E(d,c,q(e,1)[2]);return[0,aG,[0,q(e,0)[1],o,n]]}return[0,aW,E(d,c,g)]}return[0,aW,E(d,c,g)]}function
aj(d,c,j){var
r=bp(d,c,j),h=b(f[3],c,r);switch(h[0]){case
0:var
k=h[1];return[0,[1,k],a(g[2][5],k)];case
6:var
l=h[3],m=h[2];if(i(f[aE][13],c,1,l)){var
n=a(P[43],l),o=aj(d,c,m),t=o[2],u=o[1],p=aj(d,c,n),v=p[2],w=p[1],x=av(d,c,n),y=av(d,c,m);return[0,[0,[1,y,x],[0,u,[0,w,0]]],b(g[2][7],t,v)]}break;case
9:var
z=h[2],A=E(d,c,h[1]),B=function(a){return aj(d,c,a)},C=b(e[24][56],B,z),q=a(e[22][119],C),D=q[1],F=i(e[22][15],g[2][7],g[2][1],q[2]);return[0,[0,A,a(e[22][9],D)],F]}var
s=E(d,c,j);return[0,[0,s,0],g[2][1]]}function
bs(a){return 0===a[0]?1:0}function
bt(k,c,j,v){try{var
w=au(k,c,v),l=b(f[82],c,w)}catch(a){a=r(a);if(a===n[60])throw t;throw a}var
d=l[2],y=l[1],m=U(x),z=V===m?x[1]:A===m?a(T[2],x):x;if(i(P[_],c,z,y))if(3===d.length-1){var
o=aj(k,c,q(d,1)[2]),p=o[1],B=o[2],s=aj(k,c,q(d,2)[3]),u=s[1],C=s[2];if(a(g[2][20],B)===j)if(bs(p))var
e=0;else
var
E=q(d,0)[1],e=[0,i(f[5],0,c,E)];else
var
e=1;if(a(g[2][20],C)===j)if(bs(u))var
h=0;else
var
D=q(d,0)[1],h=[0,i(f[5],0,c,D)];else
var
h=1;if(1===e)if(1===h)throw t;return[0,j,e,p,h,u]}throw t}function
dQ(o,c,n,m){var
d=o,e=n,h=m;for(;;){var
p=au(d,c,h),g=b(f[3],c,p);if(6===g[0]){var
j=g[3],k=g[2],q=g[1],l=U(S),r=V===l?S[1]:A===l?a(T[2],S):S;if(i(P[_],c,r,j))return[0,bH,bt(d,c,e,k)];var
d=b(f[bR],[0,q,k],d),e=e+1|0,h=j;continue}return[0,bP,bt(d,c,e,h)]}}function
dR(d,c,g){var
n=au(d,c,g),e=b(f[3],c,n);if(6===e[0]){var
k=e[3],l=e[2],o=e[1],m=U(S),p=V===m?S[1]:A===m?a(T[2],S):S;if(i(P[_],c,p,k)){var
h=aU(d,c,l);if(aG<=h[1]){var
j=h[2];return[0,bK,[0,j[1],j[2],j[3]]]}return[0,bN,h[2]]}try{var
q=dQ(b(f[bR],[0,o,l],d),c,1,k);return q}catch(a){a=r(a);if(a===t)return[0,aW,E(d,c,g)];throw a}}return aU(d,c,g)}function
Z(c,g,e){function
h(b){return a(e,a(f[23],[0,b,g]))}var
d=U(c),i=V===d?c[1]:A===d?a(T[2],c):c,k=a(p[57][61],i);return b(j[72][1],k,h)}function
ak(c,n,v){function
d(k){function
g(p){var
q=a(j[67][3],k),w=a(j[67][1],k);function
c(r){var
x=b(o[32][7],k,p),y=a(e[24][11],n),z=i(P[54],r,x,y),c=r,j=z,g=v,d=0,A=a(f[23],[0,p,n]);for(;;){if(0===g){var
B=[0,A,a(e[22][9],d)],s=a(f[41],B);return[0,an(Q[4],q,c,s,w),s]}var
h=b(f[3],c,j);if(6===h[0]){var
t=h[3],l=eM(dU[4],0,0,0,0,0,0,0,0,q,c,h[2]),m=l[2],u=l[1],c=u,j=b(f[aE][5],m,t),g=g-1|0,d=[0,m,d];continue}throw[0,ba,dT]}}return b(dV[1],0,c)}var
d=U(c),h=V===d?c[1]:A===d?a(T[2],c):c,l=a(p[57][61],h);return b(j[72][1],l,g)}return a(j[67][7],d)}function
aw(d,c){function
e(e){var
f=Q[2];function
g(a){return b(f,0,a)}var
h=i(o[32][1],g,e,c)[1],k=b(G[137],d,c),l=a(j[65][1],h);return b(j[18],l,k)}return a(j[67][7],e)}function
bv(c,b,a){return aC(bw[6],[0,dY[128]],0,dX,dW,c,b,a)}function
I(f,e){function
c(c){var
g=a(j[67][3],c),d=bv(g,a(o[32][3],c),f),h=d[1],i=a(e,d[2]),k=a(j[65][1],h);return b(j[18],k,i)}return a(j[67][7],c)}function
u(b){var
c=F(b);return a(f[9],c)}function
J(l){function
d(k){function
g(a){return b(o[32][6],k,a)}try{var
d=l[3];switch(d[0]){case
0:var
E=a(f[9],d[1]),h=a(G[46],E);break;case
1:var
F=a(f[9],d[1]),x=u(l[1]),H=u(l[2]),K=function(a){return Z(dI,[0,a,H,x,F],G[46])},h=I(g(x),K);break;case
2:var
y=d[1],M=u(y),N=function(a){var
b=G[46];return Z(bn,[0,a,u(y)],b)},h=I(g(M),N);break;case
3:var
z=d[2],s=d[1],O=u(s[1]),A=u(s[2]),P=u(z[2]),Q=function(a){var
c=ak(bo,[0,a,O,A,P],2),d=[0,J(z),0],e=[0,J(s),d];return b(p[57][21],c,e)},h=I(g(A),Q);break;case
4:var
t=d[2],v=d[1],n=u(v[1]),i=u(t[1]),q=u(v[2]),B=u(t[2]),R=function(e){function
d(h){function
d(d){var
g=a(m[1][6],dZ),j=b(o[32][10],g,k),l=[0,a(f[10],1),[0,i]],r=a(f[23],l),s=[0,b(L[4],[0,j],0),e,r],u=ak(aT,[0,e,d,a(f[21],s),n,q],1),w=ak(aT,[0,h,d,q,i,B],1),x=a(f[23],[0,q,[0,B]]),y=a(f[23],[0,q,[0,i]]),z=ak(bo,[0,d,a(f[23],[0,n,[0,i]]),y,x],2),A=a(c[3],d0),C=[0,b(p[57][5],0,A),0],D=[0,G[123],C],E=J(t),F=[0,b(p[57][3],w,E),D],H=[0,a(p[57][26],F),0],I=J(v),K=[0,b(p[57][3],u,I),H];return b(p[57][21],z,K)}return I(g(a(f[23],[0,n,[0,i]])),d)}return I(g(i),d)},h=I(g(n),R);break;default:var
w=d[1],S=d[4],T=d[3],U=d[2],C=u(w[1]),V=u(w[2]),D=u(l[1]),W=b(e[4],1,T),X=b(e[5],W,S),Y=a(f[10],X),_=function(c){function
d(r){var
e=U[1][2],d=a(o[32][3],k),g=a(f[10],1),h=a(o[32][4],k),i=aC(bu[39],h,d,e,g,c,Y,D),l=a(m[1][6],dS),n=[0,b(o[32][10],l,k)],q=[0,b(L[4],n,0),c,i],s=ak(aT,[0,c,r,a(f[21],q),C,V],1),t=J(w),u=b(p[57][3],s,t),v=a(j[65][1],d);return b(p[57][3],v,u)}return I(g(D),d)},h=I(g(C),_)}return h}catch(c){c=r(c);if(a(j[71][9],c))return b(j[21],0,c);throw c}}return a(j[67][7],d)}function
d2(c){function
d(d){var
e=Q[2];function
f(a){return b(e,0,a)}var
g=i(o[32][1],f,d,c)[1],h=a(G[46],c),k=a(j[65][1],g);return b(j[18],k,h)}return a(j[67][7],d)}function
bx(k,h,e,i){function
c(c){var
g=u(h),d=u(e);function
j(e){var
l=a(m[1][6],d3),h=b(o[32][10],l,c),n=a(m[1][6],d4),q=b(o[32][10],n,c),r=a(f[10],1),s=[0,b(L[4],[0,q],0),e,r],t=a(f[21],s),u=[0,Z(dF,[0,e,g,t,k,d,a(f[11],h)],d2),0],j=[0,e,g,d],v=[0,J(i),u],w=[0,h],y=Z(x,j,function(a){return aw(w,a)});return b(p[57][21],y,v)}return I(b(o[32][6],c,d),j)}return a(j[67][7],c)}function
by(ac,ab){function
d(g){var
C=a(o[32][3],g);a(O[12],O[14]);s(function(b){return a(c[3],d7)});var
z=a(o[32][4],g),q=a(o[32][3],g),d=a1(ac,[0,a(j[67][11],g),q]),r=[0,0],A=[0,0];function
R(a){M(d,E(z,q,a));return 0}b(e[22][11],R,ab);var
S=a(j[67][2],g);function
T(i){var
h=a(L[11][1][2],i),f=a(n[2],h),c=dR(z,q,a(L[11][1][4],i)),g=c[1];if(aG<=g){if(bH<=g)return bP<=g?aN(d,h,1,c[2]):aN(d,h,0,c[2]);if(bK<=g){var
j=c[2];return Y(d,[0,f],j[2],j[3])}var
k=c[2];return aM(d,f,k[2],k[3])}if(bN<=g){var
l=c[2],o=a(e[3],r),p=function(a){return Y(d,[2,a[1],f],a[2],l)};b(e[22][11],p,o);A[1]=[0,[0,f,l],a(e[3],A)];return 0}var
m=c[2],s=a(e[3],A);function
t(a){return Y(d,[2,f,a[1]],m,a[2])}b(e[22][11],t,s);r[1]=[0,[0,f,m],a(e[3],r)];return 0}b(e[22][11],T,S);var
B=aU(z,q,a(o[32][5],g));if(aG<=B[1]){var
K=B[2];Y(d,0,K[2],K[3])}else{var
U=B[2],V=a(e[3],r),W=function(a){return Y(d,[1,a[1]],a[2],U)};b(e[22][11],W,V)}s(function(b){return a(c[3],d8)});var
N=as(1,d);s(function(b){return a(c[3],d9)});var
h=d[1];if(N){var
t=N[1];s(function(b){return a(c[3],d_)});if(typeof
t==="number"){var
P=a(j[67][3],g),ad=h[5],ae=function(c){var
g=ag(h,c[1]),i=c[3];function
j(a){return u(D(h,a))}var
k=b(e[22][14],j,i),d=g[1],l=d[1],m=c[2],n=[0,l,a(f[2][1],d[2])],o=[0,a(f[30],n),k];return[0,a(f[41],o),m]},af=b(e[22][68],ae,ad),ai=b(bz[3],0,d$),aj=function(a){var
c=a[2],d=aC(eb[9],0,ea,0,m[1][10][1],P,C,a[1]);function
f(a){return ai}var
g=[4,d,b(e[22][56],c,f)],h=b(bz[3],0,g);return b(ah[27],P,h)},ak=a(c[3],ec),al=a(c[3],ed),am=function(h){var
d=a(c[3],ee),e=a(c[13],0),f=a(c[3],ef),g=b(c[12],f,e);return b(c[12],g,d)},an=i(c[39],am,aj,af),ao=a(c[3],eg),ap=b(c[12],ao,an),aq=b(c[12],ap,al),ar=b(c[26],8,aq),at=a(c[3],eh),au=a(c[5],0),av=a(c[3],ei),ax=b(c[12],av,au),ay=b(c[12],ax,at),az=b(c[12],ay,ar),aA=b(c[12],az,ak);return b(p[57][4],0,aA)}else{if(0===t[0]){var
v=t[1],Q=v[2],aB=[0,bI,[0,v[1],Q,v[3],v[4]]],F=aS(a(o[32][4],g),C,h,aB);ag(h,Q[1]);var
aa=function(c){var
d=u(F[1]),g=u(F[2]),h=a(j[67][3],c),i=a(o[32][3],c),e=bv(h,i,b(o[32][6],c,d)),k=e[2],l=e[1],n=a(m[1][6],d6),f=b(o[32][10],n,c),r=[0,a(bu[17],f),0],q=[0,k,d,g],s=[0,J(F),r],t=[0,f],y=Z(x,q,function(a){return aw(t,a)}),v=b(p[57][21],y,s),w=a(j[65][1],l);return b(p[57][3],w,v)};return a(j[67][7],aa)}var
l=t[1],aD=a(j[67][3],g),w=aS(aD,C,h,[0,-608347012,[0,l[1],l[2]]]),H=D(h,l[1]),y=D(h,l[2]),k=l[3];if(typeof
k==="number")return J(w);else
switch(k[0]){case
0:var
aE=a(f[9],k[1]),X=function(c){var
d=u(H),g=u(y),h=a(m[1][6],d1),e=b(o[32][10],h,c),i=[0,aE,[0,a(f[11],e)]],j=a(f[23],i);function
k(c){var
h=[0,a(G[_],j),0],f=[0,c,d,g],i=[0,J(w),h],k=[0,e],l=Z(x,f,function(a){return aw(k,a)});return b(p[57][21],l,i)}return I(b(o[32][6],c,d),k)};return a(j[67][7],X);case
1:return bx(a(f[9],k[1]),H,y,w);default:var
aF=k[2],aH=a(f[9],k[1]),aI=a(f[9],aF),$=function(d){var
e=u(y),g=a(m[1][6],d5),c=b(o[32][10],g,d),h=[0,aI,[0,a(f[11],c)]],i=a(f[23],h),j=[0,a(G[_],i),0],k=[0,bx(aH,H,y,w),j],l=aw([0,c],e);return b(p[57][21],l,k)};return a(j[67][7],$)}}}var
aJ=a(c[3],ej);return b(p[57][4],0,aJ)}return a(j[67][7],d)}var
el=a(c[3],ek),bA=b(p[57][5],0,el);function
ad(d,c){var
e=by(d,c),f=a(p[57][32],G[17]),g=b(p[57][3],f,e);return b(p[57][12],g,bA)}function
em(c,d,l,k){function
g(m){function
c(c){var
n=Q[2];function
p(a){return b(n,0,a)}var
e=i(o[32][1],p,c,d),q=e[2],r=e[1],s=a(o[32][4],c),g=aC(bw[6],0,0,0,en,s,r,q),t=g[1],h=a(f[23],[0,m,[0,g[2],d,l]]),u=a(o[32][4],c),v=an(Q[2],0,u,t,h)[1],w=a(k,h),x=a(j[65][1],v);return b(j[18],x,w)}return a(j[67][7],c)}var
e=U(c),h=V===e?c[1]:A===e?a(T[2],c):c,m=a(p[57][61],h);return b(j[72][1],m,g)}function
eo(m){var
z=a(j[67][1],m),c=a(o[32][3],m);function
B(d,c){try{var
e=0,f=G[87],g=[0],h=function(a){return Z(bn,g,a)}(f),i=[0,a(p[57][24],h),e],k=[0,a(j[16],0),i],l=em(x,d,c,G[144]),m=b(p[57][21],l,k);return m}catch(c){c=r(c);if(a(j[71][9],c))return b(j[21],0,c);throw c}}function
C(d){var
c=d[1],e=d[2];if(c[1]!==ep[1])if(c[1]!==eq[1])return b(j[21],[0,e],c);return a(j[16],0)}var
h=b(f[3],c,z);if(9===h[0]){var
k=h[2];if(3===k.length-1){var
D=h[1],E=k[2],F=k[3],s=U(x),H=V===s?x[1]:A===s?a(T[2],x):x;if(i(P[_],c,H,D)){var
t=b(f[3],c,E),u=b(f[3],c,F);if(9===t[0])if(9===u[0]){var
w=u[2],l=t[2];if(l.length-1===w.length-1)var
y=function(c){if(0<=c){var
d=y(b(e[5],c,1)),f=q(w,c)[1+c],g=B(q(l,c)[1+c],f);return b(p[57][16],g,d)}var
h=ad(aV,0);return a(p[57][24],h)},v=y(b(e[5],l.length-1,1)),g=1;else
var
g=0}else
var
g=0;else
var
g=0;if(!g)var
v=a(j[16],0);var
n=v,d=1}else
var
d=0}else
var
d=0}else
var
d=0;if(!d)var
n=a(j[16],0);return b(j[23],n,C)}var
bB=a(j[67][7],eo);al(168,[0,J,by,bA,ad,bB],"Cc_plugin__Cctac");a(er[9],ax);var
es=0;function
et(b,a,c){return ad(b,a)}var
ev=[0,eu,[1,[0,[5,a(az[16],ay[11])]],0]],ex=[0,[0,[0,ew,[1,[5,a(az[16],ay[15])],ev]],et],es];function
ey(a,b){return ad(aV,a)}var
eB=[0,[0,[0,eA,[0,ez,[1,[0,[5,a(az[16],ay[11])]],0]]],ey],ex];function
eC(a,b){return ad(a,0)}var
eE=[0,[0,[0,eD,[1,[5,a(az[16],ay[15])],0]],eC],eB],eG=[0,[0,eF,function(a){return ad(aV,0)}],eE];aB(bC[8],ax,eH,0,0,eG);var
eI=0,eK=[0,[0,eJ,function(a){return bB}],eI];aB(bC[8],ax,eL,0,0,eK);al(173,[0,ax],"Cc_plugin__G_congruence");return}
