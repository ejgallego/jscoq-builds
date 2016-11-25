(function(e3){"use strict";var
ag="*",af=140,be="x",bj="plugins/nsatz/polynom.ml",a9="+",a8="@[%s@]",p=250,aJ="(",M="setoid_ring",bb="+ ",bi="Init",a7=124,a6="...",k=246,bd="[",bh="h",aH=" ",ah="1",aG="nsatz_compute",a5="print_pol dans dansideal",bf="]",bg=")*",T="plugins/nsatz/nsatz.ml",aF="nsatz_plugin",ba="c",C="",a$="term computed\n",aq="^",L="Ring_polynom",J="0",a_="$lt",I=136,bc="u",y="\n",v="CC",aI=1e3,u=e3.jsoo_runtime,f=u.caml_check_bound,ap=u.caml_equal,aE=u.caml_int_compare,a4=u.caml_int_of_string,e1=u.caml_list_of_js_array,x=u.caml_make_vect,b=u.caml_new_string,r=u.caml_obj_tag,S=u.caml_register_global,ad=u.caml_string_equal,ae=u.caml_string_notequal,A=u.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):u.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):u.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):u.caml_call_gen(a,[b,c,d])}function
e2(a,b,c,d,e){return a.length==4?a(b,c,d,e):u.caml_call_gen(a,[b,c,d,e])}var
o=u.caml_get_global_data(),E=[0,0],O=[0,1],W=[0,2],ay=[0,b(bi),[0,b("Datatypes"),0]],F=[0,b("Numbers"),[0,b("BinNums"),0]],ao=b(aF),U=o.CErrors,ar=o.Array,h=o.List,d=o.Pervasives,aj=o.Flags,ai=o.Format,e=o.Util,K=o.Assert_failure,as=o.Failure,aL=o.Char,D=o.Hashtbl,V=o.Not_found,au=o.String,n=o.Num,q=o.CamlinternalLazy,w=o.Term,t=o.Coqlib,aN=o.Unix,z=o.Big_int,av=o.Ratio,aC=o.Names,a2=o.Tacenv,aD=o.Mltop,bq=b(y),bn=[0,[18,[1,[0,0,b(C)]],[2,0,[12,10,[17,0,0]]]],b("@[%s\n@]")],bl=[0,[18,[1,[0,0,b(C)]],[2,0,[17,0,0]]],b(a8)],bx=[0,b(bj),363,43],bW=b(a6),bz=b(")"),bA=b(aJ),bB=b(C),bC=b(C),bD=b(J),bU=b(a6),bV=b(a9),bF=b(C),bG=b(J),bH=b(ah),bJ=b(bg),bK=b(aJ),bL=b(ag),bM=b(J),bN=b(ah),bO=b(aq),bP=b(aq),bQ=b(bg),bR=b(aJ),bS=b(aq),bT=b(ag),bI=b(a9),bE=b(J),b1=b("div_pol1"),b2=[0,b(bj),475,9],b3=b(y),b4=b("x:"),b5=b(y),b6=b("r:"),b7=b(y),b8=b("q:"),b9=b(y),b_=b("p:"),b$=b("div_pol:\n"),bZ=b(y),bY=b("\n{ "),b0=b("}"),bX=[0,[18,[1,[0,0,b(C)]],[2,0,[17,0,0]]],b(a8)],by=b(bc),bw=b("non"),bu=[0,0],bv=[0,1],c6=b(y),c2=b("homogeneous polynomials\n"),c3=b(y),c4=b("p: "),c5=b(C),c7=b("lp:\n"),c8=b("computed\n"),cX=b(ba),c1=b(bh),cY=b(y),cZ=b("\nnew polynomial: "),c0=b(J),cW=b("computation of the Groebner basis\n"),cM=b(y),cN=b("remainder: "),cO=b("polynomial reduced to 0\n"),cP=b("r ok\n"),cQ=b(y),cR=b("r: "),cS=b(y),cT=b("verif sum: "),cU=b(y),cV=b("coefficient: "),cJ=b(bf),cK=b(","),cL=b(bd),cI=[0,0,0],cG=b(" terms"),cH=b(" + "),cF=b(a5),cE=b(a5),cp=b(J),cq=b(ah),cr=b(aq),ck=b(ag),cl=b(ag),cm=b(ag),cn=b(ah),co=b(C),cs=b("-1"),ct=b(J),cu=b(ah),cx=b(bb),cy=b(C),cz=b(aH),cA=b("-"),cv=b("- "),cw=b(bb),cB=b(J),cC=b(C),cD=b(aH),cj=b(aH),ch=b(" i="),ci=b("lv= "),cf=b(bf),cg=b(bd),cc=[0,0],cd=[0,1],ce=[0,-1],cb=b("Ideal.NotInIdeal"),ea=[0,b(T),258,9],el=[0,0,0],eM=b("nsatz cannot solve this problem"),eJ=b("refl_equal"),eK=[0,b(bi),[0,b("Logic"),0]],eL=b(v),eH=b(a$),eo=b("nsatz: bad parameter"),ey=b("computation without sugar\n"),ez=b("computation with sugar\n"),eA=b("ordre lexico computation without sugar\n"),eB=b("ordre lexico computation with sugar\n"),eC=b("computation without sugar, division by pairs\n"),eD=b("computation with sugar, division by pairs\n"),eE=b("ordre lexico computation without sugar, division by pairs\n"),eF=b("ordre lexico computation with sugar, division by pairs\n"),ew=b(C),ex=b(be),ep=e1([b("a"),b("b"),b(ba),b("d"),b("e"),b("f"),b("g"),b(bh),b("i"),b("j"),b("k"),b("l"),b("m"),b("n"),b("o"),b("p"),b("q"),b("r"),b("s"),b("t"),b(bc),b("v"),b("w"),b(be),b("y"),b("z")]),eq=b("cert ok\n"),er=b(y),es=b("number of parameters: "),et=b(a$),eu=[0,b(T),568,11],ev=[0,b(T),556,14],en=[0,b(T),585,8],em=[0,b(T),505,11],ej=[0,[18,[1,[0,0,b(C)]],[8,0,[0,1,10],[0,3],[17,0,[11,b("s\n"),0]]]],b("@[%10.3f@]s\n")],ek=b("time: "),ef=b(y),eg=b("useless spolynomials: "),eh=b(y),ei=b("useful spolynomials: "),ee=[0,b(T),412,12],ec=[0,0,0],ed=b(J),d$=[0,0],d8=b("Npos"),d9=b(v),d6=b("N0"),d7=b(v),d4=b("xH"),d5=b(v),d2=b("xO"),d3=b(v),dZ=b("xI"),d0=b(v),dW=b("Zneg"),dX=b(v),dU=b("Zpos"),dV=b(v),dS=b("Z0"),dT=b(v),dQ=b("Z"),dR=b(v),dO=b("cons"),dP=b(v),dM=b("nil"),dN=b(v),dK=b("list"),dL=b(v),dH=b("PEpow"),dI=[0,b(M),[0,b(L),0]],dJ=b(v),dE=b("PEopp"),dF=[0,b(M),[0,b(L),0]],dG=b(v),dB=b("PEmul"),dC=[0,b(M),[0,b(L),0]],dD=b(v),dy=b("PEsub"),dz=[0,b(M),[0,b(L),0]],dA=b(v),dv=b("PEadd"),dw=[0,b(M),[0,b(L),0]],dx=b(v),ds=b("PEX"),dt=[0,b(M),[0,b(L),0]],du=b(v),dp=b("PEc"),dq=[0,b(M),[0,b(L),0]],dr=b(v),dl=b("PExpr"),dm=[0,b(M),[0,b(L),0]],dn=b(v),eW=b("Extension: cannot occur"),eS=b(aG),eT=b(a_),eU=b(aG),eP=b(aF),eQ=b(aF),eX=b(a_),e0=b(aG),ca=o.CList,c9=o.Tactics,eO=o.Loc,eN=o.Tacinterp;function
bk(b){return aj[24][1]?(a(ai[96],bl,b),c(d[46],d[24])):0}function
bm(b){return aj[24][1]?(a(ai[96],bn,b),c(d[46],d[24])):0}function
bo(a){return 0}function
bp(b){if(aj[24][1]){var
e=a(d[16],b,bq);c(d[27],e);return c(d[46],d[24])}return 0}function
br(b){return a(aj[51],d[34],b)}function
aK(e,d,c){var
b=c;for(;;){if(b){var
f=b[2];if(a(e,d,b[1]))return 1;var
b=f;continue}return 0}}function
bs(e,d){var
b=[0,0];function
f(a){var
c=1-aK(e,a,b[1]),d=c?(b[1]=[0,a,b[1]],0):c;return d}a(h[11],f,d);return c(h[6],b[1])}function
bt(k,f,o){function
p(a){return 1-c(f,a)}var
e=0,b=a(h[30],p,o);for(;;){if(b){var
g=b[2],i=b[1],j=[0,0],l=[0,0],q=function(h,d,i){return function(e){try{var
b=a(k,h,e),g=c(f,b)?(i[1]=1,0):(d[1]=[0,b,d[1]],0);return g}catch(a){a=A(a);if(c(U[22],a))return 0;throw a}}}(i,j,l);a(h[11],q,e);if(l[1]){var
b=g;continue}if(0===j[1]){var
m=[0,g],n=[0,0],r=function(j,g,h){return function(b){try{var
d=a(k,b,j),e=1-c(f,d),i=e?(g[1]=[0,d,g[1]],0):e;return i}catch(a){a=A(a);if(c(U[22],a)){h[1]=[0,b,h[1]];return 0}throw a}}}(i,m,n);a(h[11],r,e);var
s=m[1],e=c(h[6],[0,i,n[1]]),b=s;continue}var
b=a(d[22],j[1],g);continue}return e}}var
m=[0,bk,bm,bo,bp,br,aK,bs,bt,function(k,j,d,b,e){var
g=x(b.length-1,[0,d,0]);function
h(i,h){var
b=[0,h],d=[0,0];if(1-c(j,h)){var
l=function(f,e){try{for(;;){var
g=a(k,b[1],e);d[1]=[0,f,d[1]];b[1]=g;continue}}catch(a){a=A(a);if(c(U[22],a))return 0;throw a}};a(ar[14],l,e)}var
m=[0,b[1],d[1]];return f(g,i)[i+1]=m}a(ar[14],h,b);return[0,e,g]}];S(p,m,"Nsatz_plugin.Utile");var
at=[0,function(g){function
G(a){return c(g[14],[0,a])}var
b=G(0),T=G(1);function
H(a){return[0,c(g[14],a)]}var
k=H(bu),U=H(bv);function
V(a){return[1,a,[0,k,U]]}function
I(d,a){if(0===a)return[0,T];var
c=x(a+1|0,[0,b]);f(c,a)[a+1]=[0,T];return[1,d,c]}function
ah(a){return 0===a[0]?1:0}function
aj(a){return 0===a[0]?a[1]:c(d[2],bw)}function
ak(c){return 0===c[0]?a(g[1],c[1],b)?1:0:0}function
s(a){return 0===a[0]?0:a[1]}function
J(b){if(0===b[0])return 0;var
c=b[2],f=b[1];function
g(c,b){var
e=J(c);return a(d[5],e,b)}return i(e[19][18],g,c,f)}function
al(b){var
c=0;function
f(c,b){var
e=J(c);return a(d[5],e,b)}return i(e[19][18],f,b,c)}function
n(c,b){if(0===c[0]){var
f=c[1];if(0===b[0])return a(g[1],f,b[1])}else{var
h=c[2],j=c[1];if(0!==b[0]){var
d=j===b[1]?1:0,k=b[2];return d?i(e[19][31],n,h,k):d}}return 0}function
W(d){if(0===d[0])return d;var
e=d[2],g=e.length-1-1|0,a=[0,g],k=d[1];for(;;){if(0<a[1]){var
h=a[1];if(n(f(e,h)[h+1],[0,b])){a[1]=a[1]-1|0;continue}}if(0<=a[1]){if(0===a[1])return f(e,0)[1];if(a[1]===g)return d;var
i=x(a[1]+1|0,[0,b]),j=a[1],l=0;if(!(j<0)){var
c=l;for(;;){var
m=f(e,c)[c+1];f(i,c)[c+1]=m;var
o=c+1|0;if(j!==c){var
c=o;continue}break}}return[1,k,i]}return[0,b]}}function
h(b,a){if(1===a[0]){var
c=a[2];if(a[1]===b)return c.length-1-1|0}return 0}function
X(c){if(0===c[0])return 0;var
b=[0,0],f=c[2];function
g(e,c){var
f=e+X(c)|0;b[1]=a(d[5],b[1],f);return 0}a(e[19][14],g,f);return b[1]}function
u(b){if(0===b[0])return[0,b[1]];var
c=b[1];return[1,c,a(e[19][15],u,b[2])]}function
y(e,c,a){if(1===a[0]){var
d=a[2];if(a[1]===e)return c<d.length-1?f(d,c)[c+1]:[0,b]}return 0===c?a:[0,b]}function
p(j,c){if(0===j[0]){var
D=j[1];if(0===c[0])var
q=[0,a(g[5],D,c[1])];else{var
s=c[2],E=c[1],t=a(e[19][15],u,s),F=p(j,f(s,0)[1]);f(t,0)[1]=F;var
q=[1,E,t]}var
r=q}else{var
l=j[2],i=j[1];if(0===c[0]){var
v=a(e[19][15],u,l),G=p(f(l,0)[1],c);f(v,0)[1]=G;var
w=[1,i,v]}else{var
z=c[2],m=c[1];if(i<m){var
A=a(e[19][15],u,z),H=p(j,f(z,0)[1]);f(A,0)[1]=H;var
n=[1,m,A]}else
if(m<i){var
B=a(e[19][15],u,l),I=p(f(l,0)[1],c);f(B,0)[1]=I;var
n=[1,i,B]}else{var
J=h(i,c),K=h(i,j),o=a(d[5],K,J),C=x(o+1|0,[0,b]),L=0;if(!(o<0)){var
k=L;for(;;){var
M=y(i,k,c),N=p(y(i,k,j),M);f(C,k)[k+1]=N;var
O=k+1|0;if(o!==k){var
k=O;continue}break}}var
n=[1,i,C]}var
w=n}var
r=w}return W(r)}function
L(d){if(0===d[0])return c(g[4],d[1]);var
f=a(e[19][15],L,d[2]);return i(e[19][17],g[12],b,f)}function
M(b,c){if(0===b[0])return[0,a(g[9],b[1],c)];var
d=b[2],f=b[1];function
h(a){return M(a,c)}return[1,f,a(e[19][15],h,d)]}function
Y(c){var
d=L(c);return a(g[1],d,b)?c:M(c,d)}function
Z(b){if(0===b[0])return 0;var
d=b[1],f=c(e[19][11],b[2]),g=[0,[0,d,0],a(e[17][12],Z,f)];return c(e[17][10],g)}function
_(d,g,c){if(1===c[0]){var
e=c[2],i=c[1];if(i===g){var
j=x(e.length-1+d|0,[0,b]),k=e.length-1-1|0,m=0;if(!(k<0)){var
a=m;for(;;){var
l=a+d|0,o=f(e,a)[a+1];f(j,l)[l+1]=o;var
p=a+1|0;if(k!==a){var
a=p;continue}break}}return[1,i,j]}}if(n(c,[0,b]))return[0,b];var
h=x(d+1|0,[0,b]);f(h,d)[d+1]=c;return[1,g,h]}function
o(d,c){if(0===d[0]){var
k=d[1];if(0===c[0])return[0,a(g[6],k,c[1])];var
l=c[2],m=c[1];if(a(g[1],k,b))return[0,b];var
n=function(a){return o(d,a)};return[1,m,a(e[19][15],n,l)]}var
h=d[2],f=d[1];if(0===c[0]){if(a(g[1],c[1],b))return[0,b];var
q=function(a){return o(a,c)};return[1,f,a(e[19][15],q,h)]}var
j=c[1],r=c[2];if(f<j){var
s=function(a){return o(d,a)};return[1,j,a(e[19][15],s,r)]}if(j<f){var
t=function(a){return o(a,c)};return[1,f,a(e[19][15],t,h)]}function
u(b,a){return _(b,f,o(a,c))}var
v=a(e[19][16],u,h);return i(e[19][17],p,[0,b],v)}function
am(k,c){if(0===c[0])return[0,b];var
d=c[2],g=c[1];if(g===k){var
e=d.length-1-1|0;if(1===e)return f(d,1)[2];var
h=x(e,[0,b]),i=e-1|0,l=0;if(!(i<0)){var
a=l;for(;;){var
j=a+1|0,m=f(d,j)[j+1],n=o([0,G(a+1|0)],m);f(h,a)[a+1]=n;var
p=a+1|0;if(i!==a){var
a=p;continue}break}}return[1,g,h]}return[0,b]}function
t(b){if(0===b[0])return[0,c(g[8],b[1])];var
d=b[1];return[1,d,a(e[19][15],t,b[2])]}function
N(b,a){return p(b,t(a))}function
z(b,a){return 0===a?U:o(b,z(b,a-1|0))}function
l(b,a){return o(b,a)}function
O(b,a){return N(b,a)}function
B(b,a){return z(b,a)}function
q(b,a){return y(b,h(b,a),a)}function
an(b,a){return y(b,0,a)}function
C(b,a){var
c=h(b,a),d=z(V(b),c);return N(a,o(q(b,a),d))}function
P(c){var
a=c;for(;;){var
b=s(a);if(0<b){var
a=q(b,a);continue}if(0===a[0])return a[1];throw[0,K,bx]}}function
ao(d){var
c=Y(d),e=P(c);return a(g[3],b,e)?c:t(c)}var
$=[0,1];function
ap(b){var
a=b;for(;;){if(0===a[0])return a[1];var
a=f(a[2],0)[1];continue}}function
aa(b){if($[1]){var
f=c(d[20],b);return a(d[16],by,f)}if(3<b){var
g=c(aL[1],(b-4|0)+97|0);return a(e[15][1],1,g)}var
h=c(aL[1],b+119|0);return a(e[15][1],1,h)}var
j=[0,0];function
Q(n){if(0<j[1]){if(0===n[0]){var
o=n[1];j[1]=j[1]-1|0;if(a(g[3],b,o))return c(g[15],o);var
u=c(g[15],o),v=a(d[16],u,bz);return a(d[16],bA,v)}var
p=n[2],m=aa(n[1]),i=[0,bB],h=[0,bC],r=Q(f(p,0)[1]);if(1-ad(r,bD))i[1]=r;var
q=[0,0],s=p.length-1-1|0;if(!(s<1)){var
l=s;for(;;){if(0<=j[1]){var
k=Q(f(p,l)[l+1]);h[1]=bF;if(1===l){if(1-ad(k,bG)){j[1]=j[1]-1|0;if(ad(k,bH))h[1]=m;else
if(a(e[15][17],k,43)){var
z=a(d[16],bJ,m),A=a(d[16],k,z);h[1]=a(d[16],bK,A)}else{var
B=a(d[16],bL,m);h[1]=a(d[16],k,B)}}}else
if(1-ad(k,bM)){j[1]=j[1]-1|0;if(ad(k,bN)){var
C=c(d[20],l),D=a(d[16],bO,C);h[1]=a(d[16],m,D)}else
if(a(e[15][17],k,43)){var
E=c(d[20],l),F=a(d[16],bP,E),G=a(d[16],m,F),H=a(d[16],bQ,G),I=a(d[16],k,H);h[1]=a(d[16],bR,I)}else{var
J=c(d[20],l),K=a(d[16],bS,J),L=a(d[16],m,K),M=a(d[16],bT,L);h[1]=a(d[16],k,M)}}var
t=1-c(e[15][30],h[1]),w=t?1-q[1]:t;if(w){j[1]=j[1]-1|0;if(c(e[15][30],i[1]))i[1]=h[1];else{var
y=a(d[16],bI,h[1]);i[1]=a(d[16],i[1],y)}}}else{h[1]=bU;if(1-q[1]){var
N=a(d[16],bV,h[1]);i[1]=a(d[16],i[1],N)}q[1]=1}var
x=l-1|0;if(1!==l){var
l=x;continue}break}}if(c(e[15][30],i[1])){j[1]=j[1]-1|0;i[1]=bE}return i[1]}return bW}function
v(a){j[1]=20;return Q(a)}function
aq(b){var
c=v(b);return a(ai[96],bX,c)}function
ab(f){var
b=[0,bY];function
g(c){var
e=v(c),f=a(d[16],e,bZ);b[1]=a(d[16],b[1],f);return 0}a(e[19][13],g,f);var
h=a(d[16],b[1],b0);return c(m[3],h)}function
ar(a){return ab(c(e[19][12],a))}function
E(j,i,e){if(0===e){if(0===j[0]){var
o=j[1];if(0===i[0]){var
s=i[1],x=a(g[10],o,s);return a(g[1],x,b)?[0,[0,a(g[9],o,s)],k]:c(d[2],b1)}}throw[0,K,b2]}var
t=h(e,i),y=q(e,i),f=[0,j],m=[0,k],u=[0,1],z=C(e,i);for(;;){if(u[1])if(!n(f[1],k)){var
v=h(e,f[1]);if(v<t)u[1]=0;else{var
A=q(e,f[1]),B=C(e,f[1]),D=r(A,y,e-1|0),w=l(D,I(e,v-t|0));m[1]=p(m[1],w);f[1]=O(B,l(w,z))}continue}return[0,m[1],f[1]]}}function
r(f,e,b){var
g=E(f,e,b),h=g[2],i=g[1];if(n(h,k))return i;var
j=c(d[20],b),l=a(d[16],j,b3),m=a(d[16],b4,l),o=a(d[16],b5,m),p=v(h),q=a(d[16],p,o),r=a(d[16],b6,q),s=a(d[16],b7,r),t=v(e),u=a(d[16],t,s),w=a(d[16],b8,u),x=a(d[16],b9,w),y=v(f),z=a(d[16],y,x),A=a(d[16],b_,z),B=a(d[16],b$,A);return c(d[2],B)}function
at(c,b){var
e=s(b),f=s(c);return r(c,b,a(d[5],f,e))}function
au(c,b){var
f=s(b),g=s(c),e=a(d[5],g,f);try{var
i=h(e,b),j=(1+h(e,c)|0)-i|0;r(o(c,z([0,P(b)],j)),b,e);var
k=1;return k}catch(a){a=A(a);if(a[1]===as)return 0;throw a}}function
av(d,b,a){if(0===b[0])return[0,k,b,1,d];if(a===b[1]){var
e=[0,0],c=[0,d],f=q(a,b),j=C(a,b),g=[0,k],m=h(a,b);for(;;){var
n=h(a,b);if(n<=h(a,c[1])){var
o=h(a,c[1]),r=q(a,c[1]),s=C(a,c[1]),i=l(r,I(a,o-m|0)),t=l(i,j);c[1]=O(l(f,s),t);g[1]=p(l(f,g[1]),i);e[1]=e[1]+1|0;continue}return[0,c[1],f,e[1],g[1]]}}return[0,k,b,1,d]}function
aw(c,b){var
e=s(b),f=s(c);return ac(c,b,a(d[5],f,e))}function
ac(c,b,a){return w(c,b,a)}function
F(a,b){if(1===a[0]){var
c=a[2];if(a[1]===b){var
d=function(c,a){return w(c,a,b-1|0)};return i(e[19][17],d,k,c)}}return a}function
R(c,a,b){if(1===a[0]){var
d=a[2];if(b===a[1]){var
f=function(c,a){return w(c,a,b-1|0)};return i(e[19][17],f,c,d)}}return w(c,a,b-1|0)}function
w(f,e,b){if(0===f[0]){var
o=f[1];if(0===e[0]){var
p=c(g[4],e[1]),q=c(g[4],o);return[0,a(g[12],q,p)]}}if(n(f,k))return e;if(n(e,k))return f;if(0===h(b,e))return R(e,f,b);if(0===h(b,f))return R(f,e,b);var
s=F(f,b),i=w(s,F(e,b),b-1|0),t=c(d[20],b);c(m[1],t);var
u=r(f,i,b),j=ae(u,r(e,i,b),b);return l(i,r(j,F(j,b),b))}function
ae(j,i,b){var
c=j,a=i;for(;;){if(n(a,k))return c;var
f=h(b,c),d=h(b,a);if(f<d){var
p=a,a=c,c=p;continue}var
g=f-d|0,e=q(b,a),m=t(a),o=E(l(B(t(e),g+1|0),c),m,b)[2];return af(a,o,B(e,g),e,d,b)}}function
af(u,s,p,o,m,b){var
d=u,a=s,c=p,i=o,g=m;for(;;){if(n(a,k))return d;var
j=h(b,a),e=q(b,a),f=g-j|0,v=t(a),w=E(l(B(t(e),f+1|0),d),v,b)[2],x=ag(e,c,f,b),d=a,a=r(w,l(i,B(c,f)),b),c=x,i=e,g=j;continue}}function
ag(c,g,f,e){var
a=[0,c],d=f-1|0,h=1;if(!(d<1)){var
b=h;for(;;){a[1]=r(l(a[1],c),g,e);var
i=b+1|0;if(d!==b){var
b=i;continue}break}}return a[1]}function
S(a){if(0===a[0])return c(g[13],a[1]);var
b=a[2],d=0;function
f(b,a){return a+S(b)|0}return i(e[19][18],f,b,d)}return[0,H,V,I,ah,ak,s,J,al,n,W,h,X,u,y,p,L,M,Y,Z,aj,_,o,am,t,N,z,l,O,B,q,an,C,P,ao,ap,$,aa,j,v,aq,ab,ar,E,r,at,au,av,aw,ac,F,R,w,ae,af,ag,S,c(D[18],[0,n,S])]}];S(256,at,"Nsatz_plugin.Polynom");var
ak=[248,cb,u.caml_fresh_oo_id(0)],aM=[0,0],B=[0,function(g){var
al=c(g[1],cc),l=c(g[1],cd),am=c(g[1],ce);function
j(a){return a.length-1-1|0}function
T(c,d){var
h=j(c);if(aM[1]){var
e=[0,0],a=[0,1];for(;;){if(0===e[1])if(a[1]<=h){var
i=a[1],o=f(d,i)[i+1],k=a[1];e[1]=aE(f(c,k)[k+1],o);a[1]=a[1]+1|0;continue}return e[1]}}var
p=f(d,0)[1],l=aE(f(c,0)[1],p);if(0===l){var
g=[0,0],b=[0,h];for(;;){if(0===g[1])if(1<=b[1]){var
m=b[1],q=f(d,m)[m+1],n=b[1];g[1]=-aE(f(c,n)[n+1],q)|0;b[1]=b[1]-1|0;continue}return g[1]}}return l}function
B(c,e){var
b=j(c),d=x(b+1|0,0),g=0;if(!(b<0)){var
a=g;for(;;){var
h=f(e,a)[a+1],i=f(c,a)[a+1]-h|0;f(d,a)[a+1]=i;var
k=a+1|0;if(b!==a){var
a=k;continue}break}}return d}function
U(c,g){var
b=[0,1],a=[0,0],h=j(c);for(;;){if(b[1])if(a[1]<=h){var
d=a[1],i=f(g,d)[d+1],e=a[1];b[1]=i<=f(c,e)[e+1]?1:0;a[1]=a[1]+1|0;continue}return b[1]}}function
M(a){var
c=j(a);f(a,0)[1]=0;var
d=1;if(!(c<1)){var
b=d;for(;;){var
e=f(a,0)[1];a[1]=f(a,b)[b+1]+e|0;var
g=b+1|0;if(c!==b){var
b=g;continue}break}}return a}function
C(e,h){var
c=j(e),g=x(c+1|0,0),i=1;if(!(c<1)){var
b=i;for(;;){var
k=f(h,b)[b+1],l=f(e,b)[b+1],m=a(d[5],l,k);f(g,b)[b+1]=m;var
n=b+1|0;if(c!==b){var
b=n;continue}break}}return M(g)}function
an(a){return a}function
ao(c,b){var
e=b[2],f=c[2],d=a(g[9],c[1],b[1]),h=d?ap(f,e):d;return h}var
N=c(e[17][92],ao),aq=[0,N,function(b){function
d(a){return a[1]}var
e=a(h[13],d,b);function
f(a){return a[2]}var
j=a(h[13],f,b),k=c(D[20],j);function
l(b,a){return(b*17|0)+c(g[56],a)|0}return i(h[16],l,k,e)}],at=c(D[18],aq);function
W(f,e){try{var
b=a(h[5],f,e);return b}catch(b){b=A(b);if(b[1]===as){var
g=c(d[20],e),j=a(d[16],ch,g),k=function(c,b){var
e=a(d[16],cj,b);return a(d[16],c,e)},l=i(h[16],k,ci,f);return a(d[16],l,j)}throw b}}var
O=[0,0];function
w(b){function
h(j,i){var
e=[0,0],k=j.length-1-1|0,l=1;if(!(k<1)){var
b=l;for(;;){var
s=f(j,b)[b+1],h=c(d[20],s);if(ae(h,cp))if(ae(h,cq)){var
n=a(d[16],cr,h),o=W(O[1],b-1|0),p=[0,a(d[16],o,n),0];e[1]=a(d[22],e[1],p)}else{var
r=[0,W(O[1],b-1|0),0];e[1]=a(d[22],e[1],r)}var
q=b+1|0;if(k!==b){var
b=q;continue}break}}var
g=e[1];if(g){if(i)return a(au[7],ck,g);var
m=a(au[7],cl,g);return a(d[16],cm,m)}return i?cn:co}function
l(j,k){var
z=j?0:1;if(z)return k?cB:cC;var
A=0,B=j?j[2]:c(d[2],cE),C=l(B,A),D=a(d[16],cD,C),m=j?j[1]:c(d[2],cF),f=m[2],n=c(g[39],m[1]),o=a(d[16],n,cf),b=a(d[16],cg,o);if(ae(b,cs))if(ae(b,ct))if(ae(b,cu))if(45===u.caml_string_get(b,0))var
p=h(f,0),q=i(au[4],b,1,u.caml_ml_string_length(b)-1|0),r=a(d[16],q,p),e=a(d[16],cv,r);else
if(0===k)var
s=h(f,0),t=a(d[16],b,s),e=a(d[16],cw,t);else
var
v=h(f,0),e=a(d[16],b,v);else
if(0===k)var
w=h(f,1),e=a(d[16],cx,w);else
var
e=h(f,1);else
var
e=cy;else
var
x=h(f,1),y=a(d[16],cz,x),e=a(d[16],cA,y);return a(d[16],e,D)}return l(b,1)}var
P=[0,d[7]];function
E(b){P[1]=10;var
f=P[1];if(f<c(h[1],b))var
g=c(h[1],b),i=c(d[20],g),j=a(d[16],i,cG),k=a(d[16],cH,j),l=w([0,c(h[3],b),0]),e=a(d[16],l,k);else
var
e=w(b);P[1]=d[7];return e}var
av=0;function
r(b,a){return[0,[0,a,M(x(b+1|0,0))],0]}function
n(o,n){var
e=o,d=n,b=0;for(;;){if(e){if(d){var
i=d[2],j=d[1],k=e[2],f=e[1],l=T(f[2],j[2]);if(0<l){var
e=k,b=[0,f,b];continue}if(0<=l){var
m=a(g[15],f[1],j[1]);if(a(g[9],m,al)){var
e=k,d=i;continue}var
e=k,d=i,b=[0,[0,m,f[2]],b];continue}var
d=i,b=[0,j,b];continue}return a(h[8],b,e)}return d?a(h[8],b,d):c(h[6],b)}}function
s(n,e,b){function
c(h){var
o=h[2],p=h[1],c=j(e),d=x(c+1|0,0),i=0;if(!(c<0)){var
b=i;for(;;){var
k=f(o,b)[b+1],l=f(e,b)[b+1]+k|0;f(d,b)[b+1]=l;var
m=b+1|0;if(c!==b){var
b=m;continue}break}}return[0,a(g[22],n,p),d]}return a(ca[12],c,b)}function
t(a){return c(g[1],[0,a])}function
aw(c,a){var
b=x(c+1|0,0);f(b,a)[a+1]=1;var
d=M(b);return[0,[0,t(1),d],0]}function
ax(a){function
b(a){if(a){var
d=a[1],e=d[2],f=d[1],h=b(a[2]);return[0,[0,c(g[24],f),e],h]}return 0}return b(a)}function
v(e,b){function
c(b){if(b){var
d=b[1],f=d[2],h=d[1],i=c(b[2]);return[0,[0,a(g[22],e,h),f],i]}return 0}return c(b)}function
F(e,d){var
a=e,b=0;for(;;){if(a){var
c=a[1],f=a[2],a=f,b=n(s(c[1],c[2],d),b);continue}return b}}function
ay(a,b){if(a){if(0===b)return[0,[0,l,x(j(c(h[3],a)[2])+1|0,0)],0];var
d=function(b,a){if(1===a)return b;var
c=d(b,a/2|0),e=F(c,c);return 0===(a%2|0)?e:F(b,e)};return d(a,b)}return 0}function
X(c){if(c){var
d=c[2],b=c[1][1];if(d){if(!a(g[9],b,l))if(!a(g[9],b,am)){var
e=X(d);return a(g[48],b,e)}return b}return b}return l}function
Y(c,b){return a(g[48],c,b)}var
y=[0,[0,0],0,0];function
b(a){return a[1][1]}function
o(a){var
d=b(a);return c(h[3],d)[2]}var
p=[0,0],k=[0,x(aI,y)];function
Z(e,d){p[1]=p[1]+1|0;if(k[1].length-1<=p[1])k[1]=a(ar[5],k[1],x(p[1],y));var
b=[0,[0,e],p[1],d],c=p[1];f(k[1],c)[c+1]=b;return b}function
_(f,e){var
a=e;for(;;){if(a){var
d=a[1],g=a[2],i=b(d);if(0===U(f,c(h[3],i)[2])){var
a=g;continue}return d}return y}}function
az(e,d,c,b,a){var
f=s(b,a,d);return n(v(c,e),f)}var
$=a(D[1],0,aI);function
aa(a){throw V}function
z(c,b){return a(g[45],c,b)}function
ab(e,s){function
h(d){if(d){var
i=d[1],e=i[2],j=i[1],t=d[2];try{var
C=aa(e),m=C}catch(a){a=A(a);if(a!==V)throw a;var
k=_(e,s);b(k);var
m=k}var
f=b(m);if(f){var
n=f[1],o=n[1],u=f[2],v=n[2],p=Y(j,o),q=z(o,p),w=z(j,p),x=c(g[24],w),r=h(az(t,u,q,x,B(e,v))),y=r[2];return[0,a(g[22],q,r[1]),y]}return[0,l,d]}return[0,l,0]}var
d=h(e);return[0,d[1],d[2]]}var
q=[0,0],G=[0,0],H=a(D[1],0,51);function
ac(c,b){try{var
d=a(D[6],H,[0,c[2],b[2]]);return d}catch(a){a=A(a);if(a===V)return 0;throw a}}function
I(c,b,a){return i(D[5],H,[0,c[2],b[2]],a)}function
aA(d,c){q[1]=c;function
e(a){return X(b(a))}G[1]=a(h[13],e,c);function
f(a){return I(a,a,r(d,t(1)))}return a(h[11],f,c)}function
ad(f,p,e){function
k(a){if(a){var
h=a[1],d=h[2],q=a[2],r=h[1];try{var
w=aa(d),e=w}catch(a){a=A(a);if(a!==V)throw a;var
i=_(d,p);b(i);var
e=i}var
f=b(e);if(f){var
j=f[1],t=f[2],u=j[2],v=z(r,j[1]),l=c(g[24],v),m=B(d,u),o=k(n(q,s(l,m,t)));return[0,[0,[0,l,m,e],o[1]],o[2]]}return[0,0,a]}return cI}var
d=k(f),l=d[2],m=d[1],o=q[1];function
u(c,f){function
d(d,c){var
e=c[2],g=c[3],h=c[1],i=b(f);if(a(N,b(g),i)){var
k=t(1);return n(d,s(h,e,r(j(e),k)))}return d}return i(h[16],d,c,m)}return[0,i(h[19],u,e,o),l]}var
Q=[0,0],J=[0,y],aB=[0,1];function
R(b){var
c=0;function
e(c,b){var
e=f(b[2],0)[1];return a(d[5],c,e)}return Z(b,i(h[16],e,c,b))}function
aC(k,i){var
l=b(k),m=b(i),e=c(h[3],l)[2],q=c(h[3],m)[2],u=c(h[3],l)[1],v=c(h[3],m)[1],D=c(h[4],l),E=c(h[4],m),w=Y(u,v),x=C(e,q),y=B(x,e),A=B(x,q);function
o(b,a){var
d=z(u,w),e=s(c(g[24],d),A,a);return n(s(z(v,w),y,b),e)}var
F=o(D,E),G=i[3],H=f(A,0)[1]+G|0,J=k[3],K=f(y,0)[1]+J|0,p=Z(F,a(d[5],K,H)),L=t(1);I(p,k,o(r(j(e),L),0));var
M=t(1);I(p,i,o(0,r(j(e),M)));return p}function
K(c,b){return a(d[22],b,[0,c,0])}function
af(b,a){return T(b[2],a[2])}function
ag(b,a){return i(h[45],af,b,a)}function
ah(l,e){var
g=0;function
k(E,m){var
x=b(m),y=b(l),n=c(h[3],y)[2],v=c(h[3],x)[2],g=[0,1],e=[0,1],w=j(n);for(;;){if(g[1])if(e[1]<=w){var
p=e[1],q=0===f(n,p)[p+1]?1:0;if(q)var
r=q;else
var
s=e[1],r=0===f(v,s)[s+1]?1:0;g[1]=r;e[1]=e[1]+1|0;continue}if(g[1])var
t=0;else
var
A=o(m),B=C(o(l),A),i=m[2],k=l[2],z=0,D=u.caml_lessthan(k,i)?[0,k,i]:[0,i,k],t=[0,[0,D,B],z];return a(d[22],E,t)}}var
m=i(h[16],k,g,e);return a(h[41],af,m)}function
aD(a){function
c(a){if(a){var
b=a[2],d=a[1];if(b){var
e=c(b);return ag(ah(d,b),e)}}return 0}return c(a)}function
aF(b,i,l){var
e=b[2],g=b[1],c=g[2],d=g[1];function
j(a){var
h=a[2]!==d?1:0;if(h){var
i=a[2]!==c?1:0;if(i){var
j=U(e,o(a));if(j){var
l=a[2]<c?1:0;if(l)var
g=l;else
var
p=o(a),g=1-ap(e,C(o(f(k[1],d)[d+1]),p));if(g){var
m=a[2]<d?1:0;if(m)var
b=m;else
var
n=o(a),b=1-ap(e,C(o(f(k[1],c)[c+1]),n))}else
var
b=g}else
var
b=j}else
var
b=i}else
var
b=h;return b}return a(h[24],j,i)}var
ai=[0,0];function
aG(c,b,a){return ag(ah(c,b),a)}var
L=[0,l];function
aH(f,e){var
b=0===ai[1]?1:0;if(b){var
g=c(h[1],e),i=c(d[20],g),j=a(d[16],i,cJ),k=a(d[16],cK,j),l=c(h[1],f),n=c(d[20],l),o=a(d[16],n,k),p=a(d[16],cL,o);return c(m[5],p)}return b}function
S(j,f,p){var
s=ab(b(J[1]),f),k=s[2],z=s[1],A=E(k),B=a(d[16],A,cM),C=a(d[16],cN,B);c(m[5],C);L[1]=a(g[22],L[1],z);J[1]=R(k);if(0===k){c(m[5],cO);var
D=q[1],G=function(a){return 0},H=a(h[13],G,D),e=L[1],u=ad(v(e,j),f,H),x=u[1],I=u[2];c(m[5],cP);var
K=w(I),M=a(d[16],K,cQ),N=a(d[16],cR,M);c(m[5],N);var
l=[0,v(e,j)],O=q[1],P=function(c,a){var
d=F(c,b(a));l[1]=n(l[1],d);return 0};i(h[18],P,x,O);var
Q=w(l[1]),S=a(d[16],Q,cS),T=a(d[16],cT,S);c(m[5],T);var
U=w(r(1,e)),V=a(d[16],U,cU),W=a(d[16],cV,V);c(m[5],W);var
y=function(b){if(b){var
c=b[2],d=b[1],e=y(c),f=function(a){return ac(d,a)};return[0,a(h[13],f,c),e]}return 0},X=c(h[6],p),Y=y(c(h[6],f)),o=[0,c(h[6],Y)],Z=function(a){o[1]=c(h[4],o[1]);return 0};a(h[11],Z,p);var
_=o[1],$=c(h[6],x),aa=function(a){return v(t(-1),a)};return[0,X,j,[0,e,1,_,a(h[13],aa,$)]]}throw ak}var
aJ=[0,0];function
aj(a){return a?f(a[1][2],0)[1]:-1}function
aK(e,t,s){c(m[5],cW);ai[1]=0;c(D[2],$);return function(B){var
j=B;for(;;){var
o=j[2],g=j[1];aH(g,o);if(o){var
n=o[2],u=o[1],w=u[1],p=w[2],r=w[1];if(aF([0,[0,r,p],u[2]],g,n)){c(m[5],cX);var
j=[0,g,n];continue}var
C=f(k[1],p)[p+1],e=aC(f(k[1],r)[r+1],C);if(Q[1])if(0!==b(e)){var
Z=aj(b(J[1]));if(Z<aj(b(e))){c(m[5],c1);var
j=[0,g,n];continue}}var
x=ab(b(e),g),y=x[1];if(x[2]){var
F=v(y,b(e));e[1][1]=F;var
L=q[1],M=function(b,c){return function(a){return v(c,ac(b,a))}}(e,y),N=a(h[13],M,L),z=ad(b(e),g,N),O=z[1];e[1][1]=z[2];var
P=q[1],R=function(c){return function(d,b){a(D[9],H,[0,c[2],b[2]]);return I(c,b,d)}}(e);i(h[18],R,O,P);var
T=E(b(e)),U=a(d[16],T,cY),V=a(d[16],cZ,U);c(m[5],V);q[1]=K(e,g);G[1]=K(l,G[1]);try{var
X=K(e,g),Y=S(b(t),X,s);return Y}catch(a){a=A(a);if(a===ak){var
W=aG(e,g,n),j=[0,K(e,g),W];continue}throw a}}c(m[5],c0);var
j=[0,g,n];continue}return S(b(t),g,s)}}(e)}function
aL(b){if(b){var
c=b[2],d=f(b[1][2],0)[1],e=function(a){return ap(f(a[2],0)[1],d)};return a(h[23],e,c)}return 1}return[0,an,r,av,aw,N,O,n,ax,F,ay,G,function(o,j,g){c(D[2],$);c(D[2],H);p[1]=0;k[1]=x(aI,y);Q[1]=a(h[23],aL,[0,g,j]);if(Q[1])c(m[5],c2);var
q=E(g),r=a(d[16],q,c3),s=a(d[16],c4,r);c(m[5],s);function
t(c,b){var
e=E(b),f=a(d[16],e,c6);return a(d[16],c,f)}var
u=i(h[16],t,c5,j),v=a(d[16],c7,u);c(m[5],v);var
e=a(h[13],R,j),n=R(g);aA(o,e);L[1]=l;J[1]=n;try{var
C=S(b(n),e,e),f=C}catch(a){a=A(a);if(a!==ak)throw a;var
f=aK([0,e,aD(e)],n,e)}var
w=f[3],z=f[2],B=f[1];c(m[5],c8);return[0,a(h[13],b,B),z,w]},at,aB,aJ]},ak,aM];S(261,B,"Nsatz_plugin.Ideal");var
aO=z[35],c_=c(aO,0);c(aO,1);var
aP=n[49],aQ=z[24],aR=z[27],aS=z[16],c$=n[48],da=z[25],db=z[4],dc=z[5],dd=z[10],de=z[8],df=z[3],dg=z[15],dh=z[33];function
di(a){try{var
b=c(z[37],a);return b}catch(a){a=A(a);if(a[1]===as)return 1;throw a}}var
dj=z[19];function
dk(e,d){var
c=e,b=d;for(;;){if(a(aQ,b,c_))return c;if(a(aR,c,b)){var
g=b,b=c,c=g;continue}var
f=a(aS,c,b),c=b,b=f;continue}}function
aT(b){return a(n[32],b,E)?0:[0,b]}function
aU(a){var
b=a[2],c=a[1];return 1===b?c:[6,c,b]}function
aw(a){var
b=a[1];if(typeof
b==="number")return a[2];var
c=a[2];return typeof
c==="number"?b:[3,b,c]}function
aV(c){var
b=c[1];if(typeof
b==="number")return 0;var
d=c[2];if(typeof
d==="number")return 0;else
if(0===d[0])if(a(n[32],d[1],O))return b;if(typeof
b!=="number"&&0===b[0]){var
e=c[2];if(a(n[32],b[1],O))return e}return[5,b,c[2]]}c(w[112],1);var
ax=[k,function(a){return i(t[4],dn,dm,dl)}],P=[k,function(a){return i(t[4],dr,dq,dp)}],X=[k,function(a){return i(t[4],du,dt,ds)}],Y=[k,function(a){return i(t[4],dx,dw,dv)}],Z=[k,function(a){return i(t[4],dA,dz,dy)}],_=[k,function(a){return i(t[4],dD,dC,dB)}],$=[k,function(a){return i(t[4],dG,dF,dE)}],aa=[k,function(a){return i(t[4],dJ,dI,dH)}],aW=[k,function(a){return i(t[4],dL,ay,dK)}],aX=[k,function(a){return i(t[4],dN,ay,dM)}],aY=[k,function(a){return i(t[4],dP,ay,dO)}],j=[k,function(a){return i(t[4],dR,F,dQ)}],al=[k,function(a){return i(t[4],dT,F,dS)}],ab=[k,function(a){return i(t[4],dV,F,dU)}],dY=[k,function(a){return i(t[4],dX,F,dW)}],d1=[k,function(a){return i(t[4],d0,F,dZ)}],ac=[k,function(a){return i(t[4],d3,F,d2)}],am=[k,function(a){return i(t[4],d5,F,d4)}],an=[k,function(a){return i(t[4],d7,F,d6)}],d_=[k,function(a){return i(t[4],d9,F,d8)}];function
s(a,d){var
b=r(a),f=c(e[19][12],d),g=p===b?a[1]:k===b?c(q[2],a):a;return c(w[a7],[0,g,f])}function
az(f){var
a=r(j),b=0,d=0,e=p===a?j[1]:k===a?c(q[2],j):j;return s(aW,[0,s(ax,[0,e,d]),b])}function
Q(b){if(a(n[26],b,O)){var
d=r(am);return p===d?am[1]:k===d?c(q[2],am):am}var
e=a(n[12],b,W);return a(n[26],e,E)?s(ac,[0,Q(a(n[11],b,W)),0]):s(d1,[0,Q(a(n[11],b,W)),0])}function
aZ(b){if(a(n[26],b,E)){var
d=r(al);return p===d?al[1]:k===d?c(q[2],al):al}return a(n[28],b,E)?s(ab,[0,Q(b),0]):s(dY,[0,Q(a(n[4],d$,b)),0])}function
G(A){var
b=A;for(;;)if(typeof
b==="number"){var
b=[0,E];continue}else
switch(b[0]){case
0:var
x=c(n[50],b[1]),d=c(av[5],x),y=c(av[3],d);c(n[48],y);var
z=c(av[2],d),f=r(j),B=[0,aZ(c(n[48],z)),0],C=p===f?j[1]:k===f?c(q[2],j):j;return s(P,[0,C,B]);case
1:var
g=r(j),D=[0,Q(c(n[43],b[1])),0],F=p===g?j[1]:k===g?c(q[2],j):j;return s(X,[0,F,D]);case
2:var
h=r(j),H=[0,G(b[1]),0],I=p===h?j[1]:k===h?c(q[2],j):j;return s($,[0,I,H]);case
3:var
J=b[1],K=[0,G(b[2]),0],i=r(j),L=[0,G(J),K],M=p===i?j[1]:k===i?c(q[2],j):j;return s(Y,[0,M,L]);case
4:var
N=b[1],R=[0,G(b[2]),0],l=r(j),S=[0,G(N),R],T=p===l?j[1]:k===l?c(q[2],j):j;return s(Z,[0,T,S]);case
5:var
U=b[1],V=[0,G(b[2]),0],m=r(j),W=[0,G(U),V],ab=p===m?j[1]:k===m?c(q[2],j):j;return s(_,[0,ab,W]);default:var
o=b[2],ac=b[1];if(0===o){var
t=r(j),ad=[0,aZ(O),0],ae=p===t?j[1]:k===t?c(q[2],j):j;return s(P,[0,ae,ad])}var
u=c(n[45],o),af=0;if(a(n[32],u,E))var
e=r(an),v=p===e?an[1]:k===e?c(q[2],an):an;else
var
v=s(d_,[0,Q(u),0]);var
w=r(j),ag=[0,G(ac),[0,v,af]],ah=p===w?j[1]:k===w?c(q[2],j):j;return s(aa,[0,ah,ag])}}function
R(g){var
b=c(w[af],g);if(9===b[0]){var
d=b[2];if(1===d.length-1){var
e=d[1],f=r(ac),h=b[1],i=p===f?ac[1]:k===f?c(q[2],ac):ac;if(a(w[I],h,i)){var
j=R(e);return a(n[6],W,j)}var
l=R(e),m=a(n[6],W,l);return a(n[1],O,m)}}return O}function
H(M){var
i=c(w[af],M);if(9===i[0]){var
d=i[2],x=d.length-1,b=i[1];if(2===x){var
j=d[2],y=r(X),N=p===y?X[1]:k===y?c(q[2],X):X;if(a(w[I],b,N)){var
O=R(j);return[1,c(n[40],O)]}var
z=r(P),Q=p===z?P[1]:k===z?c(q[2],P):P;if(a(w[I],b,Q)){var
h=c(w[af],j);if(9===h[0]){var
o=h[2];if(1===o.length-1){var
s=o[1],t=r(ab),J=h[1],K=p===t?ab[1]:k===t?c(q[2],ab):ab;if(a(w[I],J,K))var
l=R(s),g=1;else
var
L=R(s),l=a(n[4],E,L),g=1}else
var
g=0}else
var
g=0;if(!g)var
l=E;return[0,l]}var
A=r($),S=p===A?$[1]:k===A?c(q[2],$):$;return a(w[I],b,S)?[2,H(j)]:0}if(3===x){var
e=d[2],f=d[3],B=r(Y),T=p===B?Y[1]:k===B?c(q[2],Y):Y;if(a(w[I],b,T)){var
U=H(f);return[3,H(e),U]}var
C=r(Z),V=p===C?Z[1]:k===C?c(q[2],Z):Z;if(a(w[I],b,V)){var
W=H(f);return[4,H(e),W]}var
D=r(_),ac=p===D?_[1]:k===D?c(q[2],_):_;if(a(w[I],b,ac)){var
ad=H(f);return[5,H(e),ad]}var
F=r(aa),ae=p===F?aa[1]:k===F?c(q[2],aa):aa;if(a(w[I],b,ae)){var
u=c(w[af],f);if(9===u[0]){var
v=u[2];if(1===v.length-1)var
G=R(v[1]),m=1;else
var
m=0}else
var
m=0;if(!m)var
G=E;var
ag=c(n[44],G);return[6,H(e),ag]}return 0}}return 0}function
a0(e){var
b=c(w[af],e);if(9===b[0]){var
a=b[2],d=a.length-1-1|0;if(!(2<d>>>0))switch(d){case
0:return 0;case
1:break;default:var
f=a[2],g=a0(a[3]);return[0,H(f),g]}}throw[0,K,ea]}var
N=[0,0];function
eb(b){function
c(e){var
b=e;for(;;)if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return 0;case
1:var
f=a4(b[1]);N[1]=a(d[5],N[1],f);return 0;case
2:var
b=b[1];continue;case
3:var
g=b[2];c(b[1]);var
b=g;continue;case
4:var
h=b[2];c(b[1]);var
b=h;continue;case
5:var
i=b[2];c(b[1]);var
b=i;continue;default:var
b=b[1];continue}}return c(b)}var
g=c(at[1],[0,aQ,aR,da,db,dc,dd,de,df,dg,aS,dj,dk,di,aP,dh]),l=c(B[1],[0,g[1],g[2],g[3],g[4],g[5],g[6],g[7],g[8],g[9],g[10],g[11],g[12],g[13],g[14],g[15],g[16],g[17],g[18],g[19],g[20],g[21],g[22],g[23],g[24],g[25],g[26],g[27],g[28],g[29],g[30],g[31],g[32],g[33],g[34],g[35],g[36],g[37],g[38],g[39],g[40],g[41],g[42],g[43],g[44],g[45],g[46],g[47],g[48],g[49],g[50],g[51],g[52],g[53],g[54],g[55],g[56],g[57]]);function
aA(j,b){var
g=c(l[1],b);function
h(g){if(g){var
p=g[2],q=g[1],r=q[2].length-1-1|0,t=q[1],u=function(g,k){var
c=k[2],e=g[2],d=g[1],a=[0,0],l=1;if(!(r<1)){var
b=l;for(;;){if(0<f(c,b)[b+1])a[1]=b;var
o=b+1|0;if(r!==b){var
b=o;continue}break}}if(0===a[1])return[0,d,e];if(d<a[1]){var
h=a[1],m=f(c,h)[h+1];return[0,a[1],m]}if(a[1]===d){var
i=a[1];if(f(c,i)[i+1]<e){var
j=a[1],n=f(c,j)[j+1];return[0,a[1],n]}}return[0,d,e]},s=i(e[17][15],u,ec,g),j=s[2],b=s[1];if(0===b){var
m=function(b){if(0===b[0])return aT(c(c$,b[1]));var
f=[0,0],g=b[2],h=b[1];function
i(b,a){var
e=aU([0,[1,c(d[20],h)],b]),g=aV([0,m(a),e]);f[1]=aw([0,f[1],g]);return 0}a(e[19][14],i,g);return f[1]},o=m(t);return c(e[17][47],p)?o:aw([0,o,h(p)])}var
k=[0,0],l=[0,0],v=function(c){var
a=c[2],d=c[1];return j<=f(a,b)[b+1]?(a[b+1]=f(a,b)[b+1]-j|0,k[1]=[0,[0,d,a],k[1]],0):(l[1]=[0,[0,d,a],l[1]],0)};a(e[17][11],v,g);var
w=1===j?[1,c(d[20],b)]:aU([0,[1,c(d[20],b)],j]),x=h(c(e[17][6],l[1]));return aw([0,aV([0,w,h(c(e[17][6],k[1]))]),x])}return aT(c(n[43],ed))}return h(g)}function
a1(b,a){function
d(b,a){if(b){if(0===b[1]){var
c=b[2];if(a){var
e=a[1];return[0,e,d(c,a[2])]}throw[0,K,em]}var
f=d(b[2],a);return[0,l[3],f]}return a}var
f=d(b,c(e[17][6],a));return c(e[17][6],f)}function
eG(a3){var
C=a0(a3);N[1]=0;a(e[17][11],eb,C);if(C){var
H=C[1];if(typeof
H==="number")var
u=0;else
if(0===H[0]){var
T=H[1];if(0===T[0]){var
I=C[2];if(I){var
J=I[1];if(typeof
J==="number")var
M=1;else
if(0===J[0]){var
W=J[1],X=T[1];if(0===W[0]){var
Y=W[1],au=I[2];if(7<X>>>0)c(U[6],eo);else
switch(X){case
0:c(m[5],ey);B[3][1]=0;l[14][1]=0;l[15][1]=0;break;case
1:c(m[5],ez);B[3][1]=0;l[14][1]=1;l[15][1]=0;break;case
2:c(m[5],eA);B[3][1]=1;l[14][1]=0;l[15][1]=0;break;case
3:c(m[5],eB);B[3][1]=1;l[14][1]=1;l[15][1]=0;break;case
4:c(m[5],eC);B[3][1]=0;l[14][1]=0;l[15][1]=1;break;case
5:c(m[5],eD);B[3][1]=0;l[14][1]=1;l[15][1]=1;break;case
6:c(m[5],eE);B[3][1]=1;l[14][1]=0;l[15][1]=1;break;default:c(m[5],eF);B[3][1]=1;l[14][1]=1;l[15][1]=1}var
Z=N[1],t=[0,0];if(!(Z<1)){var
z=Z;for(;;){var
aU=t[1],aV=c(d[20],z),aW=a(d[16],aV,ew),aZ=[0,a(d[16],ex,aW),0];t[1]=a(e[18],aZ,aU);var
a2=z-1|0;if(1!==z){var
z=a2;continue}break}}t[1]=a(e[18],ep,t[1]);l[6][1]=t[1];var
av=function(b){var
f=N[1];function
d(b){if(typeof
b==="number")var
e=l[3];else
switch(b[0]){case
0:var
i=b[1];if(a(n[32],i,E))var
j=l[3];else
var
m=[0,c(aP,i)],j=a(l[2],f,m);var
e=j;break;case
1:var
h=a4(b[1]);if(h<=Y)var
o=c(g[2],h),k=a(l[2],f,o);else
var
k=a(l[4],f,h);var
e=k;break;case
2:var
p=d(b[1]),e=c(l[8],p);break;case
3:var
q=b[1],r=d(b[2]),s=d(q),e=a(l[7],s,r);break;case
4:var
t=b[1],u=d(b[2]),v=c(l[8],u),w=d(t),e=a(l[7],w,v);break;case
5:var
x=b[1],y=d(b[2]),z=d(x),e=a(l[9],z,y);break;default:var
A=b[2],B=d(b[1]),e=a(l[10],B,A)}return e}return d(b)},L=a(e[17][12],av,au);if(L){var
aw=L[1],ay=c(e[17][6],L[2]),Q=c(l[13][1],12),at=function(b){try{var
c=a(l[13][7],Q,b);return c}catch(a){a=A(a);if(a===V){i(l[13][5],Q,b,1);return 0}throw a}},R=function(b){if(b){var
c=b[1],d=R(b[2]),e=d[2],f=d[1];if(!a(l[5],c,l[3]))if(!at(c))return[0,[0,c,f],[0,0,e]];return[0,f,[0,1,e]]}return el},S=R(ay),_=S[2],aB=S[1],ap=c(aN[87],0),v=i(l[12],N[1],aB,aw)[3];c(e[17][6],l[11][1]);var
aq=c(aN[87],0)-ap,ar=a(ai[98],ej,aq),as=a(d[16],ek,ar);c(m[5],as);c(m[5],eq);var
aC=c(e[17][6],v[3]),w=[0,v[4],aC],$=c(e[17][3],w),aa=c(e[17][1],$),b=c(e[17][1],w),D=x(b,0),O=function(d){if(b<=d)return 0;f(D,d)[d+1]=1;var
h=a(e[17][5],w,d),i=c(e[17][1],h)-1|0,j=0;if(!(i<0)){var
g=j;for(;;){var
k=a(e[17][5],h,g);if(1-a(l[5],k,l[3]))O((g+d|0)+1|0);var
m=g+1|0;if(i!==g){var
g=m;continue}break}}return 0};O(0);var
F=[0,0],P=b-1|0,ab=0;if(!(P<0)){var
h=ab;for(;;){if(f(D,h)[h+1]){var
an=F[1];F[1]=[0,a(e[17][5],w,h),an]}var
ao=h+1|0;if(P!==h){var
h=ao;continue}break}}var
ac=c(e[17][6],F[1]),ad=function(j){var
d=[0,j],h=b-1|0,k=0;if(!(h<0)){var
a=k;for(;;){if(1-f(D,a)[a+1]){var
l=(b-a|0)+(aa-b|0)|0,m=d[1],g=function(a,b){if(c(e[17][47],a))return 0;if(0<=b){if(0===b)return c(e[17][4],a);if(a){var
d=a[1];return[0,d,g(a[2],b-1|0)]}throw[0,K,ee]}return a},i=g(c(e[17][6],m),l);d[1]=c(e[17][6],i)}var
n=a+1|0;if(h!==a){var
a=n;continue}break}}return d[1]},o=a(e[17][12],ad,ac),ae=b-c(e[17][1],o)|0,af=c(d[20],ae),ag=a(d[16],af,ef),ah=a(d[16],eg,ag);c(m[5],ah);var
aj=c(e[17][1],o),ak=c(d[20],aj),al=a(d[16],ak,eh),am=a(d[16],ei,al);c(m[5],am);if(o){var
aD=o[2],aE=a1(_,o[1]),y=N[1],aF=aA(y,a(l[2],y,v[1])),aG=[6,0,v[2]],aH=c(e[17][6],aD),aI=function(a){return a1(_,a)},aJ=a(e[17][12],aI,aH),aK=function(a){return aA(y,a)},aL=c(e[17][12],aK),aM=a(e[17][12],aL,aJ),aO=function(a){return aA(y,a)},aQ=a(e[17][12],aO,aE),aR=c(d[20],Y),aS=a(d[16],aR,er),aT=a(d[16],es,aS);c(m[5],aT);c(m[5],et);var
a5=a(e[18],[0,[0,aF,[0,aG,aQ]],0],aM),a6=function(b){return a(e[17][12],G,b)},a7=a(e[17][12],a6,a5),a8=s(aX,[0,az(0),0]),a9=function(d,b){var
a=r(j),f=0,g=0,h=p===a?j[1]:k===a?c(q[2],j):j,l=s(aX,[0,s(ax,[0,h,g]),f]);function
m(d,b){var
a=r(j),e=[0,d,[0,b,0]],f=0,g=p===a?j[1]:k===a?c(q[2],j):j;return s(aY,[0,s(ax,[0,g,f]),e])}var
n=[0,i(e[17][16],m,d,l),[0,b,0]];return s(aY,[0,az(0),n])},a_=i(e[17][16],a9,a7,a8);c(m[5],eH);return a_}throw[0,K,eu]}throw[0,K,ev]}var
u=1,M=0}else
var
M=1;if(M)var
u=1}else
var
u=1}else
var
u=1}else
var
u=0}throw[0,K,en]}function
eI(a){var
b=[0,s(aW,[0,az(0),0]),a],d=[0,i(t[4],eL,eK,eJ),b],e=[0,c(w[a7],d),0];return c(c9[148],e)}var
aB=[0,function(a){try{var
d=eG(a),b=d}catch(a){a=A(a);if(a!==B[2])throw a;var
b=c(U[6],eM)}return eI(b)}];S(270,aB,"Nsatz_plugin.Nsatz");c(aD[12],eP);c(aD[12],eQ);function
eR(e){var
a=[31,eO[4],[0,[0,ao,eS],0],0],b=[28,[0,[0,[0,c(aC[1][6],eT)],0],a]],d=c(aC[1][5],eU);return e2(a2[4],1,0,d,b)}function
eV(a){if(a)if(!a[2]){var
b=a[1];return function(a){return c(aB[1],b)}}return c(d[2],eW)}var
eY=[0,[0,c(aC[1][6],eX)],0],eZ=[0,a(eN[27],eY,eV)];i(a2[9],0,[0,ao,e0],eZ);a(aD[19],eR,ao);var
a3=[0,ao];S(276,a3,"Nsatz_plugin.G_nsatz");S(277,[0,m,at,B,aB,a3],"Nsatz_plugin");return});
