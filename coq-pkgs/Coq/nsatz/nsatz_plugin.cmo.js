(function(e5){"use strict";var
ag="*",af=140,bg="x",bl="plugins/nsatz/polynom.ml",a$="+",a_="@[%s@]",p=250,aK="(",M="setoid_ring",bd="+ ",bk="Init",a9=124,a8="...",k=246,bf="[",bj="h",aI=" ",ah="1",aH="nsatz_compute",a7="print_pol dans dansideal",bh="]",bi=")*",T="plugins/nsatz/nsatz.ml",aG="nsatz_plugin",bc="c",C="",bb="term computed\n",aq="^",L="Ring_polynom",J="0",ba="$lt",I=136,be="u",y="\n",v="CC",aJ=1e3,s=e5.jsoo_runtime,f=s.caml_check_bound,ap=s.caml_equal,aF=s.caml_int_compare,a6=s.caml_int_of_string,e3=s.caml_list_of_js_array,x=s.caml_make_vect,b=s.caml_new_string,r=s.caml_obj_tag,S=s.caml_register_global,ad=s.caml_string_equal,ae=s.caml_string_notequal,a5=s.caml_trampoline,aE=s.caml_trampoline_return,A=s.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):s.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):s.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):s.caml_call_gen(a,[b,c,d])}function
e4(a,b,c,d,e){return a.length==4?a(b,c,d,e):s.caml_call_gen(a,[b,c,d,e])}var
o=s.caml_get_global_data(),E=[0,0],O=[0,1],W=[0,2],ay=[0,b(bk),[0,b("Datatypes"),0]],F=[0,b("Numbers"),[0,b("BinNums"),0]],ao=b(aG),U=o.CErrors,ar=o.Array,h=o.List,d=o.Pervasives,aj=o.Flags,ai=o.Format,e=o.Util,K=o.Assert_failure,as=o.Failure,aM=o.Char,D=o.Hashtbl,V=o.Not_found,au=o.String,n=o.Num,q=o.CamlinternalLazy,w=o.Term,u=o.Coqlib,aO=o.Unix,z=o.Big_int,av=o.Ratio,aC=o.Names,a3=o.Tacenv,aD=o.Mltop,bs=b(y),bp=[0,[18,[1,[0,0,b(C)]],[2,0,[12,10,[17,0,0]]]],b("@[%s\n@]")],bn=[0,[18,[1,[0,0,b(C)]],[2,0,[17,0,0]]],b(a_)],bz=[0,b(bl),363,43],bY=b(a8),bB=b(")"),bC=b(aK),bD=b(C),bE=b(C),bF=b(J),bW=b(a8),bX=b(a$),bH=b(C),bI=b(J),bJ=b(ah),bL=b(bi),bM=b(aK),bN=b(ag),bO=b(J),bP=b(ah),bQ=b(aq),bR=b(aq),bS=b(bi),bT=b(aK),bU=b(aq),bV=b(ag),bK=b(a$),bG=b(J),b3=b("div_pol1"),b4=[0,b(bl),475,9],b5=b(y),b6=b("x:"),b7=b(y),b8=b("r:"),b9=b(y),b_=b("q:"),b$=b(y),ca=b("p:"),cb=b("div_pol:\n"),b1=b(y),b0=b("\n{ "),b2=b("}"),bZ=[0,[18,[1,[0,0,b(C)]],[2,0,[17,0,0]]],b(a_)],bA=b(be),by=b("non"),bw=[0,0],bx=[0,1],c8=b(y),c4=b("homogeneous polynomials\n"),c5=b(y),c6=b("p: "),c7=b(C),c9=b("lp:\n"),c_=b("computed\n"),cZ=b(bc),c3=b(bj),c0=b(y),c1=b("\nnew polynomial: "),c2=b(J),cY=b("computation of the Groebner basis\n"),cO=b(y),cP=b("remainder: "),cQ=b("polynomial reduced to 0\n"),cR=b("r ok\n"),cS=b(y),cT=b("r: "),cU=b(y),cV=b("verif sum: "),cW=b(y),cX=b("coefficient: "),cL=b(bh),cM=b(","),cN=b(bf),cK=[0,0,0],cI=b(" terms"),cJ=b(" + "),cH=b(a7),cG=b(a7),cr=b(J),cs=b(ah),ct=b(aq),cm=b(ag),cn=b(ag),co=b(ag),cp=b(ah),cq=b(C),cu=b("-1"),cv=b(J),cw=b(ah),cz=b(bd),cA=b(C),cB=b(aI),cC=b("-"),cx=b("- "),cy=b(bd),cD=b(J),cE=b(C),cF=b(aI),cl=b(aI),cj=b(" i="),ck=b("lv= "),ch=b(bh),ci=b(bf),ce=[0,0],cf=[0,1],cg=[0,-1],cd=b("Ideal.NotInIdeal"),ec=[0,b(T),258,9],en=[0,0,0],eO=b("nsatz cannot solve this problem"),eL=b("refl_equal"),eM=[0,b(bk),[0,b("Logic"),0]],eN=b(v),eJ=b(bb),eq=b("nsatz: bad parameter"),eA=b("computation without sugar\n"),eB=b("computation with sugar\n"),eC=b("ordre lexico computation without sugar\n"),eD=b("ordre lexico computation with sugar\n"),eE=b("computation without sugar, division by pairs\n"),eF=b("computation with sugar, division by pairs\n"),eG=b("ordre lexico computation without sugar, division by pairs\n"),eH=b("ordre lexico computation with sugar, division by pairs\n"),ey=b(C),ez=b(bg),er=e3([b("a"),b("b"),b(bc),b("d"),b("e"),b("f"),b("g"),b(bj),b("i"),b("j"),b("k"),b("l"),b("m"),b("n"),b("o"),b("p"),b("q"),b("r"),b("s"),b("t"),b(be),b("v"),b("w"),b(bg),b("y"),b("z")]),es=b("cert ok\n"),et=b(y),eu=b("number of parameters: "),ev=b(bb),ew=[0,b(T),568,11],ex=[0,b(T),556,14],ep=[0,b(T),585,8],eo=[0,b(T),505,11],el=[0,[18,[1,[0,0,b(C)]],[8,0,[0,1,10],[0,3],[17,0,[11,b("s\n"),0]]]],b("@[%10.3f@]s\n")],em=b("time: "),eh=b(y),ei=b("useless spolynomials: "),ej=b(y),ek=b("useful spolynomials: "),eg=[0,b(T),412,12],ee=[0,0,0],ef=b(J),eb=[0,0],d_=b("Npos"),d$=b(v),d8=b("N0"),d9=b(v),d6=b("xH"),d7=b(v),d4=b("xO"),d5=b(v),d1=b("xI"),d2=b(v),dY=b("Zneg"),dZ=b(v),dW=b("Zpos"),dX=b(v),dU=b("Z0"),dV=b(v),dS=b("Z"),dT=b(v),dQ=b("cons"),dR=b(v),dO=b("nil"),dP=b(v),dM=b("list"),dN=b(v),dJ=b("PEpow"),dK=[0,b(M),[0,b(L),0]],dL=b(v),dG=b("PEopp"),dH=[0,b(M),[0,b(L),0]],dI=b(v),dD=b("PEmul"),dE=[0,b(M),[0,b(L),0]],dF=b(v),dA=b("PEsub"),dB=[0,b(M),[0,b(L),0]],dC=b(v),dx=b("PEadd"),dy=[0,b(M),[0,b(L),0]],dz=b(v),du=b("PEX"),dv=[0,b(M),[0,b(L),0]],dw=b(v),dr=b("PEc"),ds=[0,b(M),[0,b(L),0]],dt=b(v),dn=b("PExpr"),dp=[0,b(M),[0,b(L),0]],dq=b(v),eY=b("Extension: cannot occur"),eU=b(aH),eV=b(ba),eW=b(aH),eR=b(aG),eS=b(aG),eZ=b(ba),e2=b(aH),cc=o.CList,c$=o.Tactics,eQ=o.Loc,eP=o.Tacinterp;function
bm(b){return aj[24][1]?(a(ai[96],bn,b),c(d[46],d[24])):0}function
bo(b){return aj[24][1]?(a(ai[96],bp,b),c(d[46],d[24])):0}function
bq(a){return 0}function
br(b){if(aj[24][1]){var
e=a(d[16],b,bs);c(d[27],e);return c(d[46],d[24])}return 0}function
bt(b){return a(aj[51],d[34],b)}function
aL(e,d,c){var
b=c;for(;;){if(b){var
f=b[2];if(a(e,d,b[1]))return 1;var
b=f;continue}return 0}}function
bu(e,d){var
b=[0,0];function
f(a){var
c=1-aL(e,a,b[1]),d=c?(b[1]=[0,a,b[1]],0):c;return d}a(h[11],f,d);return c(h[6],b[1])}function
bv(k,f,o){function
p(a){return 1-c(f,a)}var
e=0,b=a(h[30],p,o);for(;;){if(b){var
g=b[2],i=b[1],j=[0,0],l=[0,0],q=function(h,d,i){return function(e){try{var
b=a(k,h,e),g=c(f,b)?(i[1]=1,0):(d[1]=[0,b,d[1]],0);return g}catch(a){a=A(a);if(c(U[22],a))return 0;throw a}}}(i,j,l);a(h[11],q,e);if(l[1]){var
b=g;continue}if(0===j[1]){var
m=[0,g],n=[0,0],r=function(j,g,h){return function(b){try{var
d=a(k,b,j),e=1-c(f,d),i=e?(g[1]=[0,d,g[1]],0):e;return i}catch(a){a=A(a);if(c(U[22],a)){h[1]=[0,b,h[1]];return 0}throw a}}}(i,m,n);a(h[11],r,e);var
s=m[1],e=c(h[6],[0,i,n[1]]),b=s;continue}var
b=a(d[22],j[1],g);continue}return e}}var
m=[0,bm,bo,bq,br,bt,aL,bu,bv,function(k,j,d,b,e){var
g=x(b.length-1,[0,d,0]);function
h(i,h){var
b=[0,h],d=[0,0];if(1-c(j,h)){var
l=function(f,e){try{for(;;){var
g=a(k,b[1],e);d[1]=[0,f,d[1]];b[1]=g;continue}}catch(a){a=A(a);if(c(U[22],a))return 0;throw a}};a(ar[14],l,e)}var
m=[0,b[1],d[1]];return f(g,i)[i+1]=m}a(ar[14],h,b);return[0,e,g]}];S(p,m,"Nsatz_plugin.Utile");var
at=[0,function(g){function
H(a){return c(g[14],[0,a])}var
b=H(0),U=H(1);function
I(a){return[0,c(g[14],a)]}var
k=I(bw),V=I(bx);function
W(a){return[1,a,[0,k,V]]}function
J(d,a){if(0===a)return[0,U];var
c=x(a+1|0,[0,b]);f(c,a)[a+1]=[0,U];return[1,d,c]}function
aj(a){return 0===a[0]?1:0}function
ak(a){return 0===a[0]?a[1]:c(d[2],by)}function
al(c){return 0===c[0]?a(g[1],c[1],b)?1:0:0}function
s(a){return 0===a[0]?0:a[1]}function
L(b){if(0===b[0])return 0;var
c=b[2],f=b[1];function
g(c,b){var
e=L(c);return a(d[5],e,b)}return i(e[19][18],g,c,f)}function
am(b){var
c=0;function
f(c,b){var
e=L(c);return a(d[5],e,b)}return i(e[19][18],f,b,c)}function
n(c,b){if(0===c[0]){var
f=c[1];if(0===b[0])return a(g[1],f,b[1])}else{var
h=c[2],j=c[1];if(0!==b[0]){var
d=j===b[1]?1:0,k=b[2];return d?i(e[19][31],n,h,k):d}}return 0}function
X(d){if(0===d[0])return d;var
e=d[2],g=e.length-1-1|0,a=[0,g],k=d[1];for(;;){if(0<a[1]){var
h=a[1];if(n(f(e,h)[h+1],[0,b])){a[1]=a[1]-1|0;continue}}if(0<=a[1]){if(0===a[1])return f(e,0)[1];if(a[1]===g)return d;var
i=x(a[1]+1|0,[0,b]),j=a[1],l=0;if(!(j<0)){var
c=l;for(;;){var
m=f(e,c)[c+1];f(i,c)[c+1]=m;var
o=c+1|0;if(j!==c){var
c=o;continue}break}}return[1,k,i]}return[0,b]}}function
h(b,a){if(1===a[0]){var
c=a[2];if(a[1]===b)return c.length-1-1|0}return 0}function
Y(c){if(0===c[0])return 0;var
b=[0,0],f=c[2];function
g(e,c){var
f=e+Y(c)|0;b[1]=a(d[5],b[1],f);return 0}a(e[19][14],g,f);return b[1]}function
u(b){if(0===b[0])return[0,b[1]];var
c=b[1];return[1,c,a(e[19][15],u,b[2])]}function
w(e,c,a){if(1===a[0]){var
d=a[2];if(a[1]===e)return c<d.length-1?f(d,c)[c+1]:[0,b]}return 0===c?a:[0,b]}function
p(j,c){if(0===j[0]){var
D=j[1];if(0===c[0])var
q=[0,a(g[5],D,c[1])];else{var
s=c[2],E=c[1],t=a(e[19][15],u,s),F=p(j,f(s,0)[1]);f(t,0)[1]=F;var
q=[1,E,t]}var
r=q}else{var
l=j[2],i=j[1];if(0===c[0]){var
v=a(e[19][15],u,l),G=p(f(l,0)[1],c);f(v,0)[1]=G;var
y=[1,i,v]}else{var
z=c[2],m=c[1];if(i<m){var
A=a(e[19][15],u,z),H=p(j,f(z,0)[1]);f(A,0)[1]=H;var
n=[1,m,A]}else
if(m<i){var
B=a(e[19][15],u,l),I=p(f(l,0)[1],c);f(B,0)[1]=I;var
n=[1,i,B]}else{var
J=h(i,c),K=h(i,j),o=a(d[5],K,J),C=x(o+1|0,[0,b]),L=0;if(!(o<0)){var
k=L;for(;;){var
M=w(i,k,c),N=p(w(i,k,j),M);f(C,k)[k+1]=N;var
O=k+1|0;if(o!==k){var
k=O;continue}break}}var
n=[1,i,C]}var
y=n}var
r=y}return X(r)}function
M(d){if(0===d[0])return c(g[4],d[1]);var
f=a(e[19][15],M,d[2]);return i(e[19][17],g[12],b,f)}function
N(b,c){if(0===b[0])return[0,a(g[9],b[1],c)];var
d=b[2],f=b[1];function
h(a){return N(a,c)}return[1,f,a(e[19][15],h,d)]}function
Z(c){var
d=M(c);return a(g[1],d,b)?c:N(c,d)}function
_(b){if(0===b[0])return 0;var
d=b[1],f=c(e[19][11],b[2]),g=[0,[0,d,0],a(e[17][12],_,f)];return c(e[17][10],g)}function
$(d,g,c){if(1===c[0]){var
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
u(b,a){return $(b,f,o(a,c))}var
v=a(e[19][16],u,h);return i(e[19][17],p,[0,b],v)}function
an(k,c){if(0===c[0])return[0,b];var
d=c[2],g=c[1];if(g===k){var
e=d.length-1-1|0;if(1===e)return f(d,1)[2];var
h=x(e,[0,b]),i=e-1|0,l=0;if(!(i<0)){var
a=l;for(;;){var
j=a+1|0,m=f(d,j)[j+1],n=o([0,H(a+1|0)],m);f(h,a)[a+1]=n;var
p=a+1|0;if(i!==a){var
a=p;continue}break}}return[1,g,h]}return[0,b]}function
t(b){if(0===b[0])return[0,c(g[8],b[1])];var
d=b[1];return[1,d,a(e[19][15],t,b[2])]}function
O(b,a){return p(b,t(a))}function
y(b,a){return 0===a?V:o(b,y(b,a-1|0))}function
l(b,a){return o(b,a)}function
P(b,a){return O(b,a)}function
z(b,a){return y(b,a)}function
q(b,a){return w(b,h(b,a),a)}function
ao(b,a){return w(b,0,a)}function
B(b,a){var
c=h(b,a),d=y(W(b),c);return O(a,o(q(b,a),d))}function
Q(c){var
a=c;for(;;){var
b=s(a);if(0<b){var
a=q(b,a);continue}if(0===a[0])return a[1];throw[0,K,bz]}}function
ap(d){var
c=Z(d),e=Q(c);return a(g[3],b,e)?c:t(c)}var
aa=[0,1];function
aq(b){var
a=b;for(;;){if(0===a[0])return a[1];var
a=f(a[2],0)[1];continue}}function
ab(b){if(aa[1]){var
f=c(d[20],b);return a(d[16],bA,f)}if(3<b){var
g=c(aM[1],(b-4|0)+97|0);return a(e[15][1],1,g)}var
h=c(aM[1],b+119|0);return a(e[15][1],1,h)}var
j=[0,0];function
R(n){if(0<j[1]){if(0===n[0]){var
o=n[1];j[1]=j[1]-1|0;if(a(g[3],b,o))return c(g[15],o);var
u=c(g[15],o),v=a(d[16],u,bB);return a(d[16],bC,v)}var
p=n[2],m=ab(n[1]),i=[0,bD],h=[0,bE],r=R(f(p,0)[1]);if(1-ad(r,bF))i[1]=r;var
q=[0,0],s=p.length-1-1|0;if(!(s<1)){var
l=s;for(;;){if(0<=j[1]){var
k=R(f(p,l)[l+1]);h[1]=bH;if(1===l){if(1-ad(k,bI)){j[1]=j[1]-1|0;if(ad(k,bJ))h[1]=m;else
if(a(e[15][17],k,43)){var
z=a(d[16],bL,m),A=a(d[16],k,z);h[1]=a(d[16],bM,A)}else{var
B=a(d[16],bN,m);h[1]=a(d[16],k,B)}}}else
if(1-ad(k,bO)){j[1]=j[1]-1|0;if(ad(k,bP)){var
C=c(d[20],l),D=a(d[16],bQ,C);h[1]=a(d[16],m,D)}else
if(a(e[15][17],k,43)){var
E=c(d[20],l),F=a(d[16],bR,E),G=a(d[16],m,F),H=a(d[16],bS,G),I=a(d[16],k,H);h[1]=a(d[16],bT,I)}else{var
J=c(d[20],l),K=a(d[16],bU,J),L=a(d[16],m,K),M=a(d[16],bV,L);h[1]=a(d[16],k,M)}}var
t=1-c(e[15][30],h[1]),w=t?1-q[1]:t;if(w){j[1]=j[1]-1|0;if(c(e[15][30],i[1]))i[1]=h[1];else{var
y=a(d[16],bK,h[1]);i[1]=a(d[16],i[1],y)}}}else{h[1]=bW;if(1-q[1]){var
N=a(d[16],bX,h[1]);i[1]=a(d[16],i[1],N)}q[1]=1}var
x=l-1|0;if(1!==l){var
l=x;continue}break}}if(c(e[15][30],i[1])){j[1]=j[1]-1|0;i[1]=bG}return i[1]}return bY}function
v(a){j[1]=20;return R(a)}function
ar(b){var
c=v(b);return a(ai[96],bZ,c)}function
ac(f){var
b=[0,b0];function
g(c){var
e=v(c),f=a(d[16],e,b1);b[1]=a(d[16],b[1],f);return 0}a(e[19][13],g,f);var
h=a(d[16],b[1],b2);return c(m[3],h)}function
at(a){return ac(c(e[19][12],a))}function
F(j,i,e){if(0===e){if(0===j[0]){var
o=j[1];if(0===i[0]){var
s=i[1],x=a(g[10],o,s);return a(g[1],x,b)?[0,[0,a(g[9],o,s)],k]:c(d[2],b3)}}throw[0,K,b4]}var
t=h(e,i),y=q(e,i),f=[0,j],m=[0,k],u=[0,1],z=B(e,i);for(;;){if(u[1])if(!n(f[1],k)){var
v=h(e,f[1]);if(v<t)u[1]=0;else{var
A=q(e,f[1]),C=B(e,f[1]),D=r(A,y,e-1|0),w=l(D,J(e,v-t|0));m[1]=p(m[1],w);f[1]=P(C,l(w,z))}continue}return[0,m[1],f[1]]}}function
r(f,e,b){var
g=F(f,e,b),h=g[2],i=g[1];if(n(h,k))return i;var
j=c(d[20],b),l=a(d[16],j,b5),m=a(d[16],b6,l),o=a(d[16],b7,m),p=v(h),q=a(d[16],p,o),r=a(d[16],b8,q),s=a(d[16],b9,r),t=v(e),u=a(d[16],t,s),w=a(d[16],b_,u),x=a(d[16],b$,w),y=v(f),z=a(d[16],y,x),A=a(d[16],ca,z),B=a(d[16],cb,A);return c(d[2],B)}function
au(c,b){var
e=s(b),f=s(c);return r(c,b,a(d[5],f,e))}function
av(c,b){var
f=s(b),g=s(c),e=a(d[5],g,f);try{var
i=h(e,b),j=(1+h(e,c)|0)-i|0;r(o(c,y([0,Q(b)],j)),b,e);var
k=1;return k}catch(a){a=A(a);if(a[1]===as)return 0;throw a}}function
aw(d,b,a){if(0===b[0])return[0,k,b,1,d];if(a===b[1]){var
e=[0,0],c=[0,d],f=q(a,b),j=B(a,b),g=[0,k],m=h(a,b);for(;;){var
n=h(a,b);if(n<=h(a,c[1])){var
o=h(a,c[1]),r=q(a,c[1]),s=B(a,c[1]),i=l(r,J(a,o-m|0)),t=l(i,j);c[1]=P(l(f,s),t);g[1]=p(l(f,g[1]),i);e[1]=e[1]+1|0;continue}return[0,c[1],f,e[1],g[1]]}}return[0,k,b,1,d]}function
E(f,c,a,b){if(1===a[0]){var
g=a[2];if(b===a[1]){var
h=function(c,a){return C(c,a,b-1|0)};return i(e[19][17],h,c,g)}}var
d=b-1|0;return f<50?T(f+1|0,c,a,d):aE(T,[0,c,a,d])}function
T(i,f,e,b){if(0===f[0]){var
p=f[1];if(0===e[0]){var
q=c(g[4],e[1]),s=c(g[4],p);return[0,a(g[12],s,q)]}}if(n(f,k))return e;if(n(e,k))return f;if(0===h(b,e))return i<50?E(i+1|0,e,f,b):aE(E,[0,e,f,b]);if(0===h(b,f))return i<50?E(i+1|0,f,e,b):aE(E,[0,f,e,b]);var
t=G(f,b),j=C(t,G(e,b),b-1|0),u=c(d[20],b);c(m[1],u);var
v=r(f,j,b),o=af(v,r(e,j,b),b);return l(j,r(o,G(o,b),b))}function
ay(a,b,c){return a5(E(0,a,b,c))}function
C(a,b,c){return a5(T(0,a,b,c))}function
ae(c,b,a){return C(c,b,a)}function
ax(c,b){var
e=s(b),f=s(c);return ae(c,b,a(d[5],f,e))}function
G(a,b){if(1===a[0]){var
c=a[2];if(a[1]===b){var
d=function(c,a){return C(c,a,b-1|0)};return i(e[19][17],d,k,c)}}return a}function
ag(u,s,p,o,m,b){var
d=u,a=s,c=p,i=o,g=m;for(;;){if(n(a,k))return d;var
j=h(b,a),e=q(b,a),f=g-j|0,v=t(a),w=F(l(z(t(e),f+1|0),d),v,b)[2],x=ah(e,c,f,b),d=a,a=r(w,l(i,z(c,f)),b),c=x,i=e,g=j;continue}}function
af(j,i,b){var
c=j,a=i;for(;;){if(n(a,k))return c;var
f=h(b,c),d=h(b,a);if(f<d){var
p=a,a=c,c=p;continue}var
g=f-d|0,e=q(b,a),m=t(a),o=F(l(z(t(e),g+1|0),c),m,b)[2];return ag(a,o,z(e,g),e,d,b)}}function
ah(c,g,f,e){var
a=[0,c],d=f-1|0,h=1;if(!(d<1)){var
b=h;for(;;){a[1]=r(l(a[1],c),g,e);var
i=b+1|0;if(d!==b){var
b=i;continue}break}}return a[1]}function
S(a){if(0===a[0])return c(g[13],a[1]);var
b=a[2],d=0;function
f(b,a){return a+S(b)|0}return i(e[19][18],f,b,d)}return[0,I,W,J,aj,al,s,L,am,n,X,h,Y,u,w,p,M,N,Z,_,ak,$,o,an,t,O,y,l,P,z,q,ao,B,Q,ap,aq,aa,ab,j,v,ar,ac,at,F,r,au,av,aw,ax,ae,G,ay,C,af,ag,ah,S,c(D[18],[0,n,S])]}];S(256,at,"Nsatz_plugin.Polynom");var
ak=[248,cd,s.caml_fresh_oo_id(0)],aN=[0,0],B=[0,function(g){var
al=c(g[1],ce),l=c(g[1],cf),am=c(g[1],cg);function
j(a){return a.length-1-1|0}function
T(c,d){var
h=j(c);if(aN[1]){var
e=[0,0],a=[0,1];for(;;){if(0===e[1])if(a[1]<=h){var
i=a[1],o=f(d,i)[i+1],k=a[1];e[1]=aF(f(c,k)[k+1],o);a[1]=a[1]+1|0;continue}return e[1]}}var
p=f(d,0)[1],l=aF(f(c,0)[1],p);if(0===l){var
g=[0,0],b=[0,h];for(;;){if(0===g[1])if(1<=b[1]){var
m=b[1],q=f(d,m)[m+1],n=b[1];g[1]=-aF(f(c,n)[n+1],q)|0;b[1]=b[1]-1|0;continue}return g[1]}}return l}function
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
g=c(d[20],e),j=a(d[16],cj,g),k=function(c,b){var
e=a(d[16],cl,b);return a(d[16],c,e)},l=i(h[16],k,ck,f);return a(d[16],l,j)}throw b}}var
O=[0,0];function
w(b){function
h(j,i){var
e=[0,0],k=j.length-1-1|0,l=1;if(!(k<1)){var
b=l;for(;;){var
s=f(j,b)[b+1],h=c(d[20],s);if(ae(h,cr))if(ae(h,cs)){var
n=a(d[16],ct,h),o=W(O[1],b-1|0),p=[0,a(d[16],o,n),0];e[1]=a(d[22],e[1],p)}else{var
r=[0,W(O[1],b-1|0),0];e[1]=a(d[22],e[1],r)}var
q=b+1|0;if(k!==b){var
b=q;continue}break}}var
g=e[1];if(g){if(i)return a(au[7],cm,g);var
m=a(au[7],cn,g);return a(d[16],co,m)}return i?cp:cq}function
l(j,k){var
z=j?0:1;if(z)return k?cD:cE;var
A=0,B=j?j[2]:c(d[2],cG),C=l(B,A),D=a(d[16],cF,C),m=j?j[1]:c(d[2],cH),f=m[2],n=c(g[39],m[1]),o=a(d[16],n,ch),b=a(d[16],ci,o);if(ae(b,cu))if(ae(b,cv))if(ae(b,cw))if(45===s.caml_string_get(b,0))var
p=h(f,0),q=i(au[4],b,1,s.caml_ml_string_length(b)-1|0),r=a(d[16],q,p),e=a(d[16],cx,r);else
if(0===k)var
t=h(f,0),u=a(d[16],b,t),e=a(d[16],cy,u);else
var
v=h(f,0),e=a(d[16],b,v);else
if(0===k)var
w=h(f,1),e=a(d[16],cz,w);else
var
e=h(f,1);else
var
e=cA;else
var
x=h(f,1),y=a(d[16],cB,x),e=a(d[16],cC,y);return a(d[16],e,D)}return l(b,1)}var
P=[0,d[7]];function
E(b){P[1]=10;var
f=P[1];if(f<c(h[1],b))var
g=c(h[1],b),i=c(d[20],g),j=a(d[16],i,cI),k=a(d[16],cJ,j),l=w([0,c(h[3],b),0]),e=a(d[16],l,k);else
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
t(n,e,b){function
c(h){var
o=h[2],p=h[1],c=j(e),d=x(c+1|0,0),i=0;if(!(c<0)){var
b=i;for(;;){var
k=f(o,b)[b+1],l=f(e,b)[b+1]+k|0;f(d,b)[b+1]=l;var
m=b+1|0;if(c!==b){var
b=m;continue}break}}return[0,a(g[22],n,p),d]}return a(cc[12],c,b)}function
u(a){return c(g[1],[0,a])}function
aw(c,a){var
b=x(c+1|0,0);f(b,a)[a+1]=1;var
d=M(b);return[0,[0,u(1),d],0]}function
ax(a){function
b(a){if(a){var
d=a[1],e=d[2],f=d[1],h=b(a[2]);return[0,[0,c(g[24],f),e],h]}return 0}return b(a)}function
v(e,b){function
c(b){if(b){var
d=b[1],f=d[2],h=d[1],i=c(b[2]);return[0,[0,a(g[22],e,h),f],i]}return 0}return c(b)}function
F(e,d){var
a=e,b=0;for(;;){if(a){var
c=a[1],f=a[2],a=f,b=n(t(c[1],c[2],d),b);continue}return b}}function
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
p=[0,0],k=[0,x(aJ,y)];function
Z(e,d){p[1]=p[1]+1|0;if(k[1].length-1<=p[1])k[1]=a(ar[5],k[1],x(p[1],y));var
b=[0,[0,e],p[1],d],c=p[1];f(k[1],c)[c+1]=b;return b}function
_(f,e){var
a=e;for(;;){if(a){var
d=a[1],g=a[2],i=b(d);if(0===U(f,c(h[3],i)[2])){var
a=g;continue}return d}return y}}function
az(e,d,c,b,a){var
f=t(b,a,d);return n(v(c,e),f)}var
$=a(D[1],0,aJ);function
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
f(a){return I(a,a,r(d,u(1)))}return a(h[11],f,c)}function
ad(f,p,e){function
k(a){if(a){var
h=a[1],d=h[2],q=a[2],r=h[1];try{var
w=aa(d),e=w}catch(a){a=A(a);if(a!==V)throw a;var
i=_(d,p);b(i);var
e=i}var
f=b(e);if(f){var
j=f[1],s=f[2],u=j[2],v=z(r,j[1]),l=c(g[24],v),m=B(d,u),o=k(n(q,t(l,m,s)));return[0,[0,[0,l,m,e],o[1]],o[2]]}return[0,0,a]}return cK}var
d=k(f),l=d[2],m=d[1],o=q[1];function
s(c,f){function
d(d,c){var
e=c[2],g=c[3],h=c[1],i=b(f);if(a(N,b(g),i)){var
k=u(1);return n(d,t(h,e,r(j(e),k)))}return d}return i(h[16],d,c,m)}return[0,i(h[19],s,e,o),l]}var
Q=[0,0],J=[0,y],aB=[0,1];function
R(b){var
c=0;function
e(c,b){var
e=f(b[2],0)[1];return a(d[5],c,e)}return Z(b,i(h[16],e,c,b))}function
aC(k,i){var
l=b(k),m=b(i),e=c(h[3],l)[2],q=c(h[3],m)[2],s=c(h[3],l)[1],v=c(h[3],m)[1],D=c(h[4],l),E=c(h[4],m),w=Y(s,v),x=C(e,q),y=B(x,e),A=B(x,q);function
o(b,a){var
d=z(s,w),e=t(c(g[24],d),A,a);return n(t(z(v,w),y,b),e)}var
F=o(D,E),G=i[3],H=f(A,0)[1]+G|0,J=k[3],K=f(y,0)[1]+J|0,p=Z(F,a(d[5],K,H)),L=u(1);I(p,k,o(r(j(e),L),0));var
M=u(1);I(p,i,o(0,r(j(e),M)));return p}function
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
t=e[1],r=0===f(v,t)[t+1]?1:0;g[1]=r;e[1]=e[1]+1|0;continue}if(g[1])var
u=0;else
var
A=o(m),B=C(o(l),A),i=m[2],k=l[2],z=0,D=s.caml_lessthan(k,i)?[0,k,i]:[0,i,k],u=[0,[0,D,B],z];return a(d[22],E,u)}}var
m=i(h[16],k,g,e);return a(h[41],af,m)}function
aD(a){function
c(a){if(a){var
b=a[2],d=a[1];if(b){var
e=c(b);return ag(ah(d,b),e)}}return 0}return c(a)}function
aE(b,i,l){var
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
g=c(h[1],e),i=c(d[20],g),j=a(d[16],i,cL),k=a(d[16],cM,j),l=c(h[1],f),n=c(d[20],l),o=a(d[16],n,k),p=a(d[16],cN,o);return c(m[5],p)}return b}function
S(j,f,p){var
s=ab(b(J[1]),f),k=s[2],z=s[1],A=E(k),B=a(d[16],A,cO),C=a(d[16],cP,B);c(m[5],C);L[1]=a(g[22],L[1],z);J[1]=R(k);if(0===k){c(m[5],cQ);var
D=q[1],G=function(a){return 0},H=a(h[13],G,D),e=L[1],t=ad(v(e,j),f,H),x=t[1],I=t[2];c(m[5],cR);var
K=w(I),M=a(d[16],K,cS),N=a(d[16],cT,M);c(m[5],N);var
l=[0,v(e,j)],O=q[1],P=function(c,a){var
d=F(c,b(a));l[1]=n(l[1],d);return 0};i(h[18],P,x,O);var
Q=w(l[1]),S=a(d[16],Q,cU),T=a(d[16],cV,S);c(m[5],T);var
U=w(r(1,e)),V=a(d[16],U,cW),W=a(d[16],cX,V);c(m[5],W);var
y=function(b){if(b){var
c=b[2],d=b[1],e=y(c),f=function(a){return ac(d,a)};return[0,a(h[13],f,c),e]}return 0},X=c(h[6],p),Y=y(c(h[6],f)),o=[0,c(h[6],Y)],Z=function(a){o[1]=c(h[4],o[1]);return 0};a(h[11],Z,p);var
_=o[1],$=c(h[6],x),aa=function(a){return v(u(-1),a)};return[0,X,j,[0,e,1,_,a(h[13],aa,$)]]}throw ak}var
aI=[0,0];function
aj(a){return a?f(a[1][2],0)[1]:-1}function
aK(e,t,s){c(m[5],cY);ai[1]=0;c(D[2],$);return function(B){var
j=B;for(;;){var
o=j[2],g=j[1];aH(g,o);if(o){var
n=o[2],u=o[1],w=u[1],p=w[2],r=w[1];if(aE([0,[0,r,p],u[2]],g,n)){c(m[5],cZ);var
j=[0,g,n];continue}var
C=f(k[1],p)[p+1],e=aC(f(k[1],r)[r+1],C);if(Q[1])if(0!==b(e)){var
Z=aj(b(J[1]));if(Z<aj(b(e))){c(m[5],c3);var
j=[0,g,n];continue}}var
x=ab(b(e),g),y=x[1];if(x[2]){var
F=v(y,b(e));e[1][1]=F;var
L=q[1],M=function(b,c){return function(a){return v(c,ac(b,a))}}(e,y),N=a(h[13],M,L),z=ad(b(e),g,N),O=z[1];e[1][1]=z[2];var
P=q[1],R=function(c){return function(d,b){a(D[9],H,[0,c[2],b[2]]);return I(c,b,d)}}(e);i(h[18],R,O,P);var
T=E(b(e)),U=a(d[16],T,c0),V=a(d[16],c1,U);c(m[5],V);q[1]=K(e,g);G[1]=K(l,G[1]);try{var
X=K(e,g),Y=S(b(t),X,s);return Y}catch(a){a=A(a);if(a===ak){var
W=aG(e,g,n),j=[0,K(e,g),W];continue}throw a}}c(m[5],c2);var
j=[0,g,n];continue}return S(b(t),g,s)}}(e)}function
aL(b){if(b){var
c=b[2],d=f(b[1][2],0)[1],e=function(a){return ap(f(a[2],0)[1],d)};return a(h[23],e,c)}return 1}return[0,an,r,av,aw,N,O,n,ax,F,ay,G,function(o,j,g){c(D[2],$);c(D[2],H);p[1]=0;k[1]=x(aJ,y);Q[1]=a(h[23],aL,[0,g,j]);if(Q[1])c(m[5],c4);var
q=E(g),r=a(d[16],q,c5),s=a(d[16],c6,r);c(m[5],s);function
t(c,b){var
e=E(b),f=a(d[16],e,c8);return a(d[16],c,f)}var
u=i(h[16],t,c7,j),v=a(d[16],c9,u);c(m[5],v);var
e=a(h[13],R,j),n=R(g);aA(o,e);L[1]=l;J[1]=n;try{var
C=S(b(n),e,e),f=C}catch(a){a=A(a);if(a!==ak)throw a;var
f=aK([0,e,aD(e)],n,e)}var
w=f[3],z=f[2],B=f[1];c(m[5],c_);return[0,a(h[13],b,B),z,w]},at,aB,aI]},ak,aN];S(261,B,"Nsatz_plugin.Ideal");var
aP=z[35],da=c(aP,0);c(aP,1);var
aQ=n[49],aR=z[24],aS=z[27],aT=z[16],db=n[48],dc=z[25],dd=z[4],de=z[5],df=z[10],dg=z[8],dh=z[3],di=z[15],dj=z[33];function
dk(a){try{var
b=c(z[37],a);return b}catch(a){a=A(a);if(a[1]===as)return 1;throw a}}var
dl=z[19];function
dm(e,d){var
c=e,b=d;for(;;){if(a(aR,b,da))return c;if(a(aS,c,b)){var
g=b,b=c,c=g;continue}var
f=a(aT,c,b),c=b,b=f;continue}}function
aU(b){return a(n[32],b,E)?0:[0,b]}function
aV(a){var
b=a[2],c=a[1];return 1===b?c:[6,c,b]}function
aw(a){var
b=a[1];if(typeof
b==="number")return a[2];var
c=a[2];return typeof
c==="number"?b:[3,b,c]}function
aW(c){var
b=c[1];if(typeof
b==="number")return 0;var
d=c[2];if(typeof
d==="number")return 0;else
if(0===d[0])if(a(n[32],d[1],O))return b;if(typeof
b!=="number"&&0===b[0]){var
e=c[2];if(a(n[32],b[1],O))return e}return[5,b,c[2]]}c(w[112],1);var
ax=[k,function(a){return i(u[4],dq,dp,dn)}],P=[k,function(a){return i(u[4],dt,ds,dr)}],X=[k,function(a){return i(u[4],dw,dv,du)}],Y=[k,function(a){return i(u[4],dz,dy,dx)}],Z=[k,function(a){return i(u[4],dC,dB,dA)}],_=[k,function(a){return i(u[4],dF,dE,dD)}],$=[k,function(a){return i(u[4],dI,dH,dG)}],aa=[k,function(a){return i(u[4],dL,dK,dJ)}],aX=[k,function(a){return i(u[4],dN,ay,dM)}],aY=[k,function(a){return i(u[4],dP,ay,dO)}],aZ=[k,function(a){return i(u[4],dR,ay,dQ)}],j=[k,function(a){return i(u[4],dT,F,dS)}],al=[k,function(a){return i(u[4],dV,F,dU)}],ab=[k,function(a){return i(u[4],dX,F,dW)}],d0=[k,function(a){return i(u[4],dZ,F,dY)}],d3=[k,function(a){return i(u[4],d2,F,d1)}],ac=[k,function(a){return i(u[4],d5,F,d4)}],am=[k,function(a){return i(u[4],d7,F,d6)}],an=[k,function(a){return i(u[4],d9,F,d8)}],ea=[k,function(a){return i(u[4],d$,F,d_)}];function
t(a,d){var
b=r(a),f=c(e[19][12],d),g=p===b?a[1]:k===b?c(q[2],a):a;return c(w[a9],[0,g,f])}function
az(f){var
a=r(j),b=0,d=0,e=p===a?j[1]:k===a?c(q[2],j):j;return t(aX,[0,t(ax,[0,e,d]),b])}function
Q(b){if(a(n[26],b,O)){var
d=r(am);return p===d?am[1]:k===d?c(q[2],am):am}var
e=a(n[12],b,W);return a(n[26],e,E)?t(ac,[0,Q(a(n[11],b,W)),0]):t(d3,[0,Q(a(n[11],b,W)),0])}function
a0(b){if(a(n[26],b,E)){var
d=r(al);return p===d?al[1]:k===d?c(q[2],al):al}return a(n[28],b,E)?t(ab,[0,Q(b),0]):t(d0,[0,Q(a(n[4],eb,b)),0])}function
G(A){var
b=A;for(;;)if(typeof
b==="number"){var
b=[0,E];continue}else
switch(b[0]){case
0:var
x=c(n[50],b[1]),d=c(av[5],x),y=c(av[3],d);c(n[48],y);var
z=c(av[2],d),f=r(j),B=[0,a0(c(n[48],z)),0],C=p===f?j[1]:k===f?c(q[2],j):j;return t(P,[0,C,B]);case
1:var
g=r(j),D=[0,Q(c(n[43],b[1])),0],F=p===g?j[1]:k===g?c(q[2],j):j;return t(X,[0,F,D]);case
2:var
h=r(j),H=[0,G(b[1]),0],I=p===h?j[1]:k===h?c(q[2],j):j;return t($,[0,I,H]);case
3:var
J=b[1],K=[0,G(b[2]),0],i=r(j),L=[0,G(J),K],M=p===i?j[1]:k===i?c(q[2],j):j;return t(Y,[0,M,L]);case
4:var
N=b[1],R=[0,G(b[2]),0],l=r(j),S=[0,G(N),R],T=p===l?j[1]:k===l?c(q[2],j):j;return t(Z,[0,T,S]);case
5:var
U=b[1],V=[0,G(b[2]),0],m=r(j),W=[0,G(U),V],ab=p===m?j[1]:k===m?c(q[2],j):j;return t(_,[0,ab,W]);default:var
o=b[2],ac=b[1];if(0===o){var
s=r(j),ad=[0,a0(O),0],ae=p===s?j[1]:k===s?c(q[2],j):j;return t(P,[0,ae,ad])}var
u=c(n[45],o),af=0;if(a(n[32],u,E))var
e=r(an),v=p===e?an[1]:k===e?c(q[2],an):an;else
var
v=t(ea,[0,Q(u),0]);var
w=r(j),ag=[0,G(ac),[0,v,af]],ah=p===w?j[1]:k===w?c(q[2],j):j;return t(aa,[0,ah,ag])}}function
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
a1(e){var
b=c(w[af],e);if(9===b[0]){var
a=b[2],d=a.length-1-1|0;if(!(2<d>>>0))switch(d){case
0:return 0;case
1:break;default:var
f=a[2],g=a1(a[3]);return[0,H(f),g]}}throw[0,K,ec]}var
N=[0,0];function
ed(b){function
c(e){var
b=e;for(;;)if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return 0;case
1:var
f=a6(b[1]);N[1]=a(d[5],N[1],f);return 0;case
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
g=c(at[1],[0,aR,aS,dc,dd,de,df,dg,dh,di,aT,dl,dm,dk,aQ,dj]),l=c(B[1],[0,g[1],g[2],g[3],g[4],g[5],g[6],g[7],g[8],g[9],g[10],g[11],g[12],g[13],g[14],g[15],g[16],g[17],g[18],g[19],g[20],g[21],g[22],g[23],g[24],g[25],g[26],g[27],g[28],g[29],g[30],g[31],g[32],g[33],g[34],g[35],g[36],g[37],g[38],g[39],g[40],g[41],g[42],g[43],g[44],g[45],g[46],g[47],g[48],g[49],g[50],g[51],g[52],g[53],g[54],g[55],g[56],g[57]]);function
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
j=a[1],n=f(c,j)[j+1];return[0,a[1],n]}}return[0,d,e]},s=i(e[17][15],u,ee,g),j=s[2],b=s[1];if(0===b){var
m=function(b){if(0===b[0])return aU(c(db,b[1]));var
f=[0,0],g=b[2],h=b[1];function
i(b,a){var
e=aV([0,[1,c(d[20],h)],b]),g=aW([0,m(a),e]);f[1]=aw([0,f[1],g]);return 0}a(e[19][14],i,g);return f[1]},o=m(t);return c(e[17][47],p)?o:aw([0,o,h(p)])}var
k=[0,0],l=[0,0],v=function(c){var
a=c[2],d=c[1];return j<=f(a,b)[b+1]?(a[b+1]=f(a,b)[b+1]-j|0,k[1]=[0,[0,d,a],k[1]],0):(l[1]=[0,[0,d,a],l[1]],0)};a(e[17][11],v,g);var
w=1===j?[1,c(d[20],b)]:aV([0,[1,c(d[20],b)],j]),x=h(c(e[17][6],l[1]));return aw([0,aW([0,w,h(c(e[17][6],k[1]))]),x])}return aU(c(n[43],ef))}return h(g)}function
a2(b,a){function
d(b,a){if(b){if(0===b[1]){var
c=b[2];if(a){var
e=a[1];return[0,e,d(c,a[2])]}throw[0,K,eo]}var
f=d(b[2],a);return[0,l[3],f]}return a}var
f=d(b,c(e[17][6],a));return c(e[17][6],f)}function
eI(a3){var
C=a1(a3);N[1]=0;a(e[17][11],ed,C);if(C){var
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
Y=W[1],au=I[2];if(7<X>>>0)c(U[6],eq);else
switch(X){case
0:c(m[5],eA);B[3][1]=0;l[14][1]=0;l[15][1]=0;break;case
1:c(m[5],eB);B[3][1]=0;l[14][1]=1;l[15][1]=0;break;case
2:c(m[5],eC);B[3][1]=1;l[14][1]=0;l[15][1]=0;break;case
3:c(m[5],eD);B[3][1]=1;l[14][1]=1;l[15][1]=0;break;case
4:c(m[5],eE);B[3][1]=0;l[14][1]=0;l[15][1]=1;break;case
5:c(m[5],eF);B[3][1]=0;l[14][1]=1;l[15][1]=1;break;case
6:c(m[5],eG);B[3][1]=1;l[14][1]=0;l[15][1]=1;break;default:c(m[5],eH);B[3][1]=1;l[14][1]=1;l[15][1]=1}var
Z=N[1],s=[0,0];if(!(Z<1)){var
z=Z;for(;;){var
aU=s[1],aV=c(d[20],z),aW=a(d[16],aV,ey),aX=[0,a(d[16],ez,aW),0];s[1]=a(e[18],aX,aU);var
a0=z-1|0;if(1!==z){var
z=a0;continue}break}}s[1]=a(e[18],er,s[1]);l[6][1]=s[1];var
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
m=[0,c(aQ,i)],j=a(l[2],f,m);var
e=j;break;case
1:var
h=a6(b[1]);if(h<=Y)var
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
c=b[1],d=R(b[2]),e=d[2],f=d[1];if(!a(l[5],c,l[3]))if(!at(c))return[0,[0,c,f],[0,0,e]];return[0,f,[0,1,e]]}return en},S=R(ay),_=S[2],aB=S[1],ap=c(aO[87],0),v=i(l[12],N[1],aB,aw)[3];c(e[17][6],l[11][1]);var
aq=c(aO[87],0)-ap,ar=a(ai[98],el,aq),as=a(d[16],em,ar);c(m[5],as);c(m[5],es);var
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
d=a[1];return[0,d,g(a[2],b-1|0)]}throw[0,K,eg]}return a},i=g(c(e[17][6],m),l);d[1]=c(e[17][6],i)}var
n=a+1|0;if(h!==a){var
a=n;continue}break}}return d[1]},o=a(e[17][12],ad,ac),ae=b-c(e[17][1],o)|0,af=c(d[20],ae),ag=a(d[16],af,eh),ah=a(d[16],ei,ag);c(m[5],ah);var
aj=c(e[17][1],o),ak=c(d[20],aj),al=a(d[16],ak,ej),am=a(d[16],ek,al);c(m[5],am);if(o){var
aD=o[2],aE=a2(_,o[1]),y=N[1],aF=aA(y,a(l[2],y,v[1])),aG=[6,0,v[2]],aH=c(e[17][6],aD),aI=function(a){return a2(_,a)},aJ=a(e[17][12],aI,aH),aK=function(a){return aA(y,a)},aL=c(e[17][12],aK),aM=a(e[17][12],aL,aJ),aN=function(a){return aA(y,a)},aP=a(e[17][12],aN,aE),aR=c(d[20],Y),aS=a(d[16],aR,et),aT=a(d[16],eu,aS);c(m[5],aT);c(m[5],ev);var
a4=a(e[18],[0,[0,aF,[0,aG,aP]],0],aM),a5=function(b){return a(e[17][12],G,b)},a7=a(e[17][12],a5,a4),a8=t(aY,[0,az(0),0]),a9=function(d,b){var
a=r(j),f=0,g=0,h=p===a?j[1]:k===a?c(q[2],j):j,l=t(aY,[0,t(ax,[0,h,g]),f]);function
m(d,b){var
a=r(j),e=[0,d,[0,b,0]],f=0,g=p===a?j[1]:k===a?c(q[2],j):j;return t(aZ,[0,t(ax,[0,g,f]),e])}var
n=[0,i(e[17][16],m,d,l),[0,b,0]];return t(aZ,[0,az(0),n])},a_=i(e[17][16],a9,a7,a8);c(m[5],eJ);return a_}throw[0,K,ew]}throw[0,K,ex]}var
u=1,M=0}else
var
M=1;if(M)var
u=1}else
var
u=1}else
var
u=1}else
var
u=0}throw[0,K,ep]}function
eK(a){var
b=[0,t(aX,[0,az(0),0]),a],d=[0,i(u[4],eN,eM,eL),b],e=[0,c(w[a9],d),0];return c(c$[148],e)}var
aB=[0,function(a){try{var
d=eI(a),b=d}catch(a){a=A(a);if(a!==B[2])throw a;var
b=c(U[6],eO)}return eK(b)}];S(270,aB,"Nsatz_plugin.Nsatz");c(aD[12],eR);c(aD[12],eS);function
eT(e){var
a=[31,eQ[4],[0,[0,ao,eU],0],0],b=[28,[0,[0,[0,c(aC[1][6],eV)],0],a]],d=c(aC[1][5],eW);return e4(a3[4],1,0,d,b)}function
eX(a){if(a)if(!a[2]){var
b=a[1];return function(a){return c(aB[1],b)}}return c(d[2],eY)}var
e0=[0,[0,c(aC[1][6],eZ)],0],e1=[0,a(eP[27],e0,eX)];i(a3[9],0,[0,ao,e2],e1);a(aD[19],eT,ao);var
a4=[0,ao];S(276,a4,"Nsatz_plugin.G_nsatz");S(277,[0,m,at,B,aB,a4],"Nsatz_plugin");return});
