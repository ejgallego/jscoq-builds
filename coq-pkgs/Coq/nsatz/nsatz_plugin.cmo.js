function(e2){"use strict";var
ae="*",bj="term computed",bs="plugins/nsatz/polynom.ml",bi="+",bh="@[%s@]",p=250,aM="(",J="setoid_ring",bl="+ ",br="Init",bg="...",i=246,bq=115,bp="h",aJ=" ",aK=113,bn=477,af="1",be="nsatz_compute",bf="print_pol dans dansideal",bo=")*",ad="plugins/nsatz/nsatz.ml",bk="c",B="",an="^",I="Ring_polynom",H="0",aL=248,bm="u",L="\n",u="CC",q=e2.jsoo_runtime,f=q.caml_check_bound,aH=q.caml_equal,bc=q.caml_fresh_oo_id,aG=q.caml_int_compare,bd=q.caml_int_of_string,e1=q.caml_list_of_js_array,x=q.caml_make_vect,b=q.caml_new_string,o=q.caml_obj_tag,R=q.caml_register_global,ab=q.caml_string_equal,ac=q.caml_string_notequal,bb=q.caml_trampoline,aF=q.caml_trampoline_return,z=q.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):q.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):q.caml_call_gen(a,[b,c])}function
l(a,b,c,d){return a.length==3?a(b,c,d):q.caml_call_gen(a,[b,c,d])}function
aI(a,b,c,d,e){return a.length==4?a(b,c,d,e):q.caml_call_gen(a,[b,c,d,e])}var
k=q.caml_get_global_data(),aw=[0,0,0],C=[0,0],N=[0,1],U=[0,2],aA=[0,b(br),[0,b("Datatypes"),0]],E=[0,b("Numbers"),[0,b("BinNums"),0]],aE=b("nsatz_plugin"),T=k.CErrors,ap=k.Array,g=k.List,d=k.Pervasives,S=k.Flags,ag=k.Pp,aN=k.Feedback,ao=k.Format,e=k.Util,K=k.Assert_failure,aq=k.Failure,aP=k.Char,D=k.Hashtbl,av=k.CList,M=k.Printf,aV=k.Heap,aj=k.Not_found,au=k.String,m=k.Num,n=k.CamlinternalLazy,w=k.Constr,a$=k.EConstr,a9=k.Unix,y=k.Big_int,ax=k.Ratio,bz=b(L),bw=[0,[18,[1,[0,0,b(B)]],[2,0,[12,10,[17,0,0]]]],b("@[%s\n@]")],bu=[0,[18,[1,[0,0,b(B)]],[2,0,[17,0,0]]],b(bh)],bH=[0,b(bs),365,43],b6=b(bg),bJ=b(")"),bK=b(aM),bL=b(B),bM=b(B),bN=b(H),b4=b(bg),b5=b(bi),bP=b(B),bQ=b(H),bR=b(af),bT=b(bo),bU=b(aM),bV=b(ae),bW=b(H),bX=b(af),bY=b(an),bZ=b(an),b0=b(bo),b1=b(aM),b2=b(an),b3=b(ae),bS=b(bi),bO=b(H),b$=b("div_pol1"),ca=[0,b(bs),bn,9],cb=b(L),cc=b("x:"),cd=b(L),ce=b("r:"),cf=b(L),cg=b("q:"),ch=b(L),ci=b("p:"),cj=b("div_pol:\n"),b9=b(L),b8=b("\n{ "),b_=b("}"),b7=[0,[18,[1,[0,0,b(B)]],[2,0,[17,0,0]]],b(bh)],bI=b(bm),bG=b("non"),bE=[0,0],bF=[0,1],c9=b(L),c8=b(B),c_=b("lp:\n"),c7=b("p: "),c6=b("homogeneous polynomials"),c$=b("computed"),c3=b("new polynomial: "),c2=b(bk),c5=b(bp),c4=b(H),c1=b("computation of the Groebner basis"),c0=b("coefficient: "),cZ=b("verif sum: "),cY=b("r: "),cV=b("remainder: "),cW=b("polynomial reduced to 0"),cX=b("r ok"),cT=[0,[12,91,[4,3,0,0,[12,44,[4,3,0,0,[12,93,0]]]]],b("[%i,%i]")],cS=[0,0,0],cQ=b(" terms"),cR=b(" + "),cP=b(bf),cO=b(bf),cz=b(H),cA=b(af),cB=b(an),cu=b(ae),cv=b(ae),cw=b(ae),cx=b(af),cy=b(B),cC=b("-1"),cD=b(H),cE=b(af),cH=b(bl),cI=b(B),cJ=b(aJ),cK=b("-"),cF=b("- "),cG=b(bl),cL=b(H),cM=b(B),cN=b(aJ),ct=b(aJ),cr=b(" i="),cs=b("lv= "),cp=b("]"),cq=b("["),cn=[0,0],co=[0,1],cU=b("Ideal.Make(P).NotInIdealUpdate"),ck=b("Ideal.NotInIdeal"),ee=[0,b(ad),aL,9],eM=b("nsatz cannot solve this problem"),eI=b("eq_refl"),eJ=[0,b(br),[0,b("Logic"),0]],eK=b(u),eG=b(bj),et=[0,[11,b("number of parameters: "),[4,3,0,0,0]],b("number of parameters: %i")],eq=[0,[12,120,[4,3,0,0,0]],b("x%i")],ep=b("nsatz: bad parameter"),ex=b("computation without sugar"),ey=b("computation with sugar"),ez=b("ordre lexico computation without sugar"),eA=b("ordre lexico computation with sugar"),eB=b("computation without sugar, division by pairs"),eC=b("computation with sugar, division by pairs"),eD=b("ordre lexico computation without sugar, division by pairs"),eE=b("ordre lexico computation with sugar, division by pairs"),er=e1([b("a"),b("b"),b(bk),b("d"),b("e"),b("f"),b("g"),b(bp),b("i"),b("j"),b("k"),b("l"),b("m"),b("n"),b("o"),b("p"),b("q"),b("r"),b("s"),b("t"),b(bm),b("v"),b("w"),b("x"),b("y"),b("z")]),es=b("cert ok"),eu=b(bj),ev=[0,b(ad),489,11],ew=[0,b(ad),bn,14],eo=[0,b(ad),506,8],en=[0,b(ad),444,11],em=[0,0,0],el=[0,[11,b("time: "),[18,[1,[0,0,b(B)]],[8,0,[0,1,10],[0,3],[17,0,[12,bq,0]]]]],b("time: @[%10.3f@]s")],ek=[0,[11,b("useful spolynomials: "),[4,3,0,0,[12,32,0]]],b("useful spolynomials: %i ")],ej=[0,[11,b("useless spolynomials: "),[4,3,0,0,0]],b("useless spolynomials: %i")],eg=[0,0,0],eh=[0,0,0],ei=b(H),ed=[0,0],ea=b("Npos"),eb=b(u),d_=b("N0"),d$=b(u),d8=b("xH"),d9=b(u),d6=b("xO"),d7=b(u),d3=b("xI"),d4=b(u),d0=b("Zneg"),d1=b(u),dY=b("Zpos"),dZ=b(u),dW=b("Z0"),dX=b(u),dU=b("Z"),dV=b(u),dS=b("cons"),dT=b(u),dQ=b("nil"),dR=b(u),dO=b("list"),dP=b(u),dL=b("PEpow"),dM=[0,b(J),[0,b(I),0]],dN=b(u),dI=b("PEopp"),dJ=[0,b(J),[0,b(I),0]],dK=b(u),dF=b("PEmul"),dG=[0,b(J),[0,b(I),0]],dH=b(u),dC=b("PEsub"),dD=[0,b(J),[0,b(I),0]],dE=b(u),dz=b("PEadd"),dA=[0,b(J),[0,b(I),0]],dB=b(u),dw=b("PEX"),dx=[0,b(J),[0,b(I),0]],dy=b(u),dt=b("PEc"),du=[0,b(J),[0,b(I),0]],dv=b(u),dq=b("PExpr"),dr=[0,b(J),[0,b(I),0]],ds=b(u),eQ=b("$lt"),eX=b(be),eZ=b(be),eL=k.Tactics,dn=k.Coqlib,dp=k.Universes,eN=k.Mltop,eR=k.Names,eT=k.Stdarg,eU=k.Genarg,eW=k.Loc,e0=k.Ltac_plugin;function
bt(b){return S[6][1]?(a(ao[aK],bu,b),c(d[51],d[27])):0}function
bv(b){return S[6][1]?(a(ao[aK],bw,b),c(d[51],d[27])):0}function
bx(a){return 0}function
by(b){if(S[6][1]){var
e=a(d[16],b,bz);c(d[30],e);return c(d[51],d[27])}return 0}function
bA(d){var
b=S[6][1];if(b){var
e=c(ag[3],d);return a(aN[10],0,e)}return b}function
bB(d){var
b=S[6][1];if(b){var
e=c(d,0),f=c(ag[3],e);return a(aN[10],0,f)}return b}function
aO(e,d,c){var
b=c;for(;;){if(b){var
f=b[2];if(a(e,d,b[1]))return 1;var
b=f;continue}return 0}}function
bC(e,d){var
b=[0,0];function
f(a){var
c=1-aO(e,a,b[1]),d=c?(b[1]=[0,a,b[1]],0):c;return d}a(g[15],f,d);return c(g[9],b[1])}function
bD(k,f,o){function
p(a){return 1-c(f,a)}var
e=0,b=a(g[35],p,o);for(;;){if(b){var
h=b[2],i=b[1],j=[0,0],l=[0,0],q=function(h,d,i){return function(e){try{var
b=a(k,h,e),g=c(f,b)?(i[1]=1,0):(d[1]=[0,b,d[1]],0);return g}catch(a){a=z(a);if(c(T[20],a))return 0;throw a}}}(i,j,l);a(g[15],q,e);if(l[1]){var
b=h;continue}if(0===j[1]){var
m=[0,h],n=[0,0],r=function(j,g,h){return function(b){try{var
d=a(k,b,j),e=1-c(f,d),i=e?(g[1]=[0,d,g[1]],0):e;return i}catch(a){a=z(a);if(c(T[20],a)){h[1]=[0,b,h[1]];return 0}throw a}}}(i,m,n);a(g[15],r,e);var
s=m[1],e=c(g[9],[0,i,n[1]]),b=s;continue}var
b=a(d[25],j[1],h);continue}return e}}var
j=[0,bt,bv,bx,by,bB,bA,aO,bC,bD,function(k,j,d,b,e){var
g=x(b.length-1,[0,d,0]);function
h(i,h){var
b=[0,h],d=[0,0];if(1-c(j,h)){var
l=function(f,e){try{for(;;){var
g=a(k,b[1],e);d[1]=[0,f,d[1]];b[1]=g;continue}}catch(a){a=z(a);if(c(T[20],a))return 0;throw a}};a(ap[14],l,e)}var
m=[0,b[1],d[1]];return f(g,i)[i+1]=m}a(ap[14],h,b);return[0,e,g]}];R(197,j,"Nsatz_plugin.Utile");var
ar=[0,function(g){function
H(a){return c(g[14],[0,a])}var
b=H(0),U=H(1);function
I(a){return[0,c(g[14],a)]}var
k=I(bE),V=I(bF);function
W(a){return[1,a,[0,k,V]]}function
J(d,a){if(0===a)return[0,U];var
c=x(a+1|0,[0,b]);f(c,a)[a+1]=[0,U];return[1,d,c]}function
ai(a){return 0===a[0]?1:0}function
aj(a){return 0===a[0]?a[1]:c(d[2],bG)}function
ak(c){return 0===c[0]?a(g[1],c[1],b)?1:0:0}function
s(a){return 0===a[0]?0:a[1]}function
L(b){if(0===b[0])return 0;var
c=b[2],f=b[1];function
g(c,b){var
e=L(c);return a(d[5],e,b)}return l(e[19][18],g,c,f)}function
al(b){var
c=0;function
f(c,b){var
e=L(c);return a(d[5],e,b)}return l(e[19][18],f,b,c)}function
n(c,b){if(0===c[0]){var
f=c[1];if(0===b[0])return a(g[1],f,b[1])}else{var
h=c[2],i=c[1];if(0!==b[0]){var
d=i===b[1]?1:0,j=b[2];return d?l(e[19][32],n,h,j):d}}return 0}function
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
f=a(e[19][15],M,d[2]);return l(e[19][17],g[12],b,f)}function
N(b,c){if(0===b[0])return[0,a(g[9],b[1],c)];var
d=b[2],f=b[1];function
h(a){return N(a,c)}return[1,f,a(e[19][15],h,d)]}function
Z(c){var
d=M(c);return a(g[1],d,b)?c:N(c,d)}function
_(b){if(0===b[0])return 0;var
d=b[1],f=c(e[19][11],b[2]),g=[0,[0,d,0],a(e[17][15],_,f)];return c(e[17][13],g)}function
$(d,g,c){if(1===c[0]){var
e=c[2],i=c[1];if(i===g){var
j=x(e.length-1+d|0,[0,b]),k=e.length-1-1|0,m=0;if(!(k<0)){var
a=m;for(;;){var
l=a+d|0,o=f(e,a)[a+1];f(j,l)[l+1]=o;var
p=a+1|0;if(k!==a){var
a=p;continue}break}}return[1,i,j]}}if(n(c,[0,b]))return[0,b];var
h=x(d+1|0,[0,b]);f(h,d)[d+1]=c;return[1,g,h]}function
o(d,c){if(0===d[0]){var
j=d[1];if(0===c[0])return[0,a(g[6],j,c[1])];var
k=c[2],m=c[1];if(a(g[1],j,b))return[0,b];var
n=function(a){return o(d,a)};return[1,m,a(e[19][15],n,k)]}var
h=d[2],f=d[1];if(0===c[0]){if(a(g[1],c[1],b))return[0,b];var
q=function(a){return o(a,c)};return[1,f,a(e[19][15],q,h)]}var
i=c[1],r=c[2];if(f<i){var
s=function(a){return o(d,a)};return[1,i,a(e[19][15],s,r)]}if(i<f){var
t=function(a){return o(a,c)};return[1,f,a(e[19][15],t,h)]}function
u(b,a){return $(b,f,o(a,c))}var
v=a(e[19][16],u,h);return l(e[19][17],p,[0,b],v)}function
am(k,c){if(0===c[0])return[0,b];var
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
m(b,a){return o(b,a)}function
P(b,a){return O(b,a)}function
A(b,a){return y(b,a)}function
q(b,a){return w(b,h(b,a),a)}function
an(b,a){return w(b,0,a)}function
B(b,a){var
c=h(b,a),d=y(W(b),c);return O(a,o(q(b,a),d))}function
Q(c){var
a=c;for(;;){var
b=s(a);if(0<b){var
a=q(b,a);continue}if(0===a[0])return a[1];throw[0,K,bH]}}function
ap(d){var
c=Z(d),e=Q(c);return a(g[3],b,e)?c:t(c)}var
aa=[0,1];function
ar(b){var
a=b;for(;;){if(0===a[0])return a[1];var
a=f(a[2],0)[1];continue}}function
ac(b){if(aa[1]){var
f=c(d[21],b);return a(d[16],bI,f)}if(3<b){var
g=c(aP[1],(b-4|0)+97|0);return a(e[15][1],1,g)}var
h=c(aP[1],b+119|0);return a(e[15][1],1,h)}var
i=[0,0];function
R(n){if(0<i[1]){if(0===n[0]){var
o=n[1];i[1]=i[1]-1|0;if(a(g[3],b,o))return c(g[15],o);var
u=c(g[15],o),v=a(d[16],u,bJ);return a(d[16],bK,v)}var
p=n[2],m=ac(n[1]),j=[0,bL],h=[0,bM],r=R(f(p,0)[1]);if(1-ab(r,bN))j[1]=r;var
q=[0,0],s=p.length-1-1|0;if(!(s<1)){var
l=s;for(;;){if(0<=i[1]){var
k=R(f(p,l)[l+1]);h[1]=bP;if(1===l){if(1-ab(k,bQ)){i[1]=i[1]-1|0;if(ab(k,bR))h[1]=m;else
if(a(e[15][22],k,43)){var
z=a(d[16],bT,m),A=a(d[16],k,z);h[1]=a(d[16],bU,A)}else{var
B=a(d[16],bV,m);h[1]=a(d[16],k,B)}}}else
if(1-ab(k,bW)){i[1]=i[1]-1|0;if(ab(k,bX)){var
C=c(d[21],l),D=a(d[16],bY,C);h[1]=a(d[16],m,D)}else
if(a(e[15][22],k,43)){var
E=c(d[21],l),F=a(d[16],bZ,E),G=a(d[16],m,F),H=a(d[16],b0,G),I=a(d[16],k,H);h[1]=a(d[16],b1,I)}else{var
J=c(d[21],l),K=a(d[16],b2,J),L=a(d[16],m,K),M=a(d[16],b3,L);h[1]=a(d[16],k,M)}}var
t=1-c(e[15][36],h[1]),w=t?1-q[1]:t;if(w){i[1]=i[1]-1|0;if(c(e[15][36],j[1]))j[1]=h[1];else{var
y=a(d[16],bS,h[1]);j[1]=a(d[16],j[1],y)}}}else{h[1]=b4;if(1-q[1]){var
N=a(d[16],b5,h[1]);j[1]=a(d[16],j[1],N)}q[1]=1}var
x=l-1|0;if(1!==l){var
l=x;continue}break}}if(c(e[15][36],j[1])){i[1]=i[1]-1|0;j[1]=bO}return j[1]}return b6}function
v(a){i[1]=20;return R(a)}function
as(b){var
c=v(b);return a(ao[aK],b7,c)}function
ad(f){var
b=[0,b8];function
g(c){var
e=v(c),f=a(d[16],e,b9);b[1]=a(d[16],b[1],f);return 0}a(e[19][13],g,f);var
h=a(d[16],b[1],b_);return c(j[3],h)}function
at(a){return ad(c(e[19][12],a))}function
F(j,i,e){if(0===e){if(0===j[0]){var
o=j[1];if(0===i[0]){var
s=i[1],x=a(g[10],o,s);return a(g[1],x,b)?[0,[0,a(g[9],o,s)],k]:c(d[2],b$)}}throw[0,K,ca]}var
t=h(e,i),y=q(e,i),f=[0,j],l=[0,k],u=[0,1],z=B(e,i);for(;;){if(u[1])if(!n(f[1],k)){var
v=h(e,f[1]);if(v<t)u[1]=0;else{var
A=q(e,f[1]),C=B(e,f[1]),D=r(A,y,e-1|0),w=m(D,J(e,v-t|0));l[1]=p(l[1],w);f[1]=P(C,m(w,z))}continue}return[0,l[1],f[1]]}}function
r(f,e,b){var
g=F(f,e,b),h=g[2],i=g[1];if(n(h,k))return i;var
j=c(d[21],b),l=a(d[16],j,cb),m=a(d[16],cc,l),o=a(d[16],cd,m),p=v(h),q=a(d[16],p,o),r=a(d[16],ce,q),s=a(d[16],cf,r),t=v(e),u=a(d[16],t,s),w=a(d[16],cg,u),x=a(d[16],ch,w),y=v(f),z=a(d[16],y,x),A=a(d[16],ci,z),B=a(d[16],cj,A);return c(d[2],B)}function
au(c,b){var
e=s(b),f=s(c);return r(c,b,a(d[5],f,e))}function
av(c,b){var
f=s(b),g=s(c),e=a(d[5],g,f);try{var
i=h(e,b),j=(1+h(e,c)|0)-i|0;r(o(c,y([0,Q(b)],j)),b,e);var
k=1;return k}catch(a){a=z(a);if(a[1]===aq)return 0;throw a}}function
aw(d,b,a){if(0===b[0])return[0,k,b,1,d];if(a===b[1]){var
e=[0,0],c=[0,d],f=q(a,b),j=B(a,b),g=[0,k],l=h(a,b);for(;;){var
n=h(a,b);if(n<=h(a,c[1])){var
o=h(a,c[1]),r=q(a,c[1]),s=B(a,c[1]),i=m(r,J(a,o-l|0)),t=m(i,j);c[1]=P(m(f,s),t);g[1]=p(m(f,g[1]),i);e[1]=e[1]+1|0;continue}return[0,c[1],f,e[1],g[1]]}}return[0,k,b,1,d]}function
E(f,c,a,b){if(1===a[0]){var
g=a[2];if(b===a[1]){var
h=function(c,a){return C(c,a,b-1|0)};return l(e[19][17],h,c,g)}}var
d=b-1|0;return f<50?T(f+1|0,c,a,d):aF(T,[0,c,a,d])}function
T(i,f,e,b){if(0===f[0]){var
p=f[1];if(0===e[0]){var
q=c(g[4],e[1]),s=c(g[4],p);return[0,a(g[12],s,q)]}}if(n(f,k))return e;if(n(e,k))return f;if(0===h(b,e))return i<50?E(i+1|0,e,f,b):aF(E,[0,e,f,b]);if(0===h(b,f))return i<50?E(i+1|0,f,e,b):aF(E,[0,f,e,b]);var
t=G(f,b),l=C(t,G(e,b),b-1|0),u=c(d[21],b);c(j[1],u);var
v=r(f,l,b),o=af(v,r(e,l,b),b);return m(l,r(o,G(o,b),b))}function
ay(a,b,c){return bb(E(0,a,b,c))}function
C(a,b,c){return bb(T(0,a,b,c))}function
ae(c,b,a){return C(c,b,a)}function
ax(c,b){var
e=s(b),f=s(c);return ae(c,b,a(d[5],f,e))}function
G(a,b){if(1===a[0]){var
c=a[2];if(a[1]===b){var
d=function(c,a){return C(c,a,b-1|0)};return l(e[19][17],d,k,c)}}return a}function
ag(u,s,p,o,l,b){var
d=u,a=s,c=p,i=o,g=l;for(;;){if(n(a,k))return d;var
j=h(b,a),e=q(b,a),f=g-j|0,v=t(a),w=F(m(A(t(e),f+1|0),d),v,b)[2],x=ah(e,c,f,b),d=a,a=r(w,m(i,A(c,f)),b),c=x,i=e,g=j;continue}}function
af(j,i,b){var
c=j,a=i;for(;;){if(n(a,k))return c;var
f=h(b,c),d=h(b,a);if(f<d){var
p=a,a=c,c=p;continue}var
g=f-d|0,e=q(b,a),l=t(a),o=F(m(A(t(e),g+1|0),c),l,b)[2];return ag(a,o,A(e,g),e,d,b)}}function
ah(c,g,f,e){var
a=[0,c],d=f-1|0,h=1;if(!(d<1)){var
b=h;for(;;){a[1]=r(m(a[1],c),g,e);var
i=b+1|0;if(d!==b){var
b=i;continue}break}}return a[1]}function
S(a){if(0===a[0])return c(g[13],a[1]);var
b=a[2],d=0;function
f(b,a){return a+S(b)|0}return l(e[19][18],f,b,d)}return[0,I,W,J,ai,ak,s,L,al,n,X,h,Y,u,w,p,M,N,Z,_,aj,$,o,am,t,O,y,m,P,A,q,an,B,Q,ap,ar,aa,ac,i,v,as,ad,at,F,r,au,av,aw,ax,ae,G,ay,C,af,ag,ah,S,c(D[19],[0,n,S])]}];R(203,ar,"Nsatz_plugin.Polynom");var
aQ=[aL,ck,bc(0)],aR=[0,0];function
cl(a){return a}function
cm(a){return a}function
A(a){return a.length-1-1|0}function
as(a){return f(a,0)[1]}function
aS(c,d){var
h=A(c);if(aR[1]){var
e=[0,0],a=[0,1];for(;;){if(0===e[1])if(a[1]<=h){var
i=a[1],n=f(d,i)[i+1],j=a[1];e[1]=aG(f(c,j)[j+1],n);a[1]=a[1]+1|0;continue}return e[1]}}var
o=f(d,0)[1],k=aG(f(c,0)[1],o);if(0===k){var
g=[0,0],b=[0,h];for(;;){if(0===g[1])if(1<=b[1]){var
l=b[1],p=f(d,l)[l+1],m=b[1];g[1]=-aG(f(c,m)[m+1],p)|0;b[1]=b[1]-1|0;continue}return g[1]}}return k}function
ah(c,e){var
b=A(c),d=x(b+1|0,0),g=0;if(!(b<0)){var
a=g;for(;;){var
h=f(e,a)[a+1],i=f(c,a)[a+1]-h|0;f(d,a)[a+1]=i;var
j=a+1|0;if(b!==a){var
a=j;continue}break}}return d}function
aT(c,g){var
b=[0,1],a=[0,0],h=A(c);for(;;){if(b[1])if(a[1]<=h){var
d=a[1],i=f(g,d)[d+1],e=a[1];b[1]=i<=f(c,e)[e+1]?1:0;a[1]=a[1]+1|0;continue}return b[1]}}function
at(a){var
c=A(a);f(a,0)[1]=0;var
d=1;if(!(c<1)){var
b=d;for(;;){var
e=f(a,0)[1];a[1]=f(a,b)[b+1]+e|0;var
g=b+1|0;if(c!==b){var
b=g;continue}break}}return a}function
ai(e,h){var
c=A(e),g=x(c+1|0,0),i=1;if(!(c<1)){var
b=i;for(;;){var
j=f(h,b)[b+1],k=f(e,b)[b+1],l=a(d[5],k,j);f(g,b)[b+1]=l;var
m=b+1|0;if(c!==b){var
b=m;continue}break}}return at(g)}function
aU(a){return at(x(a+1|0,0))}var
v=[0,[0,cl,cm],function(b){var
P=c(b[1],cn),t=c(b[1],co);function
Q(a){return a}function
R(d,c){var
f=c[2],g=d[2],e=a(b[9],d[1],c[1]),h=e?aH(g,f):e;return h}var
y=c(e[17][100],R),S=[0,y,function(d){function
e(a){return a[1]}var
f=a(g[17],e,d);function
h(a){return a[2]}var
i=a(g[17],h,d),j=c(D[21],i);function
k(d,a){return(d*17|0)+c(b[56],a)|0}return l(g[20],k,j,f)}],T=c(D[19],S);function
B(f,e){try{var
b=a(g[7],f,e);return b}catch(b){b=z(b);if(b[1]===aq){var
h=c(d[21],e),i=a(d[16],cr,h),j=function(c,b){var
e=a(d[16],ct,b);return a(d[16],c,e)},k=l(g[20],j,cs,f);return a(d[16],k,i)}throw b}}function
p(h,e){var
m=h[1];function
g(j,i){var
e=[0,0],k=j.length-1-1|0,l=1;if(!(k<1)){var
b=l;for(;;){var
t=f(j,b)[b+1],h=c(d[21],t);if(ac(h,cz))if(ac(h,cA)){var
o=a(d[16],cB,h),p=B(m,b-1|0),q=[0,a(d[16],p,o),0];e[1]=a(d[25],e[1],q)}else{var
s=[0,B(m,b-1|0),0];e[1]=a(d[25],e[1],s)}var
r=b+1|0;if(k!==b){var
b=r;continue}break}}var
g=e[1];if(g){if(i)return a(au[7],cu,g);var
n=a(au[7],cv,g);return a(d[16],cw,n)}return i?cx:cy}function
k(i,j){var
z=i?0:1;if(z)return j?cL:cM;var
A=0,B=i?i[2]:c(d[2],cO),C=k(B,A),D=a(d[16],cN,C),m=i?i[1]:c(d[2],cP),h=m[2],n=c(b[39],m[1]),o=a(d[16],n,cp),e=a(d[16],cq,o);if(ac(e,cC))if(ac(e,cD))if(ac(e,cE))if(45===q.caml_string_get(e,0))var
p=g(h,0),r=l(au[4],e,1,q.caml_ml_string_length(e)-1|0),s=a(d[16],r,p),f=a(d[16],cF,s);else
if(0===j)var
t=g(h,0),u=a(d[16],e,t),f=a(d[16],cG,u);else
var
v=g(h,0),f=a(d[16],e,v);else
if(0===j)var
w=g(h,1),f=a(d[16],cH,w);else
var
f=g(h,1);else
var
f=cI;else
var
x=g(h,1),y=a(d[16],cJ,x),f=a(d[16],cK,y);return a(d[16],f,D)}return k(e,1)}function
u(e,b){if(10<c(g[1],b))var
h=c(g[1],b),i=c(d[21],h),j=a(d[16],i,cQ),k=a(d[16],cR,j),l=p(e,[0,c(g[5],b),0]),f=a(d[16],l,k);else
var
f=p(e,b);return f}var
U=0;function
k(b,a){return[0,[0,a,aU(b)],0]}function
h(o,n){var
f=o,e=n,d=0;for(;;){if(f){if(e){var
i=e[2],j=e[1],k=f[2],h=f[1],l=aS(h[2],j[2]);if(0<l){var
f=k,d=[0,h,d];continue}if(0<=l){var
m=a(b[15],h[1],j[1]);if(a(b[9],m,P)){var
f=k,e=i;continue}var
f=k,e=i,d=[0,[0,m,h[2]],d];continue}var
e=i,d=[0,j,d];continue}return a(g[12],d,f)}return e?a(g[12],d,e):c(g[9],d)}}function
m(m,g,c){function
d(h){var
n=h[2],o=h[1],d=A(g),e=x(d+1|0,0),i=0;if(!(d<0)){var
c=i;for(;;){var
j=f(n,c)[c+1],k=f(g,c)[c+1]+j|0;f(e,c)[c+1]=k;var
l=c+1|0;if(d!==c){var
c=l;continue}break}}return[0,a(b[22],m,o),e]}return a(av[15],d,c)}function
n(a){return c(b[1],[0,a])}function
V(d,b){var
a=x(d+1|0,0);f(a,b)[b+1]=1;var
c=at(a);return[0,[0,n(1),c],0]}function
W(a){function
d(a){if(a){var
e=a[1],f=e[2],g=e[1],h=d(a[2]);return[0,[0,c(b[24],g),f],h]}return 0}return d(a)}function
o(f,c){function
d(c){if(c){var
e=c[1],g=e[2],h=e[1],i=d(c[2]);return[0,[0,a(b[22],f,h),g],i]}return 0}return d(c)}function
v(e,d){var
a=e,b=0;for(;;){if(a){var
c=a[1],f=a[2],a=f,b=h(m(c[1],c[2],d),b);continue}return b}}function
X(a,b){if(a){if(0===b)return[0,[0,t,aU(A(c(g[5],a)[2]))],0];var
d=function(b,a){if(1===a)return b;var
c=d(b,a/2|0),e=v(c,c);return 0===(a%2|0)?e:v(b,e)};return d(a,b)}return 0}function
C(d,c){return a(b[48],d,c)}function
i(a){return c(g[5],a[1])[2]}function
E(b,e){b[3]=b[3]+1|0;if(b[4].length-1<=b[3])b[4]=a(ap[5],b[4],x(b[3],aw));var
c=[0,e,b[3]],d=b[3];f(b[4],d)[d+1]=c;return c}function
Y(e,d){var
a=d;for(;;){if(a){var
b=a[1],f=a[2];if(0===aT(e,c(g[5],b[1])[2])){var
a=f;continue}return b}return aw}}function
Z(d,c){var
b=d[1];if(b)return a(D[6],b[1],c);throw aj}function
_(d,c,b){var
a=d[1];return a?l(D[5],a[1],c,b):0}function
F(d,c,e){try{var
a=Z(d,c);return a}catch(a){a=z(a);if(a===aj){var
b=Y(c,e)[1];return b?(_(d,c,b),b):b}throw a}}function
r(d,c){return a(b[45],d,c)}function
G(v,e,u){function
f(d){if(d){var
g=d[1],i=g[2],j=g[1],w=d[2],e=F(v,i,u);if(e){var
k=e[1],l=k[1],x=e[2],y=k[2],n=C(j,l),p=r(l,n),z=r(j,n),A=c(b[24],z),s=m(A,ah(i,y),x),q=f(h(o(p,w),s)),B=q[2];return[0,a(b[22],p,q[1]),B]}return[0,t,d]}return[0,t,0]}var
d=f(e);return[0,d[1],d[2]]}function
$(d,c,b){try{var
e=a(D[6],d[2],[0,c[2],b[2]]);return e}catch(a){a=z(a);if(a===aj)return 0;throw a}}function
H(d,c,b,a){return l(D[5],d[2],[0,c[2],b[2]],a)}function
I(o,j,e,i){function
f(a){if(a){var
g=a[1],i=g[2],p=a[2],q=g[1],d=F(o,i,e);if(d){var
j=d[1],s=d[2],t=j[2],u=r(q,j[1]),k=c(b[24],u),l=ah(i,t),n=f(h(p,m(k,l,s)));return[0,[0,[0,k,l,d],n[1]],n[2]]}return[0,0,a]}return cS}var
d=f(j),p=d[2],q=d[1];function
s(b,e){function
c(c,b){var
d=b[2],f=b[1];if(a(y,b[3],e[1])){var
g=n(1);return h(c,m(f,d,k(A(d),g)))}return c}return l(g[20],c,b,q)}return[0,l(g[23],s,i,e),p]}function
aa(s,q){var
d=s[1],e=q[1],a=c(g[5],d)[2],i=c(g[5],e)[2],j=c(g[5],d)[1],l=c(g[5],e)[1],t=c(g[6],d),u=c(g[6],e),o=C(j,l),p=ai(a,i),v=ah(p,a),w=ah(p,i);function
f(d,a){var
e=r(j,o),f=m(c(b[24],e),w,a);return h(m(r(l,o),v,d),f)}var
x=f(t,u),y=n(1),z=f(k(A(a),y),0),B=n(1);return[0,x,z,f(0,k(A(a),B))]}function
ab(c,b){return a(d[25],b,[0,c,0])}var
ad=[0,function(b,a){return aS(a[2],b[2])}],s=c(aV[2],ad),J=s[1],ae=s[2],af=s[3],ag=s[4],ak=s[6];function
K(j,d,b){function
e(r,k){var
u=k[1],l=c(g[5],j[1])[2],s=c(g[5],u)[2],d=[0,1],b=[0,1],t=A(l);for(;;){if(d[1])if(b[1]<=t){var
m=b[1],n=0===f(l,m)[m+1]?1:0;if(n)var
o=n;else
var
p=b[1],o=0===f(s,p)[p+1]?1:0;d[1]=o;b[1]=b[1]+1|0;continue}if(d[1])return r;var
v=i(k),w=ai(i(j),v),e=k[2],h=j[2],x=q.caml_lessthan(h,e)?[0,h,e]:[0,e,h];return a(ae,[0,x,w],r)}}return l(g[20],e,b,d)}function
al(e,d){var
a=e,b=d;for(;;){if(a){var
c=a[2],f=a[1];if(c){var
a=c,b=K(f,c,b);continue}}return b}}function
am(h,b,k,m){var
e=b[2],j=b[1],c=j[2],d=j[1];function
l(a){var
j=a[2]!==d?1:0;if(j){var
k=a[2]!==c?1:0;if(k){var
l=aT(e,i(a));if(l){var
m=a[2]<c?1:0;if(m)var
g=m;else
var
p=i(a),g=1-aH(e,ai(i(f(h[4],d)[d+1]),p));if(g){var
n=a[2]<d?1:0;if(n)var
b=n;else
var
o=i(a),b=1-aH(e,ai(i(f(h[4],c)[c+1]),o))}else
var
b=g}else
var
b=l}else
var
b=k}else
var
b=j;return b}return a(g[28],l,k)}function
an(e,d){function
a(h){var
a=0,b=l(ak,function(b,a){return a+1|0},d,a),f=c(g[1],e);return l(M[4],cT,f,b)}return c(j[5],a)}var
w=[aL,cU,bc(0)];function
L(t,m,f,s,e,C){var
x=G(m,t[1],e),q=x[2],D=x[1];function
E(c){var
b=u(f,q);return a(d[16],cV,b)}c(j[5],E);var
y=[0,q,a(b[22],t[2],D)];if(q)throw[0,w,y];c(j[6],cW);function
F(a){return 0}var
H=a(g[17],F,e),i=y[2],z=I(m,o(i,s),e,H),A=z[1],J=z[2];c(j[6],cX);function
K(c){var
b=p(f,J);return a(d[16],cY,b)}c(j[5],K);function
L(k){function
b(c,b,a){return h(c,v(b,a[1]))}var
c=o(i,s),j=p(f,aI(g[25],b,c,A,e));return a(d[16],cZ,j)}c(j[5],L);function
M(c){var
b=p(f,k(1,i));return a(d[16],c0,b)}c(j[5],M);var
r=0,l=c(g[9],e);for(;;){if(l){var
B=l[2],N=l[1],O=function(b){return function(a){return $(m,b,a)}}(N),r=[0,a(g[17],O,B),r],l=B;continue}var
P=a(av[bq],C,r),Q=function(a){return o(n(-1),a)};return[0,i,1,P,a(g[19],Q,A)]}}function
N(a){return a?as(a[1][2]):-1}function
O(e,r,k,Q,b,x){var
h=b[1],m=b[2];c(j[6],c1);var
i=e[1];if(i)c(D[2],i[1]);var
y=c(g[1],h);return function(S,R){var
k=S,h=R;for(;;){var
s=h[2],b=h[1];an(b,s);try{var
O=c(ag,s),P=[0,[0,c(af,s),O]],t=P}catch(a){a=z(a);if(a!==aV[1])throw a;var
t=0}if(t){var
A=t[1],i=A[2],B=A[1],C=B[1],m=C[2],n=C[1];if(am(e,[0,[0,n,m],B[2]],b,i)){c(j[6],c2);var
h=[0,b,i];continue}var
T=f(e[4],m)[m+1],v=aa(f(e[4],n)[n+1],T),p=v[1],U=v[3],V=v[2];if(Q)if(0!==p){var
ad=N(k[1]);if(ad<N(p)){c(j[6],c5);var
h=[0,b,i];continue}}var
D=G(e,p,b),F=D[1];if(D[2]){var
W=function(c,d,e,f,g){return function(a){var
b=a[2]===d?f:a[2]===c?e:0;return o(g,b)}}(m,n,U,V,F),X=a(g[17],W,b),J=I(e,o(F,p),b,X),Y=J[1],q=E(e,J[2]),Z=function(c){return function(b,a){return H(e,c,a,b)}}(q);l(g[22],Z,Y,b);var
_=function(c){return function(e){var
b=u(r,c[1]);return a(d[16],c3,b)}}(q);c(j[5],_);var
M=ab(q,b);try{var
ac=L(k,e,r,x,M,y);return ac}catch(a){a=z(a);if(a[1]===w){var
$=a[2],k=$,h=[0,M,K(q,b,i)];continue}throw a}}c(j[6],c4);var
h=[0,b,i];continue}return L(k,e,r,x,b,y)}}(k,[0,h,m])}function
ao(b){if(b){var
c=b[2],d=as(b[1][2]),e=function(a){return as(a[2])===d?1:0};return a(g[27],e,c)}return 1}return[0,Q,k,U,V,y,h,W,v,X,function(e,p,i,b){var
f=[0,0,a(D[1],0,51),0,x(1e3,aw)],m=a(g[27],ao,[0,b,i]);if(m)c(j[6],c6);function
q(f){var
c=u(e,b);return a(d[16],c7,c)}c(j[5],q);function
r(f){function
b(c,b){var
f=u(e,b),g=a(d[16],f,c9);return a(d[16],c,g)}var
c=l(g[20],b,c8,i);return a(d[16],c_,c)}c(j[5],r);function
s(a){return E(f,a)}var
h=a(g[17],s,i);function
v(a){return H(f,a,a,k(p,n(1)))}a(g[15],v,h);var
y=[0,b,t];try{var
C=O(f,e,y,m,[0,h,J],b),o=C}catch(a){a=z(a);if(a[1]!==w)throw a;var
A=a[2];try{var
B=O(f,e,A,m,[0,h,al(h,J)],b)}catch(a){a=z(a);if(a[1]===w)throw aQ;throw a}var
o=B}c(j[6],c$);return o},T]},aQ,aR];R(209,v,"Nsatz_plugin.Ideal");var
da=c(y[36],0),aW=m[52],aX=y[24],aY=y[27],aZ=y[16],db=m[51],dc=y[25],dd=y[4],de=y[5],df=y[10],dg=y[8],dh=y[3],di=y[15],dj=y[33];function
dk(a){try{var
b=c(y[38],a);return b}catch(a){a=z(a);if(a[1]===aq)return 1;throw a}}var
dl=y[19];function
dm(e,d){var
c=e,b=d;for(;;){if(a(aX,b,da))return c;if(a(aY,c,b)){var
g=b,b=c,c=g;continue}var
f=a(aZ,c,b),c=b,b=f;continue}}function
a0(b){return a(m[32],b,C)?0:[0,b]}function
a1(a){var
b=a[2],c=a[1];return 1===b?c:[6,c,b]}function
ay(a){var
b=a[1];if(typeof
b==="number")return a[2];var
c=a[2];return typeof
c==="number"?b:[3,b,c]}function
a2(c){var
b=c[1];if(typeof
b==="number")return 0;var
d=c[2];if(typeof
d==="number")return 0;else
if(0===d[0])if(a(m[32],d[1],N))return b;if(typeof
b!=="number"&&0===b[0]){var
e=c[2];if(a(m[32],b[1],N))return e}return[5,b,c[2]]}function
t(d,b,a){var
e=l(dn[2],d,b,a);return c(dp[50],e)}var
az=[i,function(a){return t(ds,dr,dq)}],O=[i,function(a){return t(dv,du,dt)}],V=[i,function(a){return t(dy,dx,dw)}],W=[i,function(a){return t(dB,dA,dz)}],X=[i,function(a){return t(dE,dD,dC)}],Y=[i,function(a){return t(dH,dG,dF)}],Z=[i,function(a){return t(dK,dJ,dI)}],_=[i,function(a){return t(dN,dM,dL)}],a3=[i,function(a){return t(dP,aA,dO)}],a4=[i,function(a){return t(dR,aA,dQ)}],a5=[i,function(a){return t(dT,aA,dS)}],h=[i,function(a){return t(dV,E,dU)}],ak=[i,function(a){return t(dX,E,dW)}],$=[i,function(a){return t(dZ,E,dY)}],d2=[i,function(a){return t(d1,E,d0)}],d5=[i,function(a){return t(d4,E,d3)}],aa=[i,function(a){return t(d7,E,d6)}],al=[i,function(a){return t(d9,E,d8)}],am=[i,function(a){return t(d$,E,d_)}],ec=[i,function(a){return t(eb,E,ea)}];function
r(a,d){var
b=o(a),f=c(e[19][12],d),g=p===b?a[1]:i===b?c(n[2],a):a;return c(w[13],[0,g,f])}function
aB(f){var
a=o(h),b=0,d=0,e=p===a?h[1]:i===a?c(n[2],h):h;return r(a3,[0,r(az,[0,e,d]),b])}function
P(b){if(a(m[26],b,N)){var
d=o(al);return p===d?al[1]:i===d?c(n[2],al):al}var
e=a(m[12],b,U);return a(m[26],e,C)?r(aa,[0,P(a(m[11],b,U)),0]):r(d5,[0,P(a(m[11],b,U)),0])}function
a6(b){if(a(m[26],b,C)){var
d=o(ak);return p===d?ak[1]:i===d?c(n[2],ak):ak}return a(m[28],b,C)?r($,[0,P(b),0]):r(d2,[0,P(a(m[4],ed,b)),0])}function
F(A){var
b=A;for(;;)if(typeof
b==="number"){var
b=[0,C];continue}else
switch(b[0]){case
0:var
x=c(m[54],b[1]),d=c(ax[5],x),y=c(ax[3],d);c(m[51],y);var
z=c(ax[2],d),f=o(h),B=[0,a6(c(m[51],z)),0],D=p===f?h[1]:i===f?c(n[2],h):h;return r(O,[0,D,B]);case
1:var
g=o(h),E=[0,P(c(m[43],b[1])),0],G=p===g?h[1]:i===g?c(n[2],h):h;return r(V,[0,G,E]);case
2:var
j=o(h),H=[0,F(b[1]),0],I=p===j?h[1]:i===j?c(n[2],h):h;return r(Z,[0,I,H]);case
3:var
J=b[1],K=[0,F(b[2]),0],k=o(h),L=[0,F(J),K],M=p===k?h[1]:i===k?c(n[2],h):h;return r(W,[0,M,L]);case
4:var
Q=b[1],R=[0,F(b[2]),0],l=o(h),S=[0,F(Q),R],T=p===l?h[1]:i===l?c(n[2],h):h;return r(X,[0,T,S]);case
5:var
U=b[1],$=[0,F(b[2]),0],q=o(h),aa=[0,F(U),$],ab=p===q?h[1]:i===q?c(n[2],h):h;return r(Y,[0,ab,aa]);default:var
s=b[2],ac=b[1];if(0===s){var
t=o(h),ad=[0,a6(N),0],ae=p===t?h[1]:i===t?c(n[2],h):h;return r(O,[0,ae,ad])}var
u=c(m[47],s),af=0;if(a(m[32],u,C))var
e=o(am),v=p===e?am[1]:i===e?c(n[2],am):am;else
var
v=r(ec,[0,P(u),0]);var
w=o(h),ag=[0,F(ac),[0,v,af]],ah=p===w?h[1]:i===w?c(n[2],h):h;return r(_,[0,ah,ag])}}function
Q(g){var
b=c(w[26],g);if(9===b[0]){var
d=b[2];if(1===d.length-1){var
e=d[1],f=o(aa),h=b[1],j=p===f?aa[1]:i===f?c(n[2],aa):aa;if(a(w[74],h,j)){var
k=Q(e);return a(m[6],U,k)}var
l=Q(e),q=a(m[6],U,l);return a(m[1],N,q)}}return N}function
G(L){var
j=c(w[26],L);if(9===j[0]){var
d=j[2],x=d.length-1,b=j[1];if(2===x){var
k=d[2],y=o(V),M=p===y?V[1]:i===y?c(n[2],V):V;if(a(w[74],b,M)){var
N=Q(k);return[1,c(m[40],N)]}var
z=o(O),P=p===z?O[1]:i===z?c(n[2],O):O;if(a(w[74],b,P)){var
h=c(w[26],k);if(9===h[0]){var
r=h[2];if(1===r.length-1){var
s=r[1],t=o($),I=h[1],J=p===t?$[1]:i===t?c(n[2],$):$;if(a(w[74],I,J))var
l=Q(s),g=1;else
var
K=Q(s),l=a(m[4],C,K),g=1}else
var
g=0}else
var
g=0;if(!g)var
l=C;return[0,l]}var
A=o(Z),R=p===A?Z[1]:i===A?c(n[2],Z):Z;return a(w[74],b,R)?[2,G(k)]:0}if(3===x){var
e=d[2],f=d[3],B=o(W),S=p===B?W[1]:i===B?c(n[2],W):W;if(a(w[74],b,S)){var
T=G(f);return[3,G(e),T]}var
D=o(X),U=p===D?X[1]:i===D?c(n[2],X):X;if(a(w[74],b,U)){var
aa=G(f);return[4,G(e),aa]}var
E=o(Y),ab=p===E?Y[1]:i===E?c(n[2],Y):Y;if(a(w[74],b,ab)){var
ac=G(f);return[5,G(e),ac]}var
F=o(_),ad=p===F?_[1]:i===F?c(n[2],_):_;if(a(w[74],b,ad)){var
u=c(w[26],f);if(9===u[0]){var
v=u[2];if(1===v.length-1)var
H=Q(v[1]),q=1;else
var
q=0}else
var
q=0;if(!q)var
H=C;var
ae=c(m[45],H);return[6,G(e),ae]}return 0}}return 0}function
a7(e){var
b=c(w[26],e);if(9===b[0]){var
a=b[2],d=a.length-1-1|0;if(!(2<d>>>0))switch(d){case
0:return 0;case
1:break;default:var
f=a[2],g=a7(a[3]);return[0,G(f),g]}}throw[0,K,ee]}function
ef(c,b){function
e(g,f){var
b=g,c=f;for(;;)if(typeof
b==="number")return c;else
switch(b[0]){case
0:return c;case
1:var
h=bd(b[1]);return a(d[5],c,h);case
2:var
b=b[1];continue;case
3:var
i=b[2],j=e(b[1],c),b=i,c=j;continue;case
4:var
k=b[2],l=e(b[1],c),b=k,c=l;continue;case
5:var
m=b[2],n=e(b[1],c),b=m,c=n;continue;default:var
b=b[1];continue}}return e(b,c)}var
a8=c(ar[1],[0,aX,aY,dc,dd,de,df,dg,dh,di,aZ,dl,dm,dk,aW,dj]),s=c(v[2],a8);function
aC(h,a){var
b=c(s[1],a);function
g(b){if(b){var
k=b[2],n=b[1],r=n[1],o=c(v[1][1],n[2]).length-1-1|0,s=function(h,l){var
g=h[2],d=h[1],e=c(v[1][1],l[2]),a=[0,0],m=1;if(!(o<1)){var
b=m;for(;;){if(0<f(e,b)[b+1])a[1]=b;var
q=b+1|0;if(o!==b){var
b=q;continue}break}}if(0===a[1])return[0,d,g];if(d<a[1]){var
i=a[1],n=f(e,i)[i+1];return[0,a[1],n]}if(a[1]===d){var
j=a[1];if(f(e,j)[j+1]<g){var
k=a[1],p=f(e,k)[k+1];return[0,a[1],p]}}return[0,d,g]},p=l(e[17][18],s,eg,b),h=p[2],a=p[1];if(0===a){var
i=function(a){if(0===a[0])return a0(c(db,a[1]));var
b=a[2],f=a[1];function
g(e,b,a){var
g=a1([0,[1,c(d[21],f)],e]);return ay([0,a,a2([0,i(b),g])])}return l(e[19][42],g,b,0)},j=i(r);return c(e[17][53],k)?j:ay([0,j,g(k)])}var
t=function(i,g){var
b=g[2],j=g[1],k=i[2],l=i[1];if(h<=f(c(v[1][1],b),a)[a+1]){var
m=c(v[1][1],b),d=c(e[19][8],m);d[a+1]=f(d,a)[a+1]-h|0;return[0,[0,[0,j,c(v[1][2],d)],l],k]}return[0,l,[0,[0,j,b],k]]},q=l(e[17][18],t,eh,b),u=q[2],w=q[1],x=1===h?[1,c(d[21],a)]:a1([0,[1,c(d[21],a)],h]),y=g(c(e[17][9],u));return ay([0,a2([0,x,g(c(e[17][9],w))]),y])}return a0(c(m[43],ei))}return g(b)}function
a_(b,a){function
d(b,a){if(b){if(0===b[1]){var
c=b[2];if(a){var
e=a[1];return[0,e,d(c,a[2])]}throw[0,K,en]}var
f=d(b[2],a);return[0,s[3],f]}return a}var
f=d(b,c(e[17][9],a));return c(e[17][9],f)}function
eF(aw){var
u=a7(aw),g=l(e[17][18],ef,0,u);if(u){var
w=u[1];if(typeof
w==="number")var
d=0;else
if(0===w[0]){var
L=w[1];if(0===L[0]){var
y=u[2];if(y){var
A=y[1];if(typeof
A==="number")var
E=1;else
if(0===A[0]){var
N=A[1],O=L[1];if(0===N[0]){var
P=N[1],Y=y[2];if(7<O>>>0){var
Z=c(ag[3],ep);l(T[6],0,0,Z)}else
switch(O){case
0:c(j[6],ex);v[4][1]=0;break;case
1:c(j[6],ey);v[4][1]=0;break;case
2:c(j[6],ez);v[4][1]=1;break;case
3:c(j[6],eA);v[4][1]=1;break;case
4:c(j[6],eB);v[4][1]=0;break;case
5:c(j[6],eC);v[4][1]=0;break;case
6:c(j[6],eD);v[4][1]=1;break;default:c(j[6],eE);v[4][1]=1}var
_=function(b){return a(M[4],eq,b+1|0)},$=a(e[17][54],g,_),aa=[0,a(e[18],er,$)],ab=function(b){function
d(b){if(typeof
b==="number")var
e=s[3];else
switch(b[0]){case
0:var
h=b[1];if(a(m[32],h,C))var
i=s[3];else
var
k=[0,c(aW,h)],i=a(s[2],g,k);var
e=i;break;case
1:var
f=bd(b[1]);if(f<=P)var
l=c(a8[2],f),j=a(s[2],g,l);else
var
j=a(s[4],g,f);var
e=j;break;case
2:var
n=d(b[1]),e=c(s[7],n);break;case
3:var
o=b[1],p=d(b[2]),q=d(o),e=a(s[6],q,p);break;case
4:var
r=b[1],t=d(b[2]),u=c(s[7],t),v=d(r),e=a(s[6],v,u);break;case
5:var
w=b[1],x=d(b[2]),y=d(w),e=a(s[8],y,x);break;default:var
z=b[2],A=d(b[1]),e=a(s[9],A,z)}return e}return d(b)},B=a(e[17][15],ab,Y);if(B){var
ac=B[1],ad=c(e[17][9],B[2]),H=c(s[11][1],12),X=function(b){try{var
c=a(s[11][7],H,b);return c}catch(a){a=z(a);if(a===aj){l(s[11][5],H,b,1);return 0}throw a}},I=function(b){if(b){var
c=b[1],d=I(b[2]),e=d[2],f=d[1];if(!a(s[5],c,s[3]))if(!X(c))return[0,[0,c,f],[0,0,e]];return[0,f,[0,1,e]]}return em},J=I(ad),Q=J[2],ae=J[1],V=c(a9[90],0),t=aI(s[10],aa,g,ae,ac),W=function(d){var
b=c(a9[90],0)-V;return a(M[4],el,b)};c(j[5],W);c(j[6],es);var
af=c(e[17][9],t[3]),D=[0,t[4],af],k=c(e[17][1],D),q=x(k,0),G=function(b){if(!(k<=b))if(!f(q,b)[b+1]){f(q,b)[b+1]=1;var
c=a(e[17][7],D,b),d=function(e,d){var
c=1-a(s[5],d,s[3]);return c?G((e+b|0)+1|0):c};return a(e[17][87],d,c)}return 0};G(0);var
R=function(b,c){function
d(a,d){if(k<=((b+a|0)+1|0))return 1;var
c=(b+a|0)+1|0;return f(q,c)[c+1]}return f(q,b)[b+1]?[0,a(e[17][79],d,c)]:0},b=a(av[71],R,D),S=function(f){var
d=k-c(e[17][1],b)|0;return a(M[4],ej,d)};c(j[5],S);var
U=function(f){var
d=c(e[17][1],b);return a(M[4],ek,d)};c(j[5],U);if(b){var
ah=b[2],ai=a_(Q,b[1]),ak=aC(g,a(s[2],g,t[1])),al=[6,0,t[2]],am=c(e[17][9],ah),an=function(a){return a_(Q,a)},ao=a(e[17][15],an,am),ap=function(a){return aC(g,a)},aq=c(e[17][15],ap),ar=a(e[17][15],aq,ao),as=function(a){return aC(g,a)},at=a(e[17][15],as,ai),au=function(b){return a(M[4],et,P)};c(j[5],au);c(j[6],eu);var
ax=a(e[18],[0,[0,ak,[0,al,at]],0],ar),ay=function(b){return a(e[17][15],F,b)},aA=a(e[17][15],ay,ax),aD=r(a4,[0,aB(0),0]),aE=function(d,b){var
a=o(h),f=0,g=0,j=p===a?h[1]:i===a?c(n[2],h):h,k=r(a4,[0,r(az,[0,j,g]),f]);function
m(d,b){var
a=o(h),e=[0,d,[0,b,0]],f=0,g=p===a?h[1]:i===a?c(n[2],h):h;return r(a5,[0,r(az,[0,g,f]),e])}var
q=[0,l(e[17][19],m,d,k),[0,b,0]];return r(a5,[0,aB(0),q])},aF=l(e[17][19],aE,aA,aD);c(j[6],eG);return aF}throw[0,K,ev]}throw[0,K,ew]}var
d=1,E=0}else
var
E=1;if(E)var
d=1}else
var
d=1}else
var
d=1}else
var
d=0}throw[0,K,eo]}function
eH(a){var
b=[0,r(a3,[0,aB(0),0]),a],d=[0,t(eK,eJ,eI),b],e=c(w[13],d),f=[0,c(a$[8],e),0];return c(eL[146],f)}var
aD=[0,function(a){try{var
e=eF(a),b=e}catch(a){a=z(a);if(a!==v[3])throw a;var
d=c(ag[3],eM),b=l(T[6],0,0,d)}return eH(b)}];R(220,aD,"Nsatz_plugin.Nsatz");c(eN[10],aE);var
eO=0;function
eP(a,d){var
b=c(a$[127][1],a);return c(aD[1],b)}var
eS=c(eR[1][7],eQ),eV=[0,[5,c(eU[16],eT[13])],eS],eY=[0,[0,[0,eX,[1,a(eW[11],0,eV),0]],eP],eO];aI(e0[10][8],aE,eZ,0,eY);var
ba=[0,aE];R(227,ba,"Nsatz_plugin.G_nsatz");R(228,[0,j,ar,v,aD,ba],"Nsatz_plugin");return}
