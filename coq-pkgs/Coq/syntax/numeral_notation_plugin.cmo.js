function(bz){"use strict";var
R=".",T="(",Q="after",t=")",P=" (require BinNums first).",S="Instead of Decimal.int, the types Decimal.uint or Z could be used",B="numnotoption",j=bz.jsoo_runtime,b=j.caml_new_string,A=j.caml_register_global,z=j.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):j.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):j.caml_call_gen(a,[b,c])}function
n(a,b,c,d){return a.length==3?a(b,c,d):j.caml_call_gen(a,[b,c,d])}function
s(a,b,c,d,e){return a.length==4?a(b,c,d,e):j.caml_call_gen(a,[b,c,d,e])}function
by(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):j.caml_call_gen(a,[b,c,d,e,f,g])}var
e=j.caml_get_global_data(),L=b("numeral_notation_plugin"),l=e.Constrexpr_ops,w=e.Smartlocate,o=e.Nametab,c=e.Pp,g=e.Libnames,J=e.CErrors,v=e.Not_found,C=e.Global,D=e.Util,h=e.Genarg,q=e.Geninterp,M=e.Genintern,m=e.Pcoq,k=e.CLexer,N=e.Ltac_plugin,r=e.Stdarg,ay=[0,0],at=b(P),ax=b(R),au=b(S),av=b(" to Decimal.int or (option Decimal.int)."),aw=b(" should go from "),an=b(P),as=b(R),ao=b(S),ap=b(")."),aq=b(" or (option "),ar=b(" should go from Decimal.int to "),V=b("option type."),W=b(") targets an "),Y=b("the parsing function ("),Z=b("The 'abstract after' directive has no effect when "),_=b("numbers"),$=b("abstract-large-number-no-op"),ac=b("Coq.Numbers.BinNums.Z"),ad=b("Coq.Numbers.BinNums.positive"),af=b("Coq.Init.Decimal.int"),ag=b("Coq.Init.Decimal.uint"),ah=b("Coq.Init.Datatypes.option"),aC=b(t),aD=b("(warning after "),aE=b(t),aF=b("(abstract after "),aG=b(B),aM=b(B),aR=b(t),aU=b(Q),aW=b("warning"),aY=b(T),a1=b(t),a4=b(Q),a6=b("abstract"),a8=b(T),ba=b(B),bg=[0,b("o")],bi=[0,b("sc")],bj=b(":"),bl=[0,b("g")],bn=[0,b("f")],bp=[0,b("ty")],bq=b("Notation"),br=b("Numeral"),bw=b("NumeralNotation"),az=e.CAst,aA=e.Notation,ak=e.Pfedit,am=e.Constrintern,al=e.Pretype_errors,X=e.Termops,aa=e.CWarnings,bu=e.Vernac_classifier,be=e.Names,bf=e.Locality,aK=e.Ftactic,aB=e.Mltop,bx=e.Vernacentries;function
U(b){var
e=a(c[22],V),f=a(c[22],W),g=a(C[2],0),h=a(X[82],g),i=d(o[42],h,b),j=a(c[22],Y),k=a(c[22],Z),l=d(c[12],k,j),m=d(c[12],l,i),n=d(c[12],m,f);return d(c[12],n,e)}var
ab=s(aa[1],$,_,0,U),E=d(g[29],0,ac),ae=d(g[29],0,ad),F=d(g[29],0,af),G=d(g[29],0,ag),ai=d(g[29],0,ah);function
u(c){var
b=a(o[9],c);if(2===b[0])return b[1];throw v}function
H(c){try{var
b=u(c);return b}catch(b){b=z(b);if(b===v)return a(o[2],c);throw b}}function
aj(c){try{var
a=u(ae),b=[0,[0,u(E),a]];return b}catch(a){a=z(a);if(a===v)return 0;throw a}}function
f(e,c){var
b=d(ak[6],0,0),f=b[2],g=b[1],h=[0,a(l[10],e),[0,c]],i=a(l[12],h);try{s(am[10],f,g,0,i);var
j=1;return j}catch(a){a=z(a);if(a[1]===al[1])return 0;throw a}}function
I(f,b,e){var
h=e?a(c[3],an):a(c[3],as),i=a(c[3],ao),j=a(c[5],0),k=a(c[3],ap),l=a(g[27],b),m=a(c[3],aq),o=a(g[27],b),p=a(c[3],ar),q=a(g[27],f),r=d(c[12],q,p),s=d(c[12],r,o),t=d(c[12],s,m),u=d(c[12],t,l),v=d(c[12],u,k),w=d(c[12],v,j),x=d(c[12],w,i),y=d(c[12],x,h);return n(J[6],0,0,y)}function
K(f,e,b){var
h=b?a(c[3],at):a(c[3],ax),i=a(c[3],au),j=a(c[5],0),k=a(c[3],av),l=a(g[27],e),m=a(c[3],aw),o=a(g[27],f),p=d(c[12],o,m),q=d(c[12],p,l),r=d(c[12],q,k),s=d(c[12],r,j),t=d(c[12],s,i),u=d(c[12],t,h);return n(J[6],0,0,u)}var
x=[0,function(N,i,g,e,M,t){var
L=H(F),h=[0,H(G),L],n=aj(0),u=a(w[4],i),O=d(w[3],0,g),P=d(w[3],0,e),b=a(l[10],i);function
p(b){return a(l[10],b)}function
c(c,b){var
e=[0,[0,d(az[1],0,0),0],ay,c,b];return a(l[15],e)}var
q=p(E),r=p(F),s=p(G),Q=p(ai);function
k(b){return a(l[11],[0,Q,[0,b,0]])}var
z=a(C[28],u)[2][4];function
A(a,b){return[3,[0,u,a+1|0]]}var
B=d(D[19][16],A,z),J=a(D[19][11],B);if(f(g,c(r,b)))var
j=[0,[0,h],1];else
if(f(g,c(r,k(b))))var
j=[0,[0,h],0];else
if(f(g,c(s,b)))var
j=[0,[1,h[1]],1];else
if(f(g,c(s,k(b))))var
j=[0,[1,h[1]],0];else
if(n)var
y=n[1],T=f(g,c(q,b))?[0,[2,y],1]:f(g,c(q,k(b)))?[0,[2,y],0]:I(g,i,0),j=T;else
var
j=I(g,i,1);if(f(e,c(b,r)))var
m=[0,[0,h],1];else
if(f(e,c(b,k(r))))var
m=[0,[0,h],0];else
if(f(e,c(b,s)))var
m=[0,[1,h[1]],1];else
if(f(e,c(b,k(s))))var
m=[0,[1,h[1]],0];else
if(n)var
x=n[1],S=f(e,c(b,q))?[0,[2,x],1]:f(e,c(b,k(q)))?[0,[2,x],0]:K(e,i,0),m=S;else
var
m=K(e,i,1);var
v=[0,j,O,m,P,i,t],U=typeof
t==="number"?0:1===t[0]?0===j[2]?(d(ab,0,v[2]),1):0:0,R=[0,N,M,[1,v],[0,a(o[36],[2,u]),0],J,1];return a(aA[22],R)}];A(64,x,"Numeral_notation_plugin.Numeral");a(aB[10],L);function
p(q,p,o,b){if(typeof
b==="number")return a(c[7],0);else{if(0===b[0]){var
e=b[1],f=a(c[3],aC),g=a(c[3],e),h=a(c[3],aD),i=d(c[12],h,g);return d(c[12],i,f)}var
j=b[1],k=a(c[3],aE),l=a(c[3],j),m=a(c[3],aF),n=d(c[12],m,l);return d(c[12],n,k)}}var
i=a(h[2],aG);function
aH(b,a){return[0,b,a]}d(M[9],i,aH);function
aI(b,a){return a}d(M[10],i,aI);function
aJ(g,b){var
c=a(h[6],i),e=a(q[3],c),f=d(q[1][8],e,b);return a(aK[1],f)}d(q[7],i,aJ);d(q[4],i,0);var
aL=a(h[4],i),y=n(m[16],m[13],aM,aL),aN=0,aO=0,aP=[0,[0,0,function(a){return 0}],aO];function
aQ(f,a,e,d,c,b){return[0,a]}var
aS=[0,a(k[10],aR)],aT=[6,m[17][11]],aV=[0,a(k[10],aU)],aX=[0,a(k[10],aW)],aZ=[0,[0,[0,[0,[0,[0,[0,0,[0,a(k[10],aY)]],aX],aV],aT],aS],aQ],aP];function
a0(f,a,e,d,c,b){return[1,a]}var
a2=[0,a(k[10],a1)],a3=[6,m[17][11]],a5=[0,a(k[10],a4)],a7=[0,a(k[10],a6)],a9=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,0,[0,a(k[10],a8)]],a7],a5],a3],a2],a0],aZ]],aN]];n(m[21],y,0,a9);s(N[5][1],i,p,p,p);var
a_=[0,y,0];function
a$(b){var
c=b[2],e=a(h[4],i);return[0,d(h[7],e,c)]}n(N[10][5],ba,a$,a_);var
bb=0,bc=0;function
bd(h,g,f,e,d,c,b){var
i=a(be[1][8],e),j=a(bf[7],c[2]);by(x[1],j,h,g,f,i,d);return b}var
bh=[1,bg,[5,a(h[16],i)],0],bk=[0,bj,[1,bi,[5,a(h[16],r[7])],bh]],bm=[1,bl,[5,a(h[16],r[18])],bk],bo=[1,bn,[5,a(h[16],r[18])],bm],bs=[0,[0,0,[0,br,[0,bq,[1,bp,[5,a(h[16],r[18])],bo]]],bd,bc],bb],bt=0,bv=[0,function(a){return bu[4]}];s(bx[10],bw,bv,bt,bs);var
O=[0,L,p,i,y];A(78,O,"Numeral_notation_plugin.G_numeral");A(79,[0,x,O],"Numeral_notation_plugin");return}
