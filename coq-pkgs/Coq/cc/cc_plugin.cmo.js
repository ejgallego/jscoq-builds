(function(fv){"use strict";var
b0=125,a9=123,aF=".",b5="cc",aL=108,ca="$l",bZ="  [",bV=3901498,bW=106,bX=" : ",b$=-431191102,b4="$n",a5=-912009552,Y=250,K=105,ad="Init",ak=124,P=246,aI="congruence",b3="[",b_=119,aH="plugins/cc/g_congruence.ml4",bU=111,aG="Extension: cannot occur",bT="A",a8=117,b9="with",aK="]",bR=915186972,bS=116,bY=134,b2=888453194,b8=135,ac="Logic",b1=" and ",a6=109,bQ=-318868643,b7=126,bP=147,a7=121,aJ=107,b6="Heq",aE="f_equal",aD=15500,a4=1e3,w=fv.jsoo_runtime,r=w.caml_check_bound,ar=w.caml_int_compare,aC=w.caml_make_vect,d=w.caml_new_string,X=w.caml_obj_tag,as=w.caml_register_global,s=w.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
B(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
bO(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
fu(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
a3(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}function
ft(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):w.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}var
h=w.caml_get_global_data(),U=d("cc_plugin"),l=h.Term,m=h.Names,ae=h.Hashset,a$=h.Sorts,i=h.Util,t=h.Not_found,av=h.Global,bh=h.Environ,e=h.EConstr,p=h.Tacmach,g=h.Int,v=h.Queue,c=h.Pp,aU=h.Control,E=h.CErrors,ag=h.Assert_failure,C=h.Termops,aN=h.Feedback,al=h.Hashtbl,L=h.Pervasives,T=h.CamlinternalLazy,q=h.Tacticals,k=h.Proofview,F=h.Tactics,ai=h.Typing,bH=h.Evarsolve,bK=h.CAst,aZ=h.Coqlib,bF=h.Equality,bE=h.Context,bA=h.CClosure,bB=h.Reductionops,aq=h.Loc,W=h.Ltac_plugin,V=h.Stdarg,aj=h.Genarg,a2=h.Mltop,aM=[0,0],dI=d("Out of depth ... "),dH=d("Out of instances ... "),dJ=d("First run was incomplete, completing ... "),dG=d("Executing ... "),dF=d("Running E-matching algorithm ... "),dE=d("paf_of_patt: pattern is trivial"),dB=d("wrong incomplete class."),du=d(" ... "),dv=d(" = "),dw=d("Checking if "),dt=d("Yes"),dx=d("No"),dq=d(aF),dr=d("Processing mark for term "),dm=d("weird error in injection subterms merge."),dn=[0,d("add_pacs")],dk=d(aF),dl=d("Updating term "),dh=d(aF),di=d(b1),dj=d("Merging "),dd=d(aF),de=d(b1),df=d("Linking "),dc=[0,d("plugins/cc/ccalgo.ml"),651,2],c7=d(aK),c8=d(" <> "),c9=d(bX),c_=d(bZ),c$=d("Adding new disequality, depth="),c2=d(aK),c3=d(" == "),c4=d(bX),c5=d(bZ),c6=d("Adding new equality, depth="),c1=d("discarding redundant (dis)equality"),cX=d(aK),cY=d(b3),cU=d(aK),cV=d(":="),cW=d(b3),cT=d("incomplete matching."),cI=d("not a node."),cJ=[0,d("subterms")],cz=d("not a constructor."),cA=[0,d("get_constructor")],cv=d("not a representative."),cw=[0,d("get_representative")],cl=d("signature already entered."),cm=[0,d("enter")],cc=[0,d("Congruence"),[0,d("Verbose"),0]],cd=d("Congruence Verbose"),cF=d("Ccalgo.Discriminable"),cL=d(bT),cN=d(bT),dy=d("_eps_"),dK=d("invalid cc transitivity."),dL=d("not enough args."),dM=[0,d("nth_arg")],dN=[0,1,20],dO=d("equal_proof "),dP=[0,1,20],dQ=d("edge_proof "),dR=[0,1,20],dS=d("constr_proof "),dU=d(","),dT=d("}"),dV=d("{"),dW=[0,1,20],dX=d("path_proof "),dY=[0,1,20],dZ=d("congr_proof "),d0=[0,1,20],d1=d("ind_proof "),em=[0,d("plugins/cc/cctac.ml"),251,9],es=d("f"),et=d("I don't know how to handle dependent equality"),eU=[0,0],eL=d("("),eM=d(")"),eF=[0,1],eD=d("Goal solved, generating proof ..."),eC=d("Computation completed."),eB=d("Problem built, solving ..."),eA=d("Reading subgoal ..."),eE=[13,0,0,0],eI=d("Goal is solvable by congruence but some arguments are missing."),eJ=d("  replacing metavariables by arbitrary terms."),eK=d(')",'),eN=d('"congruence with ('),eO=d("  Try "),eP=d("Incomplete"),eQ=d("congruence failed"),ez=d(b6),ey=d("H"),ew=d("e"),ex=d("X"),eu=d(b6),ep=[0,0],eq=[0,1],el=d("t"),d2=d("CC"),d3=d(aE),d4=[0,d(ad),[0,d(ac),0]],d5=d("eq_rect"),d6=[0,d(ad),[0,d(ac),0]],d8=d("eq_refl"),d9=[0,d(ad),[0,d(ac),0]],d_=d("eq_sym"),d$=[0,d(ad),[0,d(ac),0]],eb=d("eq_trans"),ec=[0,d(ad),[0,d(ac),0]],ed=d("eq"),ee=[0,d(ad),[0,d(ac),0]],ef=d("False"),eg=[0,d(ad),[0,d(ac),0]],eR=d("congruence failed."),fp=d(aE),fq=d(aE),fk=[0,d(aH),1,0],fg=[0,d(aH),1,0],fd=[0,d(aH),1,0],fa=[0,d(aH),1,0],e$=d(ca),fb=[0,d(b9)],fc=d(b4),fe=[0,d(aI)],ff=d(ca),fh=[0,d(b9)],fi=[0,d(aI)],fj=d(b4),fl=[0,d(aI)],fm=[0,[0,d(aI)],0],fn=d(b5),e5=d(aG),e3=d(aG),e1=d(aG),eZ=d(aG),e9=d(b5),fs=d(aE),cS=h.Typeops,dC=h.Vars,dA=h.Goal,cf=h.Goptions,eh=h.Inductiveops,ei=h.Retyping,en=h.Evarutil,eW=h.Pretype_errors,eX=h.Type_errors,eG=h.Detyping,eH=h.Printer,er=h.Evd,eo=h.Refine,e7=h.Array,Z=5;function
z(d){var
c=aM[1];if(c){var
e=a(d,0);return b(aN[10],0,e)}return c}function
cb(a){aM[1]=a;return 0}var
ce=[0,0,cd,cc,function(a){return aM[1]},cb];b(cf[4],0,ce);var
cg=g[1],ch=[0,function(b,a){return b===a?1:0},cg],at=a(al[19],ch);function
ci(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
cj=[0,ci,function(c){var
d=c[1],e=a(g[1],c[2]),f=a(g[1],d);return b(ae[2][1],f,e)}],am=a(al[19],cj);function
ck(f,e,d){if(b(am[11],d[1],e)){var
g=a(c[3],cl);j(E[3],0,cm,g)}else
j(am[10],d[1],e,f);return j(at[10],d[2],f,e)}function
cn(c,a){return b(am[7],a[1],c)}function
co(a,c){try{var
d=b(at[7],a[2],c);b(am[6],a[1],d);var
e=b(at[6],a[2],c);return e}catch(a){a=s(a);if(a===t)return 0;throw a}}function
cp(c,a){function
d(a){return co(c,a)}return b(g[2][13],d,a)}var
cq=[0,function(b,a){var
c=ar(b[1],a[1]),e=a[3],f=a[2],g=b[3],h=b[2];if(0===c){var
d=ar(h,f);return 0===d?j(i[17][51],ar,g,e):d}return c}],cr=[0,function(b,a){var
c=ar(b[1],a[1]),d=a[2],e=b[2];return 0===c?ar(e,d):c}],o=a(i[21][1],cq),n=a(i[21][1],cr);function
a_(b,a){var
c=0===b[0]?0===b[1]?0===a[0]?0===a[1]?1:0:0:0===a[0]?0===a[1]?0:1:0:0===a[0]?0:1;return c?1:0}function
aO(n,k){var
c=n,a=k;for(;;){switch(c[0]){case
0:if(0===a[0])return b(l[bY],c[1],a[1]);break;case
1:if(1===a[0]){var
o=a[2],p=c[2],f=a_(c[1],a[1]);return f?a_(p,o):f}break;case
2:if(2===a[0])return b(m[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],g=aO(c[1],a[1]);if(g){var
c=r,a=q;continue}return g}break;default:if(4===a[0]){var
d=a[1],e=c[1],h=e[2]===d[2]?1:0,s=d[3],t=d[1][1],u=e[3],v=e[1][1];if(h){var
i=u===s?1:0;if(i)return b(m[46],v,t);var
j=i}else
var
j=h;return j}}return 0}}function
aP(c){switch(c[0]){case
0:var
e=a(l[bP],c[1]);return b(ae[2][1],1,e);case
1:var
f=c[1],g=a(a$[6],c[2]),h=a(a$[6],f);return j(ae[2][3],2,h,g);case
2:var
i=a(m[1][3],c[1]);return b(ae[2][1],3,i);case
3:var
k=c[1],n=aP(c[2]),o=aP(k);return j(ae[2][3],4,o,n);default:var
d=c[1],p=d[3],q=d[2],r=a(m[50],d[1][1]);return B(ae[2][4],5,r,q,p)}}var
G=a(al[19],[0,l[bY],l[bP]]),_=a(al[19],[0,aO,aP]),aQ=a(al[19],[0,m[1][1],m[1][3]]),cs=[0,a(l[aJ],L[8])],ba=[0,[1,L[8],[0,L[8],L[8],0]],L[8],o[1],0,cs];function
bb(c){var
b=a(_[1],Z);return[0,Z,0,aC(5,ba),a(G[1],Z),0,b]}function
ct(e,d){var
f=a(G[1],Z),h=a(aQ[1],Z),i=g[2][1],j=a(v[2],0),k=a(v[2],0),l=g[2][1],b=a(at[1],Z),c=[0,a(am[1],Z),b];return[0,bb(0),c,l,k,j,0,0,i,h,e,0,f,d]}function
cu(a){return a[1]}function
y(e,g){var
c=0,a=g;for(;;){var
d=r(e[3],a)[a+1][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
f=function(b){r(e[3],b)[b+1][2]=a;return 0};b(i[17][14],f,c);return a}}function
M(e,b){var
d=r(e[3],b)[b+1][1];if(0===d[0])return d[1];var
f=a(c[3],cv);return j(E[3],0,cw,f)}function
aR(b,a){return r(b[3],a)[a+1][3]}function
cx(d,c,a){var
e=aR(d,c);return b(o[22],a,e)}function
cy(c,f,e){var
a=f;for(;;)try{var
g=aR(c,a),h=b(o[22],e,g);return h}catch(b){b=s(b);if(b===t){var
d=r(c[3],a)[a+1][1];if(0===d[0])throw t;var
a=d[1];continue}throw b}}function
bc(e,b){var
d=r(e[3],b)[b+1][5];if(4===d[0])return d[1];var
f=a(c[3],cz);return j(E[3],0,cA,f)}function
bd(b,a){return M(b,a)[1]}function
cB(a){return a[4]}function
cC(a){return a[5]}function
cD(e,d,c){var
a=M(e,d);a[1]=a[1]+1|0;a[2]=b(g[2][4],c,a[2]);a[3]=b(g[2][4],c,a[3]);return 0}function
cE(e,d,c){var
a=M(e,d);a[1]=a[1]+1|0;a[3]=b(g[2][4],c,a[3]);return 0}var
be=[248,cF,w.caml_fresh_oo_id(0)];function
cG(b){var
c=a(i[17][6],b[3]);return[0,b[1],b[2]+1|0,c]}function
cH(a,c,e){try{var
i=b(n[22],c,a[6]),d=i}catch(a){a=s(a);if(a!==t)throw a;var
d=g[2][1]}var
f=a[6],h=b(g[2][4],e,d);a[6]=j(n[4],c,h,f);return 0}function
af(b,a){return r(b[3],a)[a+1][5]}function
aS(f,b){var
d=r(f[3],b)[b+1][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[3],cI);return j(E[3],0,cJ,g)}function
bf(a,c){var
b=aS(a,c),d=b[1],e=y(a,b[2]);return[0,y(a,d),e]}function
cK(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
d=((a[1]*3|0)/2|0)+1|0,e=aC(d,ba);a[1]=d;bO(i[19][10],a[3],0,e,0,b);a[3]=e}a[2]=c;return b}function
au(a){return[0,0,g[2][1],g[2][1],0,a,n[1]]}var
cM=[0,a(m[1][6],cL)],cO=[0,a(m[1][6],cN)],cP=a(l[aJ],2),cQ=[0,0,a(l[aJ],2),cP],cR=a(l[bS],cQ);function
bg(f,c){var
g=a(l[b8],f);if(10===g[0]){var
d=g[1],h=a(av[2],0);if(b(bh[63],d[1],h)){var
e=b(m[a6][1],d[1],0);if(c){var
i=c[2],j=a(l[a7],[0,e,c[1]]);return b(l[59],j,i)}var
k=a(av[2],0),n=b(cS[27],k,d),o=a(av[2],0),p=b(bh[62],e,o)[2]+1|0,q=b(l[84],p,n)[1],r=[0,e,a(l[aJ],1)],s=a(l[a7],r);return b(l[68],s,q)}}return b(l[59],f,c)}function
H(c){switch(c[0]){case
0:return bg(c[1],0);case
1:var
i=c[1],f=[0,cO,a(l[bU],c[2]),cR],g=a(l[a8],f),h=[0,cM,a(l[bU],i),g];return a(l[a8],h);case
2:return a(l[aL],c[1]);case
3:var
j=c[1],e=[0,H(c[2]),0],d=j;for(;;){if(3===d[0]){var
k=d[1],e=[0,H(d[2]),e],d=k;continue}if(0===d[0])return bg(d[1],e);var
m=H(d);return b(l[59],m,e)}default:return a(l[b7],c[1][1])}}function
bi(q,p){var
f=a(e[a9][1],p);function
d(b){return bi(q,a(e[8],b))}var
c=a(l[b8],f);switch(c[0]){case
6:var
r=c[2],s=c[1],t=d(c[3]),u=[0,s,d(r),t];return a(l[bS],u);case
7:var
v=c[2],w=c[1],x=d(c[3]),y=[0,w,d(v),x];return a(l[a8],y);case
8:var
z=c[3],A=c[2],B=c[1],C=d(c[4]),D=d(z),E=[0,B,d(A),D,C];return a(l[118],E);case
9:var
F=c[1],G=b(i[19][52],d,c[2]),H=[0,d(F),G];return a(l[b_],H);case
10:var
g=c[1],I=g[2],J=a(m[17][6],g[1]),K=[0,a(m[17][2],J),I];return a(l[ak],K);case
11:var
h=c[1],j=h[1],L=h[2],M=j[2],N=a(m[23][6],j[1]),O=[0,[0,a(m[23][2],N),M],L];return a(l[b0],O);case
12:var
k=c[1],n=k[1],o=n[1],P=k[2],Q=n[2],R=o[2],S=a(m[23][6],o[1]),T=[0,[0,[0,a(m[23][2],S),R],Q],P];return a(l[b7],T);case
16:var
U=c[2],V=c[1],W=function(b){var
c=a(m[17][6],b);return a(m[17][2],c)},X=b(m[a6][10],W,V),Y=[0,X,d(U)];return a(l[a7],Y);default:return f}}function
aT(b,a){if(0===a[0]){var
d=a[2],e=a[1],f=function(c,a){return[3,a,aT(b,c)]};return j(i[17][19],f,d,e)}var
c=a[1]-1|0;return r(b,c)[c+1]}function
N(f,d){var
g=a(c[3],cU),h=H(af(f,d)),i=a(e[8],h),j=a(C[ak],i),k=a(c[3],cV),l=a(c[16],d),m=a(c[3],cW),n=b(c[12],m,l),o=b(c[12],n,k),p=b(c[12],o,j);return b(c[12],p,g)}function
aw(d){var
f=a(c[3],cX),g=H(d),h=a(e[8],g),i=a(C[ak],h),j=a(c[3],cY),k=b(c[12],j,i);return b(c[12],k,f)}function
Q(d,f){var
h=d[1];try{var
k=b(_[7],h[6],f);return k}catch(k){k=s(k);if(k===t){var
c=cK(h),u=H(f),w=a(e[8],u),x=b(p[15],d[13],w),i=bi(a(p[2],d[13]),x);switch(f[0]){case
2:var
C=o[1],l=[0,[0,au(i)],-1,C,0,f];break;case
3:var
D=f[2],n=Q(d,f[1]),q=Q(d,D);cD(h,y(h,n),c);cE(h,y(h,q),c);d[3]=b(g[2][4],c,d[3]);var
E=o[1],l=[0,[0,au(i)],-1,E,[0,[0,n,q]],f];break;case
4:var
F=f[1];b(v[3],[0,c,[0,[0,c,0]]],d[5]);b(v[3],[0,c,[1,[0,c,F[2],0]]],d[5]);var
I=o[1],l=[0,[0,au(i)],-1,I,0,f];break;default:b(v[3],[0,c,[0,[0,c,0]]],d[5]);var
z=o[1],l=[0,[0,au(i)],-1,z,0,f]}r(h[3],c)[c+1]=l;j(_[5],h[6],f,c);try{var
B=b(G[7],d[12],i),m=B}catch(a){a=s(a);if(a!==t)throw a;var
m=g[2][1]}var
A=b(g[2][4],c,m);j(G[10],d[12],i,A);return c}throw k}}function
bj(a,e,d,c){var
f=Q(a,d),g=Q(a,c);b(v[3],[0,f,g,[0,e,0]],a[4]);return j(G[5],a[1][4],e,[0,d,c])}function
bk(a,d,c,b){var
e=Q(a,c),f=Q(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
cZ(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
c0(a,d,c){try{var
e=a[1],f=function(a){return y(e,a)},g=b(i[19][15],f,c),h=b(aQ[9],a[9],d),k=function(b){function
c(c,b){return c===y(a[1],b)?1:0}return j(i[19][32],c,g,b)},l=b(i[17][26],k,h);return l}catch(a){a=s(a);if(a===t)return 0;throw a}}function
da(e,b,a,d){var
c=r(e[3],b)[b+1];c[1]=[1,a,d];c[2]=a;return 0}function
bl(g,f,e){var
a=f,b=e;for(;;){var
c=r(g[3],a)[a+1][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
db(c,i,h){var
o=y(c,h);if(y(c,i)===o){var
p=bl(c,h,0),a=[0,bl(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,ag,dc]}function
bm(d,h,k,w){z(function(o){var
e=a(c[3],dd),f=N(d[1],k),g=a(c[3],de),i=N(d[1],h),j=a(c[3],df),l=b(c[12],j,i),m=b(c[12],l,g),n=b(c[12],m,f);return b(c[12],n,e)});var
i=M(d[1],h),e=M(d[1],k);da(d[1],h,k,w);try{var
H=b(G[7],d[12],i[5]),p=H}catch(a){a=s(a);if(a!==t)throw a;var
p=g[2][1]}var
x=b(g[2][6],h,p);j(G[10],d[12],i[5],x);var
q=b(g[2][7],i[3],e[3]);e[1]=a(g[2][20],q);e[3]=q;e[2]=b(g[2][7],i[2],e[2]);cp(d[2],i[3]);d[3]=b(g[2][7],d[3],i[3]);var
y=r(d[1][3],h)[h+1][3];function
A(c,a){return b(v[3],[0,a,[1,c]],d[5])}b(o[10],A,y);var
B=i[6];function
C(c){function
e(a){return b(v[3],[0,a,[0,c]],d[5])}return a(g[2][13],e)}b(n[10],C,B);var
l=i[4],f=e[4];if(typeof
l==="number"){if(0===l)return 0;if(typeof
f==="number"){if(0===f){e[4]=1;return 0}}else
if(0===f[0]){d[8]=b(g[2][6],k,d[8]);e[4]=1;return 0}}else
if(0===l[0]){var
D=l[1];if(typeof
f==="number"){if(0===f){e[4]=[0,D];d[8]=b(g[2][6],h,d[8]);d[8]=b(g[2][4],k,d[8]);return 0}var
u=0}else
var
u=1===f[0]?1:0;if(!u){d[8]=b(g[2][6],h,d[8]);return 0}}else{var
m=l[1],E=m[2],F=m[1];if(typeof
f==="number"){if(0===f){e[4]=[1,m];return 0}}else
if(0!==f[0])return b(v[3],[0,F,[1,E]],d[5])}return 0}function
dg(e,f){z(function(n){var
d=a(c[3],dh),g=N(f[1],e[2]),h=a(c[3],di),i=N(f[1],e[1]),j=a(c[3],dj),k=b(c[12],j,i),l=b(c[12],k,h),m=b(c[12],l,g);return b(c[12],m,d)});var
g=f[1],h=y(g,e[1]),i=y(g,e[2]),j=1-(h===i?1:0);if(j){var
l=bd(g,i);if(bd(g,h)<l)return bm(f,h,i,e);var
d=e[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bm(f,i,h,[0,e[2],e[1],k])}return j}function
dp(f,s,d){z(function(j){var
e=a(c[3],dq),g=N(d[1],f),h=a(c[3],dr),i=b(c[12],h,g);return b(c[12],i,e)});var
p=y(d[1],f),h=M(d[1],p);if(0===s[0]){cH(h,s[1],f);d[3]=b(g[2][7],h[2],d[3]);return 0}var
e=s[1],q=r(d[1][3],p)[p+1];if(1-b(o[3],e,q[3]))q[3]=j(o[4],e,f,q[3]);var
i=h[4];if(typeof
i==="number"){if(0===i)return 0===e[2]?(h[4]=[1,[0,f,e]],0):(d[3]=b(g[2][7],h[2],d[3]),h[4]=[0,e],d[8]=b(g[2][4],p,d[8]),0)}else
if(1===i[0]){var
t=i[1],k=t[2],u=t[1];if(e[1]===k[1]){var
x=bc(d[1],e[1]),n=x[3],m=k[3],l=e[3];for(;;){var
w=0<n?1:0;if(w){if(m)if(l){var
A=l[2],B=m[2];b(v[3],[0,m[1],l[1],[1,u,k,f,e,n]],d[4]);var
n=n-1|0,m=B,l=A;continue}var
C=a(c[3],dm);return j(E[3],0,dn,C)}return w}}throw[0,be,u,k,f,e]}d[3]=b(g[2][7],h[2],d[3]);return 0}function
ds(e){var
g=e[1];function
h(f){if(f){var
d=f[1],k=f[2],l=y(g,d[2]);if(y(g,d[1])===l)var
j=a(c[3],dt),i=[0,d];else
var
m=h(k),j=a(c[3],dx),i=m;z(function(p){var
f=a(c[3],du),g=N(e[1],d[2]),h=a(c[3],dv),i=N(e[1],d[1]),k=a(c[3],dw),l=b(c[12],k,i),m=b(c[12],l,h),n=b(c[12],m,g),o=b(c[12],n,f);return b(c[12],o,j)});return i}return 0}return h(e[6])}var
dz=a(m[1][6],dy);function
dD(d){var
f=d[8];function
h(r){var
h=M(d[1],r)[4];if(typeof
h!=="number"&&0===h[0]){var
f=h[1],w=H(af(d[1],f[1])),x=a(e[8],w),y=b(p[15],d[13],x),z=a(e[a9][1],y),A=f[3],B=function(a){return H(af(d[1],a))},C=b(i[17][15],B,A),D=a(i[17][9],C),F=b(l[75],z,D),G=f[2],m=af(d[1],r),o=F,k=G;for(;;){if(0<k){var
q=a(l[33],o),t=q[3],u=q[2],g=b(p[20],dz,d[13]),n=d[13];d[13]=j(dA[4][11],n[2],n[1],[0,[0,g,u],0]);var
v=[0,a(l[aL],g),0],m=[3,m,[2,g]],o=b(dC[13],v,t),k=k-1|0;continue}d[1][5]=[0,f,d[1][5]];Q(d,m);return 0}}var
s=a(c[3],dB);return j(E[3],0,0,s)}return b(g[2][13],h,f)}function
bn(c){var
a=[0,n[1]],f=c[1],d=c[1][3];function
e(c,h){var
d=c<f[2]?1:0;if(d){var
e=h[1];if(0===e[0]){var
i=e[1][6],k=function(d,k){try{var
i=b(n[22],d,a[1]),e=i}catch(a){a=s(a);if(a!==t)throw a;var
e=g[2][1]}var
f=a[1],h=b(g[2][4],c,e);a[1]=j(n[4],d,h,f);return 0};return b(n[10],k,i)}return 0}return d}b(i[19][14],e,d);return a[1]}function
bo(q,p,d){var
c=a(i[22][9],d),l=c[3];if(l){var
e=l[2],u=l[1],f=u[2],h=u[1],j=q[1];if(0===h[0]){var
k=h[2],m=h[1];if(k){var
A=k[2],B=k[1];try{var
C=b(_[7],j[6],m),D=[0,C,a(i[17][1],k)],E=M(j,f)[6],F=b(n[22],D,E),G=function(g){var
f=bf(q[1],g),h=[0,[0,[0,m,A],f[1]],[0,[0,B,f[2]],e]],j=c[2],k=[0,a(i[19][8],c[1]),j,h];return b(i[22][3],k,d)},H=b(g[2][13],G,F);return H}catch(a){a=s(a);if(a===t)return 0;throw a}}try{var
v=y(j,b(_[7],j[6],m))===f?1:0,I=v?b(i[22][3],[0,c[1],c[2],e],d):v;return I}catch(a){a=s(a);if(a===t)return 0;throw a}}var
o=h[1],w=o-1|0;if(0<=r(c[1],w)[w+1]){var
x=o-1|0;return r(c[1],x)[x+1]===f?b(i[22][3],[0,c[1],c[2],e],d):0}var
z=o-1|0;r(c[1],z)[z+1]=f;return b(i[22][3],[0,c[1],c[2],e],d)}p[1]=[0,[0,c[2],c[1]],p[1]];return 0}function
aV(d,c){if(0===c[0]){var
e=c[1],f=a(i[17][1],c[2]);return[0,b(_[7],d,e),f]}return b(E[9],0,dE)}function
bp(c){var
k=c[1][6],f=a(i[22][2],0),l=bn(c);function
d(a){var
h=a[5];if(typeof
h==="number")if(0===h)try{var
x=aV(k,a[4]),y=b(n[22],x,l),d=y}catch(a){a=s(a);if(a!==t)throw a;var
d=g[2][1]}else
var
d=g[2][1];else{var
z=h[1];try{var
A=b(G[7],c[12],z),o=A}catch(a){a=s(a);if(a!==t)throw a;var
o=g[2][1]}var
d=o}function
p(c){return b(i[22][3],[0,aC(a[3],-1),a,[0,[0,a[4],c],0]],f)}b(g[2][13],p,d);var
j=a[7];if(typeof
j==="number")if(0===j)try{var
r=aV(k,a[6]),u=b(n[22],r,l),e=u}catch(a){a=s(a);if(a!==t)throw a;var
e=g[2][1]}else
var
e=g[2][1];else{var
v=j[1];try{var
w=b(G[7],c[12],v),m=w}catch(a){a=s(a);if(a!==t)throw a;var
m=g[2][1]}var
e=m}function
q(c){return b(i[22][3],[0,aC(a[3],-1),a,[0,[0,a[6],c],0]],f)}return b(g[2][13],q,e)}b(i[17][14],d,c[7]);return f}function
bq(b){var
d=[0,0],e=bp(b);z(function(b){return a(c[3],dF)});try{for(;;){a(aU[2],0);bo(b,d,e);continue}}catch(a){a=s(a);if(a===i[22][1])return d[1];throw a}}function
aW(w,d){z(function(b){return a(c[3],dG)});try{for(;;){a(aU[2],0);try{dg(a(v[5],d[4]),d);var
K=1,h=K}catch(e){e=s(e);if(e!==v[1])throw e;try{var
u=a(v[5],d[5]);dp(u[1],u[2],d);var
J=1,h=J}catch(e){e=s(e);if(e!==v[1])throw e;try{var
f=a(g[2][26],d[3]);d[3]=b(g[2][6],f,d[3]);z(function(i){return function(j){var
e=a(c[3],dk),f=N(d[1],i),g=a(c[3],dl),h=b(c[12],g,f);return b(c[12],h,e)}}(f));var
m=bf(d[1],f),p=m[1],y=aS(d[1],f)[2],q=M(d[1],p),r=q[4],X=typeof
r==="number"?0:0===r[0]?(q[4]=1,d[8]=b(g[2][6],p,d[8]),1):0,A=aR(d[1],p),B=function(c,e){return function(a,f){return b(v[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(y,f);b(o[10],B,A);var
D=q[6],F=function(c){return function(a,e){return b(v[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(f);b(n[10],F,D);try{var
G=cn(m,d[2]);b(v[3],[0,f,G,0],d[4])}catch(a){a=s(a);if(a!==t)throw a;ck(f,m,d[2])}var
I=1,h=I}catch(a){a=s(a);if(a!==t)throw a;var
h=0}}}if(h)continue;var
x=ds(d);if(x)var
S=x[1],T=w?[1,S]:0,k=[0,T];else
if(a(g[2][2],d[8]))if(0<d[10]){var
U=bq(d),V=function(q){var
n=q[2],f=q[1];a(aU[2],0);var
o=0<d[10]?1:0;if(o){if(c0(d,f[1],n))return z(function(b){return a(c[3],c1)});j(aQ[5],d[9],f[1],n);var
t=d[1],r=function(b){try{var
e=af(t,b);return e}catch(b){b=s(b);if(a(E[20],b)){var
d=a(c[3],cT);return j(E[3],0,0,d)}throw b}},m=b(i[19][15],r,n),u=a(l[aL],f[1]),p=b(i[19][15],H,m);a(i[19][41],p);var
g=a(l[b_],[0,u,p]),h=aT(m,f[4]),k=aT(m,f[6]);d[11]=1;d[10]=d[10]-1|0;return f[2]?(z(function(B){var
f=a(c[3],c2),i=aw(k),j=a(c[3],c3),l=aw(h),m=a(c[3],c4),n=a(e[8],g),o=a(C[ak],n),p=a(c[3],c5),q=b(c[12],p,o),r=b(c[12],q,m),s=b(c[12],r,l),t=b(c[12],s,j),u=b(c[12],t,i),v=b(c[12],u,f),w=a(c[5],0),x=a(c[16],d[10]),y=a(c[3],c6),z=b(c[12],y,x),A=b(c[12],z,w);return b(c[12],A,v)}),bj(d,g,h,k)):(z(function(B){var
f=a(c[3],c7),i=aw(k),j=a(c[3],c8),l=aw(h),m=a(c[3],c9),n=a(e[8],g),o=a(C[ak],n),p=a(c[3],c_),q=b(c[12],p,o),r=b(c[12],q,m),s=b(c[12],r,l),t=b(c[12],s,j),u=b(c[12],t,i),v=b(c[12],u,f),w=a(c[5],0),x=a(c[16],d[10]),y=a(c[3],c$),z=b(c[12],y,x),A=b(c[12],z,w);return b(c[12],A,v)}),bk(d,[0,g],h,k))}return o};b(i[17][14],V,U);var
W=d[11]?(d[11]=0,aW(1,d)):(z(function(b){return a(c[3],dH)}),0),k=W}else{z(function(b){return a(c[3],dI)});var
k=0}else{z(function(b){return a(c[3],dJ)});dD(d);var
k=aW(0,d)}return k}}catch(a){a=s(a);if(a[1]===be){var
L=a[5],O=a[4],P=a[3],Q=a[2],R=w?[0,[0,Q,P,O,L]]:0;return[0,R]}throw a}}var
f=[0,[0,n[1],n[2],n[3],n[4],n[5],n[6],n[7],n[8],n[9],n[10],n[11],n[12],n[13],n[14],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24]],[0,o[1],o[2],o[3],o[4],o[5],o[6],o[7],o[8],o[9],o[10],o[11],o[12],o[13],o[14],o[15],o[16],o[17],o[18],o[19],o[20],o[21],o[22],o[23],o[24]],G,_,aO,H,z,cu,cB,cC,ct,Q,bj,bk,cZ,cG,y,cx,cy,af,bc,aS,db,bn,bo,bp,aV,bq,aW,N,bb];as(170,f,"Cc_plugin.Ccalgo");function
an(a){return[0,a,a,[2,a]]}function
ah(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return an([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
A(o,n){var
e=o,d=n;for(;;){var
g=e[3],h=d[3];switch(g[0]){case
2:return d;case
4:var
l=g[2],m=g[1];switch(h[0]){case
2:var
i=0;break;case
3:var
k=h[1][3];if(4===k[0]){var
r=h[2],s=k[1],t=A(l,k[2]),e=ah(A(m,s),t),d=r;continue}var
i=1;break;case
4:var
u=h[1],v=A(l,h[2]);return ah(A(m,u),v);default:var
i=1}break;default:var
i=0}if(!i){if(2===h[0])return e;if(3===g[0]){var
q=g[1],e=q,d=A(g[2],d);continue}}if(b(f[5],e[2],d[1]))return[0,e[1],d[2],[3,e,d]];var
p=a(c[3],dK);return j(E[3],0,0,p)}}function
O(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=O(a[1]);return A(O(c),d);case
4:var
e=a[1],f=O(a[2]);return ah(O(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,O(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
br(d,a){var
c=b(f[3][7],d,a);return[0,c[1],c[2],[0,a]]}function
bs(d,a){var
c=b(f[3][7],d,a);return[0,c[2],c[1],[1,a]]}function
bt(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
h=b[2],i=b[1];if(0<d){var
b=i,d=d-1|0;continue}return h}var
g=a(c[3],dL);return j(E[3],0,dM,g)}}function
bu(c,d,b,a){var
e=bt(c[2],b-a|0);return[0,bt(c[1],b-a|0),e,[5,c,d,b,a]]}function
R(d,e,g){function
i(n){var
h=b(f[30],d,g),i=a(c[4],dN),j=b(f[30],d,e),k=a(c[3],dO),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],i);if(e===g)return an(b(f[20],d,e));var
h=j(f[23],d,e,g),k=h[1],l=O(ax(d,g,h[2]));return A(ax(d,e,k),l)}function
bv(d,i){var
g=i[2],j=i[1],k=j[2],l=j[1];function
p(n){var
e=b(f[30],d,k),g=a(c[4],dP),h=b(f[30],d,l),i=a(c[3],dQ),j=b(c[12],i,h),m=b(c[12],j,g);return b(c[12],m,e)}a(f[7],p);var
q=R(d,l,g[1]),r=O(R(d,k,g[2])),e=g[3];if(typeof
e==="number")var
h=bw(d,g[1],g[2]);else
if(0===e[0])var
m=e[1],s=e[2]?bs(a(f[9],d),m):br(a(f[9],d),m),h=s;else
var
n=e[2],t=e[5],u=aY(d,e[1],n,e[3],e[4]),o=b(f[21],d,n[1]),h=bu(u,o[1],o[3],t);return A(A(q,h),r)}function
aX(d,g,e){function
l(k){var
e=a(c[4],dR),h=b(f[30],d,g),i=a(c[3],dS),j=b(c[12],i,h);return b(c[12],j,e)}a(f[7],l);var
h=j(f[19],d,g,e),i=R(d,g,h);if(0===e[3])return i;var
m=a(f[16],e),k=b(f[22],d,h),n=k[1],o=b(f[20],d,k[2]),p=aX(d,n,m);return A(i,ah(p,an(o)))}function
ax(e,g,d){function
i(u){var
h=a(c[3],dT);function
i(b){return a(c[16],b[1][2])}function
k(b){return a(c[3],dU)}var
l=j(c[38],k,i,d),m=a(c[3],dV),n=a(c[4],dW),o=b(f[30],e,g),p=a(c[3],dX),q=b(c[12],p,o),r=b(c[12],q,n),s=b(c[12],r,m),t=b(c[12],s,l);return b(c[12],t,h)}a(f[7],i);if(d){var
h=d[1],k=d[2],l=bv(e,h);return A(ax(e,h[1][2],k),l)}return an(b(f[20],e,g))}function
bw(d,g,e){function
j(n){var
h=b(f[30],d,e),i=a(c[4],dY),j=b(f[30],d,g),k=a(c[3],dZ),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
h=b(f[22],d,g),k=h[2],l=h[1],i=b(f[22],d,e),m=i[1],n=R(d,k,i[2]);return ah(R(d,l,m),n)}function
aY(d,g,i,e,h){function
j(n){var
h=b(f[30],d,e),i=a(c[4],d0),j=b(f[30],d,g),k=a(c[3],d1),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
k=R(d,g,e),l=aX(d,g,i),m=A(k,aX(d,e,h));return A(O(l),m)}var
ay=[0,an,ah,A,O,br,bs,bu,R,bv,ax,bw,aY,function(c,b){if(bQ<=b[1]){var
a=b[2];return aY(c,a[1],a[2],a[3],a[4])}var
d=b[2];return R(c,d[1],d[2])}];as(171,ay,"Cc_plugin.Ccproof");function
$(b,a){return[P,function(c){return j(aZ[2],d2,b,a)}]}var
a0=$(d4,d3),d7=$(d6,d5),bx=$(d9,d8),ea=$(d$,d_),by=$(ec,eb),x=$(ee,ed),S=$(eg,ef);function
bz(c,b,a){return B(bB[18],bA[14],c,b,a)}function
az(c,b,a){return B(bB[18],bA[9],c,b,a)}function
aA(c,b,a){return j(ai[4],c,[0,b],a)}function
D(f,c,z){var
g=z;for(;;){var
A=bz(f,c,g),d=b(e[3],c,A);switch(d[0]){case
6:var
n=d[3],o=d[2];if(j(e[K][13],c,1,n)){var
p=a(C[49],n),B=aA(f,c,p),E=aA(f,c,o),F=D(f,c,p);return[3,[3,[1,E,B],D(f,c,o)],F]}break;case
9:var
G=d[2],H=D(f,c,d[1]),I=function(a){return D(f,c,a)},J=b(i[19][15],I,G),L=function(b,a){return[3,b,a]};return j(i[19][17],L,H,J);case
10:var
q=d[1],M=q[1],N=b(e[2][2],c,q[2]),O=a(m[17][6],M),P=[0,a(m[17][2],O),N];return[0,a(l[ak],P)];case
11:var
r=d[1],s=r[1],Q=s[2],R=s[1],S=b(e[2][2],c,r[2]),T=a(m[23][6],R),U=[0,[0,a(m[23][2],T),Q],S];return[0,a(l[b0],U)];case
12:var
u=d[1],v=u[1],w=v[2],x=v[1],V=x[2],W=x[1],X=b(e[2][2],c,u[2]),Y=a(m[23][6],W),h=[0,a(m[23][2],Y),V],Z=a(av[26],h)[1],y=b(eh[44],f,[0,h,w]);return[4,[0,[0,[0,h,w],X],y,y-Z[6]|0]];case
16:var
_=d[2],$=d[1],aa=function(b){var
c=a(m[17][6],b);return a(m[17][2],c)},ab=b(m[a6][10],aa,$),g=bO(ei[9],f,c,ab,_,0);continue}var
k=b(C[61],c,g);if(b(e[K][16],c,k))return[0,b(e[5],c,k)];throw t}}function
a1(d,c,g){var
k=az(d,c,g),h=b(e[3],c,k);if(9===h[0]){var
f=h[2],i=X(x),l=h[1],m=Y===i?x[1]:P===i?a(T[2],x):x;if(j(C[K],c,m,l))if(3===f.length-1){var
n=D(d,c,r(f,2)[3]),o=D(d,c,r(f,1)[2]);return[0,aD,[0,r(f,0)[1],o,n]]}return[0,a5,D(d,c,g)]}return[0,a5,D(d,c,g)]}function
ao(d,c,h){var
r=bz(d,c,h),f=b(e[3],c,r);switch(f[0]){case
0:var
k=f[1];return[0,[1,k],a(g[2][5],k)];case
6:var
l=f[3],m=f[2];if(j(e[K][13],c,1,l)){var
n=a(C[49],l),o=ao(d,c,m),t=o[2],u=o[1],p=ao(d,c,n),v=p[2],w=p[1],x=aA(d,c,n),y=aA(d,c,m);return[0,[0,[1,y,x],[0,u,[0,w,0]]],b(g[2][7],t,v)]}break;case
9:var
z=f[2],A=D(d,c,f[1]),B=function(a){return ao(d,c,a)},E=b(i[19][49],B,z),q=a(i[17][44],E),F=q[1],G=j(i[17][18],g[2][7],g[2][1],q[2]);return[0,[0,A,a(i[17][9],F)],G]}var
s=D(d,c,h);return[0,[0,s,0],g[2][1]]}function
bC(a){return 0===a[0]?1:0}function
bD(k,c,i,v){try{var
w=az(k,c,v),m=b(e[72],c,w)}catch(a){a=s(a);if(a===l[27])throw t;throw a}var
d=m[2],n=X(x),y=m[1],z=Y===n?x[1]:P===n?a(T[2],x):x;if(j(C[K],c,z,y))if(3===d.length-1){var
o=ao(k,c,r(d,1)[2]),p=o[1],A=o[2],q=ao(k,c,r(d,2)[3]),u=q[1],B=q[2];if(a(g[2][20],A)===i)if(bC(p))var
f=0;else
var
E=r(d,0)[1],f=[0,b(e[5],c,E)];else
var
f=1;if(a(g[2][20],B)===i)if(bC(u))var
h=0;else
var
D=r(d,0)[1],h=[0,b(e[5],c,D)];else
var
h=1;if(1===f)if(1===h)throw t;return[0,i,f,p,h,u]}throw t}function
ej(o,c,n,m){var
d=o,f=n,h=m;for(;;){var
p=az(d,c,h),g=b(e[3],c,p);if(6===g[0]){var
i=g[3],k=g[2],l=X(S),q=g[1],r=Y===l?S[1]:P===l?a(T[2],S):S;if(j(C[K],c,r,i))return[0,b2,bD(d,c,f,k)];var
d=b(e[bW],[0,q,k],d),f=f+1|0,h=i;continue}return[0,bR,bD(d,c,f,h)]}}function
ek(d,c,g){var
n=az(d,c,g),f=b(e[3],c,n);if(6===f[0]){var
k=f[3],l=f[2],m=X(S),o=f[1],p=Y===m?S[1]:P===m?a(T[2],S):S;if(j(C[K],c,p,k)){var
h=a1(d,c,l);if(aD<=h[1]){var
i=h[2];return[0,bV,[0,i[1],i[2],i[3]]]}return[0,b$,h[2]]}try{var
q=ej(b(e[bW],[0,o,l],d),c,1,k);return q}catch(a){a=s(a);if(a===t)return[0,a5,D(d,c,g)];throw a}}return a1(d,c,g)}function
aa(c,g,f){var
d=X(c);function
h(b){return a(f,a(e[21],[0,b,g]))}var
i=Y===d?c[1]:P===d?a(T[2],c):c,j=a(q[66][58],i);return b(k[68][1],j,h)}function
ap(c,n,w){function
d(h){var
d=X(c);function
f(o){var
q=a(k[63][5],h),x=a(k[63][3],h);function
c(r){var
y=b(p[48][8],h,o),z=a(i[19][11],n),A=j(C[60],r,y,z),c=r,k=A,f=w,d=0,D=a(e[21],[0,o,n]);for(;;){if(0===f){var
E=[0,D,a(i[17][9],d)],s=a(e[34],E),t=[0,c];B(ai[5],q,t,s,x);return[0,t[1],s]}var
g=b(e[3],c,k);if(6===g[0]){var
u=g[3],l=ft(en[3],q,c,0,0,0,0,0,0,g[2]),m=l[2],v=l[1],c=v,k=b(e[K][5],m,u),f=f-1|0,d=[0,m,d];continue}throw[0,ag,em]}}return b(eo[2],0,c)}var
g=Y===d?c[1]:P===d?a(T[2],c):c,l=a(q[66][58],g);return b(k[68][1],l,f)}return a(k[63][9],d)}function
aB(d,c){function
e(e){var
f=ai[2];function
g(a){return b(f,0,a)}var
h=j(p[48][1],g,e,c)[1],i=b(F[137],d,c),l=a(k[61][1],h);return b(k[15],l,i)}return a(k[63][9],e)}function
bG(c,b,a){return a3(bH[5],[0,er[K]],0,eq,ep,c,b,a)}function
I(f,e){function
c(c){var
g=a(k[63][5],c),d=bG(g,a(p[48][4],c),f),h=d[1],i=a(e,d[2]),j=a(k[61][1],h);return b(k[15],j,i)}return a(k[63][9],c)}function
u(b){var
c=a(f[6],b);return a(e[8],c)}function
J(j){function
d(i){function
f(a){return b(p[48][7],i,a)}try{var
d=j[3];switch(d[0]){case
0:var
D=a(e[8],d[1]),g=a(F[45],D);break;case
1:var
E=a(e[8],d[1]),w=u(j[1]),G=u(j[2]),H=function(a){return aa(ea,[0,a,G,w,E],F[45])},g=I(f(w),H);break;case
2:var
x=d[1],K=u(x),L=function(a){var
b=F[45];return aa(bx,[0,a,u(x)],b)},g=I(f(K),L);break;case
3:var
y=d[2],o=d[1],M=u(o[1]),z=u(o[2]),N=u(y[2]),O=function(a){var
c=ap(by,[0,a,M,z,N],2),d=[0,J(y),0],e=[0,J(o),d];return b(q[66][19],c,e)},g=I(f(z),O);break;case
4:var
r=d[2],t=d[1],l=u(t[1]),h=u(r[1]),n=u(t[2]),A=u(r[2]),P=function(g){function
d(j){function
d(d){var
f=a(m[1][6],es),k=b(p[48][11],f,i),o=[0,a(e[9],1),[0,h]],s=[0,[0,k],g,a(e[21],o)],u=ap(a0,[0,g,d,a(e[19],s),l,n],1),v=ap(a0,[0,j,d,n,h,A],1),w=a(e[21],[0,n,[0,A]]),x=a(e[21],[0,n,[0,h]]),y=ap(by,[0,d,a(e[21],[0,l,[0,h]]),x,w],2),z=a(c[3],et),B=[0,b(q[66][5],0,z),0],C=[0,F[a9],B],D=J(r),E=[0,b(q[66][3],v,D),C],G=[0,a(q[66][24],E),0],H=J(t),I=[0,b(q[66][3],u,H),G];return b(q[66][19],y,I)}return I(f(a(e[21],[0,l,[0,h]])),d)}return I(f(h),d)},g=I(f(l),P);break;default:var
v=d[1],Q=d[4],R=d[3],S=d[2],B=u(v[1]),T=u(v[2]),C=u(j[1]),U=a(e[9],(1+R|0)-Q|0),V=function(c){function
d(s){var
f=S[1][2],g=a(e[9],1),h=a(p[48][4],i),j=a(p[48][5],i),d=a3(bF[38],j,h,f,g,c,U,C),l=d[2],n=d[1],o=a(m[1][6],el),r=[0,[0,b(p[48][11],o,i)],c,l],t=ap(a0,[0,c,s,a(e[19],r),B,T],1),u=J(v),w=b(q[66][3],t,u),x=a(k[61][1],n);return b(q[66][3],x,w)}return I(f(C),d)},g=I(f(B),V)}return g}catch(c){c=s(c);if(a(k[67][10],c))return b(k[18],0,c);throw c}}return a(k[63][9],d)}function
ev(c){function
d(d){var
e=ai[2];function
f(a){return b(e,0,a)}var
g=j(p[48][1],f,d,c)[1],h=a(F[45],c),i=a(k[61][1],g);return b(k[15],i,h)}return a(k[63][9],d)}function
bI(j,h,f,i){function
c(c){var
g=u(h),d=u(f);function
k(f){var
l=a(m[1][6],ew),h=b(p[48][11],l,c),n=a(m[1][6],ex),o=b(p[48][11],n,c),r=[0,[0,o],f,a(e[9],1)],s=a(e[19],r),t=[0,aa(d7,[0,f,g,s,j,d,a(e[10],h)],ev),0],k=[0,f,g,d],u=[0,J(i),t],v=[0,h],w=aa(x,k,function(a){return aB(v,a)});return b(q[66][19],w,u)}return I(b(p[48][7],c,d),k)}return a(k[63][9],c)}function
bJ(Z,Y){function
d(h){var
_=a(p[48][4],h);a(aZ[3],aZ[10]);function
$(b){return a(c[3],eA)}a(f[7],$);var
z=a(p[48][5],h),r=a(p[48][4],h),N=a(k[63][1],h),O=[0,a(k[63][13],N),r],d=b(f[11],Z,O),s=[0,0],A=[0,0];function
P(a){var
c=D(z,r,a);b(f[12],d,c);return 0}b(i[17][14],P,Y);var
Q=a(k[63][4],h);function
R(j){var
h=a(bE[2][1][1],j),e=a(l[aL],h),c=ek(z,r,a(bE[2][1][3],j)),g=c[1];if(aD<=g){if(b2<=g)return bR<=g?B(f[15],d,h,1,c[2]):B(f[15],d,h,0,c[2]);if(bV<=g){var
k=c[2];return B(f[14],d,[0,e],k[2],k[3])}var
m=c[2];return B(f[13],d,e,m[2],m[3])}if(b$<=g){var
n=c[2],p=s[1],q=function(a){return B(f[14],d,[2,a[1],e],a[2],n)};b(i[17][14],q,p);A[1]=[0,[0,e,n],A[1]];return 0}var
o=c[2],t=A[1];function
u(a){return B(f[14],d,[2,e,a[1]],o,a[2])}b(i[17][14],u,t);s[1]=[0,[0,e,o],s[1]];return 0}b(i[17][14],R,Q);var
C=a1(z,r,a(p[48][6],h));if(aD<=C[1]){var
H=C[2];B(f[14],d,0,H[2],H[3])}else{var
S=C[2],T=s[1],U=function(a){return B(f[14],d,[1,a[1]],a[2],S)};b(i[17][14],U,T)}function
ab(b){return a(c[3],eB)}a(f[7],ab);var
K=b(f[29],1,d);function
ac(b){return a(c[3],eC)}a(f[7],ac);var
g=a(f[8],d);if(K){var
t=K[1],ad=function(b){return a(c[3],eD)};a(f[7],ad);if(typeof
t==="number"){var
L=a(k[63][5],h),ae=a(f[10],g),af=function(c){var
h=b(f[21],g,c[1]),j=c[3];function
k(a){return u(b(f[20],g,a))}var
l=b(i[17][17],k,j),d=h[1],m=d[1],n=c[2],o=[0,m,a(e[2][1],d[2])],p=[0,a(e[28],o),l];return[0,a(e[34],p),n]},ag=b(i[17][15],af,ae),ah=b(bK[1],0,eE),ai=function(a){var
c=a[2],d=fu(eG[6],eF,0,0,L,_,a[1]);function
e(a){return ah}var
f=[4,d,b(i[17][54],c,e)],g=b(bK[1],0,f);return b(eH[39],L,g)},aj=a(c[3],eI);b(aN[6],0,aj);var
ak=a(c[3],eJ),al=a(c[3],eK),am=function(h){var
d=a(c[3],eL),e=a(c[13],0),f=a(c[3],eM),g=b(c[12],f,e);return b(c[12],g,d)},an=j(c[38],am,ai,ag),ao=a(c[3],eN),ap=b(c[12],ao,an),aq=b(c[12],ap,al),ar=b(c[26],8,aq),as=a(c[3],eO),at=b(c[12],as,ar),au=b(c[12],at,ak);b(aN[6],0,au);var
av=a(c[3],eP);return b(q[66][4],0,av)}else{if(0===t[0]){var
v=t[1],M=v[2],E=b(ay[13],g,[0,bQ,[0,v[1],M,v[3],v[4]]]);b(f[21],g,M[1]);var
X=function(c){var
d=u(E[1]),g=u(E[2]),h=a(k[63][5],c),i=a(p[48][4],c),e=bG(h,i,b(p[48][7],c,d)),j=e[2],l=e[1],n=a(m[1][6],ez),f=b(p[48][11],n,c),r=[0,a(bF[16],f),0],o=[0,j,d,g],s=[0,J(E),r],t=[0,f],y=aa(x,o,function(a){return aB(t,a)}),v=b(q[66][19],y,s),w=a(k[61][1],l);return b(q[66][3],w,v)};return a(k[63][9],X)}var
o=t[1],w=b(ay[13],g,[0,-608347012,[0,o[1],o[2]]]),G=b(f[20],g,o[1]),y=b(f[20],g,o[2]),n=o[3];if(typeof
n==="number")return J(w);else
switch(n[0]){case
0:var
aw=a(e[8],n[1]),V=function(c){var
d=u(G),g=u(y),h=a(m[1][6],eu),f=b(p[48][11],h,c),i=[0,aw,[0,a(e[10],f)]],j=a(e[21],i);function
k(c){var
h=[0,a(F[98],j),0],e=[0,c,d,g],i=[0,J(w),h],k=[0,f],l=aa(x,e,function(a){return aB(k,a)});return b(q[66][19],l,i)}return I(b(p[48][7],c,d),k)};return a(k[63][9],V);case
1:return bI(a(e[8],n[1]),G,y,w);default:var
ax=n[2],az=a(e[8],n[1]),aA=a(e[8],ax),W=function(d){var
f=u(y),g=a(m[1][6],ey),c=b(p[48][11],g,d),h=[0,aA,[0,a(e[10],c)]],i=a(e[21],h),j=[0,a(F[98],i),0],k=[0,bI(az,G,y,w),j],l=aB([0,c],f);return b(q[66][19],l,k)};return a(k[63][9],W)}}}var
aC=a(c[3],eQ);return b(q[66][4],0,aC)}return a(k[63][9],d)}var
eS=a(c[3],eR),bL=b(q[66][5],0,eS);function
bM(d,c){var
e=bJ(d,c),f=a(q[66][29],F[17]),g=b(q[66][3],f,e);return b(q[66][12],g,bL)}function
eT(c,d,l,i){var
f=X(c);function
g(m){function
c(c){var
n=ai[2];function
o(a){return b(n,0,a)}var
f=j(p[48][1],o,c,d),q=f[2],r=f[1],s=a(p[48][5],c),g=a3(bH[5],0,0,0,eU,s,r,q),t=g[1],h=a(e[21],[0,m,[0,g[2],d,l]]),u=a(p[48][5],c),v=B(ai[2],0,u,t,h)[1],w=a(i,h),x=a(k[61][1],v);return b(k[15],x,w)}return a(k[63][9],c)}var
h=Y===f?c[1]:P===f?a(T[2],c):c,m=a(q[66][58],h);return b(k[68][1],m,g)}function
eV(l){var
y=a(k[63][3],l),c=a(p[48][4],l);function
z(d,c){try{var
e=0,f=F[85],g=[0],h=function(a){return aa(bx,g,a)}(f),i=[0,a(q[66][22],h),e],j=[0,a(k[13],0),i],l=eT(x,d,c,F[144]),m=b(q[66][19],l,j);return m}catch(c){c=s(c);if(a(k[67][10],c))return b(k[18],0,c);throw c}}function
A(d){var
c=d[1],e=d[2];if(c[1]!==eW[1])if(c[1]!==eX[1])return b(k[18],[0,e],c);return a(k[13],0)}var
g=b(e[3],c,y);if(9===g[0]){var
h=g[2];if(3===h.length-1){var
n=X(x),B=g[1],D=h[2],E=h[3],G=Y===n?x[1]:P===n?a(T[2],x):x;if(j(C[K],c,G,B)){var
o=b(e[3],c,D),t=b(e[3],c,E);if(9===o[0])if(9===t[0]){var
v=t[2],i=o[2];if(i.length-1===v.length-1)var
w=function(c){if(0<=c){var
d=w(c-1|0),e=r(v,c)[c+1],f=z(r(i,c)[c+1],e);return b(q[66][16],f,d)}var
g=bM(a4,0);return a(q[66][22],g)},u=w(i.length-1-1|0),f=1;else
var
f=0}else
var
f=0;else
var
f=0;if(!f)var
u=a(k[13],0);var
m=u,d=1}else
var
d=0}else
var
d=0}else
var
d=0;if(!d)var
m=a(k[13],0);return b(k[20],m,A)}var
ab=[0,J,bJ,bL,bM,a(k[63][9],eV)];as(193,ab,"Cc_plugin.Cctac");a(a2[12],U);var
eY=0,e0=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(aj[6],V[21]),h=b(W[12][2][7],g,f),i=a(aj[17],V[13]),j=a(aj[6],i),k=b(W[12][2][7],j,e);return function(a){return b(ab[4],h,k)}}}return a(L[2],eZ)},eY],e2=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(aj[17],V[13]),f=a(aj[6],e),g=b(W[12][2][7],f,d);return function(a){return b(ab[4],a4,g)}}return a(L[2],e1)},e0],e4=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(aj[6],V[21]),f=b(W[12][2][7],e,d);return function(a){return b(ab[4],f,0)}}return a(L[2],e3)},e2],e6=[0,function(c){return c?a(L[2],e5):function(a){return b(ab[4],a4,0)}},e4],e8=a(e7[12],e6);j(W[6][9],0,[0,U,e9],e8);function
e_(u){var
i=[0,a(m[1][7],e$)],c=V[13],g=0,h=0;if(0===c[0]){var
k=[0,fb,[0,[1,b(aq[10],0,[0,[0,[5,[0,c[1]]]],i])],h]],l=[0,a(m[1][7],fc)],d=V[21];if(0===d[0]){var
n=[0,[0,fe,[0,[1,b(aq[10],0,[0,[5,[0,d[1]]],l])],k]],g],p=[0,a(m[1][7],ff)],e=V[13],o=0;if(0===e[0]){var
q=[0,[0,fi,[0,fh,[0,[1,b(aq[10],0,[0,[0,[5,[0,e[1]]]],p])],o]]],n],s=[0,a(m[1][7],fj)],f=V[21],r=0;if(0===f[0]){var
t=[0,fm,[0,[0,fl,[0,[1,b(aq[10],0,[0,[5,[0,f[1]]],s])],r]],q]];return j(W[9][4],[0,U,fn],0,t)}throw[0,ag,fk]}throw[0,ag,fg]}throw[0,ag,fd]}throw[0,ag,fa]}b(a2[19],e_,U);function
fo(e){var
c=[28,[0,0,[31,b(aq[10],0,[0,[0,[0,U,fp],0],0])]]],d=a(m[1][6],fq);return B(W[6][4],1,0,d,c)}var
fr=[0,function(b,a){return ab[5]}];j(W[6][9],0,[0,U,fs],fr);b(a2[19],fo,U);var
bN=[0,U];as(200,bN,"Cc_plugin.G_congruence");as(201,[0,f,ay,ab,bN],"Cc_plugin");return});
