(function(Bx){"use strict";var
gT="QMicromega",k1="__varmap",lH="__p",li="__p%i",lG="__ff",kA="=",gX=">=",cw=" * ",N="micromega",lF="enum_proof\n",fd='command "',k0="( )^2",b4=148,kZ="<>",g0="real_nonlinear_prover",lE="(%a)-(%a)",kz=" [*] ",lg="__wit",lh=" ,",fm=115,ky="Zero",fg=120,fl="Lia",kx=144,lD="scale_term : not implemented",kY="]",ff=117,b1=166,kX=" \n",kw="RED",lf='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',fk=128,lC="parse_zop",lB="Zmicromega",kW="0",lA=")-(",bH=136,a8=248,gV="%a * %a",bJ=164,kv="Sos_Q",ku="Sos_Z",bu=167,fj=197,le="micromega_plugin",lz=">",ee=" + ",B="Coq",kV="LRA_Q",b2=112,ld=")+(",kT="NQA",kU="AProof : ",kt="<linear_prover",kS="LRA_R",gZ="$i",an="$t",lb="[",lc="CProof : %a",S="Tauto",ks=132,kR="%a + %a",la=" Cannot find witness",kQ="the use of a specialized external tool called csdp. \n\n",k$=157,fc=183,gS="PsatzZ",ly="Rdefinitions",kP="Timeout",k_="D",k9=" Skipping what remains of this tactic: the complexity of the goal requires ",lx="A(",kN="psatz_Q",kO='" exited ',kr=216,kL="psatz_Z",kM="Rpow_def",kK="nat",k7=154,k8="PsatzQ",lw=205,gR="pure_sos",k6=207,al="VarMap",kq="%i ",aS=140,kJ="(%a)+(%a)",lu="}",lv="monoid",k=250,k5="PsatzR",kI="buggy certificate",Y="ZMicromega",ko="NRA",kp="C0",b3="plugins/micromega/certificate.ml",kn="compare_num",d=246,lt="Nia",ec=151,am="Extension: cannot occur",aF=113,km="ZArithRing",kH="{",kl="AProof : %a\n",kG="",a9=149,bI="RingMicromega",kk=134,kj="C1",gW="real nonlinear prover",gU=100,ls="%a %s %s",lr="=<",k4="positive",ki="__arith",ed="Reals",kh=", ",gQ="<=",fb=438,kF=" -> ",lq="psatz_R",gY="[%a]",kE="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",lp="Bad logical fragment",kg=215,X="plugins/micromega/g_micromega.ml4",gP="plugins/micromega/mfourier.ml",kD=206,s=124,fi=210,lo=171,kC=127,k3="CProof : ",fa=196,ln="EnvRing",k2="Refl",kf="t",fe=209,ll="linear prover",lm="Depth",lk="Sos_R",fh=114,gO="%i",kB=")^(",lj="}\n",K=Bx.jsoo_runtime,y=K.caml_check_bound,W=K.caml_equal,kc=K.caml_float_compare,a7=K.caml_fresh_oo_id,aa=K.caml_int_compare,ke=K.caml_int_of_string,gN=K.caml_lessthan,Bu=K.caml_list_of_js_array,kd=K.caml_ml_string_length,e$=K.caml_mul,c=K.caml_new_string,j=K.caml_obj_tag,a6=K.caml_register_global,w=K.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):K.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):K.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):K.caml_call_gen(a,[b,c,d])}function
I(a,b,c,d,e){return a.length==4?a(b,c,d,e):K.caml_call_gen(a,[b,c,d,e])}function
aR(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):K.caml_call_gen(a,[b,c,d,e,f])}function
E(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):K.caml_call_gen(a,[b,c,d,e,f,g])}function
eb(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):K.caml_call_gen(a,[b,c,d,e,f,g,h])}function
b0(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):K.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
Bw(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):K.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
Bv(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):K.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
v=K.caml_get_global_data(),er=[0,0,0],Bt=[12,44,[2,0,[11,c(")\n"),0]]],eA=[0,0],c7=[0,0,0],i_=[0,c(B),[0,c("Logic"),[0,c("Decidable"),0]]],gn=Bu([[0,c(B),[0,c("Lists"),[0,c("List"),0]]],[0,c(Y),0],[0,c(S),0],[0,c(bI),0],[0,c(ln),0],[0,c(B),[0,c(N),[0,c(Y),0]]],[0,c(B),[0,c(N),[0,c("RMicromega"),0]]],[0,c(B),[0,c(N),[0,c(S),0]]],[0,c(B),[0,c(N),[0,c(bI),0]]],[0,c(B),[0,c(N),[0,c(ln),0]]],[0,c(B),[0,c("QArith"),[0,c("QArith_base"),0]]],[0,c(B),[0,c(ed),[0,c(ly),0]]],[0,c(B),[0,c(ed),[0,c(kM),0]]],[0,c("LRing_normalise"),0]]),ja=[0,[0,c(B),[0,c("Numbers"),[0,c("BinNums"),0]]],0],jb=[0,[0,c(B),[0,c(ed),[0,c(ly),0]]],[0,[0,c(B),[0,c(ed),[0,c(kM),0]]],[0,[0,c(B),[0,c(ed),[0,c("Raxioms"),0]]],0]]],jc=[0,[0,c(B),[0,c("ZArith"),[0,c("BinInt"),0]]],0],j2=c(".csdp.cache"),x=c(le),h=v.Pervasives,f=v.Num,m=v.Printf,q=v.Big_int,H=v.Unix,eh=v.Marshal,eg=v.Printexc,g=v.List,bv=v.Hashtbl,G=v.Assert_failure,g2=v.Ratio,fn=v.Failure,ef=v.Set,P=v.Not_found,bQ=v.Map,bz=v.CErrors,iy=v.String,i=v.CamlinternalLazy,J=v.Names,o=v.Term,Q=v.Loc,ai=v.Tactics,af=v.Tacmach,M=v.Tacticals,bC=v.Pp,eT=v.Proofview,F=v.Coqlib,i6=v.Termops,i5=v.Invalid_argument,aN=v.Constr,i7=v.Retyping,gi=v.Goptions,z=v.Constrarg,aD=v.Tacentries,D=v.Tacinterp,V=v.Genarg,as=v.Tacenv,at=v.Mltop,aE=v.Array,lI=c(kW),lJ=[0,[12,118,[2,0,0]],c("v%s")],lK=[0,[11,c("1/("),[15,[12,41,0]]],c("1/(%a)")],lL=[0,[11,c("- ("),[15,[12,41,0]]],c("- (%a)")],lM=[0,[12,40,[15,[11,c(ld),[15,[12,41,0]]]]],c(kJ)],lN=[0,[12,40,[15,[11,c(lA),[15,[12,41,0]]]]],c(lE)],lO=[0,[12,40,[15,[11,c(")*("),[15,[12,41,0]]]]],c("(%a)*(%a)")],lP=[0,[12,40,[15,[11,c(")/("),[15,[12,41,0]]]]],c("(%a)/(%a)")],lQ=[0,[12,40,[15,[11,c(kB),[4,3,0,0,[12,41,0]]]]],c("(%a)^(%i)")],lR=[0,[11,c("Aeq("),[4,3,0,0,[12,41,0]]],c("Aeq(%i)")],lS=[0,[11,c("Ale("),[4,3,0,0,[12,41,0]]],c("Ale(%i)")],lT=[0,[11,c("Alt("),[4,3,0,0,[12,41,0]]],c("Alt(%i)")],lU=[0,[11,c("eq("),[2,0,[12,41,0]]],c("eq(%s)")],lV=[0,[11,c("le("),[2,0,[12,41,0]]],c("le(%s)")],lW=[0,[11,c("lt("),[2,0,[12,41,0]]],c("lt(%s)")],lX=[0,[12,40,[15,[11,c(")^2"),0]]],c("(%a)^2")],lY=[0,[11,c(lv),0],c(lv)],lZ=[0,[15,[11,c(cw),[15,0]]],c(gV)],l0=[0,[15,[11,c(ee),[15,0]]],c(kR)],l1=[0,[15,[11,c(cw),[15,0]]],c(gV)],l4=c(";"),ma=c("map3"),mF=[0,[11,c(fd),[2,0,[11,c(kO),[4,3,0,0,0]]]],c('command "%s" exited %i')],mE=[0,[11,c(fd),[2,0,[11,c(kO),[2,0,0]]]],c('command "%s" exited %s')],mG=[0,[11,c(fd),[2,0,[11,c('" killed '),[4,3,0,0,0]]]],c('command "%s" killed %i')],mH=[0,[11,c(fd),[2,0,[11,c('" stopped '),[4,3,0,0,0]]]],c('command "%s" stopped %i')],mv=[0,c("plugins/micromega/mutils.ml"),287,7],mp=c("select_pos"),mi=[0,0,0],mf=c("list_fold_right_elements"),md=c("try_find"),l7=c("from_option"),nV=[0,0],n6=[0,0,0],n7=[0,[0,0],0],nP=[0,0],nO=[0,0],nN=[0,0],nF=[0,[0,0]],nG=[0,[0,0]],nH=[0,[0,0]],nI=[0,[0,0]],nB=[0,[0,0]],nC=[0,[0,0]],nD=[0,[0,0]],nE=[0,[0,0]],ns=[0,[0,0],0],nq=[0,0,0],nj=[0,1],nk=[0,2],nl=[0,3],ne=[0,0],ng=[0,0],nf=[0,1],ni=[0,3],nh=[0,0],mW=[0,[1,0]],mX=[0,0,[0,0]],mY=[0,[0,0],0],mZ=[0,0],m0=[0,[1,0]],m1=[0,[1,0]],m2=[0,0],m3=[0,[1,0]],m4=[0,[1,0]],m5=[0,[1,0]],m6=[0,0],m7=[0,[1,0]],m8=[0,0,0],m9=[0,0,0],m_=[0,0],m$=[0,0,0],na=[0,0],mS=[1,0],mR=[0,0],mJ=[1,0],mK=[1,0],mL=[0,0],mN=[0,0],mM=[0,0],nx=[0,0],nA=[0,0],nT=[0,0],nX=[0,[0,0],0],nY=[0,0,0],nZ=[0,[0,0],0],n0=[0,0,0],n1=[0,[0,0],0],n2=[0,0,0],n3=[0,0,0],n4=[0,0,0],n8=[0,[0,0],0],n9=[0,0,0],n_=[0,[0,0],0],n$=[0,0,0],oa=[0,[0,0],0],ob=[0,0,0],oc=[0,0,0],od=[0,0,0],oZ=[0,[11,c(ky),0],c(ky)],o0=[0,[11,c("Hyp "),[4,3,0,0,0]],c("Hyp %i")],o1=[0,[11,c("Def "),[4,3,0,0,0]],c("Def %i")],o2=[0,[11,c("Cst "),[2,0,0]],c("Cst %s")],o3=[0,[11,c(k0),0],c(k0)],o4=[0,[11,c("P * "),[15,0]],c("P * %a")],o5=[0,[12,40,[15,[11,c(")/"),[2,0,0]]]],c("(%a)/%s")],o6=[0,[15,[11,c(cw),[15,0]]],c(gV)],o7=[0,[15,[11,c(ee),[15,0]]],c(kR)],o8=[0,[12,91,[15,[12,93,0]]],c(gY)],o9=[0,[12,46,0],c(".")],o_=[0,[4,3,0,0,[11,c(":= "),[15,[11,c(" ; "),[15,0]]]]],c("%i:= %a ; %a")],o$=[0,[4,3,0,0,[12,123,[15,[11,c(gQ),[15,[11,c(gQ),[15,[12,125,[15,0]]]]]]]]],c("%i{%a<=%a<=%a}%a")],pp=[0,0],po=[0,[11,c("apply_pivot -> {"),[15,[11,c(lj),0]]],c("apply_pivot -> {%a}\n")],pn=[0,[11,c("xpivot_eq {"),[15,[11,c("} "),[15,[12,32,[2,0,[11,c(" {"),[15,[11,c(lj),0]]]]]]]]],c("xpivot_eq {%a} %a %s {%a}\n")],pm=[0,0],pl=[0,[11,c("mult "),[2,0,[12,32,[15,[11,c(" ("),[15,[12,44,[2,0,[11,c(") -> ("),[15,Bt]]]]]]]]]],c("mult %s %a (%a,%s) -> (%a,%s)\n")],pk=[0,0],pj=[0,0,[0,0]],pi=[0,0,[0,0]],pg=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c(ls)],pf=[0,[2,0,[12,46,[15,[11,c(" +"),0]]]],c("%s.%a +")],pc=[0,c("plugins/micromega/polynomial.ml"),542,10],pb=[0,[11,c("normalise_proof "),[15,[11,c(kF),[15,0]]]],c("normalise_proof %a -> %a")],oW=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c(ls)],oU=c(gX),oT=c(kA),oL=c(kn),oM=c(kn),oP=[0,0],oK=[0,0],oH=[0,0],oF=[0,[2,0,[12,fg,[4,3,0,0,[11,c(ee),0]]]],c("%sx%i + ")],ow=[0,0],ov=[0,1],ot=[0,0],or=[0,[2,0,[12,32,0]],c("%s ")],os=[0,[2,0,[12,42,[15,[12,32,0]]]],c("%s*%a ")],oh=[0,[12,fg,[4,3,0,0,[12,46,0]]],c("x%i.")],oi=[0,[12,fg,[4,3,0,0,[12,94,[4,3,0,0,[12,46,0]]]]],c("x%i^%i.")],oI=[0,1],pA=[0,[12,72,[4,3,0,0,0]],c("H%i")],pB=[0,[11,c("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("E(%i,%a,%a)")],pC=[0,[11,c(lx),[15,[12,44,[15,[12,41,0]]]]],c("A(%a,%a)")],qD=[0,1],qE=[0,[0,0,0]],qA=c("merge_proof : pivot is not possible"),qB=[0,1],qC=[0,1],qz=[0,0],qt=[0,1],qu=[0,1],qv=[0,1],qw=[0,1],qx=[0,1],qy=[0,1],qp=[0,0],qq=[0,-1],qr=[0,[11,c("optimise Exception : "),[2,0,0]],c("optimise Exception : %s")],qn=[0,0,0],qe=[0,0],qf=[0,0],qg=[0,0],qh=[0,0],qi=[0,0],qj=[0,0],qk=[0,0],ql=[0,0],qd=[0,0],qc=c("bound_of_variable: impossible"),qb=[0,0,0],p$=[0,0,0],qa=c("SystemContradiction"),p_=[0,0],p9=[0,[4,3,0,0,[11,c(kF),[2,0,[12,10,0]]]],c("%i -> %s\n")],p6=[0,0],p5=[0,0,0,0],p3=[0,0],p1=[0,0],p2=[0,0],p4=[0,c(gP),309,4],p0=[0,c(gP),261,9],pX=[0,1],pY=[0,0],pU=[0,c(gP),199,4],pT=[0,[11,c("(val x = "),[2,0,[11,c(lh),[15,[12,44,[2,0,[12,41,0]]]]]]],c("(val x = %s ,%a,%s)")],pO=[0,[2,0,[11,c(" <= "),0]],c("%s <= ")],pP=[0,[11,c(gQ),[2,0,[12,10,0]]],c("<=%s\n")],pQ=c("\n"),pL=[0,[4,3,0,0,[12,32,0]],c(kq)],pK=c(kH),pM=c(lu),pH=[0,[4,3,0,0,[12,32,0]],c(kq)],pG=c(kH),pI=c(lu),pE=[0,[12,40,[15,[12,44,[15,[12,41,0]]]]],c("(%a,%a)")],pD=c("oo"),pv=[0,1],py=c("Mfourier.SystemContradiction"),pW=c("Mfourier.TimeOut"),ru=c(lD),rv=c("scale term: not implemented"),rw=c(lD),rz=[0,0],rA=c("term_to_q_expr: not implemented"),rF=[0,0],rG=c("term_to_z_expr: not implemented"),rM=c("Cuts should already be compiled"),sm=[0,0],sn=[0,1],so=[0,0],sp=[0,1],sq=[0,c(b3),1276,1],sd=[0,[11,c(lF),0],c(lF)],sf=[0,c(b3),1175,2],se=[0,[11,c("Found interval: "),[15,[11,c(" in ["),[2,0,[12,59,[2,0,[11,c("] -> "),0]]]]]]],c("Found interval: %a in [%s;%s] -> ")],sg=[0,0],sh=[0,1],sk=[0,c(b3),1202,2],si=[0,[11,c("xlia:  "),[15,[11,c(kX),0]]],c("xlia:  %a \n")],sj=[0,[11,c("after reduction:  "),[15,[11,c(kX),0]]],c("after reduction:  %a \n")],sb=[0,[11,c("Found a new bound "),[15,0]],c("Found a new bound %a")],r$=[0,1],sa=[0,0,0],sc=c("Interval without proof"),r_=[0,[11,c("Bad news : loss of completeness "),[15,[12,61,[2,0,0]]]],c("Bad news : loss of completeness %a=%s")],r8=[0,0],r6=[0,1],r7=[0,-1],r4=[0,1],r5=[0,-1],r0=[0,[2,0,[11,c(cw),[2,0,[11,c(ee),[2,0,[11,c(cw),[2,0,[11,c(" = "),[2,0,[12,10,0]]]]]]]]]],c("%s * %s + %s * %s = %s\n")],rV=[0,1],rW=[0,c(b3),817,5],rX=[0,0],rT=[0,[11,c(kt),0],c(kt)],rU=[0,[12,62,0],c(lz)],rS=c("proof_of_farkas : not enough hyps"),rO=[0,[11,c(kU),[15,[12,10,0]]],c(kl)],rP=[0,[11,c(k3),[15,0]],c(lc)],rN=[0,[11,c("compiled proof "),[15,[12,10,0]]],c("compiled proof %a\n")],rL=[0,[0,0]],rK=c("id_of_hyp"),rI=[0,0],rJ=[0,1],rE=[0,0],rB=[0,1],rC=[0,0],ry=c("bad index"),rt=c("pexpr_of_cstr_compat"),rr=[0,c(b3),512,9],rp=[0,c(b3),493,12],ri=[0,0],re=c("cannot happen"),rb=[0,[11,c(kU),[15,[12,10,0]]],c(kl)],rc=[0,[11,c(k3),[15,0]],c(lc)],ra=[0,[11,c("raw certificate "),[2,0,0]],c("raw certificate %s")],q7=c("make_certificate(1)"),q8=c("empty_certificate"),q2=c("= 0"),q3=c("<> 0"),q4=c("> 0"),q5=c(">= 0"),q0=[0,1],qZ=[0,0],qU=[0,0],qV=[0,[0,0]],qW=[0,0],qY=[0,c(b3),gU,1],qX=[0,0],qT=[0,[0,0]],qS=[0,[0,0]],qK=[0,0],qP=[0,[0,0],0],qQ=[0,0,0],q9=c("Certificate.Found"),q$=c("Certificate.Strict"),rY=c("Certificate.FoundProof"),r1=c("Certificate.Result"),sz=[0,0,0],sx=[0,0,0],sv=[0,0,[0,5,0]],sy=[0,1,[0,4,[0,5,0]]],sw=[0,1,[0,6,[0,5,0]]],su=[0,1,[0,6,[0,5,0]]],ss=c("Persistent_cache.PHashtable(Key).InvalidTableFormat"),st=c("Persistent_cache.PHashtable(Key).UnboundTable"),sT=c("tt"),sU=c("ff"),sV=c("X "),sW=[0,[11,c(lx),[15,[12,41,0]]],c("A(%a)")],sX=[0,[11,c("C("),[15,[12,44,[15,[12,41,0]]]]],c("C(%a,%a)")],sY=[0,[11,c("D("),[15,[12,44,[15,[12,41,0]]]]],c("D(%a,%a)")],sZ=[0,[11,c("N("),[15,[12,41,0]]],c("N(%a)")],s1=c(kG),s0=[0,[11,c("I("),[15,[2,0,[12,44,[15,[12,41,0]]]]]],c("I(%a%s,%a)")],xu=[0,0],xI=c("[]"),xJ=[0,[12,91,[15,[12,93,0]]],c(gY)],xK=[0,[12,91,[15,[11,c(kh),[15,[11,c(kh),[15,[12,93,0]]]]]]],c("[%a, %a, %a]")],xM=[0,[12,68,0],c(k_)],xN=[0,[11,c("R["),[15,[12,44,[15,[12,93,0]]]]],c("R[%a,%a]")],xO=[0,[11,c("C["),[15,[12,44,[15,[12,93,0]]]]],c("C[%a,%a]")],xP=c(kY),xQ=c(lb),xR=[0,[11,c("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],c("EP[%a,%a,%a]")],x4=c("abstract_wrt_formula"),y0=c(g0),yY=c(g0),yV=c(g0),yP=c(gW),yO=c(gW),yN=c(gW),yy=c(kI),yx=c(kI),yt=c("csdpcert"),yu=c(N),yv=c("plugins"),yp=c(ki),yq=[0,0],yr=c(la),yj=c(lp),yk=c(kP),yl=c(kE),ym=c(lf),yn=c(kQ),yo=c(k9),yd=c(lg),ye=c(kf),yf=[0,[0,c(B),[0,c(N),[0,c(al),0]]],[0,[0,c(al),0],0]],yg=c(al),yh=c(k1),yi=c(lG),ya=c(ki),yb=[0,0],yc=c(la),x6=c(lp),x7=c(kP),x8=c(kE),x9=c(lf),x_=c(kQ),x$=c(k9),x2=c("bad old index"),x3=c("proof compaction error"),x0=[0,[15,[11,c(" ;"),0]],c("%a ;")],xZ=c(lb),x1=c(kY),xX=[0,0],xU=c(lg),xV=c(k1),xW=c(lG),xL=[0,[15,[12,47,[15,0]]],c("%a/%a")],xF=c(kf),xG=[0,[0,c(B),[0,c(N),[0,c(al),0]]],[0,[0,c(al),0],0]],xH=c(al),xC=c("Empty"),xD=[0,[0,c(B),[0,c(N),[0,c(al),0]]],[0,[0,c(al),0],0]],xE=c(al),xz=c("Leaf"),xA=[0,[0,c(B),[0,c(N),[0,c(al),0]]],[0,[0,c(al),0],0]],xB=c(al),xw=c("Node"),xx=[0,[0,c(B),[0,c(N),[0,c(al),0]]],[0,[0,c(al),0],0]],xy=c(al),v$=c(kp),wa=c(kj),wb=c("CQ _"),wc=[0,[12,40,[15,[11,c(ee),[15,[12,41,0]]]]],c("(%a + %a)")],wd=[0,[12,40,[15,[11,c(" - "),[15,[12,41,0]]]]],c("(%a - %a)")],we=[0,[12,40,[15,[11,c(cw),[15,[12,41,0]]]]],c("(%a * %a)")],wf=[0,[11,c("(/ "),[15,[12,41,0]]],c("(/ %a)")],wg=[0,[11,c("(- "),[15,[12,41,0]]],c("(- %a)")],w_=[0,0,0],xr=[0,[11,c(lH),[4,3,0,0,0]],c(li)],xq=[0,[11,c(lH),[4,3,0,0,0]],c(li)],xp=[0,[11,c("__x"),[4,3,0,0,0]],c("__x%i")],xe=c("error : parse_arith(2)"),xc=c("parse_qexpr parse error"),xa=[0,0],wW=c("get_rank"),wT=[1,c("Oups")],wR=c(lC),wQ=c(lC),wN=[0,[12,40,[15,[12,32,[15,[12,32,[15,[12,41,0]]]]]]],c("(%a %a %a)")],wG=[0,[12,61,0],c(kA)],wH=[0,[11,c(kZ),0],c(kZ)],wI=[0,[11,c(lr),0],c(lr)],wJ=[0,[11,c(gX),0],c(gX)],wK=[0,[12,60,0],c("<")],wL=[0,[12,62,0],c(lz)],wz=[0,[12,48,0],c(kW)],wA=[0,[11,c("(In "),[15,[12,41,[12,37,[11,c(kK),0]]]]],c("(In %a)%%nat")],wB=[0,[12,40,[15,[11,c("^2)"),0]]],c("(%a^2)")],wC=[0,[11,c("( "),[15,[11,c(kz),[15,[12,41,0]]]]],c("( %a [*] %a)")],wD=[0,[12,40,[15,[11,c(kz),[15,[12,41,0]]]]],c("(%a [*] %a)")],wE=[0,[12,40,[15,[11,c(" [+] "),[15,[12,41,0]]]]],c("(%a [+] %a)")],wF=[0,[12,40,[15,[12,41,[12,37,[11,c(k4),0]]]]],c("(%a)%%positive")],ww=[0,[12,91,[15,[12,93,0]]],c(gY)],wv=[0,[12,40,[15,[12,32,[12,64,[15,[12,41,0]]]]]],c("(%a @%a)")],wr=[0,[11,c("Pc "),[15,0]],c("Pc %a")],ws=[0,[11,c("Pinj("),[15,[12,44,[15,[12,41,0]]]]],c("Pinj(%a,%a)")],wt=[0,[11,c("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("PX(%a,%a,%a)")],wl=[0,[11,c("V "),[15,0]],c("V %a")],wm=[0,[12,40,[15,[11,c(ld),[15,[12,41,0]]]]],c(kJ)],wn=[0,[12,40,[15,[11,c(lA),[15,[12,41,0]]]]],c(lE)],wo=[0,[15,[11,c("*("),[15,[12,41,0]]]],c("%a*(%a)")],wp=[0,[11,c("-("),[15,[12,41,0]]],c("-(%a)")],wq=[0,[12,40,[15,[11,c(kB),[15,[12,41,0]]]]],c("(%a)^(%a)")],wi=[0,[15,[11,c(lh),[15,0]]],c("%a ,%a")],wj=[0,[15,0],c("%a")],wk=[0,[2,0,[15,[2,0,0]]],c("%s%a%s")],v9=[0,[2,0,0],c("%s")],v7=[0,[4,3,0,0,0],c(gO)],v5=[0,[4,3,0,0,0],c(gO)],v4=[0,[4,3,0,0,0],c(gO)],v0=c("ukn"),v1=c("BadTerm"),v2=c("Goal"),vV=c("Formula"),vW=[0,[0,c(B),[0,c(N),[0,c(bI),0]]],[0,[0,c(bI),0],0]],vX=c(bI),vS=c("Build_Formula"),vT=[0,[0,c(B),[0,c(N),[0,c(bI),0]]],[0,[0,c(bI),0],0]],vU=c(bI),vO=c("N_of_Z"),vP=[0,[0,c(B),[0,c("setoid_ring"),[0,c(km),0]]],0],vQ=c(km),vK=c("ZWitness"),vL=[0,[0,c(B),[0,c(N),[0,c(Y),0]]],0],vM=c(gT),vG=c("QWitness"),vH=[0,[0,c(B),[0,c(N),[0,c(gT),0]]],0],vI=c(gT),vC=c("BFormula"),vD=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vE=c(Y),vz=c("I"),vA=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vB=c(Y),vw=c("X"),vx=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vy=c(Y),vt=c("A"),vu=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vv=c(Y),vq=c("N"),vr=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vs=c(Y),vn=c(k_),vo=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vp=c(Y),vk=c("Cj"),vl=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vm=c(Y),vh=c("FF"),vi=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vj=c(Y),ve=c("TT"),vf=[0,[0,c(B),[0,c(N),[0,c(S),0]]],[0,[0,c(S),0],0]],vg=c(Y),va=c("make_conj"),vb=[0,[0,c(k2),0],0],vc=c(lB),u8=c("make_impl"),u9=[0,[0,c(k2),0],0],u_=c(lB),u6=c("coneMember"),u5=c(gS),u4=c("PsatzC"),u3=c("PsatzAdd"),u2=c("PsatzMulC"),u1=c("PsatzMulE"),u0=c("PsatzSquare"),uZ=c("PsatzIn"),uY=c("OpGt"),uX=c("OpGe"),uW=c("OpLt"),uV=c("OpLe"),uU=c("OpNEq"),uT=c("OpEq"),uS=c("Pinj"),uR=c("Pc"),uQ=c("PX"),uP=c("PEpow"),uO=c("PEsub"),uN=c("PEmul"),uM=c("PEopp"),uL=c("PEadd"),uK=c("PEc"),uJ=c("PEX"),uI=c("IQR"),uH=c("IZR"),uG=c("pow"),uF=c("Rinv"),uD=c("Rdiv"),uC=c("Rmult"),uB=c("Ropp"),uA=c("Rminus"),uz=c("Rplus"),uy=c("Rlt"),ux=c("Rle"),uw=c("Rge"),uv=c("Rgt"),uu=c("Qpower"),ut=c("Qmult"),us=c("Qopp"),ur=c("Qminus"),uq=c("Qplus"),up=c("Qeq"),uo=c("Qlt"),un=c("Qle"),ul=c("Qge"),uj=c("Qgt"),ui=c("Z.pow"),uh=c("Z.mul"),ug=c("Z.opp"),uf=c("Z.sub"),ue=c("Z.add"),ud=c("eq"),uc=c("Z.lt"),ub=c("Z.le"),ua=c("Z.ge"),t$=c("Z.gt"),t9=c("EnumProof"),t7=c("CutProof"),t5=c("RatProof"),t3=c("DoneProof"),t1=c("ZArithProof"),t0=c("R1"),tZ=c("R0"),tY=c("COpp"),tX=c("CInv"),tW=c("CMult"),tV=c("CMinus"),tU=c("CPlus"),tT=c("CZ"),tS=c("CQ"),tR=c(kj),tQ=c(kp),tO=c("Rcst"),tN=c("Qmake"),tL=c("Build_Witness"),tK=c("R"),tJ=c("Q"),tI=c("Zneg"),tH=c("Zpos"),tG=c("Z0"),tF=c("Z"),tE=c("xI"),tD=c("xO"),tC=c("xH"),tA=c(k4),ty=c("option"),tw=c("None"),tv=c("pair"),tu=c("Npos"),tt=c("N0"),tr=c(kK),tq=c("S"),tp=c("O"),tn=c("list"),tm=c("nil"),tl=c("cons"),tk=c("False"),tj=c("True"),ti=c("iff"),te=c("not"),td=c("or"),tc=c("and"),s3=[0,0,0],sK=c(kG),sJ=[0,[11,c("time "),[2,0,[12,32,[8,0,0,0,[12,10,0]]]]],c("time %s %f\n")],sM=[0,c(fl),[0,c("Enum"),0]],sN=c("Lia Enum"),sP=[0,c("Lra"),[0,c(lm),0]],sR=[0,c(fl),[0,c(lm),0]],s8=c(Y),s9=c(Y),s_=c(Y),s$=c(Y),ta=c(Y),tb=c(Y),tf=c("Coq.Init.Logic.not"),v3=c("Coq_micromega.M.ParseError"),x5=c("Coq_micromega.CsdpNotFound"),ys=c("csdp"),yA=c(".lia.cache"),yC=c(".nia.cache"),yE=c(".nra.cache"),yH=c(ll),yK=c(ll),yM=c("nra"),yQ=c("lia"),yR=c("nlia"),y1=c(gR),y4=c(gR),y7=c(gR),Bq=[0,c(X),1,0],Bo=[0,c(X),1,0],Bl=[0,c(X),1,0],Bk=c(an),Bm=[0,c(kN)],Bn=c(an),Bp=c(gZ),Br=[0,c(kN)],Bs=c(k8),Bf=c(am),Bd=c(am),A$=[0,c(X),1,0],A9=[0,c(X),1,0],A6=[0,c(X),1,0],A5=c(an),A7=[0,c(lq)],A8=c(an),A_=c(gZ),Ba=[0,c(lq)],Bb=c(k5),A0=c(am),AY=c(am),AU=[0,c(X),1,0],AT=c(an),AV=[0,c("lra_R")],AW=c(kS),AO=c(am),AK=[0,c(X),1,0],AJ=c(an),AL=[0,c("lra_Q")],AM=c(kV),AE=c(am),AA=[0,c(X),1,0],Az=c(an),AB=[0,c("sos_R")],AC=c(lk),Au=c(am),Aq=[0,c(X),1,0],Ap=c(an),Ar=[0,c("sos_Q")],As=c(kv),Ak=c(am),Ag=[0,c(X),1,0],Af=c(an),Ah=[0,c("sos_Z")],Ai=c(ku),Aa=c(am),z8=[0,c(X),1,0],z7=c(an),z9=[0,c("xnqa")],z_=c(kT),z2=c(am),zY=[0,c(X),1,0],zX=c(an),zZ=[0,c("xnra")],z0=c(ko),zS=c(am),zO=[0,c(X),1,0],zN=c(an),zP=[0,c("xnlia")],zQ=c(lt),zI=c(am),zE=[0,c(X),1,0],zD=c(an),zF=[0,c("xlia")],zG=c(fl),zy=c(am),zu=[0,c(X),1,0],zs=[0,c(X),1,0],zp=[0,c(X),1,0],zo=c(an),zq=[0,c(kL)],zr=c(an),zt=c(gZ),zv=[0,c(kL)],zw=c(gS),zj=c(am),zh=c(am),zc=c(kw),zd=c("myred"),za=c(le),zf=c(kw),zm=c(gS),zB=c(fl),zL=c(lt),zV=c(ko),z5=c(kT),Ad=c(ku),An=c(kv),Ax=c(lk),AH=c(kV),AR=c(kS),A3=c(k5),Bi=c(k8),sr=v.End_of_file,sG=v.Coq_config,sF=v.Envars,sE=v.Filename,sB=v.Reductionops,sA=v.System,sC=v.Nametab,sD=v.Libnames;function
ao(d,c){if(typeof
c==="number")return b(h[49],d,lI);else
switch(c[0]){case
0:var
n=a(f[40],c[1]);return b(h[49],d,n);case
1:return e(m[1],d,lJ,c[1]);case
2:return I(m[1],d,lK,ao,c[1]);case
3:return I(m[1],d,lL,ao,c[1]);case
4:var
g=c[1];return E(m[1],d,lM,ao,g[1],ao,g[2]);case
5:var
i=c[1];return E(m[1],d,lN,ao,i[1],ao,i[2]);case
6:var
j=c[1];return E(m[1],d,lO,ao,j[1],ao,j[2]);case
7:var
k=c[1];return E(m[1],d,lP,ao,k[1],ao,k[2]);default:var
l=c[1];return aR(m[1],d,lQ,ao,l[1],l[2])}}function
b5(d,c){switch(c[0]){case
0:return e(m[1],d,lR,c[1]);case
1:return e(m[1],d,lS,c[1]);case
2:return e(m[1],d,lT,c[1]);case
3:var
g=a(f[40],c[1]);return e(m[1],d,lU,g);case
4:var
h=a(f[40],c[1]);return e(m[1],d,lV,h);case
5:var
i=a(f[40],c[1]);return e(m[1],d,lW,i);case
6:return I(m[1],d,lX,ao,c[1]);case
7:return b(m[1],d,lY);case
8:return E(m[1],d,lZ,ao,c[1],b5,c[2]);case
9:return E(m[1],d,l0,b5,c[1],b5,c[2]);default:return E(m[1],d,l1,b5,c[1],b5,c[2])}}var
g1=[0,ao,b5];a6(727,g1,"Micromega_plugin.Sos_types");var
l2=0;function
l3(e,c,d){var
a=d;for(;;){if(a){var
f=a[2];b(e,c,a[1]);b(h[49],c,l4);var
a=f;continue}return 0}}function
g3(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
l5(c,b){return b?[0,a(c,b[1])]:0}function
l6(b){return b?b[1]:a(h[2],l7)}function
l8(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1][1],d);if(c)return c;var
b=f;continue}return 0}}function
l9(e,d){var
c=0,a=d;for(;;){if(a){var
f=a[2];b(e,c,a[1]);var
c=c+1|0,a=f;continue}return 0}}function
l_(h,f){var
c=0,a=f;for(;;){if(a){var
d=a[2],j=a[1],i=function(d){return function(c,a){return[0,b(h,d,a),c]}}(j),c=e(g[16],i,c,d),a=d;continue}return c}}function
l$(f,d){var
c=0,a=d;for(;;){if(a){var
i=a[2],j=a[1],h=function(d){return function(c,a){return[0,b(f,d,a),c]}}(j),c=e(g[16],h,c,a),a=i;continue}return c}}function
g4(f,d,c,b){if(d){if(c)if(b){var
g=b[1],i=c[1],j=d[1],k=g4(f,d[2],c[2],b[2]);return[0,e(f,j,i,g),k]}}else
if(!c)if(!b)return 0;return a(h[1],ma)}function
mb(g,f,e){var
c=f,a=e;for(;;){if(c){var
h=c[2],i=c[1];if(a){var
d=a[2];if(b(g,i,a[1])){var
c=h,a=d;continue}var
a=d;continue}return 0}return 1}}function
mc(c){return function(d){var
b=d;for(;;){if(b){var
e=b[2],f=b[1];try{var
g=a(c,f);return g}catch(a){a=w(a);if(a[1]===fn){var
b=e;continue}throw a}}return a(h[2],md)}}}function
me(g,c){function
d(c){if(c){var
e=c[2],f=c[1];return e?b(g,f,d(e)):f}return a(h[1],mf)}return d(c)}function
mg(e,d){var
a=[0,0,d];for(;;){var
b=a[2],c=a[1];if(b<e)return c;var
a=[0,[0,b,c],b-1|0];continue}}function
mh(h,b){function
c(e,b){var
c=e[2],d=e[1];if(d)return[0,d,[0,b,c]];var
f=a(h,b);return f?[0,[0,[0,f[1],b]],c]:[0,d,[0,b,c]]}return e(g[16],c,mi,b)}function
g5(d,c){var
a=b(q[17],d,c),e=b(q[15],d,a),f=b(q[15],c,a),g=b(q[10],e,f);return b(q[10],a,g)}function
ei(b){return 2===b[0]?a(g2[3],b[1]):q[2]}function
ej(b){switch(b[0]){case
0:return a(q[35],b[1]);case
1:return b[1];default:return a(g2[2],b[1])}}function
g6(d,c){var
b=d,a=c;for(;;){if(a){var
e=a[2],b=g5(b,ei(a[1])),a=e;continue}return b}}function
g7(e,d){var
c=e,a=d;for(;;){if(a){var
f=a[2],g=ej(a[1]),c=b(q[17],c,g),a=f;continue}return c}}function
mj(c){var
a=g7(q[1],c);return 0===b(q[23],a,q[1])?q[2]:a}function
mk(a){var
c=g6(q[2],a);function
d(a){var
d=ei(a),e=ej(a),f=b(q[10],e,c);return b(q[15],f,d)}return b(g[13],d,a)}function
fo(e,a){function
c(d,a){if(a){var
f=a[1],g=c(d+1|0,a[2]);return[0,b(e,f,d),g]}return 0}return c(0,a)}function
ml(c,b){var
d=fo(function(d,b){return[0,b,a(c,d)]},b);return a(g[6],d)}function
g8(c,b){var
d=c+a(g[1],b)|0;return[0,fo(function(b,a){return[0,b,a+c|0]},b),d]}function
mm(a){function
b(e,a){if(a){var
c=a[1],f=a[2],g=c[1],d=g8(e,c[2]),h=d[1];return[0,[0,g,h],b(d[2],f)]}return 0}return b(0,a)}function
mn(h,a){function
c(i){var
a=i;for(;;){if(a){var
d=a[2],e=a[1],f=e[2],j=e[1],k=function(a){return a[2]},l=b(g[13],k,f),m=function(a){return b(g[27],a,h)};if(b(g[24],m,l))return[0,[0,j,f],c(d)];var
a=d;continue}return 0}}return c(a)}function
mo(c,b){function
e(i,d,g){var
c=i,b=g;for(;;){if(d){var
j=d[2],k=d[1];if(b){var
f=b[2],l=b[1];if(c===k)return[0,l,e(c+1|0,j,f)];var
c=c+1|0,b=f;continue}return a(h[2],mp)}return 0}}return e(0,c,b)}function
g9(a){return a?g9(a[1])+1|0:0}function
b6(a){return typeof
a==="number"?1:0===a[0]?1+(2*b6(a[1])|0)|0:2*b6(a[1])|0}function
mq(a){return a?b6(a[1]):0}function
fp(a){return typeof
a==="number"?1:0===a[0]?1+(2*fp(a[1])|0)|0:2*fp(a[1])|0}function
mr(a){return typeof
a==="number"?0:0===a[0]?b6(a[1]):-b6(a[1])|0}function
cx(a){if(typeof
a==="number")return q[2];else{if(0===a[0]){var
c=cx(a[1]),d=b(q[11],2,c);return b(q[7],1,d)}var
e=cx(a[1]);return b(q[11],2,e)}}function
ek(b){if(typeof
b==="number")return q[1];else{if(0===b[0])return cx(b[1]);var
c=cx(b[1]);return a(q[3],c)}}function
ms(a){return[1,ek(a)]}var
mt=[0,g9,b6,mq,fp,mr,cx,ek,ms,function(a){var
c=a[1],d=[1,ek([0,a[2]])],e=[1,ek(c)];return b(f[9],e,d)}];function
g_(a){return 0===a?0:[0,g_(a-1|0)]}function
b7(a){return 1===a?0:1===(a&1)?[0,b7(a>>>1|0)]:[1,b7(a>>>1|0)]}function
mu(a){if(0<=a)return 0===a?0:[0,b7(a)];throw[0,G,mv]}function
fq(a){return 1===a?0:1===(a&1)?[0,fq(a>>>1|0)]:[1,fq(a>>>1|0)]}function
mw(a){var
b=aa(a,0);return 0===b?0:1===b?[0,b7(a)]:[1,b7(-a|0)]}function
el(d){var
f=a(q[35],2);function
c(a){if(b(q[24],a,q[2]))return 0;var
d=b(q[14],a,f),e=d[1];return b(q[24],q[2],d[2])?[0,c(e)]:[1,c(e)]}return c(d)}function
g$(b){var
c=a(q[22],b);return 0===c?0:1===c?[0,el(b)]:[1,el(a(q[3],b))]}var
mx=[0,g_,b7,mu,fq,mw,el,g$,function(a){var
b=el(ei(a));return[0,g$(ej(a)),b]}];function
my(d){var
b=d;for(;;){if(b){var
e=b[2],c=a(b[1],0);if(0===c){var
b=e;continue}return c}return 0}}function
mz(g,f,e){var
c=f,a=e;for(;;){if(c){if(a){var
h=a[2],i=c[2],d=b(g,c[1],a[1]);if(0===d){var
c=i,a=h;continue}return d}return 1}return a?-1:0}}var
mA=[0,my,mz,function(e,d){var
b=d,c=0;for(;;){if(b){var
f=b[2],g=a(e,b[1])^c,b=f,c=g;continue}return c^a(bv[20],0)}}];function
mB(a){return a}function
mC(a){return a+1|0}var
ha=[0,mB,mC,function(d,c){var
e=a(h[20],c);return b(h[49],d,e)},aa],mD=a(ef[1],[0,ha[4]]);function
hb(a){for(;;)try{var
d=b(H[13],0,a)[2];return d}catch(a){a=w(a);if(a[1]===H[1]){var
c=a[2];if(typeof
c==="number")if(11===c)continue}throw a}}var
l=[0,l2,l3,g3,l5,l6,l8,l9,l_,l$,g4,mb,mc,me,mg,mh,g5,ei,ej,g6,g7,mj,mk,fo,ml,g8,mm,mn,mo,mt,mx,mA,ha,mD,hb,function(c,s,r){var
f=a(H[64],0),i=f[2],j=f[1],k=a(H[64],0),l=k[2],n=k[1],o=a(H[64],0),p=o[2],t=o[1],u=aR(H[66],c,s,j,l,p),q=a(H[29],i);b(h[55],q,r);a(h[46],q);var
d=hb(u);function
v(e){var
c=[0,j,[0,i,[0,n,[0,l,[0,t,[0,p,0]]]]]];function
d(b){try{var
c=a(H[22],b);return c}catch(a){return 0}}return b(g[11],d,c)}return g3(function(p){switch(d[0]){case
0:var
b=d[1];if(0===b){var
f=a(H[28],n);try{var
j=a(eh[3],f);return j}catch(b){b=w(b);var
g=a(eg[1],b),i=e(m[4],mE,c,g);return a(h[2],i)}}var
k=e(m[4],mF,c,b);return a(h[2],k);case
1:var
l=e(m[4],mG,c,d[1]);return a(h[2],l);default:var
o=e(m[4],mH,c,d[1]);return a(h[2],o)}},v)}];a6(739,l,"Micromega_plugin.Mutils");function
bK(a){return 0===a?1:0}function
em(a,b){if(a){var
c=a[1];return[0,c,em(a[2],b)]}return b}function
hc(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
fr(b,a){return b?[0,fr(b[1],a)]:a}var
mI=[0];function
a_(a){return typeof
a==="number"?mJ:0===a[0]?[1,a_(a[1])]:[0,a[1]]}function
b8(b,a){if(typeof
b==="number")return typeof
a==="number"?mK:0===a[0]?[1,a_(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,a_(c)]:0===a[0]?[1,cy(c,a[1])]:[0,b8(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,b8(d,a[1])]:[1,b8(d,a[1])]}}function
cy(b,a){if(typeof
b==="number")return typeof
a==="number"?mL:0===a[0]?[0,a_(a[1])]:[1,a_(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,a_(c)]:0===a[0]?[0,cy(c,a[1])]:[1,cy(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,a_(d)]:0===a[0]?[1,cy(d,a[1])]:[0,b8(d,a[1])]}}function
cz(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,cz(a[1])]}function
cA(a){return typeof
a==="number"?0===a?mM:1:[0,[0,a[1]]]}function
cB(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
hd(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,cz(a[1])]]}function
b9(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?cB(b9(c,a[1])):cA(b9(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,cz(d)]:0===a[0]?cA(cC(d,a[1])):cB(b9(d,a[1]))}}function
cC(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,cz(c)]:0===a[0]?cA(cC(c,a[1])):cB(b9(c,a[1]))}var
d=b[1];return typeof
a==="number"?hd(d):0===a[0]?cB(cC(d,a[1])):cA(cC(d,a[1]))}}function
fs(c,b){var
a=b9(c,b);return typeof
a==="number"?0:a[1]}function
ft(b,a){return typeof
b==="number"?a:0===b[0]?b8(a,[1,ft(b[1],a)]):[1,ft(b[1],a)]}function
cD(a){return typeof
a==="number"?mN:0===a[0]?[0,cD(a[1])]:[0,cD(a[1])]}function
he(h,g,f){var
c=h,b=g,a=f;for(;;)if(typeof
b==="number")return typeof
a==="number"?c:1;else{if(0===b[0]){var
d=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
b=d,a=a[1];continue}var
c=2,b=d,a=a[1];continue}}var
e=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
c=1,b=e,a=a[1];continue}var
b=e,a=a[1];continue}}}var
mO=0;function
hf(a,b){return he(mO,a,b)}function
fu(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(hf(e,f)){case
0:return b;case
1:var
c=d,a=b,b=fs(f,e);continue;default:var
c=d,b=fs(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,fu(d,g,a[1])]}}}return 0}}function
mP(b,a){var
c=cD(a);return fu(fr(cD(b),c),b,a)}function
hg(a){return a?a_(hg(a[1])):0}var
A=[0,a_,b8,cy,cz,cA,cB,hd,b9,cC,fs,ft,cD,he,hf,fu,mP,hg],mQ=[0,function(b){return b?[0,a(A[17],b[1])]:0}];function
en(a,d,c){if(typeof
c==="number")return d;else{if(0===c[0]){var
e=en(a,d,c[1]);return b(a,d,b(a,e,e))}var
f=en(a,d,c[1]);return b(a,f,f)}}function
hh(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
b_(c,b){if(b){var
d=b[1],e=b_(c,b[2]);return[0,a(c,d),e]}return 0}function
fv(d,c,a){if(a){var
e=a[1];return b(d,e,fv(d,c,a[2]))}return c}function
fw(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
hi(b){return typeof
b==="number"?mR:0===b[0]?[0,[0,b[1]]]:[1,a(A[4],b[1])]}function
hj(b){return typeof
b==="number"?mS:0===b[0]?[0,a(A[4],b[1])]:[1,[0,b[1]]]}function
bL(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?[1,[1,b[1]]]:[1,a(A[4],b[1])];else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?[0,[1,d]]:0===b[0]?fw(bL(d,b[1])):hi(bL(d,b[1]))}var
e=c[1];return typeof
b==="number"?[0,a(A[4],e)]:0===b[0]?hj(bL(e,b[1])):fw(bL(e,b[1]))}}function
bw(c,a){if(typeof
c==="number")return a;else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?c:0===a[0]?[0,b(A[2],d,a[1])]:bL(d,a[1])}var
e=c[1];return typeof
a==="number"?c:0===a[0]?bL(a[1],e):[1,b(A[2],e,a[1])]}}function
bM(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
eo(b,a){return bw(b,bM(a))}function
bN(c,a){if(typeof
c==="number")return 0;else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?0:0===a[0]?[0,b(A[11],d,a[1])]:[1,b(A[11],d,a[1])]}var
e=c[1];return typeof
a==="number"?0:0===a[0]?[1,b(A[11],e,a[1])]:[0,b(A[11],e,a[1])]}}function
cE(c,a){if(typeof
c==="number")return typeof
a==="number"?0:0===a[0]?1:2;else{if(0===c[0]){var
d=c[1];if(typeof
a!=="number"&&0===a[0])return b(A[14],d,a[1]);return 2}var
e=c[1];if(typeof
a!=="number"&&1===a[0])return hc(b(A[14],e,a[1]));return 1}}function
hk(b,a){return 2<=cE(b,a)?0:1}function
fx(b,a){return 1===cE(b,a)?1:0}function
mT(b,a){return 2<=cE(b,a)?1:0}function
mU(b,a){return 1===cE(b,a)?a:b}function
ep(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
mV(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
bO(b,a){if(typeof
b==="number")return hk(mW,a)?mX:mY;else{if(0===b[0]){var
e=bO(b[1],a),f=e[1],c=bw(bN(m0,e[2]),mZ);if(fx(c,a))return[0,bN(m1,f),c];var
i=eo(c,a);return[0,bw(bN(m3,f),m2),i]}var
g=bO(b[1],a),h=g[1],d=bN(m4,g[2]);if(fx(d,a))return[0,bN(m5,h),d];var
j=eo(d,a);return[0,bw(bN(m7,h),m6),j]}}function
hl(b,a){if(typeof
b==="number")return m8;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return m9;else{if(0===a[0])return bO(c,a);var
d=bO(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,bM(f),0];var
l=bw(a,e);return[0,bM(bw(f,m_)),l]}}var
g=b[1];if(typeof
a==="number")return m$;else{if(0===a[0]){var
h=bO(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,bM(j),0];var
m=eo(a,i);return[0,bM(bw(j,na)),m]}var
k=bO(g,[0,a[1]]),n=k[1];return[0,n,bM(k[2])]}}}function
nb(b,a){return hl(b,a)[1]}var
u=[0,fw,hi,hj,bL,bw,bM,eo,bN,cE,hk,fx,mT,mU,ep,mV,bO,hl,nb,function(c,a){if(typeof
c==="number")return ep(a);else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?ep(c):0===a[0]?[0,b(A[16],d,a[1])]:[0,b(A[16],d,a[1])]}var
e=c[1];return typeof
a==="number"?ep(c):0===a[0]?[0,b(A[16],e,a[1])]:[0,b(A[16],e,a[1])]}}];function
aG(c,a){return 0===b(u[9],c,a)?1:0}function
nc(a){return[0,a]}function
nd(a){return[0,a]}function
fy(d,f,e){var
c=f,a=e;for(;;)switch(c[0]){case
0:var
g=c[1];return 0===a[0]?b(d,g,a[1]):0;case
1:var
h=c[2],i=c[1];if(1===a[0]){var
j=a[2];if(0===b(A[14],i,a[1])){var
c=h,a=j;continue}return 0}return 0;default:var
k=c[3],l=c[2],m=c[1];if(2===a[0]){var
n=a[3],o=a[1];if(0===b(A[14],l,a[2])){if(fy(d,m,o)){var
c=k,a=n;continue}return 0}return 0}return 0}}function
Z(c,a){switch(a[0]){case
0:return a;case
1:var
d=a[2];return[1,b(A[2],c,a[1]),d];default:return[1,c,a]}}function
hm(b,c){return typeof
b==="number"?c:0===b[0]?[1,[1,b[1]],c]:[1,a(A[4],b[1]),c]}function
O(f,e,a,d,c){switch(a[0]){case
0:return b(e,a[1],f)?Z(0,c):[2,a,d,c];case
1:return[2,a,d,c];default:var
g=a[2],h=a[1];return fy(e,a[3],[0,f])?[2,h,b(A[2],g,d),c]:[2,a,d,c]}}function
hn(c,b,a){return[2,[0,b],a,[0,c]]}function
ho(b,a){return hn(b,a,0)}function
ag(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1];return[1,d,ag(c,b[2])];default:var
e=b[2],f=b[1],g=ag(c,b[3]);return[2,ag(c,f),e,g]}}function
bx(d,a,c){switch(a[0]){case
0:return[0,b(d,a[1],c)];case
1:var
e=a[1];return[1,e,bx(d,a[2],c)];default:var
f=a[2],g=a[1];return[2,g,f,bx(d,a[3],c)]}}function
b$(d,a,c){switch(a[0]){case
0:return[0,b(d,a[1],c)];case
1:var
e=a[1];return[1,e,b$(d,a[2],c)];default:var
f=a[2],g=a[1];return[2,g,f,b$(d,a[3],c)]}}function
cF(g,f,e,c,d){switch(d[0]){case
0:return Z(c,bx(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=b(u[4],m,c);return typeof
h==="number"?Z(c,b(f,i,e)):0===h[0]?Z(c,b(f,[1,h[1],i],e)):Z(m,cF(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
c==="number"?[2,l,k,b(f,j,e)]:0===c[0]?[2,l,k,cF(g,f,e,[1,c[1]],j)]:[2,l,k,cF(g,f,e,a(A[4],c[1]),j)]}}function
cG(h,g,f,e,c,d){switch(d[0]){case
0:var
o=d[1];return Z(c,bx(h,ag(g,e),o));case
1:var
j=d[2],n=d[1],i=b(u[4],n,c);return typeof
i==="number"?Z(c,b(f,j,e)):0===i[0]?Z(c,b(f,[1,i[1],j],e)):Z(n,cG(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
c==="number"?[2,m,l,b(f,k,e)]:0===c[0]?[2,m,l,cG(h,g,f,e,[1,c[1]],k)]:[2,m,l,cG(h,g,f,e,a(A[4],c[1]),k)]}}function
fz(f,g,j,d,e,c){switch(c[0]){case
0:return[2,d,e,c];case
1:var
k=c[2],h=c[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,a(A[4],h[1]),k]];default:var
l=c[3],m=c[2],n=c[1],i=b(u[4],m,e);return typeof
i==="number"?O(f,g,b(j,n,d),m,l):0===i[0]?O(f,g,b(j,[2,n,i[1],[0,f]],d),e,l):O(f,g,fz(f,g,j,d,i[1],n),m,l)}}function
fA(g,f,h,k,d,e,c){switch(c[0]){case
0:return[2,ag(f,d),e,c];case
1:var
l=c[2],i=c[1];if(typeof
i==="number")return[2,ag(f,d),e,l];else{if(0===i[0]){var
p=[1,[1,i[1]],l];return[2,ag(f,d),e,p]}var
q=[1,a(A[4],i[1]),l];return[2,ag(f,d),e,q]}default:var
m=c[3],n=c[2],o=c[1],j=b(u[4],n,e);return typeof
j==="number"?O(g,h,b(k,o,d),n,m):0===j[0]?O(g,h,b(k,[2,o,j[1],[0,g]],d),e,m):O(g,h,fA(g,f,h,k,d,j[1],o),n,m)}}function
_(c,e,d,f,g){switch(g[0]){case
0:return bx(e,f,g[1]);case
1:var
q=g[2],r=g[1];return cF(e,function(a,b){return _(c,e,d,a,b)},q,r,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,bx(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,_(c,e,d,m,h)]:0===k[0]?[2,i,j,_(c,e,d,[1,[1,k[1]],m],h)]:[2,i,j,_(c,e,d,[1,a(A[4],k[1]),m],h)];default:var
n=f[3],o=f[2],p=f[1],l=b(u[4],o,j);if(typeof
l==="number"){var
s=_(c,e,d,n,h);return O(c,d,_(c,e,d,p,i),o,s)}else{if(0===l[0]){var
t=l[1],v=_(c,e,d,n,h);return O(c,d,_(c,e,d,[2,p,t,[0,c]],i),j,v)}var
w=l[1],x=_(c,e,d,n,h);return O(c,d,fz(c,d,function(a,b){return _(c,e,d,a,b)},i,w,p),o,x)}}}}function
ah(d,f,g,c,e,h,i){switch(i[0]){case
0:return b$(g,h,i[1]);case
1:var
s=i[2],t=i[1];return cG(f,c,function(a,b){return ah(d,f,g,c,e,a,b)},s,t,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],w=bx(f,ag(c,j),v);return[2,ag(c,k),l,w];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
x=ah(d,f,g,c,e,o,j);return[2,ag(c,k),l,x]}else{if(0===m[0]){var
y=ah(d,f,g,c,e,[1,[1,m[1]],o],j);return[2,ag(c,k),l,y]}var
z=ah(d,f,g,c,e,[1,a(A[4],m[1]),o],j);return[2,ag(c,k),l,z]}default:var
p=h[3],q=h[2],r=h[1],n=b(u[4],q,l);if(typeof
n==="number"){var
B=ah(d,f,g,c,e,p,j);return O(d,e,ah(d,f,g,c,e,r,k),q,B)}else{if(0===n[0]){var
C=n[1],D=ah(d,f,g,c,e,p,j);return O(d,e,ah(d,f,g,c,e,[2,r,C,[0,d]],k),l,D)}var
E=n[1],F=ah(d,f,g,c,e,p,j);return O(d,e,fA(d,c,e,function(a,b){return ah(d,f,g,c,e,a,b)},k,E,r),q,F)}}}}function
cH(f,e,d,a,c){switch(a[0]){case
0:return[0,b(e,a[1],c)];case
1:var
g=a[1];return Z(g,cH(f,e,d,a[2],c));default:var
h=a[2],i=a[1],j=cH(f,e,d,a[3],c);return O(f,d,cH(f,e,d,i,c),h,j)}}function
cI(d,g,f,c,e,a){return b(c,a,d)?[0,d]:b(c,a,g)?e:cH(d,f,c,e,a)}function
a$(f,j,i,e,g,d,c,h){switch(h[0]){case
0:return Z(c,cI(f,j,i,e,d,h[1]));case
1:var
l=h[2],p=h[1],k=b(u[4],p,c);return typeof
k==="number"?Z(c,b(g,l,d)):0===k[0]?Z(c,b(g,[1,k[1],l],d)):Z(p,a$(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
c==="number"){var
q=b(g,m,d);return O(f,e,a$(f,j,i,e,g,d,0,o),n,q)}else{if(0===c[0]){var
r=a$(f,j,i,e,g,d,[1,c[1]],m);return O(f,e,a$(f,j,i,e,g,d,c,o),n,r)}var
s=a$(f,j,i,e,g,d,a(A[4],c[1]),m);return O(f,e,a$(f,j,i,e,g,d,c,o),n,s)}}}function
ab(b,e,f,d,c,g,h){switch(h[0]){case
0:return cI(b,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return a$(b,e,d,c,function(a,g){return ab(b,e,f,d,c,a,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return cI(b,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?ab(b,e,f,d,c,l,i):0===j[0]?ab(b,e,f,d,c,[1,[1,j[1]],l],i):ab(b,e,f,d,c,[1,a(A[4],j[1]),l],i);return O(b,c,ab(b,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=ab(b,e,f,d,c,n,i),u=0,v=a$(b,e,d,c,function(a,g){return ab(b,e,f,d,c,a,g)},i,u,p),w=ab(b,e,f,d,c,Z(0,n),k),x=ab(b,e,f,d,c,p,k),y=O(b,c,v,o,t);return _(b,f,c,O(b,c,_(b,f,c,O(b,c,x,o,[0,b]),w),m,[0,b]),y)}}}function
cJ(a,e,g,f,c,d){switch(d[0]){case
0:var
h=d[1];return[0,b(f,h,h)];case
1:var
l=d[1];return[1,l,cJ(a,e,g,f,c,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=ab(a,e,g,f,c,k,Z(0,cI(a,e,f,c,i,b(g,e,e)))),n=cJ(a,e,g,f,c,i);return O(a,c,_(a,g,c,O(a,c,cJ(a,e,g,f,c,k),j,[0,a]),m),j,n)}}function
hp(c,b,a){return hm(a,ho(c,b))}function
cK(h,g,f,e,d,c,n,b,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return a(c,ab(h,g,f,e,d,j,b));else{if(0===i[0]){var
k=i[1];return a(c,ab(h,g,f,e,d,cK(h,g,f,e,d,c,cK(h,g,f,e,d,c,j,b,k),b,k),b))}var
l=i[1],j=cK(h,g,f,e,d,c,j,b,l),i=l;continue}}function
hq(h,a,g,f,e,d,c,b){return b?cK(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
ap(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return hp(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=ap(a,f,c,g,e,d,b,j[1]);return ah(a,c,e,d,b,ap(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=ap(a,f,c,g,e,d,b,i[1]);return ah(a,c,e,d,b,ap(a,f,c,g,e,d,b,j),l)}var
k=ap(a,f,c,g,e,d,b,i);return _(a,c,b,ap(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=ap(a,f,c,g,e,d,b,h[2]);return ah(a,c,e,d,b,ap(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=ap(a,f,c,g,e,d,b,h[2]);return ab(a,f,c,g,b,ap(a,f,c,g,e,d,b,p),q);case
5:return ag(d,ap(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=ap(a,f,c,g,e,d,b,h[1]);return hq(a,f,c,g,b,function(a){return a},s,r)}}function
ba(c,b){if(typeof
b==="number")switch(b){case
0:return 0;case
1:return 1;default:return 2}else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1],e=ba(c,b[2]);return[1,ba(c,d),e];case
2:var
f=b[1],g=ba(c,b[2]);return[2,ba(c,f),g];case
3:return[3,ba(c,b[1])];default:var
h=b[1],i=ba(c,b[2]);return[4,ba(c,h),i]}}var
eq=0;function
es(e,d,c,f){if(f){var
h=f[2],g=f[1],i=b(d,c,g);if(i){if(a(e,i[1]))return 0;var
j=es(e,d,c,h);return j?[0,[0,g,j[1]]]:0}var
k=es(e,d,c,h);return k?[0,[0,g,k[1]]]:0}var
l=b(d,c,c);return l?a(e,l[1])?0:[0,[0,c,0]]:[0,[0,c,0]]}function
hr(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=es(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
hs(e,d,c,a){var
b=0;return fv(function(f,a){var
b=hr(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
cL(d,c,a,b){if(a){var
e=a[2],f=hs(d,c,a[1],b);return em(cL(d,c,e,b),f)}return eq}function
et(b,a){return em(b,a)}function
aq(d,c,f,e,p,o){var
b=p,g=o;for(;;)if(typeof
g==="number")switch(g){case
0:return b?eq:er;case
1:return b?er:eq;default:return er}else
switch(g[0]){case
0:var
h=g[1];return b?a(f,h):a(e,h);case
1:var
i=g[2],j=g[1];if(b){var
q=aq(d,c,f,e,b,i);return et(aq(d,c,f,e,b,j),q)}var
r=aq(d,c,f,e,b,i);return cL(d,c,aq(d,c,f,e,b,j),r);case
2:var
k=g[2],l=g[1];if(b){var
s=aq(d,c,f,e,b,k);return cL(d,c,aq(d,c,f,e,b,l),s)}var
t=aq(d,c,f,e,b,k);return et(aq(d,c,f,e,b,l),t);case
3:var
u=g[1],b=bK(b),g=u;continue;default:var
m=g[2],n=g[1];if(b){var
v=aq(d,c,f,e,b,m);return cL(d,c,aq(d,c,f,e,bK(b),n),v)}var
w=aq(d,c,f,e,b,m);return et(aq(d,c,f,e,bK(b),n),w)}}function
ht(f,e,d){var
c=e,a=d;for(;;){if(c){var
g=c[2],h=c[1];if(a){var
i=a[2];if(b(f,h,a[1])){var
c=g,a=i;continue}return 0}return 0}return 1}}function
eu(g,f,e,d,c,b,a){return ht(c,aq(g,f,e,d,1,b),a)}function
fB(d,c,a){return bK(b(d,c,a))}function
fC(f,e,c,a){var
d=b(e,c,a);return d?fB(f,c,a):d}function
hu(b,a){switch(b){case
0:return ne;case
1:return 1===a?nf:0===a?ng:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?nh:ni}}function
hv(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?nj:0;case
2:return 1===a?0:nk;default:return 1===a?0:0===a?nl:[0,a]}}function
ev(c,b){return b?a(c,b[1]):0}function
fD(d,c,a){if(c){var
e=c[1];return a?b(d,e,a[1]):0}return 0}function
hw(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,ab(g,f,e,d,c,b,h),0]]:0}function
hx(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=hu(b[2],a[2]);return ev(function(a){return[0,[0,ab(g,f,e,d,c,h,i),a]]},j)}function
cM(e,d,c,b,a){var
f=b[1],g=a[1],h=hv(b[2],a[2]);return ev(function(a){return[0,[0,_(e,d,c,f,g),a]]},h)}function
by(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,hh(b[1],g,[0,[0,a],0])];case
1:return[0,[0,cJ(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=by(a,f,d,e,c,h,g,b[2]);return ev(function(b){return hw(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=by(a,f,d,e,c,h,g,b[2]),n=by(a,f,d,e,c,h,g,l);return fD(function(b,g){return hx(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=by(a,f,d,e,c,h,g,b[2]),q=by(a,f,d,e,c,h,g,o);return fD(function(b,e){return cM(a,d,c,b,e)},q,p);default:var
i=b[1];return fC(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
cN(a,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
c=g[1];switch(h){case
0:return fB(d,c,a);case
1:return b(d,c,a);case
2:return b(f,c,a);default:return fC(d,f,c,a)}}return 0}function
ew(c,i,h,g,b,a,f,e){var
d=by(c,i,h,g,b,a,f,e);return d?cN(c,b,a,d[1]):0}function
ca(g,f,e,d,c,b,a){return function(h){return ap(g,f,e,d,c,b,a,h)}}function
ac(e,d,c,b,a){return function(f,g){return ah(e,d,c,b,a,f,g)}}function
hy(c,b,a){return function(d,e){return _(c,b,a,d,e)}}function
hz(g,l,f,k,e,d,c,j){var
m=j[3],n=j[2],o=j[1],h=a(ca(g,l,f,k,e,d,c),o),i=a(ca(g,l,f,k,e,d,c),m);switch(n){case
0:var
p=[0,[0,b(ac(g,f,e,d,c),i,h),2],0];return[0,[0,b(ac(g,f,e,d,c),h,i),2],p];case
1:return[0,[0,b(ac(g,f,e,d,c),h,i),0],0];case
2:return[0,[0,b(ac(g,f,e,d,c),h,i),2],0];case
3:return[0,[0,b(ac(g,f,e,d,c),i,h),2],0];case
4:return[0,[0,b(ac(g,f,e,d,c),h,i),3],0];default:return[0,[0,b(ac(g,f,e,d,c),i,h),3],0]}}function
fE(h,g,f,e,d,c,b,a){var
i=hz(h,g,f,e,d,c,b,a);return b_(function(a){return[0,a,0]},i)}function
hA(g,l,f,k,e,d,c,j){var
m=j[3],n=j[2],o=j[1],h=a(ca(g,l,f,k,e,d,c),o),i=a(ca(g,l,f,k,e,d,c),m);switch(n){case
0:return[0,[0,b(ac(g,f,e,d,c),h,i),0],0];case
1:var
p=[0,[0,b(ac(g,f,e,d,c),i,h),2],0];return[0,[0,b(ac(g,f,e,d,c),h,i),2],p];case
2:return[0,[0,b(ac(g,f,e,d,c),i,h),3],0];case
3:return[0,[0,b(ac(g,f,e,d,c),h,i),3],0];case
4:return[0,[0,b(ac(g,f,e,d,c),i,h),2],0];default:return[0,[0,b(ac(g,f,e,d,c),h,i),2],0]}}function
fF(h,g,f,e,d,c,b,a){var
i=hA(h,g,f,e,d,c,b,a);return b_(function(a){return[0,a,0]},i)}function
ex(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return[0,c[1]];case
1:var
g=c[2],d=b(A[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[2],j=c[1],k=ex(a(A[1],d),h);return[2,[4,ex(d,j),[6,[1,d],[0,i]]],k]}}function
nm(a){return ex(0,a)}function
aH(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1]];case
2:var
d=b[1],e=aH(c,b[2]);return[2,aH(c,d),e];case
3:var
f=b[1],g=aH(c,b[2]);return[3,aH(c,f),g];case
4:var
h=b[1],i=aH(c,b[2]);return[4,aH(c,h),i];case
5:return[5,aH(c,b[1])];default:var
j=b[2];return[6,aH(c,b[1]),j]}}function
hB(b,a){var
c=a[2],d=a[1],e=aH(b,a[3]);return[0,aH(b,d),c,e]}function
nn(q,h,f,g,c){if(typeof
c!=="number")switch(c[0]){case
1:var
m=c[1];if(0===m[0]){var
n=m[1];return b(g,q,n)?0:[5,b(f,n,n)]}return[1,m];case
3:var
a=c[2],d=c[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
a==="number"?0:5===a[0]?[3,[5,b(f,a[1],s)],i]:c}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
a==="number"?0:5===a[0]?[3,[5,b(f,a[1],r)],j]:c}return typeof
a==="number"?0:5===a[0]?b(g,h,a[1])?d:[3,d,a]:c;case
5:var
e=d[1];if(typeof
a==="number")return 0;else
switch(a[0]){case
3:var
k=a[2],l=a[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,b(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,b(f,e,k[1])],l];return b(g,h,e)?a:[3,d,a];case
4:return[4,[3,[5,e],a[1]],[3,[5,e],a[2]]];case
5:return[5,b(f,e,a[1])];default:return b(g,h,e)?a:[3,d,a]}default:return typeof
a==="number"?0:5===a[0]?b(g,h,a[1])?d:[3,d,a]:c}case
4:var
o=c[2],p=c[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return c}function
no(a){return a[1]}function
np(a){return a[2]}function
aI(c,a){var
d=b(u[8],a[1],[0,c[2]]);return aG(b(u[8],c[1],[0,a[2]]),d)}function
cO(c,a){var
d=b(u[8],a[1],[0,c[2]]),e=b(u[8],c[1],[0,a[2]]);return b(u[10],e,d)}function
aJ(c,a){var
d=b(A[11],c[2],a[2]),e=b(u[8],a[1],[0,c[2]]),f=b(u[8],c[1],[0,a[2]]);return[0,b(u[5],f,e),d]}function
bb(c,a){var
d=b(A[11],c[2],a[2]);return[0,b(u[8],c[1],a[1]),d]}function
bP(b){var
c=b[2];return[0,a(u[6],b[1]),c]}function
cb(b,a){return aJ(b,bP(a))}function
fG(b){var
a=b[1];return typeof
a==="number"?nq:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
fH(a,b){return en(bb,a,b)}function
nr(b,a){return typeof
a==="number"?ns:0===a[0]?fH(b,a[1]):fG(fH(b,a[1]))}function
nt(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
cc(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,cc(b,a[1],c)]:[1,cc(b,a[1],c),b,0]}function
fI(d,a,b,c){if(typeof
c==="number")return cc(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,cc(d,a[1],b)]:[1,cc(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,fI(d,a[1],b,e)]:[1,fI(d,a[1],b,f),h,e]}}var
nu=u[10],nv=u[8],nw=u[5],ny=0;function
nz(a,b){return ew(ny,nx,nw,nv,aG,nu,a,b)}var
ad=ac(0,u[5],u[7],u[6],aG),aK=hy(0,u[5],aG),cP=ca(0,nA,u[5],u[8],u[7],u[6],aG);function
hC(e){var
f=e[3],g=e[2],c=a(cP,e[1]),d=a(cP,f);switch(g){case
0:var
h=[0,[0,b(ad,d,b(aK,c,nB)),3],0];return[0,[0,b(ad,c,b(aK,d,nC)),3],h];case
1:return[0,[0,b(ad,c,d),0],0];case
2:return[0,[0,b(ad,c,b(aK,d,nD)),3],0];case
3:return[0,[0,b(ad,d,b(aK,c,nE)),3],0];case
4:return[0,[0,b(ad,c,d),3],0];default:return[0,[0,b(ad,d,c),3],0]}}function
hD(a){var
b=hC(a);return b_(function(a){return[0,a,0]},b)}function
hE(e){var
f=e[3],g=e[2],c=a(cP,e[1]),d=a(cP,f);switch(g){case
0:return[0,[0,b(ad,c,d),0],0];case
1:var
h=[0,[0,b(ad,d,b(aK,c,nF)),3],0];return[0,[0,b(ad,c,b(aK,d,nG)),3],h];case
2:return[0,[0,b(ad,d,c),3],0];case
3:return[0,[0,b(ad,c,d),3],0];case
4:return[0,[0,b(ad,d,b(aK,c,nH)),3],0];default:return[0,[0,b(ad,c,b(aK,d,nI)),3],0]}}function
hF(a){var
b=hE(a);return b_(function(a){return[0,a,0]},b)}var
nJ=u[10],nK=0;function
fJ(a){return cN(nK,aG,nJ,a)}var
nL=u[5],nM=0;function
hG(a,b){return cM(nM,nL,aG,a,b)}function
hH(e,d){var
a=b(u[17],e,d),c=a[1];return typeof
a[2]==="number"?c:b(u[5],c,nN)}function
fK(c,a){var
d=b(u[19],c,a);return b(u[13],d,nO)}function
cQ(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=cQ(a[1]),f=b[2],g=b[1],c=cQ(e),h=c[2],i=c[1];return[0,fK(fK(g,f),i),h]}}function
cR(a,c){switch(a[0]){case
0:return[0,b(u[18],a[1],c)];case
1:var
d=a[1];return[1,d,cR(a[2],c)];default:var
e=a[2],f=a[1],g=cR(a[3],c);return[2,cR(f,c),e,g]}}function
ey(c){var
e=cQ(c),f=e[2],d=e[1];if(b(u[12],d,0)){var
g=hH(a(u[6],f),d),h=a(u[6],g);return[0,cR(b$(u[7],c,f),d),h]}return[0,c,0]}function
ez(d){var
e=d[2],a=d[1];switch(e){case
0:var
f=cQ(a),g=f[2],c=f[1];if(b(u[12],c,0))if(bK(aG(g,0)))if(bK(aG(b(u[19],c,g),c)))return 0;return[0,[0,ey(a),0]];case
1:return[0,[0,[0,a,0],e]];case
2:return[0,[0,ey(b$(u[7],a,nP)),3]];default:return[0,[0,ey(a),3]]}}function
hI(a){var
c=a[1],d=a[2];return[0,b(aK,c[1],[0,c[2]]),d]}function
hJ(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
nQ=u[10],nR=u[8],nS=u[5],nU=0;function
cS(a,b){return by(nU,nT,nS,nR,aG,nQ,a,b)}function
fL(a){return 0===a?1:3<=a?1:0}function
fM(w,v){var
d=w,c=v;for(;;)if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
x=c[2],g=cS(d,c[1]);if(g){var
h=g[1];if(fJ(h))return 1;var
d=[0,h,d],c=x;continue}return 0;case
1:var
y=c[2],i=cS(d,c[1]);if(i){var
j=ez(i[1]);if(j){var
d=[0,hI(j[1]),d],c=y;continue}return 1}return 0;default:var
z=c[3],A=c[2],k=cS(d,c[1]);if(k){var
B=k[1],l=cS(d,A);if(l){var
C=l[1],m=ez(B);if(m){var
n=m[1],o=n[1],p=o[1],D=n[2],E=o[2],q=ez(C);if(q){var
r=q[1],s=r[1],F=r[2],G=s[2],H=s[1];if(fL(D))if(fL(F))if(hJ(b(aK,p,H))){var
f=z,e=a(u[6],E);for(;;){if(f){var
I=f[2],J=f[1],t=fM([0,[0,b(ad,p,[0,e]),0],d],J);if(t){var
f=I,e=b(u[5],e,nV);continue}return t}return b(u[12],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
nW(b,a){return eu(fJ,hG,hD,hF,fM,b,a)}function
hK(a,b){return ew(nY,nX,aJ,bb,aI,cO,a,b)}function
hL(a){return fE(n0,nZ,aJ,bb,cb,bP,aI,a)}function
hM(a){return fF(n2,n1,aJ,bb,cb,bP,aI,a)}function
hN(a){return cN(n3,aI,cO,a)}function
hO(a,b){return cM(n4,aJ,aI,a,b)}function
n5(b,a){return eu(hN,hO,hL,hM,hK,b,a)}function
aT(a){if(typeof
a==="number")return 0===a?n6:n7;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aT(a[2]);return aJ(aT(b),c);case
3:var
d=a[1],e=aT(a[2]);return cb(aT(d),e);case
4:var
f=a[1],g=aT(a[2]);return bb(aT(f),g);case
5:return fG(aT(a[1]));default:return bP(aT(a[1]))}}function
hP(a,b){return ew(n9,n8,aJ,bb,aI,cO,a,b)}function
hQ(a){return fE(n$,n_,aJ,bb,cb,bP,aI,a)}function
hR(a){return fF(ob,oa,aJ,bb,cb,bP,aI,a)}function
hS(a){return cN(oc,aI,cO,a)}function
hT(a,b){return cM(od,aJ,aI,a,b)}var
r=[0,bK,em,hc,fr,mI,A,mQ,en,hh,b_,fv,u,aG,nc,nd,fy,Z,hm,O,hn,ho,ag,bx,b$,cF,cG,fz,fA,_,ah,cH,cI,a$,ab,cJ,hp,cK,hq,ap,ba,eq,er,es,hr,hs,cL,et,aq,ht,eu,fB,fC,hu,hv,ev,fD,hw,hx,cM,by,cN,ew,ca,ac,hy,hz,fE,hA,fF,ex,nm,aH,hB,nn,no,np,aI,cO,aJ,bb,bP,cb,fG,fH,nr,nt,cc,fI,nz,ad,aK,cP,hC,hD,hE,hF,fJ,hG,hH,fK,cQ,cR,ey,ez,hI,hJ,cS,fL,fM,nW,hK,hL,hM,hN,hO,n5,aT,hP,hQ,hR,hS,hT,function(b,a){return eu(hS,hT,hQ,hR,hP,ba(function(a){return hB(aT,a)},b),a)}];a6(740,r,"Micromega_plugin.Micromega");var
fN=f[2],bR=f[7],of=f[3],T=a(bQ[1],[0,aa]),oe=0;function
og(a,c){function
d(c,b){return 1===b?e(m[1],a,oh,c):I(m[1],a,oi,c,b)}return b(T[11],d,c)}var
fO=T[1];function
hU(a){var
b=0;function
c(c,b,a){return a+b|0}return e(T[12],c,a,b)}function
oj(b,a){var
c=hU(b),d=hU(a);return c===d?e(T[9],aa,b,a):aa(c,d)}function
hV(a){return W(a,T[1])}function
ok(a){return e(T[4],a,1,T[1])}function
ol(a){try{var
b=1,c=function(c,b,a){if(1===a){if(1===b)return 0;throw P}throw P},d=1-e(T[12],c,a,b);return d}catch(a){a=w(a);if(a===P)return 0;throw a}}function
om(a){if(hV(a))return 0;try{var
b=function(c,b,f){var
d=b/2|0;if(0===(b%2|0))return e(T[4],c,d,a);throw P},c=[0,e(T[12],b,a,fO)];return c}catch(a){a=w(a);if(a===P)return 0;throw a}}function
cT(c,a){try{var
d=b(T[23],c,a);return d}catch(a){a=w(a);if(a===P)return 0;throw a}}function
on(b,a){var
c=cT(b,a)+1|0;return e(T[4],b,c,a)}function
hW(b,a){function
c(b,c,a){var
d=cT(b,a)+c|0;return e(T[4],b,d,a)}return e(T[12],c,b,a)}function
oo(d,c){var
b=fO,a=c;for(;;){if(0===a)return b;var
b=hW(b,d),a=a-1|0;continue}}function
op(c,a){var
f=h[7];function
g(e,d,a){var
f=K.caml_div(cT(e,c),d);return b(h[4],a,f)}var
d=e(T[12],g,a,f),i=T[1];function
j(c,g,b){var
f=g-e$(cT(c,a),d)|0;return 0===f?b:e(T[4],c,f,b)}return[0,e(T[12],j,c,i),d]}var
L=[0,fO,hV,ok,ol,cT,on,hW,oo,op,oj,og,T[12],om],U=a(bQ[1],[0,L[10]]);function
oq(c,d){function
g(g,d){if(0===b(L[10],L[1],g)){var
h=a(f[40],d);return e(m[1],c,or,h)}var
i=L[11],j=a(f[40],d);return aR(m[1],c,os,j,i,g)}return b(U[11],g,d)}function
hX(c,a){try{var
d=b(U[23],c,a);return d}catch(a){a=w(a);if(a===P)return ot;throw a}}function
ou(b){var
c=U[1],d=a(L[3],b);return e(U[4],d,ov,c)}function
hY(a){return e(U[4],L[1],a,U[1])}function
hZ(d,g,c){if(0===a(f[25],g))return c;var
h=b(fN,hX(d,c),g);return 0===a(f[25],h)?b(U[6],d,c):e(U[4],d,h,c)}function
h0(g,c,d){if(0===a(f[25],c))return hY(ow);var
h=U[1];function
i(f,d,a){var
h=b(bR,c,d),i=b(L[7],g,f);return e(U[4],i,h,a)}return e(U[12],i,d,h)}function
h1(b,a){function
c(c,b,a){return hZ(c,b,a)}return e(U[12],c,b,a)}function
ox(b,a){var
c=U[1];function
d(d,c,b){return h1(h0(d,c,a),b)}return e(U[12],d,b,c)}function
oy(c){function
d(b){return a(f[3],b)}return b(U[24],d,c)}var
h2=U[12];function
oz(b){var
c=1;return e(h2,function(e,c,b){var
d=b?0===a(f[25],c)?1:0:b;return d},b,c)}var
oA=a(U[9],f[37]),h3=[0,hX,ou,hZ,hY,h0,ox,h1,oy,h2,oq,oA,oz,function(b){var
c=1;function
d(c,f,b){if(b){var
d=a(L[2],c);if(!d)return a(L[4],c);var
e=d}else
var
e=b;return e}return e(U[12],d,b,c)}];function
oB(k,j){var
c=k,a=j;for(;;){if(c){var
d=c[1],l=c[2],m=d[2],n=d[1];if(a){var
e=a[1],g=n===e[1]?1:0,o=a[2],p=e[2];if(g){var
h=b(f[26],m,p);if(h){var
c=l,a=o;continue}var
i=h}else
var
i=g;return i}return 0}return a?0:1}}function
oC(e){var
c=0,b=e;for(;;){if(b){var
d=b[1],g=b[2],h=d[1],i=[0,h,a(f[52],d[2])],c=c+a(bv[20],i)|0,b=g;continue}return a(bv[20],c)}}var
oD=0;function
oE(h,c){function
d(b){var
c=b[1],d=a(f[40],b[2]);return e(m[2],oF,d,c)}return b(g[11],d,c)}function
oG(a){function
d(i,h){var
c=i,a=h;for(;;){if(a){var
e=a[2],g=a[1];if(b(f[31],g,oH))return[0,[0,c,g],d(c+1|0,e)];var
c=c+1|0,a=e;continue}return 0}}return d(0,a)}function
oJ(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,eA,b(c+1|0,a)]}return 0}return b(0,a)}function
bS(d,c,a){return b(f[26],c,oK)?a:[0,[0,d,c],a]}function
h4(d,c,b){if(b){var
f=b[2],g=b[1],i=g[2],e=g[1],j=aa(d,e)+1|0;if(2<j>>>0)return a(h[2],oL);switch(j){case
0:return bS(d,a(c,eA),b);case
1:return bS(e,a(c,i),f);default:return[0,[0,e,i],h4(d,c,f)]}}return bS(d,a(c,eA),0)}function
h5(d,c,b){if(b){var
f=b[2],g=b[1],e=g[1],i=aa(d,e)+1|0,j=g[2];if(2<i>>>0)return a(h[2],oM);switch(i){case
0:return bS(d,c,b);case
1:return bS(e,c,f);default:return[0,[0,e,j],h5(d,c,f)]}}return bS(d,c,0)}function
oN(d){var
f=q[1];function
h(d,c){var
e=a(l[18],c[2]);return b(q[17],d,e)}var
c=e(g[16],h,f,d);return 0===b(q[23],c,q[1])?q[2]:c}function
oO(a,c){if(0===a[0]){var
d=a[1];if(0===d)return 0;if(1===d)return c}function
e(c){var
d=c[1];return[0,d,b(f[7],a,c[2])]}return b(g[13],e,c)}function
eB(o,n){var
c=o,a=n;for(;;){if(c){if(a){var
e=a[2],i=a[1],j=i[2],g=i[1],h=c[2],k=c[1],l=k[2],d=k[1];if(W(d,g)){var
m=b(f[1],l,j);if(b(f[26],m,oP)){var
c=h,a=e;continue}return[0,[0,d,m],eB(h,e)]}return gN(d,g)?[0,[0,d,l],eB(h,a)]:[0,[0,g,j],eB(c,e)]}return c}return a?a:0}}function
oQ(d,c){var
e=0,g=[0,function(a){return b(f[37],d[2],c[2])},e],h=[0,function(a){return aa(d[1],c[1])},g];return a(l[31][1],h)}var
oR=a(l[31][2],oQ);function
h6(e,d){var
a=d;for(;;){if(a){var
b=a[1],c=aa(b[1],e),f=a[2],g=b[2];if(-1===c){var
a=f;continue}return 0===c?[0,[0,g,a]]:0}return 0}}function
oS(c,b){var
a=h6(c,b);return a?[0,a[1][1]]:0}var
bT=[0,oB,oC,oD,oE,oG,eA,oI,oJ,bS,h4,h5,oN,oO,eB,oR,h6,oS,function(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}];function
fP(a){return 0===a?oT:oU}function
oV(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fP(d);return E(m[1],c,oW,bT[4],e,h,g)}function
oX(b,a){if(0===b){if(0===a)return 0}else
if(0!==a)return 1;return 1}function
oY(b,a){if(0!==b)if(0!==a)return 1;return 0}function
aL(d,c){if(typeof
c==="number")return b(m[1],d,oZ);else
switch(c[0]){case
0:return e(m[1],d,o0,c[1]);case
1:return e(m[1],d,o1,c[1]);case
2:var
f=a(q[33],c[1]);return e(m[1],d,o2,f);case
3:return b(m[1],d,o3);case
4:return I(m[1],d,o4,aL,c[2]);case
5:var
g=c[2],h=a(q[33],c[1]);return aR(m[1],d,o5,aL,g,h);case
6:return E(m[1],d,o6,aL,c[1],aL,c[2]);case
7:return E(m[1],d,o7,aL,c[1],aL,c[2]);default:return I(m[1],d,o8,aL,c[1])}}function
cU(d,c){if(typeof
c==="number")return b(m[1],d,o9);else{if(0===c[0])return eb(m[1],d,o_,c[1],aL,c[2],cU,c[3]);var
e=c[5],f=c[4],g=c[3],h=c[2],i=c[1],j=a(l[2],cU);return Bv(m[1],d,o$,i,aL,h,bT[4],g,aL,f,j,e)}}function
cd(e){var
a=e;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
8:var
d=a[1],c=1;break;case
0:case
1:return a[1];case
4:case
5:var
d=a[2],c=1;break;case
6:case
7:var
f=a[1],g=cd(a[2]),i=cd(f);return b(h[5],i,g);default:var
c=0}if(c){var
a=d;continue}return-1}}function
fQ(a){if(typeof
a==="number")return-1;else{if(0===a[0]){var
c=a[2],d=a[1],f=fQ(a[3]),i=cd(c),j=b(h[5],i,f);return b(h[5],d,j)}var
k=a[5],l=a[2],m=a[1],n=cd(a[4]),o=cd(l),p=b(h[5],o,n),q=b(h[5],m,p),r=function(c,a){var
d=fQ(a);return b(h[5],c,d)};return e(g[16],r,q,k)}}function
bc(c,a){if(typeof
a!=="number")switch(a[0]){case
4:var
n=a[1],d=bc(c,a[2]);return[0,d[1],d[2],[4,n,d[3]]];case
5:var
e=bc(c,a[2]),f=e[2];return[0,[0,[0,f,e[3]],e[1]],f+1|0,[1,f]];case
6:var
o=a[2],g=bc(c,a[1]),p=g[3],q=g[1],i=bc(g[2],o),r=i[2],s=[6,p,i[3]];return[0,b(h[22],i[1],q),r,s];case
7:var
t=a[2],j=bc(c,a[1]),u=j[3],v=j[1],k=bc(j[2],t),w=k[2],x=[7,u,k[3]];return[0,b(h[22],k[1],v),w,x];case
8:var
l=bc(c,a[1]),m=l[2];return[0,[0,[0,m,l[3]],l[1]],m+1|0,[1,m]]}return[0,0,c,a]}function
eC(c,a){if(typeof
a!=="number"&&8===a[0]){var
b=bc(c,a[1]);return[0,b[1],b[2],[8,b[3]]]}return bc(c,a)}function
fR(b){var
a=b;for(;;){if(typeof
a!=="number"&&8===a[0]){var
a=a[1];continue}return a}}function
fS(f,o){var
c=o;for(;;)if(typeof
c==="number")return[0,f,0];else{if(0===c[0]){var
d=c[2],l=c[1];if(typeof
d!=="number"&&5===d[0])if(typeof
c[3]==="number"){var
c=[0,l,d[2],0];continue}var
p=c[3],i=eC(f,d),q=i[3],r=i[1],m=fS(i[2],p),s=m[1],t=[0,l,q,m[2]],u=function(b,a){return[0,a[1],[8,a[2]],b]};return[0,s,e(g[16],u,t,r)]}var
v=c[5],w=c[4],x=c[3],y=c[1],j=eC(f,fR(c[2])),z=j[3],A=j[2],B=j[1],k=eC(A,fR(w)),C=k[3],D=k[2],E=k[1],F=function(a){return fS(D,a)},G=b(g[13],F,v),n=a(g[39],G),H=n[2],I=n[1],J=b(h[22],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[8,a[2]],b]},M=e(g[16],L,K,J);return[0,e(g[16],h[5],0,I),M]}}function
pa(c,a){var
b=fS(c,a);if(l[1])aR(m[2],pb,cU,a,cU,b[2]);return b}function
h7(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[7,b,a];var
c=b}return c}function
fT(c,d){var
e=a(q[22],c)+1|0;if(2<e>>>0)throw[0,G,pc];switch(e){case
0:return[4,[0,0,[1,c]],d];case
1:return 0;default:return b(q[24],c,q[2])?d:[6,[2,c],d]}}function
h8(c,b){var
d=c[2],e=c[1];return e?[4,[0,e,d],b]:fT(a(l[18],d),b)}var
cV=a(bQ[1],[0,L[10]]),cW=a(bQ[1],[0,aa]),cX=[0,cV[1]],cY=[0,cW[1]],eD=[0,0];function
pd(a){cX[1]=cV[1];cY[1]=cW[1];eD[1]=0;return 0}function
pe(c){try{var
a=b(cV[23],c,cX[1]);return a}catch(a){a=w(a);if(a===P){var
d=eD[1];cX[1]=e(cV[4],c,d,cX[1]);cY[1]=e(cW[4],d,c,cY[1]);eD[1]++;return d}throw a}}var
aU=[0,cV,cW,cX,cY,eD,pd,pe,function(a){return b(cW[23],a,cY[1])}];function
fU(a){var
c=a[2],d=a[1];function
e(b,a){return aa(b[1],a[1])}return[0,b(g[41],e,d),c]}function
eE(c,b){var
d=b[2],e=a(aU[8],b[1]),g=L[11],h=a(f[40],d);return aR(m[1],c,pf,h,g,e)}function
eF(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fP(d),i=a(l[2],eE);return E(m[1],c,pg,i,e,h,g)}function
ph(c){function
d(d,c,b){var
e=b[1],f=b[2];return a(L[2],d)?[0,e,c]:[0,[0,[0,a(aU[7],d),c],e],f]}var
b=e(h3[9],d,c,pi);return fU([0,b[1],b[2]])}function
h9(c,d,n){var
e=n[2],j=n[1];if(a(L[2],d))var
q=b(bR,c,e),i=[0,b(bT[13],c,j),q];else
if(0===a(f[25],c))var
i=pj;else{if(0===a(f[25],e))var
k=0;else
var
t=b(bR,c,e),k=[0,[0,a(aU[7],d),t],0];var
r=function(e){var
f=e[2],g=a(aU[8],e[1]),h=b(L[7],d,g),i=a(aU[7],h);return[0,i,b(bR,c,f)]},s=b(g[13],r,j),i=fU([0,b(h[22],k,s),pk])}var
o=i[2],p=i[1];if(l[1]){var
u=a(f[40],o),v=a(l[2],eE),w=a(f[40],e),x=a(l[2],eE),y=L[11],z=a(f[40],c);Bw(m[2],pl,z,y,d,x,j,w,v,p,u)}return[0,p,o]}function
h_(c,b){return a(L[2],b)?[0,0,c]:[0,[0,[0,a(aU[7],b),c],0],pm]}function
h$(w,v,d,u){var
i=u[1],j=w[1],F=u[2],G=w[2];if(l[1]){var
H=a(f[40],d),I=a(aU[8],v);b0(m[2],pn,eF,j,L[11],I,H,eF,i)}var
J=i[2],x=a(aU[8],v);function
C(c,a){var
d=a[2],e=c[2],f=a[1],g=c[1];return gN(e,d)?-1:W(e,d)?b(L[10],g,f):1}var
p=[0,L[1],h[7]],o=[0,i,F];for(;;){var
q=o[2],c=o[1],_=p[2],$=p[1],U=c[1],V=[0,pp,L[1],0],X=function(c,d){var
e=c[3],f=c[2],j=d[2],k=c[1],l=a(aU[8],d[1]),g=b(L[9],l,x),h=g[2],i=g[1];return-1===C([0,f,e],[0,i,h])?[0,j,i,h]:[0,k,f,e]},n=e(g[16],X,V,U),D=n[3],Y=n[2],Z=n[1],E=0<D?[0,[0,Z,Y,D]]:0;if(E){var
r=E[1],s=r[3],t=r[2],aa=r[1];if(-1===C([0,t,s],[0,$,_])){var
k=a(f[15],d),y=b(bR,[0,-a(f[25],d)|0],aa),K=b(L[8],x,s-1|0),z=b(L[7],t,K),M=a(f[3],j[3]),A=h9(y,z,[0,j[1],M]),N=A[2],O=A[1],P=b(bR,k,c[3]),Q=b(fN,a(f[3],N),P),R=b(bT[13],k,c[1]),B=[0,b(bT[14],R,O),J,Q],S=fT(a(l[18],k),q),T=h7(h8(h_(y,z),G),S);if(l[1])e(m[2],po,eF,B);var
p=[0,t,s],o=[0,B,T];continue}return[0,c,q]}return[0,c,q]}}var
p=[0,oe,fN,of,bR,L,h3,bT,fP,oV,oX,oY,aL,cU,cd,fQ,eC,fR,pa,h7,fT,h8,[0,aU,fU,eE,eF,ph,h9,h_,h$,function(c,a){var
d=a[1],f=a[2],e=b(bT[17],c,d[1]);if(e){var
g=e[1];return function(a){return[0,h$([0,d,f],c,g,a)]}}return function(a){return 0}}]];a6(743,p,"Micromega_plugin.Polynomial");var
au=l[4],eG=l[5],pq=0,pr=0,ps=kc;function
ia(a){var
c=a[1];if(c){var
d=a[2];if(d)return b(f[29],c[1],d[1])?[0,a]:0}return[0,a]}function
pt(a){var
c=a[2],d=b(au,f[3],a[1]);return[0,b(au,f[3],c),d]}function
pu(c,a){var
e=a[2],g=a[1],h=c[2],i=c[1];function
d(d,c,a){if(c){var
e=c[1];return a?[0,b(d,e,a[1])]:c}return a?a:0}var
j=d(f[39],h,e);return ia([0,d(f[38],i,g),j])}function
fV(c){var
d=c[1];if(d){var
e=c[2];if(e){var
g=e[1],h=a(f[24],d[1]),i=a(f[22],g),j=b(f[4],i,h);return[0,b(f[1],j,pv)]}}return 0}function
pw(e,d){var
a=fV(e),c=fV(d);return a?c?b(f[29],a[1],c[1]):1:0}var
ce=[0,ia,pt,pu,fV,pw,function(d,a){var
c=d[2],e=d[1];if(e){var
g=e[1];if(c){var
i=c[1],h=b(f[29],g,a);return h?b(f[29],a,i):h}return b(f[29],g,a)}return c?b(f[29],a,c[1]):1}],aV=a(ef[1],[0,aa]),ib=p[7],ae=a(bv[18],[0,ib[1],ib[2]]),ic=[0,h[7]],cf=[a8,py,a7(0)],px=0;function
pz(a){function
d(h,g){var
a=h,c=g;for(;;){switch(a[0]){case
0:return b(aV[4],a[1],c);case
1:var
f=a[3],e=a[2];break;default:var
f=a[2],e=a[1]}var
a=e,c=d(f,c);continue}}return d(a,aV[1])}function
cZ(b,a){switch(a[0]){case
0:return e(m[1],b,pA,a[1]);case
1:return eb(m[1],b,pB,a[1],cZ,a[2],cZ,a[3]);default:return E(m[1],b,pC,cZ,a[1],cZ,a[2])}}function
fW(d,c){if(c){var
e=a(f[40],c[1]);return b(h[49],d,e)}return b(h[49],d,pD)}function
id(b,a){return E(m[1],b,pE,fW,a[1],fW,a[2])}function
pF(a,c){b(h[49],a,pG);var
d=0;function
f(b,c){return e(m[1],a,pH,b)}e(aV[14],f,c,d);return b(h[49],a,pI)}function
pJ(a,c){b(h[49],a,pK);var
d=0;function
f(b,c){return e(m[1],a,pL,b)}e(aV[14],f,c,d);return b(h[49],a,pM)}function
pN(b,a){return id(b,a[1])}function
ie(c,d){var
g=d[2],i=g[2],j=g[1],k=d[1];if(j){var
l=a(f[40],j[1]);e(m[1],c,pO,l)}b(p[7][4],c,k);if(i){var
n=a(f[40],i[1]);return e(m[1],c,pP,n)}return b(h[49],c,pQ)}function
pR(c,a){function
d(b,a){return ie(c,[0,b,a[1][1]])}return b(ae[11],d,a)}function
pS(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=p[7][4],i=a(f[40],e);return E(m[1],c,pT,i,h,d,g)}function
ig(c,a){var
d=c[4],e=c[3],g=a[4],h=a[2],i=a[1],j=c[2],k=c[1];if(e===a[3])if(d===g){var
f=b(ce[3],k,i);return f?[0,[0,f[1],[2,j,h],e,d]]:0}throw[0,G,pU]}function
pV(f,c,d){try{var
a=b(ae[7],d,f),g=ig(c,a[1]);if(g){a[1]=g[1];var
h=0;return h}throw[0,cf,[2,c[2],a[1][2]]]}catch(a){a=w(a);if(a===P)return e(ae[9],d,f,[0,c]);throw a}}var
ih=[a8,pW,a7(0)];function
eH(d,c,b){var
e=ic[1];if(a(ae[14],b)<e)return pV(d,c,b);throw ih}function
eI(d,c){var
k=a(ce[1],c[1]);if(k){var
l=k[1],i=l[2],j=l[1];if(d){var
e=d[1][2],h=function(a){return b(f[9],a,e)};if(1===a(f[25],e))var
o=c[4],p=c[3],q=c[2],r=b(au,h,i),m=[0,[0,b(au,h,j),r],q,p,o];else
var
t=c[3],u=c[4],v=c[2],w=b(au,h,j),m=[0,[0,b(au,h,i),w],v,u,t];if(b(f[31],e,pX))var
s=function(a){var
c=a[1];return[0,c,b(f[9],a[2],e)]},n=b(g[13],s,d);else
var
n=d;return[0,n,m]}return b(ce[6],[0,j,i],pY)?1:0}return 0}function
pZ(a){return 0===a?f[26]:f[30]}function
fX(h){var
d=0,c=0,b=h;for(;;){if(b){var
e=b[2],g=a(f[25],b[1][2]);if(0===g)throw[0,G,p0];if(1===g){var
c=c+1|0,b=e;continue}var
d=d+1|0,b=e;continue}return[0,d,c]}}function
fY(a,e){var
b=a[3],c=a[1],f=a[2],d=fX(c),g=d[2],h=d[1],i=[0,e],j=0===f?[0,[0,b],[0,b]]:[0,[0,b],0];return eI(c,[0,j,i,g,h])}function
ii(d){var
c=a(ae[1],1e3);function
f(b,a){return[0,b,a]}var
h=b(l[23],f,d),i=aV[1];function
j(f,d){var
h=d[2],i=d[1],a=fY(i,h);if(typeof
a==="number"){if(0===a)throw[0,cf,[0,h]];return f}eH(a[1],a[2],c);var
j=i[1];function
k(c,a){return b(aV[4],a[1],c)}return e(g[16],k,f,j)}return[0,c,e(g[16],j,i,h)]}function
fZ(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return e(ae[13],d,b,c)}function
eJ(e,c){var
h=c[2],i=e[2],j=c[1],k=e[1];if(b(f[31],i,p1))if(b(f[31],h,p2)){var
d=function(s,r){var
c=s,a=r;for(;;){if(c){if(a){var
j=a[2],m=a[1],n=m[2],k=m[1],l=c[2],o=c[1],p=o[2],e=o[1];if(e===k){var
t=b(f[9],n,h),u=b(f[9],p,i),q=b(f[1],u,t);if(b(f[26],q,p3)){var
c=l,a=j;continue}return[0,[0,e,q],d(l,j)]}if(e<k){var
v=d(l,a);return[0,[0,e,b(f[9],p,i)],v]}var
w=d(c,j);return[0,[0,k,b(f[9],n,h)],w]}var
x=function(a){var
c=a[1];return[0,c,b(f[9],a[2],i)]};return b(g[13],x,c)}if(a){var
y=function(a){var
c=a[1];return[0,c,b(f[9],a[2],h)]};return b(g[13],y,a)}return 0}},a=d(k,j);return[0,a,fX(a)]}throw[0,G,p4]}function
ij(q,g,c,e){var
h=e[3],i=e[2],j=e[1],k=b(p[7][17],q,g);if(k){var
l=k[1],d=function(b,a){return a?[0,[0,l,g,[0,[0,[0,a[1]],0],c[2],c[3],c[4]]],b]:b},m=c[1],n=m[2],o=m[1];if(1===a(f[25],l)){var
r=d(h,n);return[0,d(j,o),i,r]}var
s=d(h,o);return[0,d(j,n),i,s]}return[0,j,[0,[0,g,c],i],h]}function
ik(d,c){var
j=c[1];function
k(c,b,a){return ij(d,c,b[1],a)}var
h=e(ae[13],k,j,p5),l=h[3],m=h[2],n=h[1],o=a(ae[14],c[1]),i=a(ae[1],o);function
p(a){return e(ae[9],i,a[1],[0,a[2]])}b(g[11],p,m);function
q(e){function
c(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=a(eG,k[1][1]),t=a(eG,r[1]),u=a(f[3],j),v=b(f[9],t,u),w=b(f[9],s,l),x=b(f[1],w,v),m=eJ([0,q,l],[0,p,a(f[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,d,k[2],h[2]],n[2],n[1]],c=eI(m[1],o);if(typeof
c==="number"){if(0===c)throw[0,cf,o[2]];return 0}return eH(c[1],c[2],i)}return b(g[11],c,l)}b(g[11],q,n);return[0,i,b(aV[6],d,c[2])]}function
il(h,g,t,s,r,e){var
c=e[2],d=e[1],i=b(p[7][17],h,d);if(i){var
j=i[1],k=b(f[30],j,p6)?a(f[3],g):g,l=a(f[15],j),m=eJ([0,t,k],[0,d,l]),n=m[2],u=n[2],v=n[1],w=m[1],x=b(f[9],s,k),o=function(a){var
c=b(f[9],a,l);return b(f[1],x,c)},q=c[1],y=q[1],z=b(au,o,q[2]),A=[0,b(au,o,y),z];return[0,w,[0,A,[1,h,r,c[2]],u,v]]}return[0,d,c]}function
im(d,e,h,g,c){var
i=a(eG,b(p[7][17],d,e)),j=a(ae[14],c[1]),f=a(ae[1],j),k=c[1];function
l(k,j){var
c=il(d,i,e,h,g,[0,k,j[1]]),b=c[2],a=eI(c[1],b);if(typeof
a==="number"){if(0===a)throw[0,cf,b[2]];return 0}return eH(a[1],a[2],f)}b(ae[11],l,k);return[0,f,b(aV[6],d,c[2])]}function
p7(a){var
b=0;function
c(c,a,b){return(b+a[1][4]|0)+a[1][3]|0}return e(ae[13],c,a,b)}var
bU=a(bQ[1],[0,aa]);function
p8(c,b){var
d=0;function
g(d,b,g){var
e=a(f[40],b);return I(m[1],c,p9,d,e)}return e(bU[12],g,b,d)}function
io(k,a){function
d(m,e,l){var
c=m,a=l;for(;;){if(c){var
g=c[2],h=c[1],i=h[2],j=h[1];try{var
n=b(bU[23],j,k),o=b(f[6],n,i),p=d(g,b(f[1],e,o),a);return p}catch(b){b=w(b);if(b===P){var
c=g,a=[0,[0,j,i],a];continue}throw b}}return[0,e,a]}}return d(a,p_,0)}function
ip(g,e,d){function
c(a){var
c=b(f[4],a,e);return b(f[9],c,g)}var
i=d[2],j=d[1],k=a(f[25],g);if(0===k)return b(ce[6],d,e)?p$:a(h[2],qa);if(1===k){var
l=b(au,c,i);return[0,b(au,c,j),l]}var
m=b(au,c,j);return[0,b(au,c,i),m]}function
f0(g,f,c){function
d(k,j,i){var
c=io(g,k),l=c[1],d=b(p[7][17],f,c[2]),m=d?d[1]:qd,n=ip(m,l,j[1][1]),e=b(ce[3],i,n);return e?e[1]:a(h[2],qc)}return e(ae[13],d,c,qb)}function
iq(d){var
e=d[1];if(e){var
g=d[2],c=e[1];if(g){var
h=g[1];if(b(f[29],c,qe))if(b(f[29],qf,h))return qg;var
k=a(f[22],h),l=a(f[24],c);return b(f[29],l,k)?a(f[24],c):c}return b(f[29],c,qh)?qi:a(f[24],c)}var
i=d[2];if(i){var
j=i[1],m=a(f[22],j);return b(f[29],qj,m)?qk:a(f[22],j)}return ql}function
ir(h,l,k,d,c){function
e(c,f){var
m=a(l,c);try{var
q=function(a){return a[1][1]!==h?1:0},d=b(g[29],q,m)[1],j=d[1],r=e(im(j,d[2],d[3],d[4],c),[0,[0,j,c],f]);return r}catch(d){d=w(d);if(d===P){var
n=a(k,c);try{var
o=function(a){return a[1]!==h?1:0},i=b(g[29],o,n)[1],p=e(ik(i,c),[0,[0,i,c],f]);return p}catch(a){a=w(a);if(a===P)return[0,[0,c,f]];throw a}}throw d}}return e(d,c)}function
f1(d,c,b,a){try{var
e=ir(d,c,b,ii(a),0);return e}catch(a){a=w(a);if(a[1]===cf)return[1,a[2]];throw a}}function
is(w,v){var
d=v,c=0,i=0,j=0,h=0;for(;;){if(d){var
k=d[2],n=d[1],b=n[2],l=n[1];if(l){var
o=l[2],p=l[1],x=p[2];if(w===p[1]){var
m=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(b),q=b[1],r=q[2],s=q[1];if(1===a(f[25],x)){var
y=m(h,r),d=k,c=[0,[0,o,b],c],i=m(i,s),h=y;continue}var
z=m(h,s),d=k,c=[0,[0,o,b],c],i=m(i,r),h=z;continue}var
d=k,c=[0,[0,l,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
d=k,c=[0,[0,0,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
t=a(g[1],i),A=0,B=function(b,a){return b+a|0},C=e(g[16],B,A,i),u=a(g[1],h),D=0,E=function(b,a){return b+a|0};return[0,c,j+u*C+t*e(g[16],E,D,h)-u*t]}}var
f2=[0,is,function(a){var
c=a[2],d=[0,0,fZ(a)];function
f(b,a){var
d=a[1],c=is(b,a[2]);return[0,[0,[0,b,c[2]],d],c[1]]}var
h=e(aV[14],f,c,d)[1];function
i(b,a){return kc(b[2],a[2])}return b(g[41],i,h)}];function
f3(a){var
c=a[1];if(c){var
d=a[2];if(d)return b(f[26],c[1],d[1])}return 0}function
qm(a,h){var
c=a[1];if(c){var
d=a[2];if(d){var
e=d[1],g=b(f[26],c[1],e);return g?b(f[26],h,e):g}}return 0}function
f4(b,e){var
a=e;for(;;){if(a){var
c=a[2],d=a[1][1];if(d===b)return[0,1,c];if(d<b){var
a=c;continue}return[0,0,a]}return qn}}function
it(d){var
a=d;for(;;){if(a){var
b=a[1],c=b[1],e=a[2],f=b[4],g=b[3],h=b[2];if(c)if(!c[2])return[0,[0,c[1][1],c,h,g,f]];var
a=e;continue}return 0}}function
iu(f,k){var
j=it(f);if(j)return[0,j[1]];var
b=f;a:for(;;){if(b){var
c=b[1],i=c[1],a=i,o=b[2],p=c[4],q=c[3],r=c[2];for(;;){if(a){var
h=a[1][1],n=a[2],l=0,m=function(d){return function(a,b){var
c=b[2];return f4(d,b[1])[1]?f3(c[1])?a+1|0:a:a}}(h);if(2!==e(g[16],m,l,k)){var
a=n;continue}var
d=[0,h]}else
var
d=0;if(d)return[0,[0,d[1],i,r,q,p]];var
b=o;continue a}}return 0}}var
f5=[0,f3,qm,f4,it,iu,function(i){var
h=fZ(i),j=0;function
k(c,d){var
a=d[2],e=a[1],g=e[1],j=d[1];if(g){var
h=e[2];if(h){var
i=g[1];return b(f[26],i,h[1])?[0,[0,j,i,a[2],a[4]+a[3]|0],c]:c}}return c}var
c=e(g[16],k,j,h),d=iu(c,h);if(d){var
a=d[1];return[0,[0,[0,a[1],a[2],a[3],a[4]],0],0]}var
l=0;function
m(s,f){var
p=f[1],e=p,n=h,k=s,t=f[4],u=f[3],v=f[2];a:for(;;){if(e){var
o=e[1][1],c=n,b=0,a=0,q=e[2],r=t-1|0;for(;;){if(c){var
g=c[2],l=c[1],d=l[2],i=d[3]+d[4]|0,m=f4(o,l[1]),j=m[2];if(0===m[1]){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}if(f3(d[1])){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}var
c=g,b=(b+i|0)+r|0,a=[0,[0,j,d],a];continue}var
e=q,n=a,k=[0,[0,[0,o,p,v,u],b],k];continue a}}return k}}var
n=e(g[16],m,l,c);function
o(b,a){return aa(b[2],a[2])}return b(g[41],o,n)}];function
qo(i,d){var
j=0;function
k(d,c){var
e=a(p[7][18],c[1]);return b(h[5],d,e)}var
c=e(g[16],k,j,d),l=[0,[0,e(p[7][11],c,qq,i),0,qp],d],f=f1(c,f5[6],f2[2],l);if(0===f[0]){var
n=f[1][1];try{var
q=[0,f0(bU[1],c,n[1])];return q}catch(c){c=w(c);if(a(bz[22],c)){var
o=a(eg[1],c);b(m[2],qr,o);return 0}throw c}}return 0}var
qs=[0,qo,function(j){var
d=f1(h[7],f5[6],f2[2],j);if(0===d[0]){var
c=d[1][2],b=bU[1];for(;;){if(c){var
f=c[1],i=f[1],k=c[2],l=iq(f0(b,i,f[2][1])),c=k,b=e(bU[4],i,l,b);continue}var
m=0,n=function(c,b,a){return[0,[0,c,b],a]},o=e(bU[12],n,b,m);return[0,a(g[6],o)]}}return[1,d[1]]}];function
bA(b,a){return eJ(b,a)[1]}function
iv(d,c,a){var
f=0;function
h(c,f){function
h(a,e){var
c=b(d,f,e);return c?[0,c[1],a]:a}return e(g[16],h,c,a)}return e(g[16],h,f,c)}function
eK(b,a){if(0===b)if(0===a)return 0;return 1}function
f6(s,r,q){var
j=q[2],k=q[1],l=r[2],m=r[1],n=j[3],e=j[2],g=j[1],o=l[3],h=l[2],i=l[1],t=b(p[7][17],s,i),u=b(p[7][17],s,g);if(t)if(u){var
c=u[1],d=t[1],v=a(f[25],c);if(-1===e$(a(f[25],d),v)){var
w=a(f[15],c),x=b(f[9],n,w),y=a(f[15],d),z=b(f[9],o,y),A=b(f[1],z,x),B=eK(h,e),C=[0,g,a(f[15],c)],D=[0,bA([0,i,a(f[15],d)],C),B,A],E=[0,k,a(f[15],c)];return[0,[0,bA([0,m,a(f[15],d)],E),D]]}if(0===h){var
F=b(f[9],n,qt),G=b(f[9],d,c),H=a(f[3],G),I=b(f[9],o,H),J=b(f[1],I,F),K=eK(h,e),L=b(f[9],d,c),M=[0,bA([0,i,a(f[3],L)],[0,g,qu]),K,J],N=b(f[9],d,c);return[0,[0,bA([0,m,a(f[3],N)],[0,k,qv]),M]]}if(0===e){var
O=b(f[9],o,qw),P=b(f[9],c,d),Q=a(f[3],P),R=b(f[9],n,Q),S=b(f[1],R,O),T=eK(h,e),U=b(f[9],c,d),V=[0,bA([0,g,a(f[3],U)],[0,i,qx]),T,S],W=b(f[9],c,d);return[0,[0,bA([0,k,a(f[3],W)],[0,m,qy]),V]]}return 0}return 0}function
iw(a){function
b(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=fY(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b}return e(g[16],b,qz,a)}function
ix(t,e,s){var
k=e[2],l=e[1],m=e[4][1],u=m[2],v=m[1];function
n(e,c,a){if(c){var
f=c[1][3];if(a){var
d=a[1];return b(e,f,d)?[0,[0,l,k,d]]:c}return c}return a?[0,[0,l,k,a[1]]]:0}var
c=n(f[29],t,v),d=n(f[30],s,u);if(c)if(d){var
g=d[1],i=g[2],o=g[1],j=c[1],p=j[1],w=j[2];if(b(f[29],j[3],g[3]))return[0,[0,c,d]];var
q=i[1];if(q){var
r=f6(q[1][1],[0,p,w],[0,o,i]);return r?[1,r[1]]:a(h[2],qA)}return[1,[0,bA([0,p,qC],[0,o,qB]),i]]}return[0,[0,c,d]]}var
C=[0,pq,au,eG,pr,ps,ce,aV,px,ae,ic,cf,pz,cZ,fW,id,pF,pJ,pN,ie,pR,pS,ig,ih,eH,eI,pZ,fX,fY,ii,fZ,eJ,ij,ik,il,im,p7,bU,p8,io,ip,f0,iq,ir,f1,f2,f5,qs,[0,bA,iv,eK,f6,iw,ix,function(t,a){function
c(a){switch(a[0]){case
0:var
j=a[1];return[0,[0,[0,[0,j,qD],0],b(g[5],t,j)],0];case
1:var
u=a[3],v=a[1],w=c(a[2]),x=c(u);return iv(function(a,b){return f6(v,a,b)},w,x);default:var
y=a[2],z=c(a[1]),A=c(y),f=iw(b(h[22],z,A));if(0===f[0]){var
B=f[1],C=function(a,c){if(0===a[0]){var
b=a[1];return ix(b[1],c,b[2])}return a},i=e(g[16],C,qE,B);if(0===i[0]){var
k=i[1],d=k[2],l=k[1];if(l){var
m=l[1],n=m[2],o=m[1];if(d){var
p=d[1];return[0,[0,o,n],[0,[0,p[1],p[2]],0]]}var
r=n,q=o}else{if(!d)return 0;var
s=d[1],r=s[2],q=s[1]}return[0,[0,q,r],0]}return[0,i[1],0]}return[0,f[1],0]}}return c(a)}]];a6(745,C,"Micromega_plugin.Mfourier");var
qF=0,qG=0,qH=0,qI=r[13],qJ=r[12][8],qL=0;function
qM(b){return[1,a(l[29][7],b)]}var
c0=[0,l[30][7],qM,qL,qK,qJ,qI],qN=r[77],qO=r[80],qR=l[29][9],bB=[0,function(b){return[0,a(l[30][7],b),0]},qR,qQ,qP,qO,qN];function
c1(e,c){function
d(c){switch(c[0]){case
0:var
g=a(e[2],c[1]);return a(p[6][4],g);case
1:var
h=a(l[29][2],c[1]);return a(p[6][2],h);case
2:var
i=c[1],j=d(c[2]),k=d(i);return b(p[6][7],k,j);case
3:var
m=c[1],n=d(c[2]),o=a(p[6][8],n),q=d(m);return b(p[6][7],q,o);case
4:var
r=c[2],s=d(c[1]),t=d(r);return b(p[6][6],s,t);case
5:var
u=d(c[1]);return a(p[6][8],u);default:var
v=c[2],w=d(c[1]),x=a(l[29][3],v),f=function(c){if(0===c){var
d=a(e[2],e[4]);return a(p[6][4],d)}var
g=f(c-1|0);return b(p[6][6],w,g)};return f(x)}}return d(c)}function
iz(b){function
c(f,c,b){var
d=a(l[30][2],f),e=1===c?[1,d]:[6,[1,d],a(l[30][3],c)];return W(b,qT)?e:[4,e,b]}return e(p[5][12],c,b,qS)}function
iA(o,h){function
p(c){var
d=a(f[24],c);return b(f[26],d,c)}if(b(g[23],p,h)){var
i=function(a){return iz(b(g[5],o,a))},e=qX,c=0,d=h;for(;;){if(d){var
j=d[2],k=d[1];if(b(f[26],k,qU)){var
c=c+1|0,d=j;continue}var
q=a(l[18],k),m=[0,a(l[30][7],q)],n=W(m,qV)?i(c):[4,m,i(c)],r=W(e,qW)?n:[2,n,e],e=r,c=c+1|0,d=j;continue}return e}}throw[0,G,qY]}function
iB(e,d){var
b=d;for(;;){var
c=a(e,b);if(W(c,b))return c;var
b=c;continue}}function
iC(b,e){var
d=I(r[74],b[3],b[4],b[5],b[6]);function
c(b){if(typeof
b!=="number")switch(b[0]){case
3:var
e=b[1],f=c(b[2]);return a(d,[3,c(e),f]);case
4:var
g=b[1],h=c(b[2]);return a(d,[4,c(g),h])}return a(d,b)}return c(e)}function
eL(b,a){return iB(function(a){return iC(b,a)},a)}function
iD(d){function
j(p,o){var
f=p,d=o;for(;;){if(f){var
g=f[2],a=f[1];if(d){var
c=d[1];if(typeof
c==="number")var
b=0;else
switch(c[0]){case
2:if(typeof
a==="number")var
b=0;else
if(2===a[0]){var
h=c[1],k=a[2],l=c[2];if(W(h,a[1]))var
e=[0,[2,h,[4,l,k]]],b=1;else
var
e=0,b=1}else
var
b=0;break;case
3:if(typeof
a==="number")var
b=0;else
if(3===a[0]){var
i=c[1],m=a[2],n=c[2];if(W(i,a[1]))var
e=[0,[3,i,[4,n,m]]],b=1;else
var
e=0,b=1}else
var
b=0;break;default:var
b=0}if(!b)var
e=0;if(e){var
f=g,d=[0,e[1]];continue}return[4,c,j(g,[0,a])]}var
f=g,d=[0,a];continue}return d?d[1]:0}}var
a=d,c=0,f=0;for(;;){if(typeof
a!=="number"&&4===a[0]){var
e=[0,a[1],c],a=a[2],c=e;continue}return j(b(g[41],K.caml_compare,[0,a,c]),f)}}function
iE(c,f){var
h=0;function
i(d,a){return[0,b(p[6][1],c,a),d]}var
d=e(g[16],i,h,f);if(W(c,p[5][1])){var
j=[1,q[1]],k=a(g[6],d);return[0,a(p[7][5],[0,[1,q[2]],k]),0,j]}var
l=[1,q[1]],m=a(g[6],d);return[0,a(p[7][5],[0,[1,q[1]],m]),0,l]}function
iF(a){function
c(f,d){var
b=f,a=d;for(;;){if(a){if(0===a[1][2]){var
b=b+1|0,a=a[2];continue}var
g=c(b+1|0,a[2]),h=1,i=p[7][3],j=function(a){return q0};return[0,[0,e(p[7][10],b+1|0,j,i),h,qZ],g]}return 0}}return c(0,a)}function
q1(a){switch(a){case
0:return q2;case
1:return q3;case
2:return q4;default:return q5}}var
eM=a(ef[1],[0,p[5][10]]);function
iG(c){function
f(a){return a[1]}var
d=b(g[13],f,c),i=a(eM[5],p[5][1]);function
j(c,a){function
d(c,d,a){return b(eM[4],c,a)}return e(p[6][9],d,a,c)}var
k=e(g[16],j,i,d),l=0;function
m(b,a){return[0,iE(b,d),a]}var
n=e(eM[14],m,k,l),o=[1,q[2]],r=1;function
s(a){return 2===a[2]?[1,q[2]]:[1,q[1]]}var
t=b(g[13],s,c),u=[0,a(p[7][5],[0,[1,q[2]],t]),r,o],v=[0,u,iF(c)],w=b(h[22],v,n),x=[1,q[1]];return[0,[0,a(p[7][5],[0,[1,q[2]],0]),1,x],w]}var
q6=l[30][7];function
f7(d,c){var
e=c[1],g=d[1],f=c[2];if(e){var
j=e[2],i=function(d,c){if(d){var
e=d[1],m=d[2];if(c){var
j=c[1],f=i(m,c[2]),k=b(q[23],e,q[1]);if(-1===k){var
n=[0,a(l[30][1],j)];return[4,[2,[0,a(g,e)],n],f]}if(0===k)return f;var
o=[0,a(l[30][1],j)];return[4,[3,[5,a(g,e)],o],f]}return a(h[2],q7)}return 0};return iD(eL(d,i(j,f)))}return a(h[2],q8)}var
q_=[a8,q9,a7(0)],f8=[a8,q$,a7(0)],aW=a(bQ[1],[0,p[5][10]]);function
iH(c){var
d=[0,0];function
o(b,a){return aa(b[1],a[1])}var
h=[0,aW[1],0];function
i(i,h){var
j=i[2],k=i[1],q=h[2],m=[0,h[1],0];function
n(g,j,i){var
h=i[2],c=i[1];if(W(g,p[5][1]))return[0,c,h];try{var
q=b(aW[23],g,c),l=q,k=c}catch(a){a=w(a);if(a!==P)throw a;var
m=e(aW[4],g,d[1],c),n=d[1];d[1]++;var
l=n,k=m}var
o=0===a(f[25],j)?h:[0,[0,l,j],h];return[0,k,o]}var
c=e(p[6][9],n,k,m),r=c[2],s=c[1],t=b(p[6][1],p[5][1],k),u=a(f[3],t);if(0===j)var
l=0;else{if(!(3<=j))throw f8;var
l=1}return[0,s,[0,[0,b(g[41],o,r),l,u],q]]}return e(g[17],i,c,h)[2]}function
iI(e){var
f=iG(e);try{var
c=a(C[47][2],f);if(0===c[0])var
i=a(p[7][8],c[1]),d=[0,a(l[22],i)];else
var
d=0;return d}catch(c){c=w(c);if(a(bz[22],c)){if(C[4]){var
g=a(eg[1],c);b(m[2],ra,g);a(h[46],h[24])}return 0}throw c}}function
iJ(c){try{var
d=iH(c),f=a(C[47][2],d);if(0===f[0])var
h=0;else{var
i=f[1];if(C[4])e(m[2],rb,C[13],i);var
k=b(C[48][7],d,i),n=a(g[3],k)[1],o=function(a){return[0,a[1]+1|0,a[2]]},j=b(g[13],o,n);if(C[4])e(m[2],rc,p[7][4],j);var
q=a(p[7][8],j),h=[0,a(l[22],q)]}return h}catch(a){a=w(a);if(a===f8)return iI(c);throw a}}function
iK(d){var
b=a(g[39],d),e=b[2],c=iJ(b[1]);return c?[0,[0,c[1],e]]:0}function
rd(e,c){var
d=a(g[1],c)-1|0,f=b(l[14],0,d),i=b(g[40],c,f);function
j(a){return 1===a[1][2]?1:0}var
k=b(g[32],j,i)[2];function
m(b){var
c=b[1],d=c[2],f=b[2],g=c[1];return 1===d?a(h[2],re):[0,[0,c1(e,g),d],f]}return iK(b(g[13],m,k))}function
iL(c,b){try{var
e=rd(c,b);return e}catch(b){b=w(b);if(a(bz[22],b)){var
d=a(eg[1],b);a(h[27],d);return 0}throw b}}function
c2(e,c){var
d=a(g[1],e),f=b(h[5],c,e$(d,c));return b(h[5],d,f)}function
rf(d,b,a){var
e=c2(a,d);C[10][1]=e;var
c=iL(b,a);return c?[0,f7(b,c[1])]:0}function
rg(n,c){a(p[22][1][6],0);var
o=c2(c,n);C[10][1]=o;function
q(c,b){return[0,c,[0,a(l[30][1],b)]]}var
f=b(l[23],q,c);function
s(c,a){var
d=a[1],e=c[1],f=d[1],g=e[1],h=[3,c[2],a[2]];return[0,[0,[4,g,f],b(r[53],e[2],d[2])],h]}var
t=b(l[9],s,f),u=0;function
v(b,a){var
c=a[1],d=c[2],e=a[2],f=c[1];return d?[0,[0,[0,f,d[1]],e],b]:b}var
w=e(g[16],v,u,t),i=b(h[22],f,w);function
x(a){return c1(bB,a[1][1])}var
y=b(g[13],x,i),z=aW[1];function
A(c,b){function
d(c,f,b){var
d=a(p[5][13],c);return d?e(aW[4],d[1],c,b):b}return e(p[6][9],d,b,c)}var
B=e(g[16],A,z,y);function
j(b){var
c=[0,bB[4]];function
d(d,c,b){var
e=a(l[30][3],c);return[4,[6,[1,a(l[30][2],d)],e],b]}return e(p[5][12],d,b,c)}var
D=eb(r[63],bB[3],bB[4],r[79],r[80],r[82],r[81],r[77]),E=0;function
F(d,c,b){var
e=[1,a(D,j(d))];return[0,[0,[0,j(c),3],e],b]}var
G=e(aW[12],F,B,E),k=b(h[22],i,G);function
H(a){return a[1]}var
m=iL(bB,b(g[13],H,k));if(m){var
I=f7(bB,m[1]),d=function(c){if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
e=a(l[29][1],c[1]);return b(g[5],k,e)[2];case
1:return[1,c[1]];case
2:var
f=c[1];return[2,f,d(c[2])];case
3:var
h=c[1],i=d(c[2]);return[3,d(h),i];case
4:var
j=c[1],m=d(c[2]);return[4,d(j),m];default:return[5,c[1]]}};return[0,d(I)]}return 0}function
rh(c){function
h(a){return a[1]}var
i=b(g[13],h,c),j=a(p[6][4],ri);function
k(c,a){return b(p[6][7],a,c)}var
l=e(g[16],k,j,i),m=0;function
n(b,c,a){return W(b,p[5][1])?a:[0,b,a]}var
d=e(p[6][9],n,l,m);function
o(c){var
e=c[1],h=c[2],i=b(p[6][1],p[5][1],e),j=a(f[3],i);function
k(a){return b(p[6][1],a,e)}var
l=b(g[13],k,d);return[0,a(p[7][5],l),h,j]}return[0,b(g[13],o,c),d]}function
rj(b,a){return[2,b,a]}function
rk(b,a){return[4,b,a]}function
rl(a){return[0,a]}function
rm(a){return[5,a]}function
rn(e,d,c){var
a=c;for(;;){if(a){var
f=a[2];if(b(e,d,a[1]))return 1;var
a=f;continue}return 0}}function
iM(d,c,g){var
a=g;for(;;){if(a){var
e=a[2],f=a[1];if(b(d,c,f[1])){var
a=e;continue}return[0,f,iM(d,c,e)]}return 0}}function
iN(c,a){return 0===b(p[7][15],c,a)?1:0}function
ro(c,a){var
b=0;function
d(b,a){return iN(a,c)?b:[0,a,b]}return e(g[16],d,b,a)}function
f9(b,a){var
c=a[2],d=a[1];if(0===c)return[0,c1(b,d),0];if(3<=c)return[0,c1(b,d),1];throw[0,G,rp]}function
iO(a){return 0===a?0:3}function
rq(d){var
c=a(g[39],d),e=c[1],f=a(l[22],c[2]);function
h(a){return[1,a]}var
i=b(g[13],h,f);return b(g[40],e,i)}function
iP(c){var
f=c[3],h=c[2],e=a(g[39],c[1]),i=e[1],d=a(l[22],[0,f,e[2]]);if(d){var
j=d[2],k=[1,d[1]],m=function(a){return[1,a]},n=b(g[13],m,j);return[0,b(g[40],i,n),h,k]}throw[0,G,rr]}function
rs(e,d){var
b=iP(d),c=b[3],f=b[2],g=b[1];try{var
i=iA(e,a(p[7][8],g)),j=a(l[17],c),k=a(l[30][7],j),m=a(l[18],c),n=a(l[30][7],m),o=[0,[2,[4,[0,k],i],[5,[0,n]]],iO(f)];return o}catch(b){b=w(b);if(b[1]===fn)return a(h[2],rt);throw b}}function
bV(c){if(typeof
c==="number")return[0,q[2],0];else
switch(c[0]){case
0:var
e=c[1],z=[0,[1,a(l[18],e)]];return[0,a(l[17],e),z];case
1:return[0,q[2],[1,c[1]]];case
2:return a(h[2],ru);case
3:var
f=bV(c[1]);return[0,f[1],[3,f[2]]];case
4:var
g=c[1],A=g[2],i=bV(g[1]),j=i[2],k=i[1],m=bV(A),n=m[2],o=m[1],d=b(q[17],k,o),p=b(q[15],k,d),r=b(q[15],o,d),B=b(q[10],p,r),s=b(q[10],d,B);return 0===b(q[23],s,q[2])?[0,q[2],[4,[0,j,n]]]:[0,s,[4,[0,[6,[0,[0,[1,r]],j]],[6,[0,[0,[1,p]],n]]]]];case
5:return a(h[2],rv);case
6:var
t=c[1],C=t[2],u=bV(t[1]),D=u[2],E=u[1],v=bV(C),F=[6,[0,D,v[2]]];return[0,b(q[10],E,v[1]),F];case
7:return a(h[2],rw);default:var
w=c[1],x=w[2],y=bV(w[1]),G=[8,[0,y[2],x]];return[0,b(q[19],y[1],x),G]}}function
f_(b){var
a=bV(b);return[0,a[1],a[2]]}function
rx(i,g,f){var
d=0,c=0,b=f;for(;;){if(b){var
e=b[2];if(a(i,b[1])){if(d===g)return c;var
d=d+1|0,c=c+1|0,b=e;continue}var
c=c+1|0,b=e;continue}return a(h[2],ry)}}function
bW(c){switch(c[0]){case
0:return[0,q[2],[0,c[1]]];case
1:return[0,q[2],[1,c[1]]];case
2:return[0,q[2],[2,c[1]]];case
3:var
e=c[1],v=[3,[1,a(l[18],e)]];return[0,a(l[17],e),v];case
4:var
f=c[1],w=[4,[1,a(l[18],f)]];return[0,a(l[17],f),w];case
5:var
g=c[1],x=[5,[1,a(l[18],g)]];return[0,a(l[17],g),x];case
6:var
h=f_(c[1]),i=h[1],y=[6,h[2]];return[0,b(q[10],i,i),y];case
7:return[0,q[2],[7,c[1]]];case
8:var
z=c[2],j=f_(c[1]),A=j[2],B=j[1],k=bW(z),C=[8,A,k[2]];return[0,b(q[10],B,k[1]),C];case
9:var
D=c[2],m=bW(c[1]),n=m[1],E=m[2],o=bW(D),p=o[1],F=o[2],d=b(q[17],n,p),r=b(q[15],n,d),s=b(q[15],p,d),G=b(q[10],r,s);return[0,b(q[10],d,G),[9,[10,[4,[1,s]],E],[10,[4,[1,r]],F]]];default:var
H=c[2],t=bW(c[1]),I=t[2],J=t[1],u=bW(H),K=[10,I,u[2]];return[0,b(q[10],J,u[1]),K]}}function
aX(b){if(typeof
b==="number")return[0,a(l[30][8],rz)];else
switch(b[0]){case
0:return[0,a(l[30][8],b[1])];case
1:var
c=b[1],j=ke(e(iy[4],c,1,kd(c)-1|0));return[1,a(l[30][4],j)];case
3:return[5,aX(b[1])];case
4:var
d=b[1],k=d[1],m=aX(d[2]);return[2,aX(k),m];case
5:var
f=b[1],n=f[1],o=aX(f[2]);return[3,aX(n),o];case
6:var
g=b[1],p=g[1],q=aX(g[2]);return[4,aX(p),q];case
8:var
i=b[1],r=i[1],s=a(l[30][3],i[2]);return[6,aX(r),s];default:return a(h[2],rA)}}function
f$(b){var
c=aX(b),d=r[77],e=r[81],f=r[82],g=r[80],h=r[79],i=a(l[30][8],rB),j=a(l[30][8],rC);return b0(r[39],j,i,h,g,f,e,d,c)}function
eN(b){if(b){var
c=b[2],d=b[1];if(c){var
e=eN(c);return[3,[0,a(l[30][1],d)],e]}return[0,a(l[30][1],d)]}return 0}function
rD(c){function
d(c){switch(c[0]){case
0:return[0,a(l[30][1],c[1])];case
1:return[0,a(l[30][1],c[1])];case
2:return[0,a(l[30][1],c[1])];case
6:return[1,f$(c[1])];case
7:return eN(c[1]);case
8:var
g=c[1],h=d(c[2]);return[2,f$(g),h];case
9:var
i=c[1],j=d(c[2]);return[4,d(i),j];case
10:var
k=c[1],m=d(c[2]);return[3,d(k),m];default:var
e=c[1];return 0===b(f[37],e,rE)?0:[5,a(l[30][8],e)]}}return eL(bB,d(c))}function
aY(b){if(typeof
b==="number")return rF;else
switch(b[0]){case
0:var
k=a(f[49],b[1]);return[0,a(l[30][7],k)];case
1:var
c=b[1],m=ke(e(iy[4],c,1,kd(c)-1|0));return[1,a(l[30][4],m)];case
3:return[5,aY(b[1])];case
4:var
d=b[1],n=d[1],o=aY(d[2]);return[2,aY(n),o];case
5:var
g=b[1],p=g[1],q=aY(g[2]);return[3,aY(p),q];case
6:var
i=b[1],r=i[1],s=aY(i[2]);return[4,aY(r),s];case
8:var
j=b[1],t=j[1],u=a(l[30][3],j[2]);return[6,aY(t),u];default:return a(h[2],rG)}}function
ga(b){var
c=aY(b),d=r[13],e=r[12][6],f=r[12][7],g=r[12][8],h=r[12][5],i=a(l[30][5],1),j=a(l[30][5],0);return b0(r[39],j,i,h,g,f,e,d,c)}function
rH(c){var
e=bW(c)[2];function
d(k){var
c=k;for(;;)switch(c[0]){case
0:return[0,a(l[30][1],c[1])];case
1:return[0,a(l[30][1],c[1])];case
2:return[0,a(l[30][1],c[1])];case
6:return[1,ga(c[1])];case
7:return eN(c[1]);case
8:var
i=c[2],e=c[1];if(typeof
e==="number")var
g=0;else
if(0===e[0])var
j=b(f[26],e[1],rJ),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
c=i;continue}var
n=d(i);return[2,ga(e),n];case
9:var
o=c[1],p=d(c[2]);return[4,d(o),p];case
10:var
q=c[1],r=d(c[2]);return[3,d(q),r];default:var
h=c[1];if(0===b(f[37],h,rI))return 0;var
m=a(f[49],h);return[5,a(l[30][7],m)]}}return eL(c0,d(e))}var
iQ=[0,function(e,d){var
c=0,b=d;for(;;){if(b){var
f=b[2];if(W(e,b[1]))return c;var
c=c+1|0,b=f;continue}return a(h[2],rK)}}];function
gb(b){var
c=b[1],d=a(l[18],b[2]),f=[0,a(l[30][7],d)];function
h(f,b){var
g=b[2],h=a(p[22][1][8],b[1]);function
c(d,c,b){var
e=a(l[30][3],c);return[4,[6,[1,a(l[30][2],d)],e],b]}var
d=e(p[5][12],c,h,rL),i=a(l[18],g);return[2,[4,[0,a(l[30][7],i)],d],f]}return e(g[16],h,f,c)}function
aZ(d,c){if(typeof
c==="number")return 0;else
switch(c[0]){case
2:return[5,a(l[30][7],c[1])];case
3:var
f=gb(c[1]);return[1,a(r[92],f)];case
4:var
g=c[2],i=gb(c[1]),j=a(r[92],i);return[2,j,aZ(d,g)];case
6:var
k=c[1],m=aZ(d,c[2]);return[3,aZ(d,k),m];case
7:var
n=c[1],o=aZ(d,c[2]);return[4,aZ(d,n),o];case
0:case
1:var
e=b(iQ[1],c[1],d);return[0,a(l[30][1],e)];default:return a(h[2],rM)}}function
c3(c,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
e=a[3],d=a[2],f=a[1];if(typeof
d!=="number"&&8===d[0]){var
i=d[1],j=c3([0,f,c],e);return[1,aZ(c,i),j]}var
h=c3([0,f,c],e);return[0,aZ(c,d),h]}var
k=a[5],l=a[4],m=a[2],n=[0,a[1],c],o=function(a){return c3(n,a)},p=b(g[13],o,k),q=aZ(c,l);return[2,aZ(c,m),q,p]}}function
iR(e,c){var
f=1+a(p[15],c)|0,d=b(p[18],f,c)[2];if(C[4])I(m[1],h[24],rN,p[13],d);return c3(e,d)}function
eO(c){var
d=a(C[47][2],c);if(0===d[0])return 0;var
f=d[1];if(C[4])e(m[2],rO,C[13],f);var
i=b(C[48][7],c,f),h=a(g[3],i)[1];if(C[4])e(m[2],rP,p[7][4],h);var
j=a(p[7][8],h);return[0,a(l[22],j)]}function
rQ(d,c){var
e=a(f[40],c);return b(h[49],d,e)}function
rR(d,c){var
e=a(q[33],c);return b(h[49],d,e)}function
eP(g,f){var
e=0,d=g,c=f;for(;;){if(c){if(d){var
i=c[2],j=d[2],k=b(p[20],c[1],d[1]),e=b(p[19],k,e),d=j,c=i;continue}return a(h[2],rS)}return e}}function
iS(d){var
b=a(g[39],d),e=b[2],c=eO(b[1]);return c?[0,eP(e,c[1])]:0}var
iT=C[4]?function(b){a(m[2],rT);a(h[46],h[24]);var
c=iS(b);a(m[2],rU);a(h[46],h[24]);return c}:iS;function
gc(m){var
d=m[2],h=m[1],i=h[3],j=h[2],k=h[1];if(k){var
o=function(a){return a[2]},p=b(g[13],o,k),n=a(l[21],p),c=[1,n];if(b(f[32],c,rV))return[2,h,d];var
q=b(f[12],i,c);if(0===a(f[25],q)){if(1<=a(f[25],c)){var
r=b(f[9],i,c),s=function(a){var
d=a[1];return[0,d,b(f[9],a[2],c)]};return[2,[0,b(g[13],s,k),j,r],[5,n,d]]}throw[0,G,rW]}if(0===j)return[0,[8,d]];var
t=b(f[9],i,c),u=a(f[24],t),v=function(a){var
d=a[1];return[0,d,b(f[9],a[2],c)]};return[1,[0,b(g[13],v,k),j,u],[8,d]]}return e(C[26],j,rX,i)?0:[0,d]}function
iU(k,j,i){var
e=i[1],g=j[1],m=e[2],n=e[1],o=g[2],q=g[1],t=i[2],u=j[2],v=e[3],w=g[3];function
h(d,c){var
e=a(l[18],c),g=b(p[20],e,t),h=a(l[18],d),i=[7,b(p[20],h,u),g],j=b(f[6],v,c),k=b(f[6],w,d),r=b(f[1],k,j),s=b(C[48][3],o,m),x=b(p[7][13],c,n),y=b(p[7][13],d,q);return[0,[0,b(p[7][14],y,x),s,r],i]}var
r=b(p[7][17],k,q),s=b(p[7][17],k,n);if(r)if(s){var
c=s[1],d=r[1],x=a(f[25],c);if(-1===e$(a(f[25],d),x)){var
y=a(f[15],c);return[0,h(y,a(f[15],d))]}if(0===o){var
z=[0,a(f[25],d)],A=b(f[6],c,z),B=a(f[3],A);return[0,h(B,a(f[15],d))]}if(0===m){var
D=a(f[15],c),E=[0,a(f[25],c)],F=b(f[6],d,E);return[0,h(D,a(f[3],F))]}return 0}return 0}var
c4=[a8,rY,a7(0)];function
iV(a){var
b=0;function
c(b,c){var
a=gc([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,c4,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return e(g[16],c,b,a)}function
c5(g,c){if(0===a(q[22],c))return[0,q[2],q[1]];var
d=b(q[14],g,c),h=d[1],e=c5(c,d[2]),f=e[2],i=e[1],j=b(q[10],h,f);return[0,f,b(q[8],i,j)]}function
rZ(j,i){var
c=a(q[35],j),d=a(q[35],i),e=c5(c,d),f=e[2],g=e[1],k=b(q[10],f,d),l=b(q[10],g,c),n=b(q[5],l,k),o=a(q[33],n),p=a(q[33],d),r=a(q[33],f),s=a(q[33],c),t=a(q[33],g);return eb(m[1],h[24],r0,t,s,r,p,o)}var
r2=[a8,r1,a7(0)];function
r3(c){function
b(a){return 0===a[1][2]?1:0}return a(g[32],b)}function
iW(r,p){var
f=p[1],g=r[1];if(0===g[2])if(0===f[2]){var
d=g[1],c=f[1];for(;;){if(d)if(c){var
h=c[2],i=c[1],j=i[2],k=i[1],m=d[2],n=d[1],o=n[2],e=n[1];if(W(e,k)){var
s=q[2],t=a(l[18],j),u=a(l[18],o),v=b(q[17],u,t);if(0===b(q[23],v,s))return[0,[0,e,o,j]];var
d=m,c=h;continue}if(gN(e,k)){var
d=m;continue}var
c=h;continue}return 0}}return 0}function
iX(m,k){var
d=0,c=k;for(;;){if(c){var
f=c[2],e=c[1],n=a(m,e),h=b(l[15],n,f),i=h[1];if(i){var
j=i[1],o=j[2],p=j[1];return[0,[0,[0,p,e,o]],b(g[8],d,h[2])]}var
d=[0,e,d],c=f;continue}return[0,0,d]}}function
iY(a){return iX(iW,a)}function
eQ(f,b){var
c=0;function
d(c,d){var
e=a(f,d);if(e){var
b=gc(e[1]);if(typeof
b==="number")return c;else
switch(b[0]){case
0:throw[0,c4,b[1]];case
1:return[0,[0,b[1],b[2]],c];default:return[0,[0,b[1],b[2]],c]}}return[0,d,c]}return e(g[16],d,c,b)}function
eR(c,b,a){return eQ(function(a){return iU(c,b,a)},a)}function
iZ(r){var
i=iY(r),j=i[1],s=i[2];if(j){var
c=j[1],k=c[3],m=k[1],n=c[2],o=n[2],d=n[1],e=c[1],t=k[2],u=e[2],v=e[1],w=a(l[18],e[3]),q=c5(a(l[18],u),w),g=[1,q[1]],h=[1,q[2]],x=b(f[6],h,m[3]),y=b(f[6],g,d[3]),z=b(f[1],y,x),A=b(p[7][13],h,m[1]),B=b(p[7][13],g,d[1]),C=[0,b(p[7][14],B,A),0,z],D=a(l[18],h),E=b(p[20],D,t),F=a(l[18],g),G=b(p[20],F,o);return[0,eR(v,[0,C,b(p[19],G,E)],[0,[0,d,o],s])]}return 0}function
i0(e){function
h(c){var
a=c[1];if(0===a[2])try{var
d=a[1],e=function(d){var
a=d[2],c=b(f[26],a,r4);return c?c:b(f[26],a,r5)},h=[0,b(g[29],e,d)[1]];return h}catch(a){a=w(a);if(a===P)return 0;throw a}return 0}var
a=b(l[15],h,e),c=a[1],i=a[2];if(c){var
d=c[1];return[0,eR(d[1],d[2],i)]}return 0}function
i1(h){function
i(e){var
c=e[1];if(0===c[2])try{var
h=c[1],i=function(c){var
d=c[2],g=c[1],h=b(f[26],d,r6),e=h||b(f[26],d,r7);if(e){var
i=a(p[22][1][8],g);return a(p[5][4],i)}return e},d=b(g[29],i,h)[1],j=a(p[22][1][8],d),k=c[1],l=function(g){var
c=g[1],e=c===d?1:0;if(e)var
f=e;else
var
h=a(p[22][1][8],c),f=0===b(p[5][9],h,j)[2]?1:0;return f},m=b(g[23],l,k)?[0,d]:0;return m}catch(a){a=w(a);if(a===P)return 0;throw a}return 0}var
c=b(l[15],i,h),d=c[1],j=c[2];if(d){var
e=d[1];return[0,eQ(b(p[22][9],e[1],e[2]),j)]}return 0}function
i2(r){function
s(i){var
c=i;for(;;){if(c){var
d=c[2],e=c[1],j=e[1],f=a(l[18],e[2]);try{var
k=function(g){return function(c){var
d=a(l[18],c[2]),e=q[2],f=b(q[17],g,d);return b(q[24],f,e)}}(f),h=b(g[29],k,d),m=h[1],n=[0,[0,[0,j,f],[0,m,a(l[18],h[2])]]];return n}catch(a){a=w(a);if(a===P){var
c=d;continue}throw a}}return 0}}function
t(b){var
a=b[1];return 0===a[2]?s(a[1]):0}var
c=b(l[15],t,r),d=c[1],u=c[2];if(d){var
e=d[1],h=e[2],i=h[1],j=e[1],k=j[2],m=j[1],v=h[2],x=k[1],y=m[1],n=c5(m[2],k[2]),z=[1,n[2]],A=[1,n[1]],o=function(d,c){var
a=b(p[7][17],d,c);return a?a[1]:r8};return[0,eQ(function(g){var
c=g[1],d=c[1],h=g[2],j=c[3],k=c[2],l=o(y,d),m=o(x,d),n=b(f[6],m,z),q=b(f[6],l,A),r=b(f[1],q,n),e=a(f[3],r),s=b(f[6],e,i[3]),t=b(f[1],s,j),u=b(p[7][13],e,i[1]);return[0,[0,[0,b(p[7][14],u,d),k,t],[7,[4,[0,0,e],v],h]]]},u)]}return 0}function
r9(i){function
j(c){var
b=c[1];if(0===b[2])try{var
d=[0,a(g[3],b[1])[1]];return d}catch(a){a=w(a);if(a===P)return 0;throw a}return 0}var
d=b(l[15],j,i),e=d[1],k=d[2];if(e){var
h=e[1],c=h[2],n=h[1];if(C[4]){var
o=a(f[40],c[1][3]);I(m[2],r_,p[7][4],c[1][1],o)}return[0,eR(n,c,k)]}return 0}function
gd(e,d){var
b=d;for(;;){var
c=a(e,b);if(c){var
b=c[1];continue}return b}}function
ge(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1],d);if(c)return[0,c[1]];var
b=f;continue}return 0}}function
gf(a){var
b=[0,i0,[0,iZ,[0,i2,0]]];return gd(function(a){return ge(b,a)},a)}function
i3(a){var
b=[0,i1,0];return gd(function(a){return ge(b,a)},a)}function
i4(d){function
B(a){return 0===a[2]?1:0}var
r=b(g[32],B,d),s=r[2],t=r[1];if(t)var
D=0,E=function(c,a){function
d(c){return b(p[7][1],a[1],c[1])}return b(g[24],d,t)?c:[0,a[1],c]},u=e(g[16],E,D,s);else
var
F=function(a){return a[1]},u=b(g[15],F,s);var
G=[0,p[7][3],sa];function
H(c,g){var
h=a(C[6][4],c[2]),n=h?b(f[29],h[1],r$):0;if(n)return c;var
j=b(C[47][1],g,d);if(j){var
k=j[1];if(C[4])e(m[2],sb,p[7][4],g);var
i=c[2],l=c[1];return b(C[6][5],k,i)?[0,g,k]:[0,l,i]}return c}var
v=e(g[16],H,G,u),w=v[2],x=w[1],I=v[1];if(x){var
y=w[2];if(y)var
c=[0,[0,x[1],I,y[1]]],o=1;else
var
o=0}else
var
o=0;if(!o)var
c=0;if(c){var
i=c[1],j=i[3],k=i[2],n=i[1],J=a(l[17],n),K=q[2],L=a(l[18],n),M=b(q[8],L,K),N=a(l[17],j),O=a(l[18],j),P=[1,b(q[5],q[2],O)],z=eO([0,[0,b(p[7][13],[1,N],k),1,P],d]),Q=a(f[3],[1,M]),R=a(f[3],[1,J]),A=eO([0,[0,b(p[7][13],R,k),1,Q],d]);if(z)if(A){var
S=A[1],T=a(g[4],z[1]);return[0,[0,a(g[4],S),[0,n,k,j],T]]}return a(h[2],sc)}return 0}function
eS(c){function
d(c){var
d=c[1][1];function
e(b){return 0!==a(f[25],b[2])?1:0}return b(g[23],e,d)}return b(g[23],d,c)}function
gg(o,n,c){function
q(j,b){if(C[4]){a(m[2],sd);a(h[46],h[24])}if(eS(b)){var
k=a(g[39],b),l=k[2],n=i4(k[1]);if(n){var
c=n[1],d=c[2],o=d[3],e=d[2],q=d[1],s=c[3],t=c[1];if(C[4]){var
u=a(f[40],o),v=a(f[40],q);aR(m[2],se,p[7][4],e,v,u)}var
w=a(f[22],o),r=i(j,e,a(f[24],q),w,b);if(r){var
x=r[1],y=eP(l,s);return[0,[1,j,eP(l,t),e,y,x]]}return 0}return 0}throw[0,G,sf]}function
i(c,g,a,e,d){if(b(f[28],a,e))return sg;var
h=j(c+1|0,[0,[0,[0,g,0,a],[1,c]],d]);if(h){var
l=h[1],k=i(c,g,b(f[1],a,sh),e,d);return k?[0,[0,l,k[1]]]:0}return 0}function
j(d,c){if(eS(c)){if(C[4]){var
h=function(c,a){return b(p[9],c,a[1])},i=a(l[2],h);e(m[2],si,i,c)}try{var
f=a(n,c);if(C[4]){var
j=function(c,a){return b(p[9],c,a[1])},k=a(l[2],j);e(m[2],sj,k,f)}var
g=iT(f),r=g?[0,[0,d,g[1],0]]:o?q(d,f):0;return r}catch(a){a=w(a);if(a[1]===c4)return[0,[0,d,a[2],0]];throw a}}throw[0,G,sk]}var
k=a(g[1],c);try{var
t=j(k,iV(c)),d=t}catch(a){a=w(a);if(a[1]!==c4)throw a;var
d=[0,[0,k,a[2],0]]}if(d){var
r=d[1],s=function(b,a){return a};return[0,iR(b(l[23],s,c),r)]}return 0}function
gh(b){var
d=b[2],c=a(p[22][5],b[1]),e=c[1];return[0,e,d,a(f[3],c[2])]}function
sl(e,d,c){a(p[22][1][6],0);var
f=c2(c,d);C[10][1]=f;function
h(a){return f9(c0,a)}var
i=b(g[13],h,c),j=b(g[13],gh,i);function
k(b,a){return[0,b,[0,a]]}return gg(e,gf,b(l[23],k,j))}var
aM=[0,qF,qG,qH,c0,bB,c0,c1,iz,iA,iB,iC,eL,iD,iE,iF,q1,eM,iG,q6,f7,q_,f8,aW,iH,iI,iJ,iK,c2,rf,rg,rh,rj,rk,rl,rm,rn,iM,iN,ro,f9,iO,rq,iP,rs,f_,rx,bW,aX,f$,eN,rD,aY,ga,rH,iQ,gb,aZ,c3,iR,eO,rQ,rR,eP,iT,gc,iU,c4,iV,c5,rZ,r2,r3,iW,iX,iY,eQ,eR,iZ,i0,i1,i2,r9,gd,ge,gf,i3,i4,eS,gg,gh,sl,function(n,m,f){a(p[22][1][6],0);var
o=c2(f,m);C[10][1]=o;function
q(a){return f9(c0,a)}var
r=b(g[13],q,f);function
s(b,a){return[0,b,[0,a]]}var
c=b(l[23],s,r);function
t(b){return a(p[6][13],b[1][1])}var
i=b(g[23],t,c),u=aW[1];function
v(c,b){var
d=b[1][1];function
f(c,f,b){var
d=a(p[5][13],c);return d?e(aW[4],d[1],c,b):b}return e(p[6][9],f,d,c)}var
w=e(g[16],v,u,c);function
x(d,c,b){var
f=a(p[6][4],sm),g=e(p[6][3],d,sn,f),h=a(p[22][5],g),i=a(p[6][4],so);return[0,[0,[0,e(p[6][3],c,sp,i),1],[3,h]],b]}var
d=e(aW[12],x,w,c);if(i)var
j=d;else
var
A=function(c,a){var
d=a[1],e=c[1],f=d[1],g=e[1],h=[6,c[2],a[2]],i=b(p[10],e[2],d[2]);return[0,[0,b(p[6][6],g,f),i],h]},B=b(l[8],A,d),j=b(h[22],d,B);function
y(a){var
b=a[2];return[0,gh(a[1]),b]}var
k=b(g[13],y,j);if(eS(k)){var
z=i?gf:i3;return gg(n,z,k)}throw[0,G,sq]}];a6(747,aM,"Micromega_plugin.Certificate");var
c6=[0,function(p){var
c=a(bv[18],p),l=[a8,ss,a7(0)],f=[a8,st,a7(0)];function
q(d,b){var
f=a(c[1],d),g=e(H[21],b,su,fb);return[0,a(H[29],g),1,f]}function
r(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
s(b){try{var
c=[0,a(eh[3],b)];return c}catch(b){b=w(b);if(b===sr)return 0;if(a(bz[22],b))throw l;throw b}}function
t(c,a){var
d=e(H[32],a,0,1);try{e(H[32],a,0,0);var
f=0===c?4:1;e(H[80],a,f,1);var
g=1,b=g}catch(a){a=w(a);if(a[1]!==H[1])throw a;var
b=0}e(H[32],a,d,0);return b}function
u(a){var
c=e(H[32],a,0,1);try{e(H[32],a,0,0);var
b=e(H[80],a,0,1);return b}catch(b){b=w(b);if(b[1]===H[1]){e(H[32],a,c,0);return 0}throw b}}function
g(d,c,b){return t(d,c)?r(b,function(a){return u(c)}):a(b,0)}function
m(i){var
f=e(H[21],i,sv,fb),j=a(H[28],f),d=a(c[1],gU);function
n(f){for(;;){var
a=s(j);if(a){var
b=a[1];e(c[5],d,b[1],b[2]);continue}return 0}}try{g(0,f,n);a(h[77],j);var
o=e(H[21],i,sy,fb),p=[0,a(H[29],o),1,d];return p}catch(f){f=w(f);if(f===l){a(h[77],j);var
m=e(H[21],i,sw,fb),k=a(H[29],m);g(1,m,function(g){function
f(b,a){return e(eh[1],k,[0,b,a],sx)}b(c[11],f,d);return a(h[46],k)});return[0,k,1,d]}throw f}}function
v(b){var
d=b[1],e=b[3];return 0===b[2]?0:(a(h[59],d),a(c[2],e),b[2]=0,0)}function
n(b,j,i){var
d=b[1],k=b[3];if(0===b[2])throw f;var
l=a(H[31],d);e(c[5],k,j,i);return g(1,l,function(b){e(eh[1],d,[0,j,i],sz);return a(h[46],d)})}function
o(a,d){var
e=a[3];if(0===a[2])throw f;return b(c[7],e,d)}return[0,q,m,o,n,v,function(c,e){var
b=[d,function(b){try{var
a=[0,m(c)];return a}catch(a){return 0}}];return function(c){var
f=j(b),g=k===f?b[1]:d===f?a(i[2],b):b;if(g){var
h=g[1];try{var
m=o(h,c);return m}catch(b){b=w(b);if(b===P){var
l=a(e,c);n(h,c,l);return l}throw b}}return a(e,c)}}]}];a6(750,c6,"Micromega_plugin.Persistent_cache");var
sH=0;function
sI(d,c,b){var
f=a(H[94],0)[1],g=a(c,b),i=a(H[94],0)[1]-f;e(m[2],sJ,d,i);a(h[46],h[24]);return g}var
eU=h[7],gj=[0,eU],eV=[0,1],gk=[0,eU];function
gl(a){return[0,eV[1],gk[1]]}function
eW(a){return gj[1]}function
i8(b,a){function
c(b){var
c=b?b[1]:eU;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,1,0,e(g[17],h[16],b,sK),b,d,c]}function
sL(a){eV[1]=a;return 0}var
sO=[0,1,0,sN,sM,function(a){return eV[1]},sL],sQ=i8(sP,gj);b(gi[3],0,sQ);var
sS=i8(sR,gk);b(gi[3],0,sS);b(gi[4],0,sO);function
bD(d,c){if(typeof
c==="number")return 0===c?b(h[49],d,sT):b(h[49],d,sU);else
switch(c[0]){case
0:return b(h[49],d,sV);case
1:return I(m[1],d,sW,l[32][3],c[2]);case
2:return E(m[1],d,sX,bD,c[1],bD,c[2]);case
3:return E(m[1],d,sY,bD,c[1],bD,c[2]);case
4:return I(m[1],d,sZ,bD,c[1]);default:var
e=c[2],f=c[3],g=c[1],i=e?a(J[1][7],e[1]):s1;return eb(m[1],d,s0,bD,g,i,bD,f)}}function
a0(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
d=b[3],e=b[2];return[1,a(c,b[1]),e,d];case
2:var
f=b[1],g=a0(c,b[2]);return[2,a0(c,f),g];case
3:var
h=b[1],i=a0(c,b[2]);return[3,a0(c,h),i];case
4:return[4,a0(c,b[1])];default:var
j=b[2],k=b[1],l=a0(c,b[3]);return[5,a0(c,k),j,l]}}function
bd(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1],b[2],b[3]];case
2:var
d=b[1],e=bd(c,b[2]);return[2,bd(c,d),e];case
3:var
f=b[1],g=bd(c,b[2]);return[3,bd(c,f),g];case
4:return[4,bd(c,b[1])];default:var
h=b[2],i=b[1],j=bd(c,b[3]);return[5,bd(c,i),h,j]}}function
gm(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];if(b){var
c=b[1];return[0,c,gm(a[3])]}}return 0}var
eX=0,s2=0;function
eY(I,H,m,l,c){function
j(c,a){return b(h[22],c,a)}function
k(c,e){if(e){var
h=e[2],d=e[1],i=b(l,c[1],d[1]);if(i){if(a(m,i[1]))return[0,[0,c[2],[0,d[2],0]]];var
f=k(c,h);return 0===f[0]?[0,f[1]]:[1,[0,d,f[1]]]}var
g=k(c,h);return 0===g[0]?[0,g[1]]:[1,[0,d,g[1]]]}var
j=b(l,c[1],c[1]);return j?a(m,j[1])?[0,[0,c[2],0]]:[1,[0,c,0]]:[1,[0,c,0]]}function
i(a,d){if(a){var
m=a[1],f=i(a[2],d),l=f[2],n=f[1],j=function(l,f){var
g=f[2],i=f[1],a=m,d=l;for(;;){if(a){var
j=a[2],e=k(a[1],d);if(0!==e[0]){var
a=j,d=e[1];continue}var
c=[0,e[1]]}else
var
c=[1,d];return 0===c[0]?[0,i,b(h[22],g,c[1])]:[0,[0,c[1],i],g]}},c=e(g[17],j,d,s3),o=c[1],p=b(h[22],l,c[2]);return[0,b(h[22],n,o),p]}return[0,eX,0]}function
f(O,N){var
c=O,d=N;for(;;)if(typeof
d==="number")return 0===d?c?[0,eX,0]:[0,c7,0]:c?[0,c7,0]:[0,eX,0];else
switch(d[0]){case
0:return c?[0,c7,0]:[0,c7,0];case
1:var
e=d[2],k=d[1],P=0;if(c)var
L=a(H,k),M=function(a){function
c(a){return[0,a,e]}return b(g[13],c,a)},l=b(g[13],M,L);else
var
J=a(I,k),K=function(a){function
c(a){return[0,a,e]}return b(g[13],c,a)},l=b(g[13],K,J);return[0,l,P];case
2:var
Q=d[2],m=f(c,d[1]),n=m[2],o=m[1],p=f(c,Q),q=p[2],r=p[1];if(c){var
R=b(h[22],n,q);return[0,j(o,r),R]}var
s=i(o,r),S=s[1],T=b(h[22],q,s[2]);return[0,S,b(h[22],n,T)];case
3:var
U=d[2],t=f(c,d[1]),u=t[2],v=t[1],w=f(c,U),x=w[2],y=w[1];if(c){var
z=i(v,y),V=z[1],W=b(h[22],x,z[2]);return[0,V,b(h[22],u,W)]}var
X=b(h[22],u,x);return[0,j(v,y),X];case
4:var
c=1-c,d=d[1];continue;default:var
Y=d[3],A=f(1-c,d[1]),B=A[2],C=A[1],D=f(c,Y),E=D[2],F=D[1];if(c){var
G=i(C,F),Z=G[1],_=b(h[22],E,G[2]);return[0,Z,b(h[22],B,_)]}var
$=b(h[22],B,E);return[0,j(C,F),$]}}return f(1,c)}var
av=a(ef[1],[0,aa]),s4=a(bQ[1],[0,aa]);function
i9(f,a){function
d(h,g){var
c=h,a=g;for(;;){if(a){var
e=a[2],i=a[1];if(b(av[3],c,f))return[0,i,d(c+1|0,e)];var
c=c+1|0,a=e;continue}return 0}}return d(0,a)}var
s5=b(h[22],F[9],gn),s6=b(h[22],F[8],s5),s7=b(h[22],[0,i_,0],s6),i$=b(h[22],F[10],s7),ar=b(F[6],s8,F[10]),t=b(F[6],s9,i$),aO=b(F[6],s_,ja),aw=b(F[6],s$,jb),a1=b(F[6],ta,jc),a2=b(F[6],tb,gn),be=[d,function(b){return a(ar,tc)}],bf=[d,function(b){return a(ar,td)}],c8=[d,function(b){return a(ar,te)}],tg=a(sD[31],tf),th=a(sC[9],tg),c9=[d,function(b){return a(ar,ti)}],bg=[d,function(b){return a(ar,tj)}],ax=[d,function(b){return a(ar,tk)}],c_=[d,function(b){return a(t,tl)}],c$=[d,function(b){return a(t,tm)}],to=[d,function(b){return a(t,tn)}],da=[d,function(b){return a(ar,tp)}],db=[d,function(b){return a(ar,tq)}],ts=[d,function(b){return a(ar,tr)}],dc=[d,function(b){return a(aO,tt)}],dd=[d,function(b){return a(aO,tu)}],de=[d,function(b){return a(ar,tv)}],tx=[d,function(b){return a(ar,tw)}],tz=[d,function(b){return a(ar,ty)}],tB=[d,function(b){return a(aO,tA)}],bh=[d,function(b){return a(aO,tC)}],bi=[d,function(b){return a(aO,tD)}],bj=[d,function(b){return a(aO,tE)}],bk=[d,function(b){return a(aO,tF)}],df=[d,function(b){return a(aO,tG)}],dg=[d,function(b){return a(aO,tH)}],dh=[d,function(b){return a(aO,tI)}],di=[d,function(b){return a(t,tJ)}],bl=[d,function(b){return a(t,tK)}],tM=[d,function(b){return a(t,tL)}],ay=[d,function(b){return a(t,tN)}],tP=[d,function(b){return a(t,tO)}],dj=[d,function(b){return a(a2,tQ)}],dk=[d,function(b){return a(a2,tR)}],dl=[d,function(b){return a(a2,tS)}],dm=[d,function(b){return a(a2,tT)}],dn=[d,function(b){return a(a2,tU)}],dp=[d,function(b){return a(a2,tV)}],dq=[d,function(b){return a(a2,tW)}],dr=[d,function(b){return a(a2,tX)}],ds=[d,function(b){return a(a2,tY)}],az=[d,function(b){return a(t,tZ)}],aA=[d,function(b){return a(t,t0)}],t2=[d,function(b){return a(t,t1)}],t4=[d,function(b){return a(t,t3)}],t6=[d,function(b){return a(t,t5)}],t8=[d,function(b){return a(t,t7)}],t_=[d,function(b){return a(t,t9)}],jd=[d,function(b){return a(a1,t$)}],je=[d,function(b){return a(a1,ua)}],jf=[d,function(b){return a(a1,ub)}],jg=[d,function(b){return a(a1,uc)}],aB=[d,function(b){return a(ar,ud)}],cg=[d,function(b){return a(a1,ue)}],ch=[d,function(b){return a(a1,uf)}],ci=[d,function(b){return a(a1,ug)}],cj=[d,function(b){return a(a1,uh)}],ck=[d,function(b){return a(a1,ui)}],uk=[d,function(b){return a(t,uj)}],um=[d,function(b){return a(t,ul)}],jh=[d,function(b){return a(t,un)}],ji=[d,function(b){return a(t,uo)}],jj=[d,function(b){return a(t,up)}],cl=[d,function(b){return a(t,uq)}],cm=[d,function(b){return a(t,ur)}],cn=[d,function(b){return a(t,us)}],co=[d,function(b){return a(t,ut)}],cp=[d,function(b){return a(t,uu)}],jk=[d,function(b){return a(aw,uv)}],jl=[d,function(b){return a(aw,uw)}],jm=[d,function(b){return a(aw,ux)}],jn=[d,function(b){return a(aw,uy)}],aj=[d,function(b){return a(aw,uz)}],aP=[d,function(b){return a(aw,uA)}],a3=[d,function(b){return a(aw,uB)}],ak=[d,function(b){return a(aw,uC)}],uE=[d,function(b){return a(aw,uD)}],bm=[d,function(b){return a(aw,uF)}],cq=[d,function(b){return a(aw,uG)}],bn=[d,function(b){return a(aw,uH)}],bo=[d,function(b){return a(t,uI)}],dt=[d,function(b){return a(t,uJ)}],du=[d,function(b){return a(t,uK)}],dv=[d,function(b){return a(t,uL)}],dw=[d,function(b){return a(t,uM)}],dx=[d,function(b){return a(t,uN)}],dy=[d,function(b){return a(t,uO)}],dz=[d,function(b){return a(t,uP)}],dA=[d,function(b){return a(t,uQ)}],dB=[d,function(b){return a(t,uR)}],dC=[d,function(b){return a(t,uS)}],dD=[d,function(b){return a(t,uT)}],dE=[d,function(b){return a(t,uU)}],dF=[d,function(b){return a(t,uV)}],dG=[d,function(b){return a(t,uW)}],dH=[d,function(b){return a(t,uX)}],dI=[d,function(b){return a(t,uY)}],dJ=[d,function(b){return a(t,uZ)}],dK=[d,function(b){return a(t,u0)}],dL=[d,function(b){return a(t,u1)}],dM=[d,function(b){return a(t,u2)}],dN=[d,function(b){return a(t,u3)}],dO=[d,function(b){return a(t,u4)}],dP=[d,function(b){return a(t,u5)}],u7=[d,function(b){return a(t,u6)}],u$=[d,function(a){return e(F[6],u_,u9,u8)}],vd=[d,function(a){return e(F[6],vc,vb,va)}],dQ=[d,function(a){return e(F[6],vg,vf,ve)}],dR=[d,function(a){return e(F[6],vj,vi,vh)}],dS=[d,function(a){return e(F[6],vm,vl,vk)}],dT=[d,function(a){return e(F[6],vp,vo,vn)}],dU=[d,function(a){return e(F[6],vs,vr,vq)}],dV=[d,function(a){return e(F[6],vv,vu,vt)}],dW=[d,function(a){return e(F[6],vy,vx,vw)}],dX=[d,function(a){return e(F[6],vB,vA,vz)}],vF=[d,function(a){return e(F[6],vE,vD,vC)}],vJ=[d,function(a){return e(F[6],vI,vH,vG)}],vN=[d,function(a){return e(F[6],vM,vL,vK)}],vR=[d,function(a){return e(F[6],vQ,vP,vO)}],dY=[d,function(a){return e(F[6],vU,vT,vS)}],vY=[d,function(a){return e(F[6],vX,vW,vV)}];function
vZ(b){if(typeof
b==="number")return v0;else
switch(b[0]){case
0:return b[1];case
1:return a(h[20],b[1]);case
2:return v1;case
3:return b[1];default:return v2}}var
R=[a8,v3,a7(0)];function
cr(d){var
b=a(o[aS],d);switch(b[0]){case
9:var
e=b[2],c=a(o[aS],b[1]);if(12===c[0])return[0,c[1][1][2],e];throw R;case
12:return[0,b[1][1][2],[0]];default:throw R}}function
go(c){var
a=cr(c),b=a[1],d=a[2];if(1===b)return 0;if(2===b)return[0,go(y(d,0)[1])];throw R}function
jo(c,b){var
d=a(l[29][1],b);return e(m[1],c,v4,d)}function
eZ(b){if(b){var
c=j(db),f=[0,eZ(b[1])],g=k===c?db[1]:d===c?a(i[2],db):db;return a(o[s],[0,g,f])}var
e=j(da);return k===e?da[1]:d===e?a(i[2],da):da}function
cs(d){var
a=cr(d),b=a[2],c=a[1]-1|0;if(2<c>>>0)throw R;switch(c){case
0:return[0,cs(y(b,0)[1])];case
1:return[1,cs(y(b,0)[1])];default:return 0}}function
aC(b){if(typeof
b==="number"){var
c=j(bh);return k===c?bh[1]:d===c?a(i[2],bh):bh}else{if(0===b[0]){var
e=j(bj),g=[0,aC(b[1])],h=k===e?bj[1]:d===e?a(i[2],bj):bj;return a(o[s],[0,h,g])}var
f=j(bi),l=[0,aC(b[1])],m=k===f?bi[1]:d===f?a(i[2],bi):bi;return a(o[s],[0,m,l])}}function
dZ(c,b){var
d=a(l[29][2],b);return e(m[1],c,v5,d)}function
jp(b){if(b){var
c=j(dd),f=[0,aC(b[1])],g=k===c?dd[1]:d===c?a(i[2],dd):dd;return a(o[s],[0,g,f])}var
e=j(dc);return k===e?dc[1]:d===e?a(i[2],dc):dc}function
gp(b){if(typeof
b==="number"){var
c=j(bh);return k===c?bh[1]:d===c?a(i[2],bh):bh}else{if(0===b[0]){var
e=j(bj),g=[0,gp(b[1])],h=k===e?bj[1]:d===e?a(i[2],bj):bj;return a(o[s],[0,h,g])}var
f=j(bi),l=[0,gp(b[1])],m=k===f?bi[1]:d===f?a(i[2],bi):bi;return a(o[s],[0,m,l])}}function
v6(c,b){var
d=a(l[29][4],b);return e(m[1],c,v7,d)}function
jq(d,c){var
e=a(l[29][3],c),f=a(h[20],e);return b(h[49],d,f)}function
v8(h,g,f,e,b){var
l=b[1],m=a(e,b[2]),c=j(de),n=[0,h,g,a(f,l),m],p=k===c?de[1]:d===c?a(i[2],de):de;return a(o[s],[0,p,n])}function
bX(d){var
a=cr(d),b=a[2],c=a[1]-1|0;if(2<c>>>0)throw R;switch(c){case
0:return 0;case
1:return[0,cs(y(b,0)[1])];default:return[1,cs(y(b,0)[1])]}}function
bE(b){if(typeof
b==="number"){var
c=j(df);return k===c?df[1]:d===c?a(i[2],df):df}else{if(0===b[0]){var
e=j(dg),g=[0,aC(b[1])],h=k===e?dg[1]:d===e?a(i[2],dg):dg;return a(o[s],[0,h,g])}var
f=j(dh),l=[0,aC(b[1])],m=k===f?dh[1]:d===f?a(i[2],dh):dh;return a(o[s],[0,m,l])}}function
jr(c,b){var
d=a(l[29][7],b),f=a(q[33],d);return e(m[1],c,v9,f)}function
v_(b){var
e=a(l[17],b),f=aC(a(l[30][6],e)),g=a(l[18],b),c=j(ay),h=[0,bE(a(l[30][7],g)),f],m=k===c?ay[1]:d===c?a(i[2],ay):ay;return a(o[s],[0,m,h])}function
e0(b){var
e=aC(b[2]),c=j(ay),f=[0,bE(b[1]),e],g=k===c?ay[1]:d===c?a(i[2],ay):ay;return a(o[s],[0,g,f])}function
d0(g){var
c=a(o[aS],g);if(9===c[0]){var
e=c[2],f=j(ay),h=c[1],l=k===f?ay[1]:d===f?a(i[2],ay):ay;if(b(aN[27],h,l)){var
m=cs(y(e,1)[2]);return[0,bX(y(e,0)[1]),m]}throw R}throw R}function
bp(c,a){if(typeof
a==="number")return 0===a?b(h[49],c,v$):b(h[49],c,wa);else
switch(a[0]){case
0:return b(h[49],c,wb);case
1:return jr(c,a[1]);case
2:return E(m[1],c,wc,bp,a[1],bp,a[2]);case
3:return E(m[1],c,wd,bp,a[1],bp,a[2]);case
4:return E(m[1],c,we,bp,a[1],bp,a[2]);case
5:return I(m[1],c,wf,bp,a[1]);default:return I(m[1],c,wg,bp,a[1])}}function
bq(b){if(typeof
b==="number"){if(0===b){var
c=j(dj);return k===c?dj[1]:d===c?a(i[2],dj):dj}var
e=j(dk);return k===e?dk[1]:d===e?a(i[2],dk):dk}else
switch(b[0]){case
0:var
f=j(dl),q=[0,e0(b[1])],r=k===f?dl[1]:d===f?a(i[2],dl):dl;return a(o[s],[0,r,q]);case
1:var
g=j(dm),t=[0,bE(b[1])],u=k===g?dm[1]:d===g?a(i[2],dm):dm;return a(o[s],[0,u,t]);case
2:var
v=b[1],w=bq(b[2]),h=j(dn),x=[0,bq(v),w],y=k===h?dn[1]:d===h?a(i[2],dn):dn;return a(o[s],[0,y,x]);case
3:var
z=b[1],A=bq(b[2]),l=j(dp),B=[0,bq(z),A],C=k===l?dp[1]:d===l?a(i[2],dp):dp;return a(o[s],[0,C,B]);case
4:var
D=b[1],E=bq(b[2]),m=j(dq),F=[0,bq(D),E],G=k===m?dq[1]:d===m?a(i[2],dq):dq;return a(o[s],[0,G,F]);case
5:var
n=j(dr),H=[0,bq(b[1])],I=k===n?dr[1]:d===n?a(i[2],dr):dr;return a(o[s],[0,I,H]);default:var
p=j(ds),J=[0,bq(b[1])],K=k===p?ds[1]:d===p?a(i[2],ds):ds;return a(o[s],[0,K,J])}}function
br(d){var
b=cr(d),a=b[2],c=b[1]-1|0;if(7<c>>>0)throw R;switch(c){case
0:return 0;case
1:return 1;case
2:return[0,d0(y(a,0)[1])];case
3:var
e=br(y(a,1)[2]);return[2,br(y(a,0)[1]),e];case
4:var
f=br(y(a,1)[2]);return[3,br(y(a,0)[1]),f];case
5:var
g=br(y(a,1)[2]);return[4,br(y(a,0)[1]),g];case
6:return[5,br(y(a,0)[1])];default:return[6,br(y(a,0)[1])]}}function
js(b,f){var
c=cr(f),d=c[2],e=c[1];if(1===e)return 0;if(2===e){var
g=js(b,y(d,2)[3]);return[0,a(b,y(d,1)[2]),g]}throw R}function
jt(c,e,b){if(b){var
h=b[1],l=jt(c,e,b[2]),f=j(c_),m=[0,c,a(e,h),l],n=k===f?c_[1]:d===f?a(i[2],c_):c_;return a(o[s],[0,n,m])}var
g=j(c$),p=[0,c],q=k===g?c$[1]:d===g?a(i[2],c$):c$;return a(o[s],[0,q,p])}function
wh(f,e,b,d,a){function
c(d,a){if(a){var
e=a[2],f=a[1];return e?E(m[1],d,wi,b,f,c,e):I(m[1],d,wj,b,f)}return 0}return E(m[1],d,wk,f,c,a,e)}function
gq(e,d,a){function
c(d,a){switch(a[0]){case
0:return b(e,d,a[1]);case
1:return I(m[1],d,wl,dZ,a[1]);case
2:return E(m[1],d,wm,c,a[1],c,a[2]);case
3:return E(m[1],d,wn,c,a[1],c,a[2]);case
4:return E(m[1],d,wo,c,a[1],c,a[2]);case
5:return I(m[1],d,wp,c,a[1]);default:return E(m[1],d,wq,c,a[1],jq,a[2])}}return c(d,a)}function
gr(e,q,b){function
c(b){switch(b[0]){case
0:var
f=j(du),r=[0,e,a(q,b[1])],t=k===f?du[1]:d===f?a(i[2],du):du;return a(o[s],[0,t,r]);case
1:var
g=j(dt),u=[0,e,aC(b[1])],v=k===g?dt[1]:d===g?a(i[2],dt):dt;return a(o[s],[0,v,u]);case
2:var
w=b[1],x=c(b[2]),h=j(dv),y=[0,e,c(w),x],z=k===h?dv[1]:d===h?a(i[2],dv):dv;return a(o[s],[0,z,y]);case
3:var
A=b[1],B=c(b[2]),l=j(dy),C=[0,e,c(A),B],D=k===l?dy[1]:d===l?a(i[2],dy):dy;return a(o[s],[0,D,C]);case
4:var
E=b[1],F=c(b[2]),m=j(dx),G=[0,e,c(E),F],H=k===m?dx[1]:d===m?a(i[2],dx):dx;return a(o[s],[0,H,G]);case
5:var
n=j(dw),I=[0,e,c(b[1])],J=k===n?dw[1]:d===n?a(i[2],dw):dw;return a(o[s],[0,J,I]);default:var
K=b[1],L=jp(b[2]),p=j(dz),M=[0,e,c(K),L],N=k===p?dz[1]:d===p?a(i[2],dz):dz;return a(o[s],[0,N,M])}}return c(b)}function
gs(e,l,b){function
c(b){switch(b[0]){case
0:var
f=j(dB),m=[0,e,a(l,b[1])],n=k===f?dB[1]:d===f?a(i[2],dB):dB;return a(o[s],[0,n,m]);case
1:var
p=b[1],q=c(b[2]),g=j(dC),r=[0,e,aC(p),q],t=k===g?dC[1]:d===g?a(i[2],dC):dC;return a(o[s],[0,t,r]);default:var
u=b[2],v=b[1],w=c(b[3]),x=aC(u),h=j(dA),y=[0,e,c(v),x,w],z=k===h?dA[1]:d===h?a(i[2],dA):dA;return a(o[s],[0,z,y])}}return c(b)}function
e1(d,c,a){function
b(c,a){switch(a[0]){case
0:return I(m[1],c,wr,d,a[1]);case
1:return E(m[1],c,ws,dZ,a[1],b,a[2]);default:return b0(m[1],c,wt,b,a[1],dZ,a[2],b,a[3])}}return b(c,a)}function
wu(d,c,a){function
e(c,a){function
e(a){var
b=a[2],e=a[1][1],f=l[32][3];function
g(a,b){return e1(d,a,b)}return E(m[1],c,wv,g,e,f,b)}return b(g[11],e,a)}function
f(a){return I(m[1],c,ww,e,a)}return b(g[11],f,a)}function
wx(b,f,h){var
g=j(b),c=k===g?b[1]:d===g?a(i[2],b):b;function
e(b){if(typeof
b==="number"){var
g=j(dP),r=[0,c],t=k===g?dP[1]:d===g?a(i[2],dP):dP;return a(o[s],[0,t,r])}else
switch(b[0]){case
0:var
h=j(dJ),u=[0,c,eZ(b[1])],v=k===h?dJ[1]:d===h?a(i[2],dJ):dJ;return a(o[s],[0,v,u]);case
1:var
l=j(dK),w=[0,c,gs(c,f,b[1])],x=k===l?dK[1]:d===l?a(i[2],dK):dK;return a(o[s],[0,x,w]);case
2:var
y=b[1],z=e(b[2]),m=j(dM),A=[0,c,gs(c,f,y),z],B=k===m?dM[1]:d===m?a(i[2],dM):dM;return a(o[s],[0,B,A]);case
3:var
C=b[1],D=e(b[2]),n=j(dL),E=[0,c,e(C),D],F=k===n?dL[1]:d===n?a(i[2],dL):dL;return a(o[s],[0,F,E]);case
4:var
G=b[1],H=e(b[2]),p=j(dN),I=[0,c,e(G),H],J=k===p?dN[1]:d===p?a(i[2],dN):dN;return a(o[s],[0,J,I]);default:var
q=j(dO),K=[0,c,a(f,b[1])],L=k===q?dO[1]:d===q?a(i[2],dO):dO;return a(o[s],[0,L,K])}}return e(h)}function
wy(e,c,a){function
d(c,a){if(typeof
a==="number")return b(m[1],c,wz);else
switch(a[0]){case
0:return I(m[1],c,wA,jo,a[1]);case
1:var
f=a[1],g=function(a,b){return e1(e,a,b)};return I(m[1],c,wB,g,f);case
2:var
h=a[2],i=a[1],j=function(a,b){return e1(e,a,b)};return E(m[1],c,wC,j,i,d,h);case
3:return E(m[1],c,wD,d,a[1],d,a[2]);case
4:return E(m[1],c,wE,d,a[1],d,a[2]);default:return I(m[1],c,wF,e,a[1])}}return d(c,a)}function
ju(l){switch(l){case
0:var
b=j(dD);return k===b?dD[1]:d===b?a(i[2],dD):dD;case
1:var
c=j(dE);return k===c?dE[1]:d===c?a(i[2],dE):dE;case
2:var
e=j(dF);return k===e?dF[1]:d===e?a(i[2],dF):dF;case
3:var
f=j(dH);return k===f?dH[1]:d===f?a(i[2],dH):dH;case
4:var
g=j(dG);return k===g?dG[1]:d===g?a(i[2],dG):dG;default:var
h=j(dI);return k===h?dI[1]:d===h?a(i[2],dI):dI}}function
jv(a,c){switch(c){case
0:return b(m[1],a,wG);case
1:return b(m[1],a,wH);case
2:return b(m[1],a,wI);case
3:return b(m[1],a,wJ);case
4:return b(m[1],a,wK);default:return b(m[1],a,wL)}}function
wM(b,c,a){var
d=a[3],e=a[2],f=a[1];function
g(a,c){return gq(b,a,c)}function
h(a,c){return gq(b,a,c)}return b0(m[1],c,wN,h,f,jv,e,g,d)}function
wO(c,e,b){var
g=b[2],h=b[1],l=gr(c,e,b[3]),m=ju(g),f=j(dY),n=[0,c,gr(c,e,h),m,l],p=k===f?dY[1]:d===f?a(i[2],dY):dY;return a(o[s],[0,p,n])}function
d1(f,c){try{var
e=function(g){var
c=g[1],e=j(c),h=k===e?c[1]:d===e?a(i[2],c):c;return b(aN[27],f,h)},h=b(g[29],e,c)[2];return h}catch(a){a=w(a);if(a===P)throw R;throw a}}var
gt=[0,[0,jd,5],[0,[0,je,3],[0,[0,jg,4],[0,[0,jf,2],0]]]],gu=[0,[0,jk,5],[0,[0,jl,3],[0,[0,jn,4],[0,[0,jm,2],0]]]],gv=[0,[0,ji,4],[0,[0,jh,2],[0,[0,jj,0],0]]];function
wP(c,e,d){var
f=a(af[2],c),g=a(af[8],c),h=aR(i7[2],0,0,g,f,e);return b(aN[27],h,d)}function
gw(b,d,c){var
e=a(af[2],b),f=a(af[8],b);return aR(sB[77],0,f,e,d,c)}function
jw(n,f){var
c=f[2],e=f[1],g=a(o[aS],e);switch(g[0]){case
10:var
p=y(c,1)[2],q=y(c,0)[1];return[0,d1(e,gt),q,p];case
11:if(0===g[1][1][2]){var
l=j(aB),r=k===l?aB[1]:d===l?a(i[2],aB):aB;if(b(aN[27],e,r)){var
m=j(bk),s=k===m?bk[1]:d===m?a(i[2],bk):bk;if(gw(n,y(c,0)[1],s)){var
t=y(c,2)[3];return[0,0,y(c,1)[2],t]}}throw R}break}return a(h[2],wQ)}function
jx(n,f){var
c=f[2],e=f[1],g=a(o[aS],e);switch(g[0]){case
10:var
p=y(c,1)[2],q=y(c,0)[1];return[0,d1(e,gu),q,p];case
11:if(0===g[1][1][2]){var
l=j(aB),r=k===l?aB[1]:d===l?a(i[2],aB):aB;if(b(aN[27],e,r)){var
m=j(bl),s=k===m?bl[1]:d===m?a(i[2],bl):bl;if(gw(n,y(c,0)[1],s)){var
t=y(c,2)[3];return[0,0,y(c,1)[2],t]}}throw R}break}return a(h[2],wR)}function
jy(f,a){var
b=a[2],c=a[1],d=y(b,1)[2],e=y(b,0)[1];return[0,d1(c,gv),e,d]}function
wS(b){return 12===a(o[aS],b)[0]?1:0}function
jz(f,c){try{var
e=function(g){var
c=g[1],e=j(c),h=k===e?c[1]:d===e?a(i[2],c):c;return b(aN[27],f,h)},h=b(g[29],e,c)[2];return h}catch(a){a=w(a);if(a===P)return wT;throw a}}function
wU(f,d){function
e(a,d,c){if(a){var
f=a[1],h=a[2];if(b(o[bH],f,c))return[0,a,d];var
g=e(h,d+1|0,c);return[0,[0,f,g[1]],g[2]]}return[0,[0,c,0],d]}var
c=e(f,1,d),g=c[1];return[0,g,a(l[30][2],c[2])]}function
wV(e,d){var
a=e,c=1;for(;;){if(a){var
f=a[2];if(b(o[bH],a[1],d))return c;var
a=f,c=c+1|0;continue}throw[0,i5,wW]}}var
wX=0,bY=[0,wU,wV,wX,function(a){return a}];function
e2(g,r,q,d,c){function
j(d,c){var
a=b(bY[1],d,c);return[0,[1,a[2]],a[1]]}function
e(c,d){function
s(g,f,a){var
h=a[2],c=e(g,a[1]),i=c[1],d=e(c[2],h),j=d[2];return[0,b(f,i,d[1]),j]}try{var
A=[0,[0,a(g,d)],c];return A}catch(g){g=w(g);if(g===R){var
i=a(o[aS],d);if(9===i[0]){var
f=i[2],k=i[1];if(10===a(o[aS],k)[0]){var
h=jz(k,q);if(typeof
h==="number"){if(0===h){var
l=e(c,y(f,0)[1]);return[0,[5,l[1]],l[2]]}try{var
n=e(c,y(f,0)[1]),t=n[2],u=n[1],v=[0,b(r,u,y(f,1)[2]),t];return v}catch(e){e=w(e);if(a(bz[22],e)){var
m=b(bY[1],c,d);return[0,[1,m[2]],m[1]]}throw e}}else{if(0===h[0]){var
x=h[1],z=y(f,1)[2];return s(c,x,[0,y(f,0)[1],z])}var
p=b(bY[1],c,d);return[0,[1,p[2]],p[1]]}}return j(c,d)}return j(c,d)}throw g}}return e(d,c)}var
wY=[0,[0,ci,0],[0,[0,ck,1],0]],wZ=[0,[0,cj,[0,function(b,a){return[4,b,a]}]],wY],w0=[0,[0,ch,[0,function(b,a){return[3,b,a]}]],wZ],jA=[0,[0,cg,[0,function(b,a){return[2,b,a]}]],w0],w1=[0,[0,cn,0],[0,[0,cp,1],0]],w2=[0,[0,co,[0,function(b,a){return[4,b,a]}]],w1],w3=[0,[0,cm,[0,function(b,a){return[3,b,a]}]],w2],jB=[0,[0,cl,[0,function(b,a){return[2,b,a]}]],w3],w4=[0,[0,a3,0],[0,[0,cq,1],0]],w5=[0,[0,ak,[0,function(b,a){return[4,b,a]}]],w4],w6=[0,[0,aP,[0,function(b,a){return[3,b,a]}]],w5],jC=[0,[0,aj,[0,function(b,a){return[2,b,a]}]],w6],w7=0,w8=[0,[0,ak,function(b,a){return[4,b,a]}],w7],w9=[0,[0,aP,function(b,a){return[3,b,a]}],w8],jD=[0,[0,aj,function(b,a){return[2,b,a]}],w9];function
e3(f){var
g=a(o[aS],f);switch(g[0]){case
9:var
c=g[2],e=g[1];try{var
x=d1(e,jD),z=e3(y(c,0)[1]),A=b(x,z,e3(y(c,1)[2]));return A}catch(f){f=w(f);if(f===R){var
h=j(bm),s=k===h?bm[1]:d===h?a(i[2],bm):bm;if(b(aN[27],e,s)){var
l=e3(y(c,0)[1]),t=a(r[ff],l);if(b(r[77],t,w_))throw R;return[5,l]}var
m=j(bo),u=k===m?bo[1]:d===m?a(i[2],bo):bo;if(b(aN[27],e,u))return[0,d0(y(c,0)[1])];var
n=j(bn),v=k===n?bn[1]:d===n?a(i[2],bn):bn;if(b(aN[27],e,v))return[1,bX(y(c,0)[1])];throw R}throw f}case
10:var
p=j(az),B=k===p?az[1]:d===p?a(i[2],az):az;if(b(aN[27],f,B))return 0;var
q=j(aA),C=k===q?aA[1]:d===q?a(i[2],aA):aA;if(b(aN[27],f,C))return 1;throw R;default:throw R}}function
jE(a){return e3(a)}function
w$(d,c){var
b=bX(c);if(typeof
b!=="number"&&1===b[0])return xa;return[6,d,a(r[12][15],b)]}function
jF(a,b){return e2(bX,w$,jA,a,b)}function
xb(d,e){var
c=bX(e);if(typeof
c!=="number"&&1===c[0]){if(0===d[0])return[0,b(r[85],d[1],c)];a(h[27],xc);a(h[46],h[24]);throw R}return[6,d,a(r[12][15],c)]}function
jG(a,b){return e2(d0,xb,jB,a,b)}function
xd(c,b){var
d=go(b);return[6,c,a(r[7][1],d)]}function
jH(a,b){return e2(jE,xd,jC,a,b)}function
e4(l,e,k,j,i){var
c=a(o[aS],j);if(9===c[0]){var
d=b(l,i,[0,c[1],c[2]]),m=d[3],n=d[1],f=b(e,k,d[2]),p=f[1],g=b(e,f[2],m);return[0,[0,p,n,g[1]],g[2]]}return a(h[2],xe)}function
xf(a,b,c){return e4(jw,jF,a,b,c)}function
xg(a,b,c){return e4(jy,jG,a,b,c)}function
xh(a,b,c){return e4(jx,jH,a,b,c)}function
bF(a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return 2;case
1:return[0,a[1]];case
2:var
b=a[1],c=bF(a[2]);return[1,bF(b),c];case
3:var
d=a[1],e=bF(a[2]);return[2,bF(d),e];case
4:return[3,bF(a[1])];default:var
f=a[1],g=bF(a[3]);return[4,bF(f),g]}}function
jI(b,a){return[2,b,a]}function
jJ(b,a){return[3,b,a]}function
jK(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
jL(b,a){return[5,b,0,a]}function
d2(e,d,c,a){if(typeof
c!=="number"&&0===c[0])if(typeof
a!=="number"&&0===a[0])return[0,d];return b(e,c,a)}function
xi(n,m,h,g,f){function
J(f,d,c){try{var
b=e(m,f,c,n),g=b[2],h=b[1],i=[0,[1,h,d,c],g,a(l[32][2],d)];return i}catch(b){b=w(b);if(a(bz[22],b))return[0,[0,c],f,d];throw b}}function
c(g,f,e){var
h=a(o[aS],e);switch(h[0]){case
6:var
C=h[3],P=h[2],Q=a(o[b2],1);if(!b(i6[45],Q,C)){var
p=c(g,f,P),S=p[1],q=c(p[2],p[3],C),T=q[3],U=q[2];return[0,d2(jL,e,S,q[1]),U,T]}break;case
9:var
l=h[2],m=h[1],D=l.length-1;if(!(3<=D))switch(D){case
0:break;case
1:var
E=j(c8),V=l[1],W=k===E?c8[1]:d===E?a(i[2],c8):c8;if(b(o[bH],m,W)){var
r=c(g,f,V);return[0,[4,r[1]],r[2],r[3]]}break;default:var
s=l[1],t=l[2],F=j(be),X=k===F?be[1]:d===F?a(i[2],be):be;if(b(o[bH],m,X)){var
u=c(g,f,s),Y=u[1],v=c(u[2],u[3],t),Z=v[3],_=v[2];return[0,d2(jI,e,Y,v[1]),_,Z]}var
G=j(bf),$=k===G?bf[1]:d===G?a(i[2],bf):bf;if(b(o[bH],m,$)){var
w=c(g,f,s),aa=w[1],x=c(w[2],w[3],t),ab=x[3],ac=x[2];return[0,d2(jJ,e,aa,x[1]),ac,ab]}var
H=j(c9),ad=k===H?c9[1]:d===H?a(i[2],c9):c9;if(b(o[bH],m,ad)){var
y=c(g,f,s),ae=y[1],z=c(y[2],y[3],t),ag=z[3],ah=z[2];return[0,d2(jK,e,ae,z[1]),ah,ag]}}return J(g,f,e)}var
A=j(bg),N=k===A?bg[1]:d===A?a(i[2],bg):bg;if(b(o[bH],e,N))return[0,0,g,f];var
B=j(ax),O=k===B?ax[1]:d===B?a(i[2],ax):ax;if(b(o[bH],e,O))return[0,1,g,f];var
K=a(af[2],n),L=a(af[8],n),M=I(i7[3],0,L,K,e);if(a(o[110],M))return[0,[0,e],g,f];throw R}return c(h,g,f)}function
xj(c,r,b){function
e(b){if(typeof
b==="number"){if(0===b){var
f=j(dQ),t=[0,c],u=k===f?dQ[1]:d===f?a(i[2],dQ):dQ;return a(o[s],[0,u,t])}var
g=j(dR),v=[0,c],w=k===g?dR[1]:d===g?a(i[2],dR):dR;return a(o[s],[0,w,v])}else
switch(b[0]){case
0:var
h=j(dW),x=[0,c,b[1]],y=k===h?dW[1]:d===h?a(i[2],dW):dW;return a(o[s],[0,y,x]);case
1:var
l=j(dV),z=[0,c,a(r,b[1])],A=k===l?dV[1]:d===l?a(i[2],dV):dV;return a(o[s],[0,A,z]);case
2:var
B=b[1],C=e(b[2]),m=j(dS),D=[0,c,e(B),C],E=k===m?dS[1]:d===m?a(i[2],dS):dS;return a(o[s],[0,E,D]);case
3:var
F=b[1],G=e(b[2]),n=j(dT),H=[0,c,e(F),G],I=k===n?dT[1]:d===n?a(i[2],dT):dT;return a(o[s],[0,I,H]);case
4:var
p=j(dU),J=[0,c,e(b[1])],K=k===p?dU[1]:d===p?a(i[2],dU):dU;return a(o[s],[0,K,J]);default:var
L=b[1],M=e(b[3]),q=j(dX),N=[0,c,e(L),M],O=k===q?dX[1]:d===q?a(i[2],dX):dX;return a(o[s],[0,O,N])}}return e(b)}function
jM(a){function
e(i,h){var
c=i,a=h;for(;;){if(typeof
a==="number")var
d=0;else
switch(a[0]){case
0:return b(bY[1],c,a[1])[1];case
4:var
a=a[1];continue;case
5:var
g=a[3],f=a[1],d=1;break;case
1:var
d=0;break;default:var
g=a[2],f=a[1],d=1}if(d){var
c=e(c,f),a=g;continue}return c}}return e(0,a)}function
jN(c){function
d(e){var
c=e;for(;;)switch(c[0]){case
0:return av[1];case
1:var
f=a(l[29][2],c[1]);return a(av[5],f);case
5:case
6:var
c=c[1];continue;default:var
g=c[1],h=d(c[2]),i=d(g);return b(av[7],i,h)}}function
e(l){var
a=l;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
1:var
f=a[1],i=f[1],j=d(f[3]),k=d(i);return b(av[7],k,j);case
4:var
a=a[1];continue;case
5:var
h=a[3],g=a[1],c=1;break;case
0:var
c=0;break;default:var
h=a[2],g=a[1],c=1}if(c){var
m=e(h),n=e(g);return b(av[7],n,m)}return av[1]}}return e(c)}var
xk=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=b(g[13],o,gt),c=j(ck);function
q(b){var
c=a(l[29][3],b);return bE(a(l[30][5],c))}var
r=k===c?ck[1]:d===c?a(i[2],ck):ck,e=j(cj),s=k===e?cj[1]:d===e?a(i[2],cj):cj,f=j(ci),t=k===f?ci[1]:d===f?a(i[2],ci):ci,h=j(ch),u=k===h?ch[1]:d===h?a(i[2],ch):ch,m=j(cg),v=k===m?cg[1]:d===m?a(i[2],cg):cg,n=j(bk),w=k===n?bk[1]:d===n?a(i[2],bk):bk;return[0,w,bE,v,u,t,s,r,q,p]}],xl=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=b(g[13],o,gv),c=j(cp);function
q(b){var
c=a(l[29][3],b);return bE(a(l[30][5],c))}var
r=k===c?cp[1]:d===c?a(i[2],cp):cp,e=j(co),s=k===e?co[1]:d===e?a(i[2],co):co,f=j(cn),t=k===f?cn[1]:d===f?a(i[2],cn):cn,h=j(cm),u=k===h?cm[1]:d===h?a(i[2],cm):cm,m=j(cl),v=k===m?cl[1]:d===m?a(i[2],cl):cl,n=j(di),w=k===n?di[1]:d===n?a(i[2],di):di;return[0,w,e0,v,u,t,s,r,q,p]}];function
jO(n){var
e=j(ak),p=k===e?ak[1]:d===e?a(i[2],ak):ak,f=j(aj),q=k===f?aj[1]:d===f?a(i[2],aj):aj,g=j(aA),b=k===g?aA[1]:d===g?a(i[2],aA):aA;function
h(c,b){return a(o[s],[0,q,[0,c,b]])}function
l(c,b){return a(o[s],[0,p,[0,c,b]])}var
m=h(b,b);function
c(a){return typeof
a==="number"?b:0===a[0]?h(b,l(m,c(a[1]))):l(m,c(a[1]))}return c(n)}function
xm(e){var
b=a(l[29][3],e);if(0===b){var
c=j(az);return k===c?az[1]:d===c?a(i[2],az):az}return jO(a(l[30][2],b))}function
a4(b){if(typeof
b==="number"){if(0===b){var
c=j(az);return k===c?az[1]:d===c?a(i[2],az):az}var
e=j(aA);return k===e?aA[1]:d===e?a(i[2],aA):aA}else
switch(b[0]){case
0:var
f=j(bo),q=[0,e0(b[1])],r=k===f?bo[1]:d===f?a(i[2],bo):bo;return a(o[s],[0,r,q]);case
1:var
g=j(bn),t=[0,bE(b[1])],u=k===g?bn[1]:d===g?a(i[2],bn):bn;return a(o[s],[0,u,t]);case
2:var
v=b[1],w=a4(b[2]),h=j(aj),x=[0,a4(v),w],y=k===h?aj[1]:d===h?a(i[2],aj):aj;return a(o[s],[0,y,x]);case
3:var
z=b[1],A=a4(b[2]),l=j(aP),B=[0,a4(z),A],C=k===l?aP[1]:d===l?a(i[2],aP):aP;return a(o[s],[0,C,B]);case
4:var
D=b[1],E=a4(b[2]),m=j(ak),F=[0,a4(D),E],G=k===m?ak[1]:d===m?a(i[2],ak):ak;return a(o[s],[0,G,F]);case
5:var
n=j(bm),H=[0,a4(b[1])],I=k===n?bm[1]:d===n?a(i[2],bm):bm;return a(o[s],[0,I,H]);default:var
p=j(a3),J=[0,a4(b[1])],K=k===p?a3[1]:d===p?a(i[2],a3):a3;return a(o[s],[0,K,J])}}var
xn=[d,function(x){function
o(c){var
b=c[1],e=j(b),f=c[2],g=k===e?b[1]:d===e?a(i[2],b):b;return[0,f,g]}var
p=b(g[13],o,gu),c=j(cq);function
q(b){var
c=a(l[29][3],b);return eZ(a(l[30][1],c))}var
r=k===c?cq[1]:d===c?a(i[2],cq):cq,e=j(ak),s=k===e?ak[1]:d===e?a(i[2],ak):ak,f=j(a3),t=k===f?a3[1]:d===f?a(i[2],a3):a3,h=j(aP),u=k===h?aP[1]:d===h?a(i[2],aP):aP,m=j(aj),v=k===m?aj[1]:d===m?a(i[2],aj):aj,n=j(bl),w=k===n?bl[1]:d===n?a(i[2],bl):bl;return[0,w,a4,v,u,t,s,r,q,p]}];function
xo(f,c){var
u=jN(c),v=a(av[20],u);function
x(b,a){return[0,a,b+1|0]}var
p=b(g[14],x,v),q=jM(c);function
y(c){var
d=f[1],e=b(m[4],xp,c[2]);return[0,a(J[69],e),d]}var
n=b(g[13],y,p);function
z(c,f){var
d=o[ff],e=b(m[4],xq,c+1|0);return[0,a(J[69],e),d]}var
r=b(g[14],z,q);function
A(b,a){return[0,a[1],b[1]]}var
B=e(g[19],A,p,n);function
t(e,c){function
d(c){switch(c[0]){case
0:return a(f[2],c[1]);case
1:var
h=a(l[29][2],c[1]),i=e+b(g[33],h,p)|0;return a(o[b2],i);case
2:var
j=c[1],k=d(c[2]),m=[0,d(j),k];return a(o[s],[0,f[3],m]);case
3:var
n=c[1],q=d(c[2]),r=[0,d(n),q];return a(o[s],[0,f[4],r]);case
4:var
t=c[1],u=d(c[2]),v=[0,d(t),u];return a(o[s],[0,f[6],v]);case
5:var
w=[0,d(c[1])];return a(o[s],[0,f[5],w]);default:var
x=c[1],y=a(f[8],c[2]),z=[0,d(x),y];return a(o[s],[0,f[7],z])}}return d(c)}function
C(l,e,c){try{var
p=[0,b(g[33],l,f[9]),[0,e,c]],q=a(o[s],p);return q}catch(b){b=w(b);if(b===P){var
h=j(aB),m=[0,f[1],e,c],n=k===h?aB[1]:d===h?a(i[2],aB):aB;return a(o[s],[0,n,m])}throw b}}function
h(f,e,c){if(typeof
c==="number"){if(0===c){var
l=j(bg);return k===l?bg[1]:d===l?a(i[2],bg):bg}var
m=j(ax);return k===m?ax[1]:d===m?a(i[2],ax):ax}else
switch(c[0]){case
0:var
x=f+b(bY[2],q,c[1])|0;return a(o[b2],x);case
1:var
g=c[1],u=g[2],v=g[1],w=t(e,g[3]);return C(u,t(e,v),w);case
2:var
y=c[1],z=h(f,e,c[2]),n=j(be),A=[0,h(f,e,y),z],B=k===n?be[1]:d===n?a(i[2],be):be;return a(o[s],[0,B,A]);case
3:var
D=c[1],E=h(f,e,c[2]),p=j(bf),F=[0,h(f,e,D),E],G=k===p?bf[1]:d===p?a(i[2],bf):bf;return a(o[s],[0,G,F]);case
4:var
r=j(ax),H=c[1],I=k===r?ax[1]:d===r?a(i[2],ax):ax,J=h(f,e,H);return b(o[49],J,I);default:var
K=c[1],L=h(f+1|0,e+1|0,c[3]),M=h(f,e,K);return b(o[49],M,L)}}var
D=a(g[1],n),E=a(g[1],r),F=bd(function(c){var
d=b(bY[2],q,c),e=b(m[4],xr,d),f=a(J[69],e);return a(o[aF],f)},c),G=a(g[6],B),H=a(g[6],r),I=h(a(g[1],n),0,c);function
K(a){return[0,[0,a[1]],a[2]]}var
L=b(g[13],K,n),M=e(o[63],D,L,I);function
N(a){return[0,[0,a[1]],a[2]]}var
O=b(g[13],N,r);return[0,e(o[63],E,O,M),H,G,F]}var
n=[0,i_,gn,i$,ja,jb,jc,ar,t,aO,aw,a1,a2,be,bf,c8,th,c9,bg,ax,c_,c$,to,da,db,ts,dc,dd,de,tx,tz,tB,bh,bi,bj,bk,df,dg,dh,di,bl,tM,ay,tP,dj,dk,dl,dm,dn,dp,dq,dr,ds,az,aA,t2,t4,t6,t8,t_,jd,je,jf,jg,aB,cg,ch,ci,cj,ck,uk,um,jh,ji,jj,cl,cm,cn,co,cp,jk,jl,jm,jn,aj,aP,a3,ak,uE,bm,cq,bn,bo,dt,du,dv,dw,dx,dy,dz,dA,dB,dC,dD,dE,dF,dG,dH,dI,dJ,dK,dL,dM,dN,dO,dP,u7,u$,vd,dQ,dR,dS,dT,dU,dV,dW,dX,vF,vJ,vN,vR,dY,vY,vZ,R,cr,go,jo,eZ,cs,aC,dZ,jp,gp,v6,jq,v8,bX,bE,jr,v_,e0,d0,bp,bq,br,js,jt,wh,dZ,aC,gq,gr,gs,e1,wu,wx,wy,ju,jv,wM,wO,d1,gt,gu,gv,wP,gw,jw,jx,jy,wS,jz,bY,e2,jA,jB,jC,bX,d0,jD,jE,jF,jG,jH,e4,xf,xg,xh,bF,jI,jJ,jK,jL,d2,xi,xj,jM,jN,xk,xl,jO,xm,a4,xn,xo,function(f,e){var
c=e,b=f;for(;;){if(b){var
d=b[1],g=b[2],h=d[3],i=d[2],j=a(J[1][5],d[1]),c=I(o[51],j,i,h,c),b=g;continue}return c}}];function
d3(d){var
c=d;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:return[0,a(l[29][1],c[1]),0];case
2:var
c=c[2];continue;case
3:var
e=c[1],f=d3(c[2]),g=d3(e);return b(h[22],g,f);case
4:var
i=c[1],j=d3(c[2]),k=d3(i);return b(h[22],k,j)}return 0}}function
xs(a,f,e){return function(h){var
a=h;for(;;){if(a){var
d=a[1],i=a[2];try{var
j=b(g[5],e,d),k=b(g[5],f,d)===j?1:0,c=k}catch(a){a=w(a);if(a[1]!==i5)throw a;var
c=0}if(c){var
a=i;continue}return c}return 1}}(a)}function
xt(d,c,f){function
e(i,h){var
d=i,c=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
j=a(l[29][1],c[1]),k=b(g[5],f,j)[2];return b(J[1][9][7],d,k);case
2:var
c=c[2];continue;case
3:case
4:var
m=c[2],d=e(d,c[1]),c=m;continue}return d}}return e(d,c)}function
jP(e,d,a){if(a){var
g=a[2],c=b(l[6],e,[0,a[1],d]);if(c){var
h=c[1],f=jP(e,d,g);return f?[0,[0,h,f[1]]]:0}return 0}return xu}function
xv(e,d){var
c=e,b=d;for(;;){if(b){var
f=b[2],g=[0,c,[0,a(o[aF],b[1])]],c=a(o[s],g),b=f;continue}return c}}var
d4=[d,function(a){return e(F[6],xy,xx,xw)}],d5=[d,function(a){return e(F[6],xB,xA,xz)}],d6=[d,function(a){return e(F[6],xE,xD,xC)}],d7=[d,function(a){return e(F[6],xH,xG,xF)}];function
d8(c,b){if(typeof
b==="number"){var
e=j(d6),h=[0,c],l=k===e?d6[1]:d===e?a(i[2],d6):d6;return a(o[s],[0,l,h])}else{if(0===b[0]){var
f=j(d5),m=[0,c,b[1]],n=k===f?d5[1]:d===f?a(i[2],d5):d5;return a(o[s],[0,n,m])}var
p=b[2],q=b[1],r=d8(c,b[3]),g=j(d4),t=[0,c,d8(c,q),p,r],u=k===g?d4[1]:d===g?a(i[2],d4):d4;return a(o[s],[0,u,t])}}function
gx(b){if(b){var
c=b[1][1],d=0,f=function(d,b){var
e=b[1],f=a(l[30][2],b[2]);return I(r[88],c,f,e,d)};return e(g[16],f,d,b)}return 0}function
gy(c,a){return typeof
a==="number"?b(h[49],c,xI):0===a[0]?I(m[1],c,xJ,n[a9],a[1]):b0(m[1],c,xK,gy,a[1],n[a9],a[2],gy,a[3])}function
e5(b){if(typeof
b==="number"){var
c=n[56],m=j(c);return k===m?c[1]:d===m?a(i[2],c):c}else
switch(b[0]){case
0:var
u=b[1],v=e5(b[2]),w=[0,e(n[b1],n[35],n[b4],u),v],f=n[57],p=j(f),x=k===p?f[1]:d===p?a(i[2],f):f;return a(o[s],[0,x,w]);case
1:var
y=b[1],z=e5(b[2]),A=[0,e(n[b1],n[35],n[b4],y),z],g=n[58],q=j(g),B=k===q?g[1]:d===q?a(i[2],g):g;return a(o[s],[0,B,A]);default:var
h=n[55],r=j(h),C=b[3],D=b[2],E=b[1],F=k===r?h[1]:d===r?a(i[2],h):h,G=e(n[k$],F,e5,C),H=e(n[b1],n[35],n[b4],D),I=[0,e(n[b1],n[35],n[b4],E),H,G],l=n[59],t=j(l),J=k===t?l[1]:d===t?a(i[2],l):l;return a(o[s],[0,J,I])}}function
bG(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:return 1;case
1:return 1;case
2:return 1+bG(a[2])|0;case
5:return 1;default:var
b=a[1],c=bG(a[2]);return bG(b)+c|0}}function
e6(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:var
b=a[2],c=bG(a[1]);return e6(b)+c|0;case
1:var
d=a[2],f=bG(a[1]);return e6(d)+f|0;default:var
h=a[3],i=a[2],j=a[1],k=0,l=function(b,a){return e6(a)+b|0},m=e(g[16],l,k,h),n=bG(i);return(bG(j)+n|0)+m|0}}function
jQ(a){return e5(a)}function
aQ(b,a){return E(m[1],b,xL,n[a9],a[1],n[141],a[2])}function
bZ(d,c){if(typeof
c==="number")return b(m[1],d,xM);else
switch(c[0]){case
0:var
f=c[2],g=c[1],h=a(n[bu],n[a9]);return E(m[1],d,xN,h,g,bZ,f);case
1:var
i=c[2],j=c[1],k=a(n[bu],n[a9]);return E(m[1],d,xO,k,j,bZ,i);default:var
l=c[3],o=c[2],p=c[1],q=e(n[158],xQ,xP,bZ),r=a(n[bu],n[a9]),s=a(n[bu],n[a9]);return b0(m[1],d,xR,s,p,r,o,q,l)}}function
gz(h,g,f,e,b){if(b){var
i=b[1],m=i[2],o=i[1],c=gz(h,g,f,e,b[2]),j=c[3],k=c[2],l=c[1];try{var
d=aR(n[lw],h,g,k,j,m),p=[0,[0,[0,o,d[1]],l],d[2],d[3]];return p}catch(b){b=w(b);if(a(bz[22],b))return[0,l,k,j];throw b}}return[0,0,f,e]}function
gA(d,c,h,g,f){var
i=a(l[32][1],0),b=aR(n[lw],d,c,h,i,f),j=b[1],e=gz(d,c,b[2],b[3],g);return[0,e[1],j,e[2]]}var
d9=[d,function(q){var
b=n[55],f=j(b),l=k===f?b[1]:d===f?a(i[2],b):b,c=n[35],g=j(c),m=n[b4],o=k===g?c[1]:d===g?a(i[2],c):c,e=n[35],h=j(e),p=k===h?e[1]:d===h?a(i[2],e):e;return[0,p,o,m,l,jQ]}],d_=[d,function(s){var
m=b(n[b1],n[39],n[ec]),c=n[fk],g=j(c),o=k===g?c[1]:d===g?a(i[2],c):c,e=n[39],h=j(e),p=n[ec],q=k===h?e[1]:d===h?a(i[2],e):e,f=n[39],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,o,m]}],xS=[d,function(s){var
m=b(n[b1],n[39],n[ec]),c=n[fk],g=j(c),o=k===g?c[1]:d===g?a(i[2],c):c,e=n[43],h=j(e),p=n[k7],q=k===h?e[1]:d===h?a(i[2],e):e,f=n[40],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,o,m]}];function
jR(d,c,a){return b(d,c,a)?1:b(d,a,c)?0:1}function
jS(d,c,a){function
e(b){return jR(d,a,b)}return b(g[23],e,c)}function
jT(i,g){var
d=0,c=g;for(;;){if(c){var
f=c[2],e=c[1];if(a(i,e))return[0,[0,e],b(h[22],d,f)];var
d=[0,e,d],c=f;continue}return[0,0,d]}}function
jU(b,a){return jT(function(c){return jS(b,a,c)},a)}function
gB(a,d){var
b=jU(a,d),c=b[1];if(c){var
e=c[1];return[0,e,gB(a,b[2])]}return 0}function
xT(a){return gB(i6[45],a)}function
jV(c,q,p,m,l){var
f=n[ks],g=j(f),r=[0,c[2]],t=k===g?f[1]:d===g?a(i[2],f):f,h=a(o[s],[0,t,r]),u=b(n[lo],c[2],c[3]),v=e(n[kD],h,u,l),w=gx(m),x=d8(c[1],w),y=[0,function(l){function
m(a){return a}var
r=b(af[48][3],m,l),u=a(af[7],r),f=j(d7),t=0,w=[0,[0,xU,q,p],0],y=[0,c[1]],z=k===f?d7[1]:d===f?a(i[2],d7):d7,A=[0,[0,xV,x,a(o[s],[0,z,y])],w],e=n[kC],g=j(e),B=[0,h],C=k===g?e[1]:d===g?a(i[2],e):e,D=[0,[0,xW,v,a(o[s],[0,C,B])],A],E=b(n[kr],D,u),F=[0,a(ai[52],E),t];return a(M[70][20],F)}];return a(eT[62][9],y)}function
jW(d,c){function
e(b){var
c=b[1];return[0,function(d){var
e=[0,a(b[2],0),d],c=a(b[3],e);return c?[0,[0,c[1],b]]:0},c]}var
f=b(g[13],e,d);function
h(a){return a[1]}var
i=b(g[13],h,c);return b(l[6],f,i)}function
e7(e,a){function
b(a){if(a){var
f=a[2],c=jW(e,a[1]);if(c){var
g=c[1],d=b(f);return d?[0,[0,g,d[1]]]:0}return 0}return xX}return b(a)}function
xY(d,a,c){b(h[49],a,xZ);function
e(b){return I(m[1],a,x0,d,b)}b(g[11],e,c);return b(h[49],a,x1)}function
jX(f,d,c){function
i(k,c,j){var
d=c[2],m=c[1];function
n(b,a){return[0,b[1],a]}var
e=b(l[23],n,j);function
o(d){try{var
f=b(g[5],k,d)[1],c=f}catch(b){b=w(b);if(b[1]!==fn)throw b;var
c=a(h[2],x2)}return b(g[33],c,e)}try{var
t=b(d[5],m,o),i=t}catch(c){c=w(c);if(!a(bz[22],c))throw c;var
p=function(a){return a[1]},q=b(g[13],p,e),r=[0,a(d[2],0),q],f=a(d[3],r),s=f?f[1]:a(h[2],x3),i=s}return i}var
j=b(g[40],f,d);function
k(c){function
f(b){var
d=b[2],g=b[1],f=i9(a(d[2][4],d[1]),g);return e(l[11],W,f,c)}var
d=b(g[29],f,j);return i(d[1],d[2],c)}return b(g[13],k,c)}function
jY(C,c){function
e(c){if(typeof
c==="number")return 0===c?0:1;else
switch(c[0]){case
0:return[0,c[1]];case
1:var
w=c[3],x=c[2],D=c[1];return b(l[33][3],x,C)?[1,D,x,w]:[0,w];case
2:var
E=c[2],g=e(c[1]),h=e(E);if(typeof
g!=="number"&&0===g[0])if(typeof
h!=="number"&&0===h[0]){var
m=n[13],y=j(m),F=[0,g[1],h[1]],G=k===y?m[1]:d===y?a(i[2],m):m;return[0,a(o[s],[0,G,F])]}return[2,g,h];case
3:var
H=c[2],p=e(c[1]),q=e(H);if(typeof
p!=="number"&&0===p[0])if(typeof
q!=="number"&&0===q[0]){var
r=n[14],z=j(r),I=[0,p[1],q[1]],J=k===z?r[1]:d===z?a(i[2],r):r;return[0,a(o[s],[0,J,I])]}return[3,p,q];case
4:var
t=e(c[1]);if(typeof
t!=="number"&&0===t[0]){var
u=n[15],A=j(u),K=[0,t[1]],L=k===A?u[1]:d===A?a(i[2],u):u;return[0,a(o[s],[0,L,K])]}return[4,t];default:var
B=c[2],M=c[3],v=e(c[1]),f=e(M);if(typeof
v!=="number"&&0===v[0]){var
N=v[1];if(B)return f;if(typeof
f!=="number"&&0===f[0])return[0,b(o[49],N,f[1])]}return[5,v,B,f]}}return e(c)}function
bs(c,b){if(typeof
c==="number"){if(0===c){if(typeof
b==="number")if(0===b)return 0}else
if(typeof
b==="number")if(0!==b)return 1}else
switch(c[0]){case
0:return[0,c[1]];case
1:if(typeof
b!=="number"&&1===b[0])return b;break;case
2:if(typeof
b!=="number"&&2===b[0]){var
d=b[1],e=c[1],f=bs(c[2],b[2]);return[2,bs(e,d),f]}break;case
3:if(typeof
b!=="number"&&3===b[0]){var
g=b[1],i=c[1],j=bs(c[2],b[2]);return[3,bs(i,g),j]}break;case
4:if(typeof
b!=="number"&&4===b[0])return[4,bs(c[1],b[1])];break;default:if(typeof
b!=="number"&&5===b[0]){var
k=b[2],l=b[1],m=c[1],n=bs(c[3],b[3]);return[5,bs(m,l),k,n]}}return a(h[2],x4)}var
e8=[a8,x5,a7(0)];function
gC(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return e(g[17],d,b,c)}function
gD(k,j,i,h,f,t,D,s,r,C){var
m=gC(s,r)[1],o=eY(k,j,i,h,m),c=o[1],u=o[2],p=e7(t,c);if(p){var
q=p[1],v=b(g[40],c,q),w=l[33][1],x=function(c,a){return b(l[33][4],a,c)},y=e(g[16],x,w,u),z=function(f,c){var
d=c[2],h=c[1],i=l[33][1],j=a(d[2][4],d[1]);function
k(c,a){var
d=b(g[5],h,c)[2];return b(l[33][4],d,a)}var
m=e(av[14],k,j,i);return b(l[33][7],f,m)},d=jY(e(g[16],z,y,v),m),A=jX(c,q,eY(k,j,i,h,d)[1]),B=gm(d);return[0,[0,B,d,e(n[k$],f[4],f[5],A)]]}return 0}function
bt(aL,aK,aJ,aI,aH,p,m){return function(aM,aN){var
c=[0,function(G){function
H(a){return a}var
c=b(af[48][3],H,G),K=a(af[7],c),L=a(af[10],c);try{var
q=gA(c,aL,n[fc][3],L,K),U=q[2],V=q[1],v=a(n[fc][4],q[3]),x=j(p),r=k===x?p[1]:d===x?a(i[2],p):p,y=j(m),W=k===y?m[1]:d===y?a(i[2],m):m,z=gD(aK,aJ,aI,aH,r,aM,v,V,U,c);if(z)var
t=z[1],A=t[2],X=t[3],Y=t[1],f=b(n[kg],W,A),u=f[3],Z=f[4],_=f[2],$=f[1],B=function(a){return b(ai[2],0,a[1])},aa=b(g[13],B,u),ab=a(M[70][20],aa),ac=b(g[13],B,_),ad=a(M[70][20],ac),ae=function(a){return[0,[0,Q[4],[1,[0,a]]]]},ag=a(J[1][5],ya),D=e(ai[14],0,ag,c),ah=function(b){var
c=b[2];return[0,a(o[aF],b[1]),c]},aj=b(g[13],ah,u),l=n[22],E=j(l),ak=0,al=[0,r[4]],am=k===E?l[1]:d===E?a(i[2],l):l,an=[0,ad,[0,ab,[0,jV(r,X,a(o[s],[0,am,al]),aj,Z),ak]]],ao=a(M[70][20],an),ap=a(n[k6],A),aq=a(g[6],ap),ar=function(a){return b(g[5],v,a[2]-1|0)},as=b(g[13],ar,u),at=b(h[22],aq,as),au=b(M[70][3],ao,aN),av=a(ai[77],0),aw=b(M[70][3],av,au),ax=[0,a(o[aF],D),at],ay=a(o[59],ax),az=[0,a(ai[45],ay),0],aA=b(g[13],o[aF],Y),aB=[0,a(ai[b4],aA),az],aC=[0,aw,[0,a(M[70][20],aB),0]],aD=ae(D),aE=I(ai[kx],1,yb,aD,$),F=b(M[70][19],aE,aC);else
var
aG=a(bC[1],yc),F=b(M[70][4],0,aG);return F}catch(c){c=w(c);if(c===n[kk]){var
N=a(bC[1],x6);return b(M[70][4],0,N)}if(c===C[23]){var
O=a(bC[1],x7);return b(M[70][4],0,O)}if(c===e8){a(h[46],h[24]);var
P=b(h[16],x9,x8),R=b(h[16],x_,P),S=b(h[16],x$,R),T=a(bC[1],S);return b(M[70][4],0,T)}throw c}}];return a(eT[62][9],c)}}function
jZ(z,y,x){var
c=n[43],m=j(c),p=k===m?c[1]:d===m?a(i[2],c):c,f=n[40],q=j(f),A=n[k7],r=k===q?f[1]:d===q?a(i[2],f):f,g=n[fk],t=j(g),B=k===t?g[1]:d===t?a(i[2],g):g,h=n[22],u=j(h),C=[0,B],D=k===u?h[1]:d===u?a(i[2],h):h,E=a(o[s],[0,D,C]),l=n[ks],v=j(l),G=[0,p],H=k===v?l[1]:d===v?a(i[2],l):l,w=a(o[s],[0,H,G]),I=b(n[lo],p,A),J=e(n[kD],w,I,x),K=d8(r,gx(y)),L=[0,function(g){function
h(a){return a}var
l=b(af[48][3],h,g),p=a(af[7],l),q=[0,e(F[6],yg,yf,ye),[0,r]],t=[0,[0,yh,K,a(o[s],q)],[0,[0,yd,z,E],0]],c=n[kC],f=j(c),m=0,u=[0,w],v=k===f?c[1]:d===f?a(i[2],c):c,x=[0,[0,yi,J,a(o[s],[0,v,u])],t],y=b(n[kr],x,p),A=[0,a(ai[52],y),m];return a(M[70][20],A)}];return a(eT[62][9],L)}function
d$(aM){return function(aN){var
G=n[198],H=r[fg],K=r[119],L=r[121],N=r[122],f=[d,function(s){var
m=b(n[b1],n[39],n[ec]),c=n[fk],g=j(c),o=k===g?c[1]:d===g?a(i[2],c):c,e=n[43],h=j(e),p=n[ec],q=k===h?e[1]:d===h?a(i[2],e):e,f=n[40],l=j(f),r=k===l?f[1]:d===l?a(i[2],f):f;return[0,r,q,p,o,m]}],c=[0,function(O){function
P(a){return a}var
c=b(af[48][3],P,O),R=a(af[7],c),S=a(af[10],c);try{var
p=gA(c,G,n[fc][3],S,R),t=p[2],u=p[1],v=a(n[fc][4],p[3]),x=j(f),Z=k===x?f[1]:d===x?a(i[2],f):f,_=function(b){var
c=b[2],d=b[1];return[0,d,a0(a(r[73],r[ff]),c)]},$=b(g[13],_,u),y=gD(H,K,L,N,Z,aM,v,$,a0(a(r[73],r[ff]),t),c);if(y)var
q=y[1],aa=q[3],ab=q[2],ac=q[1],ad=function(a){return b(g[27],a[1],ac)},z=gC(b(g[30],ad,u),t),ae=z[2],A=bs(ab,z[1]),l=n[214],B=j(l),ag=k===B?l[1]:d===B?a(i[2],l):l,m=b(n[kg],ag,A),s=m[3],ah=m[4],aj=m[2],ak=m[1],D=function(a){return b(ai[2],0,a[1])},al=b(g[13],D,s),am=a(M[70][20],al),an=b(g[13],D,aj),ao=a(M[70][20],an),ap=function(a){return[0,[0,Q[4],[1,[0,a]]]]},aq=a(J[1][5],yp),E=e(ai[14],0,aq,c),ar=function(b){var
c=b[2];return[0,a(o[aF],b[1]),c]},as=[0,ao,[0,am,[0,jZ(aa,b(g[13],ar,s),ah),0]]],at=a(M[70][20],as),au=a(n[k6],A),av=a(g[6],au),aw=function(a){return b(g[5],v,a[2]-1|0)},ax=b(g[13],aw,s),ay=b(h[22],av,ax),az=b(M[70][3],at,aN),aA=a(ai[77],0),aB=b(M[70][3],aA,az),aC=[0,a(o[aF],E),ay],aD=a(o[59],aC),aE=[0,a(ai[45],aD),0],aG=b(g[13],o[aF],ae),aH=[0,a(ai[b4],aG),aE],aI=[0,aB,[0,a(M[70][20],aH),0]],aJ=ap(E),aK=I(ai[kx],1,yq,aJ,ak),F=b(M[70][19],aK,aI);else
var
aL=a(bC[1],yr),F=b(M[70][4],0,aL);return F}catch(c){c=w(c);if(c===n[kk]){var
T=a(bC[1],yj);return b(M[70][4],0,T)}if(c===C[23]){var
U=a(bC[1],yk);return b(M[70][4],0,U)}if(c===e8){a(h[46],h[24]);var
V=b(h[16],ym,yl),W=b(h[16],yn,V),X=b(h[16],yo,W),Y=a(bC[1],X);return b(M[70][4],0,Y)}throw c}}];return a(eT[62][9],c)}}function
j0(d,c){var
b=a(d,c);return b?[0,[0,b[1],0]]:0}var
j1=a(c6[1],[0,W,bv[20]]),gE=a(sA[8],ys)?0:[d,function(a){throw e8}];function
j3(o,n){var
f=j(gE);if(k!==f)if(d===f)a(i[2],gE);var
p=[0,yv,[0,yu,[0,b(h[16],yt,sG[30]),0]]],q=a(sF[3],0),m=e(g[16],sE[4],q,p),c=e(l[35],m,[0,m],[0,o,n]);return 0===c[0]?c[1]:a(h[2],c[1])}function
yw(a){return j3(a[1],a[2])}var
j4=b(j1[6],j2,yw);function
gF(c,b){return a(j4,[0,c,b])}function
ea(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,ea(a[2])];default:var
c=a[2],d=a[1],e=ea(a[3]);return[2,ea(d),c,e]}}function
gG(f,c){var
d=gF(f,c);if(d){var
e=a(aM[51],d[1]);return b(r[111],c,e)?[0,e]:(a(h[27],yx),0)}return 0}function
j5(f,c){function
i(a){var
b=a[2];return[0,ea(a[1]),b]}var
d=gF(f,b(g[13],i,c));if(d){var
e=a(aM[54],d[1]);return b(r[89],c,e)?[0,e]:(a(h[27],yy),a(h[46],h[24]),0)}return 0}function
ct(e,d,c){function
f(i,h){var
c=i,d=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
g=a(l[29][1],c[1]);return e<=g?b(av[4],g-e|0,d):d;case
2:var
c=c[2];continue;case
3:case
4:var
j=c[1],k=f(c[2],d),c=j,d=k;continue}return d}}return f(c,d)}function
cu(a){return ct(0,av[1],a)}function
a5(b,d){function
c(b){if(typeof
b!=="number")switch(b[0]){case
0:var
e=a(d,a(l[29][1],b[1]));return[0,a(l[30][1],e)];case
2:var
f=b[1];return[2,f,c(b[2])];case
3:var
g=b[1],h=c(b[2]);return[3,c(g),h];case
4:var
i=b[1],j=c(b[2]);return[4,c(i),j]}return b}return c(b)}function
e9(a){function
d(i,h,f){var
b=i,a=h,c=f;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=ct(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=ct(b,c,a[1]),b=b+1|0,a=l,c=m;continue;default:var
n=a[3],o=a[1],p=ct(b,ct(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return e(g[16],q,p,n)}}return d(0,a,av[1])}function
e_(c,f){function
d(c,b){return b<c?b:a(f,b-c|0)+c|0}function
e(c,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
f=a[1],g=e(c+1|0,a[2]);return[0,a5(f,function(a){return d(c,a)}),g];case
1:var
h=a[1],i=e(c+1|0,a[2]);return[1,a5(h,function(a){return d(c,a)}),i];default:var
j=a[3],k=a[2],l=a[1],m=function(a){return e(c+1|0,a)},n=b(r[10],m,j),o=a5(k,function(a){return d(c,a)});return[2,a5(l,function(a){return d(c,a)}),o,n]}}return e(0,c)}function
cv(d,c){function
e(b){var
c=b[2];return[0,a(r[71],b[1]),c]}return a(d,b(g[13],e,c))}var
gH=a(c6[1],[0,W,bv[20]]),j6=a(c6[1],[0,W,bv[20]]);function
yz(a){var
c=a[1],d=a[2];return cv(b(aM[91],c[1],c[2]),d)}var
j7=b(gH[6],yA,yz);function
yB(a){var
c=a[1],d=a[2];return cv(b(aM[92],c[1],c[2]),d)}var
j8=b(gH[6],yC,yB);function
yD(b){var
c=b[2];return cv(a(aM[30],b[1]),c)}var
j9=b(j6[6],yE,yD);function
yF(b,a){return e(n[bJ],aQ,b,a[1])}var
yG=a(n[bu],aQ),j_=[0,yH,eW,function(a){var
c=a[2];return cv(b(aM[29],a[1],aM[5]),c)},cu,a5,yG,yF];function
yI(b,a){return e(n[bJ],aQ,b,a[1])}var
yJ=a(n[bu],aQ),j$=[0,yK,eW,function(a){var
c=a[2];return cv(b(aM[29],a[1],aM[5]),c)},cu,a5,yJ,yI];function
yL(b,a){return e(n[bJ],aQ,b,a[1])}var
gI=[0,yM,eW,j9,cu,a5,a(n[bu],aQ),yL];function
gJ(c,b){function
d(b,a){return e(n[bJ],aQ,b,a[1])}var
f=a(n[bu],aQ);function
g(a){return gG(a[1],a[2])}return[0,yN,function(a){return[0,c,b]},g,cu,a5,f,d]}function
gK(c,b){function
d(b,a){return e(n[bJ],aQ,b,a[1])}var
f=a(n[bu],aQ);function
g(a){return gG(a[1],a[2])}return[0,yO,function(a){return[0,c,b]},g,cu,a5,f,d]}function
gL(b,a){function
c(b,a){return e(n[bJ],n[a9],b,a[1])}function
d(a){var
b=a[2],c=a[1];return j0(function(a){return j5(c,a)},b)}return[0,yP,function(c){return[0,b,a]},d,e9,e_,bZ,c]}var
gM=[0,yQ,gl,j7,e9,e_,bZ,function(b,a){return e(n[bJ],n[a9],b,a[1])}],ka=[0,yR,gl,j8,e9,e_,bZ,function(b,a){return e(n[bJ],n[a9],b,a[1])}];function
yS(c){var
a=e7([0,gM,0],eY(r[96],r[94],r[97],r[98],c)[1]);if(a){var
d=a[1],e=function(a){return a[1]};return[0,b(g[13],e,d)]}return 0}var
yT=a(bt(n[fj],r[aF],r[b2],r[fh],r[fm],d_,n[fi]),[0,j_,0]);function
yU(b){var
c=[0,gJ(yV,[0,b]),0];return a(bt(n[fj],r[aF],r[b2],r[fh],r[fm],d_,n[fi]),c)}var
yW=d$([0,j$,0]);function
yX(a){return d$([0,gK(yY,[0,a]),0])}function
yZ(b){var
c=[0,gL(y0,[0,b]),0];return a(bt(n[fa],r[96],r[94],r[97],r[98],d9,n[fe]),c)}var
y2=[0,gL(y1,0),0],y3=a(bt(n[fa],r[96],r[94],r[97],r[98],d9,n[fe]),y2),y5=[0,gJ(y4,0),0],y6=a(bt(n[fj],r[aF],r[b2],r[fh],r[fm],d_,n[fi]),y5),y8=d$([0,gK(y7,0),0]),y9=a(bt(n[fa],r[96],r[94],r[97],r[98],d9,n[fe]),[0,gM,0]),y_=a(bt(n[fa],r[96],r[94],r[97],r[98],d9,n[fe]),[0,ka,0]),y$=d$([0,gI,0]),$=[0,sH,sI,eU,gj,eV,gk,gl,eW,bD,a0,bd,gm,eX,c7,s2,eY,av,s4,i9,n,d3,xs,xt,jP,xv,d4,d5,d6,d7,d8,gx,gy,bG,e6,jQ,aQ,bZ,gz,gA,d9,d_,xS,jR,jS,jT,jU,gB,xT,jV,jW,e7,e7,xY,jX,jY,bs,e8,gC,gD,bt,jZ,d$,j0,j1,j2,gE,j3,j4,gF,ea,gG,j5,ct,cu,a5,e9,e_,cv,gH,j6,j7,j8,j9,j_,j$,gI,gJ,gK,gL,gM,ka,yS,yT,yU,yW,yX,yZ,y3,y6,y8,y9,y_,y$,a(bt(n[fj],r[aF],r[b2],r[fh],r[fm],d_,n[fi]),[0,gI,0])];a6(774,$,"Micromega_plugin.Coq_micromega");a(at[12],za);function
zb(d){var
b=[28,[0,0,[31,Q[4],[0,[0,x,zc],0],0]]],c=a(J[1][5],zd);return I(as[4],1,0,c,b)}var
ze=[0,function(b,a){return ai[54]}];e(as[9],0,[0,x,zf],ze);b(at[19],zb,x);var
zg=0,zi=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(a){var
c=b(D[19],a,f);return b($[97],-1,c)}}return a(h[2],zh)},zg],zk=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(V[6],z[2]),i=b(D[2][7],g,f),j=a(V[6],z[14]),k=b(D[2][7],j,e);return function(a){var
c=b(D[19],a,k);return b($[97],i,c)}}}return a(h[2],zj)},zi],zl=a(aE[12],zk);e(as[9],0,[0,x,zm],zl);function
zn(n){var
h=a(J[1][6],zo),c=z[14],f=0,g=0;if(0===c[0]){var
i=[0,[0,zq,[0,[1,Q[4],[5,[0,c[1]]],h],g]],f],k=a(J[1][6],zr),d=z[14],j=0;if(0===d[0]){var
l=[0,[1,Q[4],[5,[0,d[1]]],k],j],m=a(J[1][6],zt),e=z[2];if(0===e[0])return b(aD[4],[0,x,zw],[0,[0,zv,[0,[1,Q[4],[5,[0,e[1]]],m],l]],i]);throw[0,G,zu]}throw[0,G,zs]}throw[0,G,zp]}b(at[19],zn,x);var
zx=0,zz=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[101],d)}}return a(h[2],zy)},zx],zA=a(aE[12],zz);e(as[9],0,[0,x,zB],zA);function
zC(g){var
f=a(J[1][6],zD),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,zG],[0,[0,zF,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,zE]}b(at[19],zC,x);var
zH=0,zJ=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[102],d)}}return a(h[2],zI)},zH],zK=a(aE[12],zJ);e(as[9],0,[0,x,zL],zK);function
zM(g){var
f=a(J[1][6],zN),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,zQ],[0,[0,zP,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,zO]}b(at[19],zM,x);var
zR=0,zT=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[103],d)}}return a(h[2],zS)},zR],zU=a(aE[12],zT);e(as[9],0,[0,x,zV],zU);function
zW(g){var
f=a(J[1][6],zX),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,z0],[0,[0,zZ,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,zY]}b(at[19],zW,x);var
z1=0,z3=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[104],d)}}return a(h[2],z2)},z1],z4=a(aE[12],z3);e(as[9],0,[0,x,z5],z4);function
z6(g){var
f=a(J[1][6],z7),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,z_],[0,[0,z9,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,z8]}b(at[19],z6,x);var
z$=0,Ab=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[98],d)}}return a(h[2],Aa)},z$],Ac=a(aE[12],Ab);e(as[9],0,[0,x,Ad],Ac);function
Ae(g){var
f=a(J[1][6],Af),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,Ai],[0,[0,Ah,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,Ag]}b(at[19],Ae,x);var
Aj=0,Al=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[99],d)}}return a(h[2],Ak)},Aj],Am=a(aE[12],Al);e(as[9],0,[0,x,An],Am);function
Ao(g){var
f=a(J[1][6],Ap),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,As],[0,[0,Ar,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,Aq]}b(at[19],Ao,x);var
At=0,Av=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[gU],d)}}return a(h[2],Au)},At],Aw=a(aE[12],Av);e(as[9],0,[0,x,Ax],Aw);function
Ay(g){var
f=a(J[1][6],Az),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,AC],[0,[0,AB,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,AA]}b(at[19],Ay,x);var
AD=0,AF=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[93],d)}}return a(h[2],AE)},AD],AG=a(aE[12],AF);e(as[9],0,[0,x,AH],AG);function
AI(g){var
f=a(J[1][6],AJ),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,AM],[0,[0,AL,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,AK]}b(at[19],AI,x);var
AN=0,AP=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(c){var
d=b(D[19],c,f);return a($[95],d)}}return a(h[2],AO)},AN],AQ=a(aE[12],AP);e(as[9],0,[0,x,AR],AQ);function
AS(g){var
f=a(J[1][6],AT),c=z[14],d=0,e=0;if(0===c[0])return b(aD[4],[0,x,AW],[0,[0,AV,[0,[1,Q[4],[5,[0,c[1]]],f],e]],d]);throw[0,G,AU]}b(at[19],AS,x);var
AX=0,AZ=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(a){var
c=b(D[19],a,f);return b($[96],-1,c)}}return a(h[2],AY)},AX],A1=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(V[6],z[2]),i=b(D[2][7],g,f),j=a(V[6],z[14]),k=b(D[2][7],j,e);return function(a){var
c=b(D[19],a,k);return b($[96],i,c)}}}return a(h[2],A0)},AZ],A2=a(aE[12],A1);e(as[9],0,[0,x,A3],A2);function
A4(n){var
h=a(J[1][6],A5),c=z[14],f=0,g=0;if(0===c[0]){var
i=[0,[0,A7,[0,[1,Q[4],[5,[0,c[1]]],h],g]],f],k=a(J[1][6],A8),d=z[14],j=0;if(0===d[0]){var
l=[0,[1,Q[4],[5,[0,d[1]]],k],j],m=a(J[1][6],A_),e=z[2];if(0===e[0])return b(aD[4],[0,x,Bb],[0,[0,Ba,[0,[1,Q[4],[5,[0,e[1]]],m],l]],i]);throw[0,G,A$]}throw[0,G,A9]}throw[0,G,A6]}b(at[19],A4,x);var
Bc=0,Be=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(V[6],z[14]),f=b(D[2][7],e,d);return function(a){var
c=b(D[19],a,f);return b($[94],-1,c)}}return a(h[2],Bd)},Bc],Bg=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(V[6],z[2]),i=b(D[2][7],g,f),j=a(V[6],z[14]),k=b(D[2][7],j,e);return function(a){var
c=b(D[19],a,k);return b($[94],i,c)}}}return a(h[2],Bf)},Be],Bh=a(aE[12],Bg);e(as[9],0,[0,x,Bi],Bh);function
Bj(n){var
h=a(J[1][6],Bk),c=z[14],f=0,g=0;if(0===c[0]){var
i=[0,[0,Bm,[0,[1,Q[4],[5,[0,c[1]]],h],g]],f],k=a(J[1][6],Bn),d=z[14],j=0;if(0===d[0]){var
l=[0,[1,Q[4],[5,[0,d[1]]],k],j],m=a(J[1][6],Bp),e=z[2];if(0===e[0])return b(aD[4],[0,x,Bs],[0,[0,Br,[0,[1,Q[4],[5,[0,e[1]]],m],l]],i]);throw[0,G,Bq]}throw[0,G,Bo]}throw[0,G,Bl]}b(at[19],Bj,x);var
kb=[0,x];a6(782,kb,"Micromega_plugin.G_micromega");a6(783,[0,g1,l,r,p,C,aM,c6,$,kb],"Micromega_plugin");return});
