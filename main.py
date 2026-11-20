import json,os,time,zlib,base64 as b,urllib.request as ur
P='PNBRQK';V=[100,320,330,500,900,0];I=10**9;n=0;K={};H={}
T=[(c-75)*5 for c in zlib.decompress(b.b85decode(b'c-n<fI}SrJ2s>MZ5aJh_q4&Q^P4Y5ysOlk(0XrY}gZG2XTB|Z6^7X=sRuz^obyq_<MbLh1K8OJtB&~$lbIx?;m{l{Fy}5PQgE5Ctq3aXf5Iu3K9-Xe*Cq+Of0G%7C_gLul-Fw98haGAExGf!p9JBB90lf6E`NJBgj{cszJ|-H2@R%Pgy<--Y4c(uKFRhjCx#|N<L2m>'))]
D=[(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
def g(s,o,k=1):return[8*(s//8+x*i)+s%8+y*i for x,y in o for i in range(1,k+1)if 0<=s//8+x*i<8 and 0<=s%8+y*i<8]
R=[[g(s,[x],7)for x in D]for s in range(64)]
N=[g(s,[(1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1)])for s in range(64)]
Y=[g(s,D)for s in range(64)]
X=[[g(s,[(x,1),(x,-1)])for s in range(64)]for x in(-1,1)]
Z=Exception
def W(x,q):k=P.index(x.upper());h=1-2*(x>'Z');return h*(V[k]+T[64*k+(q^56*(h<0))])
def A(b,s,o):
 p=P if o else P.lower()
 return any(b[q]==p[i]for i,L in((0,X[o]),(1,N),(5,Y))for q in L[s])or any(next((b[q]for q in r if b[q]>'.'),'')in(p[4],p[2+(k<4)])for k,r in enumerate(R[s]))
def M(p,m):
 b,w,c,e,v=p;s,t,z=m
 b=b[:];x=b[s];v-=W(x,s)
 if x in'Pp'and s%8!=t%8 and b[t]=='.':q=t+8*(2*w-1);v-=W(b[q],q);b[q]='.'
 elif b[t]>'.':v-=W(b[t],t)
 y=(z.upper()if w else z)or x;b[t]=y;b[s]='.';v+=W(y,t)
 if A(b,b.index('kK'[w]),not w):return
 return b,not w,c,(s+t)//2 if x in'Pp'and abs(t-s)==16 else-1,v
def G(p):
 b,w,c,e,_=p;m=[]
 for s,x in enumerate(b):
  if x=='.'or x.isupper()!=w:continue
  k=x.lower()
  if k=='p':
   d=-8 if w else 8;t=s+d;z=[t]*(b[t]=='.')
   if z and s//8==(6 if w else 1)and b[t+d]=='.':z+=[t+d]
   z+=[t+h for h in(-1,1)if 0<=s%8+h<8 and(b[t+h]>'.'and b[t+h].isupper()!=w or t+h==e)]
   m+=[(s,t,q)for t in z for q in'qrbn'*(t<8 or t>55)or['']]
  elif k in'nk':m+=[(s,t,'')for t in(N if k=='n'else Y)[s]if b[t]=='.'or b[t].isupper()!=w]
  else:
   for r in R[s][:4]*(k!='b')+R[s][4:]*(k!='r'):
    for t in r:
     if b[t]>'.'and b[t].isupper()==w:break
     m+=[(s,t,'')]
     if b[t]>'.':break
 return m
def Pr(b,m,kl):
 s,t,z=m;x=b[s];v=b[t]
 if v>'.'or x in'Pp'and s%8!=t%8:return 10000+10*(V[P.find(v.upper())]if v>'.'else 100)-P.index(x.upper())
 if z:return 9000
 return 8000*(m in kl)
def Q(p,d,a,b,y,q):
 global n
 n+=1
 if n>8e4:raise Z
 bd,w,c,e,v=p;k=''.join(bd)+str(w);h=H.get(k);oa=a;bm=None
 if h and h[0]>=d and(h[2]==0 or h[2]>0 and h[1]>=b or h[2]<0 and h[1]<=a):return h[1]
 ck=A(bd,bd.index('kK'[w]),not w);s=None;f=0;sv=v if w else-v
 if d<=0 and(q>=4 or not ck and sv>=b):return sv
 if d<=0 and not ck:a=max(a,sv);s=sv
 kl=K.setdefault(y,[]);best=-I if s is None else s;al=G(p);ms=al
 if s is not None:ms=[m for m in al if bd[m[1]]>'.'or m[2]or m[1]==e and bd[m[0]]in'Pp']
 for _,m in sorted([(-Pr(bd,m,kl),m)for m in ms]):
  nq=M(p,m)
  if nq is None:continue
  f=1;t=-Q(nq,d-1 if d>0 else 0,-b,-a,y+1,q+(d<=0))
  if t>best:best=t;bm=m
  a=max(a,best)
  if a>=b:
   if bd[m[1]]=='.'and not m[2]and m not in kl:kl[:]=([m]+kl)[:2]
   break
 if not f:return best if s!=None else(-99999+y if ck else 0)
 H[k]=(d,best,0 if oa<best<b else(1 if best>=b else-1),bm)
 return best
def pk(p):
 global n,K,H
 n=0;K={};H={};bm=None
 try:
  for d in range(5):
   a=-I;nb=None
   for _,m in sorted([(-Pr(p[0],m,[]),m)for m in G(p)]):
    nq=M(p,m)
    if nq is None:continue
    v=-Q(nq,d-1,-I,-a,1,0)
    if nb is None or v>a:a=v;nb=m
   bm=nb
 except:pass
 return bm
def F(f):
 a,w,c,e=f.split()[:4];b=[y for x in a.replace('/','')for y in(['.']*int(x)if x.isdigit()else[x])]
 return b,w=='w',c,-1 if e=='-'else 8*(8-int(e[1]))+'abcdefgh'.index(e[0]),sum(W(x,i)for i,x in enumerate(b)if x>'.')
if __name__=='__main__':
 U=os.environ['MAKEFUN_ENV_URL']
 def po(p,d):return json.load(ur.urlopen(ur.Request(U+p,json.dumps(d).encode())))
 while 1:
  try:g=po('/game',{})
  except:time.sleep(1);continue
  if g['status']=='done':break
  while g['status']=='your_move'and g['legal_moves']:
   m=pk(F(g['fen']));u=''.join('abcdefgh'[x%8]+str(8-x//8)for x in m[:2])+m[2]if m else g['legal_moves'][0]
   try:g=po('/move',{'game':g['game'],'move':u})
   except:time.sleep(1)
