"""Provider contracts with fixed fake responses; no credentials or network."""
import importlib.util
import sys

spec=importlib.util.spec_from_file_location('context_source',sys.argv[1])
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
config=dict(channels=['C1'],include_dms=False,lookback_hours=24,retention_days=30,max_threads=20,max_observations=100)
s=m.Slack.__new__(m.Slack);s.config=config;s.base='https://test.slack.com/';s.identity={'user_id':'U1'}
root={'ts':'100.000001','text':'old root','user':'U2'}
second={'ts':'200.000001','text':'another root','user':'U2'}
reply={'ts':'10000.000001','thread_ts':root['ts'],'text':'please respond','user':'U3'}
phase=0;calls=[]
def api(method,**params):
    calls.append((method,params))
    if method=='conversations.history':
        if params.get('cursor')=='page2':
            assert params['latest']=='1000'
            return dict(messages=[root],response_metadata=dict(next_cursor=''))
        return dict(messages=[second],has_more=True,response_metadata=dict(next_cursor='page2'))
    if phase==1:raise m.SourceError('Slack rate limited',900)
    return dict(messages=[root,reply] if params['ts']==root['ts'] else [second],response_metadata=dict(next_cursor=''))
s.api=api
first=s.collect(None,1000,1)
assert first['checkpoint']['channels']['C1']['cursor']=='page2'
# A later tick rotates to threads, retaining in-flight history on throttling.
phase=1
partial=s.collect(first['checkpoint'],2000,1)
assert partial['error']=='Slack rate limited' and partial['retry_after']==900
assert partial['checkpoint']['channels']['C1']['cursor']=='page2'
phase=0
second_page=s.collect(partial['checkpoint'],3000,1)
assert any(o['native_id']=='C1:'+root['ts'] for o in second_page['observations'])
assert second_page['checkpoint']['channels']['C1']['through']==1000
# Old roots are explicitly scanned and a new reply is retained as evidence.
thread=s.collect(second_page['checkpoint'],4000,3)
assert any(o['native_id']=='C1:'+reply['ts'] for o in thread['observations'])
# Repeated reads produce no observations and do not recreate expired cache.
cp=thread['checkpoint']
# Complete any remaining scan and then only repeat identical complete pages.
s.api=lambda method,**p: dict(messages=([root,second] if method=='conversations.history' else [root,reply] if p['ts']==root['ts'] else [second]),response_metadata=dict(next_cursor=''))
for _ in range(3):cp=s.collect(cp,5000,10)['checkpoint']
assert s.collect(cp,6000,10)['observations']==[]
# A disappeared known message produces one tombstone, even after cache expiry.
s.api=lambda method,**p: dict(messages=[],response_metadata=dict(next_cursor=''))
deleted=s.collect(cp,7000,10)
assert any(o['deleted'] for o in deleted['observations'])
assert not s.collect(deleted['checkpoint'],8000,10)['observations']
# Changed old roots preserve content revisions, with bounded UTF-8 content.
a=s.normalize('C1',dict(root, text='å'*16000))
assert len(a['content'].encode())<=16000
assert s.normalize('C1',dict(root,edited={'ts':'10'}))['revision']!=s.normalize('C1',root)['revision']

# Expired opaque cursors replay the original interval instead of retrying forever
# or skipping the unseen tail; deletion reconciliation keeps its prior snapshot.
for kind in ('history','thread'):
    key='C1' if kind=='history' else 'C1:'+root['ts']
    bucket='channels' if kind=='history' else 'threads'
    prior=[s.normalize('C1',root),s.normalize('C1',second)]
    scan=dict(until=1000,oldest=0,previous=prior,seen=prior[:1],cursor='expired')
    cp=dict(channels={'C1':{}},threads={},turn=0 if kind=='history' else 1)
    cp[bucket][key]=scan
    def expired(method,**params):
        assert params['cursor']=='expired'
        raise m.SourceError('Slack API: invalid_cursor')
    s.api=expired
    reset=s.collect(cp,2000,1)
    assert 'cursor expired' in reset['error']
    restarted=reset['checkpoint'][bucket][key]
    assert restarted['until']==1000 and restarted['oldest']==0
    assert restarted['previous']==prior and restarted['seen']==[] and 'cursor' not in restarted
    assert scan['cursor']=='expired'  # Input checkpoint remains untouched.
    def replay(method,**params):
        if (method=='conversations.history')==(kind=='history'):
            assert params['latest']=='1000' and params['cursor']==''
        return dict(messages=[dict(root,text='edited during scan')],response_metadata=dict(next_cursor=''))
    s.api=replay
    recovered=s.collect(reset['checkpoint'],3000,2)
    assert recovered['error'] is None
    assert recovered['checkpoint'][bucket][key]['through']==1000
    assert any(o['content']=='edited during scan' for o in recovered['observations'])
    assert any(o['native_id']=='C1:'+second['ts'] and o['deleted'] for o in recovered['observations'])

s.config=dict(config,include_dms=True)
def expired_dm(method,**params):
    assert method=='conversations.list' and params['cursor']=='expired'
    raise m.SourceError('Slack API: invalid_cursor')
s.api=expired_dm
reset=s.collect(dict(channels={'D1':{}},threads={},turn=0,dm_cursor='expired'),1000,1)
assert reset['checkpoint']['dm_cursor']=='' and 'D1' in reset['checkpoint']['channels']
assert 'cursor expired' in reset['error']
def replay_dm(method,**params):
    assert method=='conversations.list' and params['cursor']==''
    return dict(channels=[dict(id='D2')],response_metadata=dict(next_cursor=''))
s.api=replay_dm
recovered=s.collect(reset['checkpoint'],2000,1)
assert recovered['error'] is None and {'D1','D2'}<=recovered['checkpoint']['channels'].keys()

z=m.Zulip.__new__(m.Zulip);z.config=dict(config,channels=['10']);z.realm='https://zulip.test';z.identity={'user_id':7}
class Client:
    def get_messages(self,request):
        if request['anchor']=='newest':
            assert request['include_anchor'] is True
            return dict(result='success',found_oldest=False,messages=[dict(id=200,type='stream',stream_id=10,subject='old topic',timestamp=900,sender_id=7,content='latest')])
        assert request['anchor']==200 and request['include_anchor'] is False
        return dict(result='success',found_oldest=True,messages=[dict(id=100,type='stream',stream_id=10,subject='old topic',timestamp=800,sender_id=8,content='earlier')])
z.client=Client()
first=z.collect(None,1000,1)
assert first['checkpoint']['scopes']['10']['anchor']==200
second=z.collect(first['checkpoint'],2000,1)
assert second['checkpoint']['scopes']['10']['through']==1000
assert second['observations'][0]['conversation']=='10:old topic'
# Invalid CLI stdout, HTTP status, API-level error and Retry-After were covered
# through the checkout binary and fake executable in test_context.py.
print('context adapter contracts passed: durable pagination, throttling, old replies, deduplication, deletion, revisions and UTF-8 bounds')
