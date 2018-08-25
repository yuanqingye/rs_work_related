select k1.*,k2.onejump,k3.persvg,k4.liucun from 
(
	select 
	a.dt,
	count(u_mid) as tpv,
	count(distinct u_mid) as tuv, 
	count(if(p_channel='deco',u_mid,null)) dpv,
	count(distinct if(p_channel='deco',u_mid,null)) duv,
	count(if(p_channel='deco',u_mid,null))/count(u_mid) dpvrate,
	count(distinct if(p_channel='deco',u_mid,null))/count(distinct u_mid) duvrate,
	count(if(p_channel='deco',u_mid,null))/count(distinct if(p_channel='deco',u_mid,null)) perpv
	from 
	(  
			select date_sort dt from dm.d_date where date_sort >= ${d14} and date_sort <= ${d1}  --这里的特殊符号
		) a
	left outer join 
	(
      select dt,p_channel,u_mid from ods.ods_app_pageview_info
	where l_ip!='210.13.91.146'  and service in ('h5.pvuv','ios.pvuv','android.pvuv','m.pvuv') and substr(u_mid,1,2)!='a_' and path='p' 
	and l_city!='测试'
	and dt>=${d14}  and dt<=${d1}
      )  b on a.dt=b.dt
      group by a.dt
)k1
left outer join 
(
	select dt,1-count( case when r_url in ('apk://com.RedStarMain+RSFiHomeViewController',
	'com.redstar.mainapp.business.jiazhuang.JiaZhuangMainActivity',
	'com.redstar.mainapp.business.jiazhuang.home.JiaHomeFragment') then u_mid else null end)/count(case when p_type='page.deco.main' then u_mid else null end)  as onejump
	from ods.ods_app_pageview_info where l_ip!='210.13.91.146'  and service in ('h5.pvuv','ios.pvuv','android.pvuv','m.pvuv') and substr(u_mid,1,2)!='a_' and path='p' 
	and l_city!='测试' and p_channel='deco' and dt>=${d7}  and dt<=${d1} group by dt
)k2 on k2.dt=k1.dt
left outer join 
(
select dt,avg(persvg) persvg
from
(
	select dt,u_mid,sum(p_stay_time)/1000/60 persvg
	from  ods.ods_app_pageview_info where l_ip!='210.13.91.146'  and service like '%staytime%' and substr(u_mid,1,2)!='a_' and path='z' 
	and l_city!='测试' and p_channel='deco' and  dt>=${d7}  and dt<=${d1} and length(p_stay_time)<=7 group by dt,u_mid
	  )a group by dt
)k3 on k3.dt=k1.dt
left outer join 
(
	select k1.dt1 dt,
	count(distinct k2.u_mid) /count(distinct k1.u_mid)  as liucun
	from 	  
		  
	(
		select distinct a0.dt as dt1,a.dt as dt2,u_mid
		 from 
		(  
			select date_sort dt from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1}
		) a0
		JOIN
		(
			select distinct dt,u_mid from
			ods.ods_app_pageview_info where 
			l_ip!='210.13.91.146'  and service in ('h5.pvuv','ios.pvuv','android.pvuv','m.pvuv') and substr(u_mid,1,2)!='a_' and path='p' 
			and l_city!='测试' and p_channel='deco' and dt>=${d21} and dt<=${d8}
		 )a ON  1 = 1
		where  datediff(concat(substr(a0.dt,1,4),'-',substr(a0.dt,5,2),'-',substr(a0.dt,7,2)),
						concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))>=8
				and 
					datediff(concat(substr(a0.dt,1,4),'-',substr(a0.dt,5,2),'-',substr(a0.dt,7,2)),
						concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))<=14
					and u_mid is not null
	)k1
	left outer join 
	(
	select distinct a0.dt as dt1,a.dt as dt2,u_mid
		 from 
		(  
			select date_sort dt from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1}
		) a0
		JOIN
		(
			select distinct dt,u_mid from
			ods.ods_app_pageview_info where 
			l_ip!='210.13.91.146'  and service in ('h5.pvuv','ios.pvuv','android.pvuv','m.pvuv') and substr(u_mid,1,2)!='a_' and path='p' 
			and l_city!='测试' and p_channel='deco' and dt>=${d14} and dt<=${d1}
		 )a ON  1 = 1
		where  datediff(concat(substr(a0.dt,1,4),'-',substr(a0.dt,5,2),'-',substr(a0.dt,7,2)),
						concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))>=1
				and 
					datediff(concat(substr(a0.dt,1,4),'-',substr(a0.dt,5,2),'-',substr(a0.dt,7,2)),
						concat(substr(a.dt,1,4),'-',substr(a.dt,5,2),'-',substr(a.dt,7,2)))<=7
					and u_mid is not null
	)k2 on k1.dt1=k2.dt1 and k1.u_mid=k2.u_mid
	group by k1.dt1

)k4 on k4.dt=k1.dt;

drop table test.deco_table2;
create table  test.deco_table2
as
select k1.*,k2.booking,k3.baoming,k3.zhuanhua 
from
(
	select 
	dt,
	count(if(p_type='page.designer.detail',u_mid,null)) designerpv,
	count(distinct if(p_type='page.designer.detail',u_mid,null)) designeruv,
	count(if(p_type='page.case.detail',u_mid,null)) casepv,
	count(distinct if(p_type='page.case.detail',u_mid,null)) caseuv,
	count(if(p_type='page.designer.reserved.detail.deco',u_mid,null)) reservedpv,
	count(distinct if(p_type='page.designer.reserved.detail.deco',u_mid,null)) reserveduv,
	count(if(p_type='page.photo.detail',u_mid,null)) photopv,
	count(distinct if(p_type='page.photo.detail',u_mid,null)) photouv,
	count(if(p_type='page.lifestyle.detail',u_mid,null)) lifestylepv,
	count(distinct if(p_type='page.lifestyle.detail',u_mid,null)) lifestyleuv,
	count(if(p_type='page.wiki.detail',u_mid,null)) wikipv,
	count(distinct if(p_type='page.wiki.detail',u_mid,null)) wikiuv,
	count(if(p_type='page.question.detail',u_mid,null)) questionpv,
	count(distinct if(p_type='page.question.detail',u_mid,null)) questionuv
	from ods.ods_app_pageview_info where l_ip!='210.13.91.146'  and service in ('h5.pvuv','ios.pvuv','android.pvuv','m.pvuv') and substr(u_mid,1,2)!='a_' and path='p' 
	and l_city!='测试' and p_channel='deco'
	and  dt>=${d7}  and dt<=${d1} group by dt
)k1
left outer join 
(
-- 预约
select  a.date_sort dt,  count(case when regexp_replace(split(b.create_time,' ')[0],'-','')=a.date_sort then 
						b.id else null end ) booking
						from 
						(select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a
						join
						ods.ods_db_crm_booking_info_dt  b on 1=1  and booking_type=1 and is_del=0
                        group by a.date_sort
)k2 on k2.dt=k1.dt
left outer join 
(
-- 活动报名
select 
a.date_sort dt,
count(distinct if(split(b.create_date,' ')[0]=date_id,b.id,null)) as baoming,
count(distinct if(split(c.create_date,' ')[0]=date_id,c.id,null)) as zhuanhua
from 
(select date_id,date_sort  from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a
left outer join 
ods.ods_jz_business_jz_activity_user_dt b on 1=1
left outer join 
ods.ods_jz_business_jz_activity_user_feedback_dt c
on c.activity_user_id=b.id and c.process_status=4
where is_del=0 group by a.date_sort
)k3 on k3.dt=k1.dt;

drop table test.deco_table3;
create table test.deco_table3
as
select k1.*,k2.designer,k3.cases,k4.tuji,k5.ask,k6.answer,k7.artical
from
	(
	select date_sort dt,count(distinct if(split(pass_time,' ')[0]<=date_id,b.id,null)) as company from  
                        (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a 
						left outer join  
                        ods.ods_jz_business_jz_company_dt b on 1=1 where is_online=1 and is_del=0 and status=3  group by date_sort
	)k1   
    left outer join 
	(
	select date_sort dt ,count(distinct if(split(pass_time,' ')[0]<=date_id,b.id,null)) as designer from  
                        (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a left outer join 
                        ods.ods_jz_business_jz_designer_dt b on 1=1 where  is_del=0 and open_status=1  group by date_sort 
	)k2 on k1.dt=k2.dt
	left outer join 
	(
	select date_sort dt,count(distinct if(split(pass_time,' ')[0]<=date_id,b.id,null)) as cases from  
                    (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a left outer join 
                    ods.ods_jz_business_jz_case_dt b on 1=1 where  is_del=0 and status=3 and open_status=1  group by date_sort
	)k3 on k1.dt=k3.dt
	left outer join 
	(
	select date_sort dt ,count(distinct if(split(create_date,' ')[0]<=date_id,b.id,null)) as tuji from  
                        (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a left outer join 
                        ods.ods_jz_business_jz_atlas_dt b on 1=1 where is_del=0  and check_status=1 group by date_sort 
	)k4 on k1.dt=k4.dt
	left outer join 
	(
	select date_sort dt,count(distinct if(split(create_date,' ')[0]<=date_id,b.id,null)) as ask from  
                        (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a left outer join 
                        ods.ods_jz_business_jz_asking_dt b on 1=1 where asking_type=1 and is_online=1 and is_del=0 group by date_sort
	)k5 on k1.dt=k5.dt
	left outer join 
	(
	select date_sort dt,count(distinct if(split(b.create_date,' ')[0]<=date_id,b.id,null)) as answer from  
                        (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a left outer join 
                        ods.ods_jz_business_jz_asking_answer_dt b on 1=1 join ods.ods_jz_business_jz_asking_dt c on b.asking_id=c.id 
                        where c.asking_type=1 and b.is_online=1   group by date_sort 
	
	)k6 on k1.dt=k6.dt
	left outer join 
	(
	select date_sort dt ,count(distinct if(split(create_date,' ')[0]<=date_id,b.id,null)) as artical from  
                        (select date_id,date_sort from dm.d_date where date_sort >= ${d7} and date_sort <= ${d1})a left outer join 
                        ods.ods_jz_business_jz_article_dt b on 1=1 where  is_del=0 and  check_status=1 group by date_sort
	)k7 on k1.dt=k7.dt
"