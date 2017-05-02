use recommend;

create table user(
id int,
name varchar(20),
primary key(id)
);

create table anime(
id int,
name varchar(20),
brief varchar(100),
primary key(id)
);

insert into user values(1,"Tom");
select * from user;

insert into anime values(279,"a","A");
select * from anime;

insert into anime values(3494,"b","B");
insert into anime values(3377,"c","C");
insert into anime values(3452,"d","D");
insert into anime values(782,"e","E");
insert into anime values(3421,"f","F");
insert into anime values(2730,"g","G");




create table anime_style(
anime_id int,
style_id int,
foreign key(anime_id) references anime(id)
);

insert into anime_style values(279,26);
insert into anime_style values(279,30);
insert into anime_style values(279,32);
insert into anime_style values(279,8);
insert into anime_style values(279,7);

insert into anime_style values(3494,9);
insert into anime_style values(3494,19);
insert into anime_style values(3494,29);
insert into anime_style values(3494,46);

insert into anime_style values(3377,34);
insert into anime_style values(3377,7);
insert into anime_style values(3377,18);

insert into anime_style values(3452,30);
insert into anime_style values(3452,32);
insert into anime_style values(3452,7);
insert into anime_style values(3452,22);

insert into anime_style values(782,30);
insert into anime_style values(782,32);
insert into anime_style values(782,7);
insert into anime_style values(782,1);
insert into anime_style values(782,50);

insert into anime_style values(3421,30);
insert into anime_style values(3421,32);
insert into anime_style values(3421,7);
insert into anime_style values(3421,22);

insert into anime_style values(2730,11);
insert into anime_style values(2730,30);
insert into anime_style values(2730,22);

create table user_anime(
user_id int,
anime_id int,
foreign key(user_id) references user(id),
foreign key(anime_id) references anime(id)
);

insert into user_anime values(1,782);
insert into user_anime values(1,3421);
insert into user_anime values(1,2730);