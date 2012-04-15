# datacontext
datacontext为逻辑函数提供上下文中的数据，并回收经过逻辑处理的数据。目的是使clojure的逻辑函数能专注于处理纯粹的数据逻辑而不需要了解数据的来源，从而使函数的意义更加清楚，更易于测试和维护。
    本质上，datacontext把数据来源从数据处理的过程中抽取出来。

## 基本概念
    坐标:              
         数据存储的位置信息，provide和recover根据坐标信息来提供和回收数据。
    接口函数(inf-fn):   
         开发最终需要实现的函数，由datacontext根据逻辑函数自动创建。参数表里面包含数据的坐标信息。
    逻辑函数(logic-fn):
         开发时需要编写的处理数据的函数，参数表里包含由provide注入的直接可用的数据。
    provide函数: 
         根据接口函数提供的坐标，为逻辑函数注入上下文中的数据。
    recover函数: 
        根据接口函数提供的坐标，回收逻辑函数声明需要回收的数据。
    recover函数要求返回'回收之后的数据'，它有可能附加了回收时在上下文中产生的'坐标'信息。
    数据的key:   
        要求提供或者回收的数据都有一个key，数据上下文有一个判断自己是否适用于某个key的函数。参数注入和回收时，参数名被作为key。

## 核心流程
    datacontext把更容易编写和维护的logic-fn包装成可以使用的inf-fn.它以
    logic-fn参数名为key去查找适用的数据上下文，找到之后就根据上下文中
    要求提供的坐标信息，把logic-fn参数表中的的数据参数映射成inf-fn参数
    表中的坐标参数。

    执行inf-fn的时候，根据inf-fn参数中的坐标信息，去构造出logic-fn中的
    数据参数表，把逻辑转交给logic-fn，处理完逻辑之后，又根据
    logic-fn(显式或隐式)的声明，回收要求回收的数据。当回收数据的key等
    于某个参数时，坐标信息可以重新从inf-fn参数表中构造出来，其它key若
    需要坐标信息则要求在声明的时候提供。


## 例子

```clojure

```



