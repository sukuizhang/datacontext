# datacontext
datacontext为逻辑函数提供上下文中的数据，并回收经过逻辑处理的数据。

## 基本概念
    datacontext为逻辑函数提供上下文中的数据，并回收经过逻辑处理的数据。目的是使clojure的逻辑函数能专注于处理纯粹的数据逻辑而不需要了解数据的来源，从而使函数的意义更加清楚，本质更加突出，更具有可测性和易于维护。
下面是涉及到的基本概念:
    坐标:        数据存储的位置信息，provide和recover根据坐标信息来提供和回收数据。
    接口函数:     开发最终需要实现的函数，由datacontext根据逻辑函数自动创建。参数表里面包含数据的坐标信息。
    逻辑函数:     开发时需要编写的处理数据的函数，参数表里包含由provide注入的直接可用的数据。
    provide函数: 根据接口函数提供的坐标，为逻辑函数注入上下文中的数据。
    recover函数: 根据接口函数提供的坐标，回收逻辑函数声明需要回收的数据。recover函数必须返回'回收之后的数据'，所谓'回收之后的数据'有可能在要求回收的数据上附加了回收时在上下文中产生的'坐标'信息。
    数据上下文:   数据上下文包含provide和recover函数，表示一个数据来源，此外还包含一个用来判断自己是否适用于某个key的函数，以及为了方便复用加上的ops选项和use-key?选项。
    key:        应用中要求注入和回收的数据都以key来定位适用的数据上下文或参数上下文。

## Usage


```clojure

```


