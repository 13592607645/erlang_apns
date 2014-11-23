# Erlang_apns #

这份软件是用**Erlang**语言写的，可用于**APNs**的工具软件。

这份软件是修改自[**Apns4erl**](https://github.com/inaka/apns4erl "An APNs provider for Apple Push Notificaion services (APNs) in Erlang.")。因为原软件功能比较完整，而我的实际需要只是其中的一小部分。 我是想通过一种最简单的方式，用Erlang语言操作APNs。

## 使用说明 ##
1. 导出证书：
导出带私钥的APNs证书，保存为**.p12**格式的文件。

2. 转换证书：
使用**priv/p12-pem.sh**工具转换.p12格式的证书为.pem格式的证书。转换后的证书不包含私钥密码。

3. 配置参数：
默认的开发模式的证书文件是**priv/cert.sandbox.pem**，默认的发布模式的证书文件是**priv/cert.pem**，默认的证书文件不设密码。

4. 推送消息：
目前只能推送文字内容，最多几十个汉字吧（iOS8下会多一些）。下面是示例代码（Token表示自己的设备token，请使用**UTF-8**编码格式）。
```erlang
    application:ensure_all_started(apns),
    apns:connect(myid),
    apns:send_message(myid, Token, list_to_binary("你好！测试。")).
```

## 测试说明 ##
指定设备token的方式有两种：一种是将设备token字符串（64个十六进制字符）写入**priv**目录下相应的**test.token**或**test.sandbox.token**文件，另一种是修改**test/apns_test.erl**文件，替换文件顶部的TOKEN为自己的设备token。然后准备好证书之后，执行`make test`测试推送。
