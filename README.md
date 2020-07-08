# tunnel
A socks5 tunnel

## 使用方法

    在服务器上，使用default.json.server中的配置参数
    在客户端上，使用default.json.client中的配置参数
    然后将文件改名为default.json
    
secret 需要在服务器和客户端上一致，代表密码
在客户端上，需要自行替换192.168.0.1为真实的ip地址，server_port为目标服务器的UDP
端口，port为本地socks5服务器端口

支持随机负载均衡，如果在客户端的server_host中配置多个IP，客户端会随机分配到这些
IP上
