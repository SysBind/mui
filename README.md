# Moodle UI


## Development Env

Put a moodle  directory along side mui:
mui/
moodle/

install the Restful plugin for moodle:
https://github.com/SysBind/moodle-webservice_restful

under moodle/webservices

inside moodle code directory:
```php -S 0.0.0.0:8080```

enable mobile web services in moodle.
enable a user with some courses and generate token for this user,
put it in the head of src/App.elm

inside mui:
```make test```

browse to http://localhost:8080/index.html


