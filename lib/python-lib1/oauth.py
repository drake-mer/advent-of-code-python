import requests
import lxml


"""
Summary:
    Advent
"""


def google(*args, **kwargs):
    """
    1. Make a GET HTTP request to https://adventofcode.com/auth/google
    2. Redirect location (302) to
    https://accounts.google.com/o/oauth2/auth?client_id=298437688853-bsj22m2o7foi8713ff0hl03hs020b6pb.apps.googleusercontent.com&duration=temporary&redirect_uri=https%3A%2F%2Fadventofcode.com%2Fauth%2Fgoogle%2Fcallback&response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=x -> HTTP 200
    3. Do a signing POST on https://accounts.google.com/_/signin/oauth?authuser=0&hl=en&_reqid=53745&rt=j
    4. Generate a callback on https://adventofcode.com/auth/google/callback?state=x&code=4/0AX4XfWhIyhGrscLYawygIsMd9b3JVpCKxvfnvf5YGjT-N3rTLty76eBi8Awruqod1FT4ow&scope=profile%20https://www.googleapis.com/auth/userinfo.profile which
       then as a session cookie
    5. Do a get https://adventofcode.com/2021/auth/login with session cookie
    6. Get a redirect on https://adventofcode.com/2021
    """

    raise NotImplementedError()


def github(*args, **kwargs):
    """
    1. Make a GET HTTP request to https://adventofcode.com/auth/github
    2. Get redirected to
       https://github.com/login/oauth/authorize?client_id=7bb0a7ec13388aa67963&duration=temporary&redirect_uri=https://adventofcode.com/auth/github/callback&response_type=code&scope=&state=x
    3. Get redirected to
       https://adventofcode.com/auth/github/callback?code=d416bc58496b1cef6f9b&state=x
       -> answer set a `session` cookie
    4. Get redirected to
       https://adventofcode.com/2021/auth/login
    5. Get redirected to /2021
    """
    response_one = requests.get(
        "https://adventofcode.com/auth/github", allow_redirects=False
    )
    response_two = requests.get(response_one.headers["location"], allow_redirects=False)
    response_three = requests.get(
        response_two.headers["location"], allow_redirects=False
    )
    raise NotImplementedError()


def get_session():
    return "53616c7465645f5f88f0895810bc906c06ac49bc244a5cb4cd480ab6e7d02fd73e23a553ab75ef1239bfee5b21317e844ce348263c6feebba4b52ed34045feb8"


def get_problem(year, day):
    page = requests.get(
        f"https://adventofcode.com/{year}/day/{day}", cookies={"session": get_session()}
    ).content
    return page
