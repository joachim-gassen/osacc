from os import environ

ROOMS = [
    dict(
        name='pre_test',
        display_name='Pre-test',
        participant_label_file='_rooms/pre_test.txt',
        use_secure_urls=False
    ),
    dict(
        name='science_study',
        display_name='Science Study',
        participant_label_file='_rooms/science_study.txt',
        use_secure_urls=False
    ),
]

SESSION_CONFIGS = [
    {
        'name': 'science_study_main_test',
        'app_sequence': ['instructions', 'science_study', 'epq', 'end'],
        'num_demo_participants': 20,
        'display_name': "Science Study - Main test",
        'control': None,
        'private': None,
        'journal': 0,
        'full_random': 0.5
    },
    {
        'name': 'science_study_random',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 6,
        'display_name': "Science Study - Random Treatment",
        'control': None,
        'private': None,
        'journal': None,
        'full_random': 1
    },
    {
        'name': 'science_study_ctrl_private',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 4,
        'display_name': "Science Study - Control and Private",
        'control': 1,
        'private': 1,
        'journal': 0,
        'full_random': 0
    },
    {
        'name': 'science_study_ctrl_public',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 4,
        'display_name': "Science Study - Control and Public",
        'control': 1,
        'private': 0,
        'journal': 0,
        'full_random': 0
    },
    {
        'name': 'science_study_collab_private',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 4,
        'display_name': "Science Study - Collab and Private",
        'control': 0,
        'private': 1,
        'journal': 0,
        'full_random': 0
    },
    {
        'name': 'science_study_collab_public',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 4,
        'display_name': "Science Study - Collab and Public",
        'control': 0,
        'private': 0,
        'journal': 0,
        'full_random': 0
    },
    {
        'name': 'science_study_ctrl_public_j',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 4,
        'display_name': "Science Study - Control, Public, Journal",
        'control': 1,
        'private': 0,
        'journal': 1,
        'full_random': 0
    },
    {
        'name': 'science_study_collab_public_j',
        'app_sequence': ['instructions', 'science_study', 'epq'],
        'num_demo_participants': 4,
        'display_name': "Science Study - Collab, Public, Journal",
        'control': 0,
        'private': 0,
        'journal': 1,
        'full_random': 0
    },
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=1.00, participation_fee=0.00, doc=""
)

PARTICIPANT_FIELDS = [
    'share',
    'control_treatment',
    'private_treatment',
    'journal_treatment',
    'dofus'
]
SESSION_FIELDS = []

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """ """

SECRET_KEY = '9915744037387'
