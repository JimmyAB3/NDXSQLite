# Example 47: Feature Flags Database

This example demonstrates how to implement a feature flag/toggle system using SQLite with NDXSQLite.

## Features Demonstrated

- **Feature Toggles**: Enable/disable features dynamically
- **Percentage Rollouts**: Gradual feature deployment
- **User Targeting**: Enable features for specific users
- **Group Targeting**: Enable features for user groups
- **Environment Support**: Different flags per environment
- **A/B Testing**: Support for experiment variants
- **Change History**: Complete audit trail of flag changes

## Database Schema

### Tables

1. **feature_flags**: Core flag definitions
   - Name and description
   - Enabled state
   - Rollout percentage (0-100)
   - Environment targeting
   - Optional expiration

2. **flag_user_rules**: User-specific overrides
   - Force enable/disable per user

3. **flag_group_rules**: Group-based targeting
   - Enable features for user groups

4. **user_groups**: User group membership

5. **flag_history**: Change audit trail

6. **ab_test_variants**: A/B test configuration

7. **user_variants**: Sticky variant assignments

## Key Operations

### Create Feature Flag
```pascal
CreateFlag('new_checkout', 'New checkout flow', False, 100, 'prod');
```

### Check Feature for User
```pascal
if IsFeatureEnabled('new_checkout', 'user123', 'prod') then
  ShowNewCheckout
else
  ShowOldCheckout;
```

### Percentage Rollout
```pascal
// Enable for 30% of users
CreateFlag('new_ui', 'New UI', True, 30);

// Increase to 70%
SetRolloutPercentage('new_ui', 70);
```

### User Targeting
```pascal
// Enable for specific users
AddUserRule('premium_feature', 'premium_user_1', True);

// Block specific user
AddUserRule('feature', 'blocked_user', False);
```

### Group Targeting
```pascal
AddUserToGroup('alice', 'beta_testers');
AddGroupRule('experimental_feature', 'beta_testers', True);
```

## Evaluation Priority

1. **User Rules**: Highest priority - user-specific overrides
2. **Group Rules**: Group membership rules
3. **Percentage Rollout**: Deterministic hash-based rollout
4. **Environment Check**: Must match configured environment

## Percentage Rollout Logic

The rollout percentage is deterministic based on user ID hash:
- Same user always gets same result for same flag
- Distribution is statistically even across users
- Increasing percentage includes previous users

## Build and Run

```bash
cd 47_FeatureFlags
fpc FeatureFlags.lpr
./FeatureFlags
```

## Cross-Platform

This example works on Windows, Linux, and macOS.

## Use Cases

- Gradual feature rollouts
- A/B testing
- Kill switches for problematic features
- Beta testing with specific users
- Environment-specific features
- Premium feature gating
- Operational toggles
