## Claude Code Sub-Agents 

Claude Code supports specialized sub-agents that handle specific tasks with custom system prompts, tools, and separate context windows. Sub-agents are AI assistants that your primary Claude Code agent can delegate tasks to.

### Understanding Sub-Agents: System Prompts, Not User Prompts

**Critical Concept**: The content in agent files (`.claude/agents/*.md`) are **system prompts** that configure the sub-agent's behavior. They are NOT user prompts. This is the #1 misunderstanding when creating agents.

**Information Flow**:
```
You (User) → Primary Agent → Sub-Agent → Primary Agent → You (User)
```

1. **You** make a request to Claude Code (primary agent)
2. **Primary Agent** analyzes your request and delegates to appropriate sub-agent
3. **Sub-Agent** executes task using its system prompt instructions
4. **Sub-Agent** reports results back to primary agent
5. **Primary Agent** synthesizes and presents results to you

**Key Points**:
- Sub-agents NEVER communicate directly with you
- Sub-agents start fresh with no conversation history
- Sub-agents respond to the primary agent's prompt, not yours
- The `description` field tells the primary agent WHEN to use the sub-agent

**Storage Hierarchy**:
- **Project agents**: `.claude/agents/` (higher priority, project-specific)
- **User agents**: `~/.claude/agents/` (lower priority, available across all projects)
- **Format**: Markdown files with YAML frontmatter


**Agent File Structure:**
```yaml
---
name: agent-name
description: When to use this agent (critical for automatic delegation)
tools: Tool1, Tool2, Tool3  # Optional - inherits all tools if omitted
color: Cyan  # Visual identifier in terminal
model: opus # Optional - haiku | sonnet | opus - defaults to sonnet
---

# Purpose
You are a [role definition]. 

## Instructions
1. Step-by-step instructions
2. What the agent should do
3. How to report results

## Report/Response Format
Specify how the agent should communicate results back to the primary agent.
```

Sub-agents enable:
- **Task specialization** - Code reviewers, debuggers, test runners
- **Context preservation** - Each agent operates independently  
- **Tool restrictions** - Grant only necessary permissions
- **Automatic delegation** - Claude proactively uses the right agent

### Key Engineering Insights

**Two Critical Mistakes to Avoid:**

1. **Misunderstanding the System Prompt** - What you write in agent files is the *system prompt*, not a user prompt. This changes how you structure instructions and what information is available to the agent.

2. **Ignoring Information Flow** - Sub-agents respond to your primary agent, not to you. Your primary agent prompts sub-agents based on your original request, and sub-agents report back to the primary agent, which then reports to you.

**Best Practices:**
- Use the `description` field to tell your primary agent *when* and *how* to prompt sub-agents
- Include phrases like "use PROACTIVELY" or trigger words (e.g., "if they say TTS") in descriptions
- Remember sub-agents start fresh with no context - be explicit about what they need to know
- Follow Problem → Solution → Technology approach when building agents

### Complex Workflows & Agent Chaining

Claude Code can intelligently chain multiple sub-agents together for complex tasks:

For example:
- "First analyze the market with crypto-market-agent, then use crypto-investment-plays to find opportunities"
- "Use the debugger agent to fix errors, then have the code-reviewer check the changes"
- "Generate a new agent with meta-agent, then test it on a specific task"

This chaining allows you to build sophisticated workflows while maintaining clean separation of concerns.